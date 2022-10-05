###
# Project: Parks - montes
# Data:    BRUV fish, habitat
# Task:    Check predictors and combine data for FSSgam fish length and abundance
# author:  Claude
# date:    April 2022
##

# Part 1-FSS modeling----
rm(list=ls())

## librarys----
# detach("package:plyr", unload=TRUE)#will error - don't worry
library(tidyr)
library(dplyr)
# options(dplyr.width = Inf) #enables head() to display all coloums
library(mgcv)
library(MuMIn)
library(car)
library(doBy)
library(gplots)
library(RColorBrewer)
library(doParallel) #this can removed?
library(doSNOW)
library(gamm4)
library(RCurl) #needed to download data from GitHub
library(GlobalArchive)
library(ggplot2)
library(devtools)
library(FSSgam)
library(sf)
library(purrr)

## set study name
study <- "montebello.synthesis" 
name <- study

maxn <- read.csv("data/tidy/montebello.synthesis.complete.maxn.csv")%>%
  mutate(scientific=paste(genus, species, sep= " "))%>%
  dplyr::select(campaignid, sample, family, species, genus, scientific, maxn) %>%
  glimpse()

length(unique(maxn$sample))

names(maxn)

habitat <- readRDS("data/tidy/broad_merged_habitat.rds")%>%
  glimpse()

metadata.maxn <- read.csv('data/tidy/montebello.synthesis.checked.metadata.csv')%>%
  dplyr::filter(successful.count %in% c("Yes"))%>%
  glimpse()

check <- metadata.maxn %>%
  dplyr::anti_join(maxn)%>%
  glimpse()

# look at top species ----
maxn.sum <- maxn%>%
    group_by(scientific)%>%
    dplyr::summarise(maxn=sum(maxn))%>%
    ungroup()%>%
    top_n(15)%>%                   
    ungroup()

## Total frequency of occurance
ggplot(maxn.sum, aes(x=reorder(scientific,maxn), y=maxn)) +   
    geom_bar(stat="identity",position=position_dodge())+
    coord_flip()+
    xlab("Species")+
    ylab(expression(Overall~abundance~(Sigma~MaxN)))+
    #Theme1+
    theme(axis.text.y = element_text(face="italic"))+
    #theme_collapse+
    scale_y_continuous(expand = expand_scale(mult = c(0, .1)))#+

# Create total abundance and species richness ----
ta.sr <- maxn %>%
  dplyr::ungroup() %>%
  dplyr::group_by(scientific,sample) %>%
  dplyr::summarise(maxn = sum(maxn)) %>%
  tidyr::spread(scientific,maxn, fill = 0) %>%
    dplyr::mutate(total.abundance=rowSums(.[,2:(ncol(.))],na.rm = TRUE )) %>% #Add in Totals
    dplyr::mutate(species.richness=rowSums(.[,2:(ncol(.))] > 0)) %>% # double check these
    dplyr::select(sample,total.abundance,species.richness) %>%
    tidyr::gather(.,"response","number",2:3) %>%
  dplyr::glimpse()

# # Create abundance of all recreational fished species ----
# url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?ts=5e6f36e2#gid=825736197"
# 
# master<-googlesheets4::read_sheet(url)%>%
#   ga.clean.names()%>%
#   filter(grepl('Australia', global.region))%>% # Change country here
#   dplyr::select(family,genus,species,fishing.type,australian.common.name,minlegal.wa)%>%
#   distinct()%>%
#   glimpse()

# unique(master$fishing.type)
# 
# fished.species <- maxn %>%
#   dplyr::left_join(master) %>%
#   dplyr::mutate(fishing.type = ifelse(scientific %in%c("Plectropomus spp","Scomberomorus spp","Sillago spp",
#                                                        "Herklotsichthys spp","Lethrinus spp")
#                                       ,"R",fishing.type))%>%
#   dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Plectropomus spp"), "450", minlegal.wa))%>%
#   dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Scomberomorus spp"), "900", minlegal.wa))%>%
#   dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Lethrinus spp"), "280", minlegal.wa))%>%
#   dplyr::filter(fishing.type %in% c("B/R","B/C/R","R","C/R"))%>% 
#   dplyr::filter(!species %in% c('Loxodon macrorhinus'))%>%          #removed Loxodon macrorhinus - maybe remove herklotsichthys if it cooks everything
#   glimpse()
#   
# unique(fished.species$scientific)

# Come back to maybe getting rid of some of these, but for now we continue on
# fished.maxn <- fished.species %>%
#   dplyr::ungroup() %>%
#   dplyr::group_by(scientific,sample) %>%
#   dplyr::summarise(maxn = sum(maxn)) %>%
#   spread(scientific,maxn, fill = 0) %>%
#   dplyr::mutate(targeted.abundance=rowSums(.[,2:(ncol(.))],na.rm = TRUE )) %>% #Add in Totals
#   dplyr::select(sample,targeted.abundance) %>%
#   gather(.,"response","number",2:2) %>%
#   dplyr::glimpse()

# Pick top 3 species                      second most abundant species is 'unknown spp'
# species.maxn <- maxn %>%
#   dplyr::filter(scientific %in% c("Pomacentrus coelestis",
#                                   "Chromis fumea",
#                                   "Lethrinus atkinsoni"
#   ))%>%
#   dplyr::select(sample,scientific,maxn) %>%
#   dplyr::rename('number' = maxn)%>%
#   dplyr::mutate(response = scientific)%>%
#   distinct()

dat.maxn <- bind_rows(ta.sr) %>% # fished.maxn, species.maxn, 
  dplyr::left_join(metadata.maxn) %>%
  dplyr::left_join(habitat) %>%
  drop_na(fieldofview.open) %>%                                                  #get rid of drops with no habitat
  glimpse()


wgscrs <- "+proj=longlat +datum=WGS84"
cwatr <- st_read("data/spatial/shape/coastal-waters_polygon.shp")
cwatr <- st_transform(cwatr, wgscrs)

maxn.sf <- dat.maxn %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = wgscrs)
maxn.sf <- maxn.sf %>%
  dplyr::mutate(longitude = unlist(map(maxn.sf$geometry, 1)),
                latitude = unlist(map(maxn.sf$geometry, 2)),
                state = lengths(st_intersects(maxn.sf, cwatr)) > 0) %>%
  glimpse()

dat.maxn <- as.data.frame(maxn.sf) %>%
  dplyr::select(-geometry) %>%
  dplyr::filter(state %in% "FALSE") %>%
  glimpse()

ggplot() +
  geom_sf(data = maxn.sf) +
  geom_sf(data = cwatr) +
  coord_sf(xlim = c(min(maxn.sf$longitude), max(maxn.sf$longitude)), 
           ylim = c(min(maxn.sf$latitude), max(maxn.sf$latitude)))

#Export the data to .rds for use in next script
saveRDS(dat.maxn, "data/tidy/dat.maxn.rds")

#Import data for lengths
length <- read.csv("data/tidy/montebello.synthesis.complete.length.csv")%>%
  mutate(scientific = paste(family,genus,species))%>%
  glimpse()

length(unique(length$sample)) #205 samples

metadata.length <- read.csv('data/tidy/montebello.synthesis.checked.metadata.csv')%>%
  dplyr::filter(successful.length %in% c('Yes'))%>%
  glimpse()

length(unique(metadata.length$sample)) #179

check <- metadata.length %>%
  dplyr::anti_join(length)%>%
  glimpse()

# Create abundance of all recreational fished species ----
url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?ts=5e6f36e2#gid=825736197"

master<-googlesheets4::read_sheet(url)%>%
  ga.clean.names()%>%
  filter(grepl('Australia', global.region))%>% # Change country here
  dplyr::select(family,genus,species,fishing.type,australian.common.name,minlegal.wa)%>%
  distinct()%>%
  glimpse()

# Create abundance of all recreational fished species ----
fished.species <- length %>%
  dplyr::left_join(master) %>%
  dplyr::mutate(fishing.type = ifelse(scientific %in%c("Plectropomus spp","Scomberomorus spp","Sillago spp",
                                                       "Herklotsichthys spp","Lethrinus spp")
                                      ,"R",fishing.type))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Plectropomus spp"), "450", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Scomberomorus spp"), "900", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Lethrinus spp"), "280", minlegal.wa))%>%
  dplyr::filter(!species %in% c('Loxodon macrorhinus'))%>%          #removed Loxodon macrorhinus
  glimpse()

unique(fished.species$scientific)

# Come back to maybe getting rid of some of these, but for now we continue on
without.min.length <- fished.species %>%
  filter(is.na(minlegal.wa))%>%
  distinct(scientific)  #there are heaps here without min legal sizes

legal <- fished.species %>%
  tidyr::replace_na(list(minlegal.wa=0)) %>%
  dplyr::filter(length>minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "greater than legal size") %>%
  dplyr::glimpse()

sublegal <- fished.species %>%
  dplyr::filter(length<minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "smaller than legal size") %>%
  dplyr::glimpse()


# #Atkinsoni
# atkinsoni.legal <- fished.species %>%
#   dplyr::filter(species%in%c("atkinsoni")) %>%
#   dplyr::filter(length>minlegal.wa) %>%
#   dplyr::group_by(sample) %>%
#   dplyr::summarise(number = sum(number)) %>%
#   dplyr::mutate(scientific = "legal size atkinsoni") %>%
#   dplyr::glimpse()
# 
# atkinsoni.sublegal <- fished.species %>%
#   dplyr::filter(species%in%c("atkinsoni")) %>%
#   dplyr::filter(length<minlegal.wa) %>%
#   dplyr::group_by(sample) %>%
#   dplyr::summarise(number = sum(number)) %>%
#   dplyr::mutate(scientific = "sublegal size atkinsoni") %>%
#   dplyr::glimpse() 
# 
# #plectropomus
# plectropomus.legal <- fished.species %>%
#   dplyr::filter(genus%in%c("Plectropomus")) %>%
#   dplyr::filter(length>minlegal.wa) %>%
#   dplyr::group_by(sample) %>%
#   dplyr::summarise(number = sum(number)) %>%
#   dplyr::mutate(scientific = "legal size trout") %>%
#   dplyr::glimpse()
# 
# plectropomus.sublegal <- fished.species %>%
#   dplyr::filter(genus%in%c("Plectropomus")) %>%
#   dplyr::filter(length<minlegal.wa) %>%
#   dplyr::group_by(sample) %>%
#   dplyr::summarise(number = sum(number)) %>%
#   dplyr::mutate(scientific = "sublegal size trout") %>%
#   dplyr::glimpse() 
# 
# #nebulosus
# nebulosus.legal <- fished.species %>%
#   dplyr::filter(species%in%c("nebulosus")) %>%
#   dplyr::filter(length>minlegal.wa) %>%
#   dplyr::group_by(sample) %>%
#   dplyr::summarise(number = sum(number)) %>%
#   dplyr::mutate(scientific = "legal size spango") %>%
#   dplyr::glimpse()
# 
# nebulosus.sublegal <- fished.species %>%
#   dplyr::filter(species%in%c("nebulosus")) %>%
#   dplyr::filter(length<minlegal.wa) %>%
#   dplyr::group_by(sample) %>%
#   dplyr::summarise(number = sum(number)) %>%
#   dplyr::mutate(scientific = "sublegal size spango") %>%
#   dplyr::glimpse() 

# #lethrinids
# lethrinid.legal <- fished.species %>%
#   dplyr::filter(genus%in%c("Lethrinus")) %>%
#   dplyr::filter(length>minlegal.wa) %>%
#   dplyr::group_by(sample) %>%
#   dplyr::summarise(number = sum(number)) %>%
#   dplyr::mutate(scientific = "legal size emperor") %>%
#   dplyr::glimpse()
# 
# lethrinid.sublegal <- fished.species %>%
#   dplyr::filter(genus%in%c("Lethrinus")) %>%
#   dplyr::filter(length<minlegal.wa) %>%
#   dplyr::group_by(sample) %>%
#   dplyr::summarise(number = sum(number)) %>%
#   dplyr::mutate(scientific = "sublegal size emperor") %>%
#   dplyr::glimpse() 

combined.length <- bind_rows(legal, sublegal)

unique(combined.length$scientific)

samples <- metadata.length %>%
  dplyr::select(sample)%>%
  glimpse()

dat.length <- combined.length %>%
  dplyr::right_join(metadata.length, by = c("sample")) %>% # add in all samples
  dplyr::select(sample,scientific,number) %>%
  tidyr::complete(nesting(sample), scientific) %>%
  replace_na(list(number = 0)) %>% #we add in zeros - in case we want to calculate abundance of species based on a length rule (e.g. greater than legal size)
  dplyr::ungroup()%>%
  dplyr::filter(!is.na(scientific)) %>% # this should not do anything
  dplyr::left_join(.,metadata.length) %>%
  dplyr::left_join(.,habitat) %>%
  dplyr::filter(successful.length%in%c("Yes")) %>%
  dplyr::mutate(scientific=as.character(scientific)) %>%
  dplyr::rename(response = scientific)%>%
  drop_na(fieldofview.open)%>%
  dplyr::glimpse()

check <- metadata.length %>%
  dplyr::anti_join(dat.length)%>%
  glimpse()
#there are two samples that are yeeted but they have no habitat

length.sf <- dat.length %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = wgscrs)
length.sf <- length.sf %>%
  dplyr::mutate(longitude = unlist(map(length.sf$geometry, 1)),
                latitude = unlist(map(length.sf$geometry, 2)),
                state = lengths(st_intersects(length.sf, cwatr)) > 0) %>%
  glimpse()

dat.length <- as.data.frame(length.sf) %>%
  dplyr::select(-geometry) %>%
  dplyr::filter(state %in% "FALSE") %>%
  glimpse()

saveRDS(dat.length, "data/tidy/dat.length.rds")

#Set and check predictor variables - these don't change so just doing for lengths
names(dat.maxn)
names(habitat)

pred.vars=c("depth","biota.unconsolidated",
            "photic.reef","mesophotic.reef", 
            "mean.relief", "sd.relief", "tpi", 
            "roughness", "detrended", "depth_ga") 

# Check for correalation of predictor variables- remove anything highly correlated (>0.95)---
round(cor(dat.maxn[,pred.vars]),2)
#reef and sand
#roughness and slope
#depth and bathy_depth
#macroalgae and photic reef - photic reef is just coral + macroalgae so remove macroalgae

# Plot of likely transformations - thanks to Anna Cresswell for this loop!
par(mfrow=c(3,2))
for (i in pred.vars) {
  x<-dat.length[ ,i]
  x = as.numeric(unlist(x))
  hist((x))#Looks best
  plot((x),main = paste(i))
  hist(sqrt(x))
  plot(sqrt(x))
  hist(log(x+1))
  plot(log(x+1))
}

# Review of individual predictors - we have to make sure they have an even distribution---
#If the data are squewed to low numbers try sqrt>log or if squewed to high numbers try ^2 of ^3

#remove sand and slope
#also remove all 'non-reef' predictors

