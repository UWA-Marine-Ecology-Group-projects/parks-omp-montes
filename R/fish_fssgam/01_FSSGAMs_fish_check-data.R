###
# Project: Parks - Abrolhos
# Data:    BOSS fish, habitat
# Task:    Check predictors and combine data for FSSgam fish length and abundance
# author:  Claude
# date:    Nov-Dec 2021
##

# Part 1-FSS modeling----
rm(list=ls())

## librarys----
detach("package:plyr", unload=TRUE)#will error - don't worry
library(tidyr)
library(dplyr)
options(dplyr.width = Inf) #enables head() to display all coloums
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

## set study name
study <- "montebello.synthesis" 
name <- study

## Set your working directory ----
working.dir <- getwd()
setwd(working.dir)

maxn <- read.csv("data/tidy/montebello.synthesis.checked.maxn.csv")%>%
  mutate(scientific=paste(genus,species,sep=" "))%>%
  dplyr::select(campaignid, sample, family, species, genus, scientific, maxn)

names(maxn)

habitat <- readRDS("data/tidy/merged_habitat.rds")%>%
  dplyr::rename('tpi'='layer_tpi',
                'slope'='layer_slope',
                'roughness'='layer_roughness',
                'detrended'='layer_detrended',
                'bathy_depth'='layer_depth',)%>%
  glimpse()

metadata <- read.csv('data/tidy/montebello.synthesis.checked.metadata.csv')%>%
  glimpse()

# look at top species ----
  maxn.sum<-maxn%>%
    group_by(scientific)%>%
    dplyr::summarise(maxn=sum(maxn))%>%
    ungroup()%>%
    top_n(15)%>%                   #Claude added this - what is the point of plotting 6 billion species you cannot read the names of...
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

# Create abundance of all recreational fished species ----
url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?ts=5e6f36e2#gid=825736197"

master<-googlesheets4::read_sheet(url)%>%
  ga.clean.names()%>%
  filter(grepl('Australia', global.region))%>% # Change country here
  dplyr::select(family,genus,species,fishing.type,australian.common.name,minlegal.wa)%>%
  distinct()%>%
  glimpse()

unique(master$fishing.type)

fished.species <- maxn %>%
  dplyr::left_join(master) %>%
  dplyr::mutate(fishing.type = ifelse(scientific %in%c("Serranidae Plectropomus spp")
                                      ,"R",fishing.type))%>%
  dplyr::filter(fishing.type %in% c("B/R","B/C/R","R","C/R"))%>% 
  dplyr::filter(!species %in% c('Loxodon macrorhinus'))%>%          #removed Loxodon macrorhinus
  glimpse()
  
unique(fished.species$scientific)

# Come back to maybe getting rid of some of these, but for now we continue on
fished.maxn <- fished.species %>%
  dplyr::ungroup() %>%
  dplyr::group_by(scientific,sample) %>%
  dplyr::summarise(maxn = sum(maxn)) %>%
  spread(scientific,maxn, fill = 0) %>%
  dplyr::mutate(targeted.abundance=rowSums(.[,2:(ncol(.))],na.rm = TRUE )) %>% #Add in Totals
  dplyr::select(sample,targeted.abundance) %>%
  gather(.,"response","number",2:2) %>%
  dplyr::glimpse()

# Pick top 3 species                      second most abundant species is 'unknown spp'
species.maxn <- maxn %>%
  dplyr::filter(scientific %in% c("Pomacentrus coelestis",
                                  "Chromis fumea",
                                  "Lethrinus atkinsoni"
  ))%>%
  dplyr::select(sample,scientific,maxn) %>%
  dplyr::rename('number' = maxn)%>%
  dplyr::mutate(response = scientific)%>%
  distinct()

dat.maxn <- bind_rows(fished.maxn, species.maxn, 
                           ta.sr)%>%
  left_join(habitat) %>%
   left_join(metadata) %>%
  drop_na(fieldofview.open)%>%
  distinct()

#Export the data to .rds for use in next script
saveRDS(dat.maxn, "data/tidy/dat.maxn.rds")

#Import data for lengths
length <- read.csv("data/tidy/montebello.synthesis.checked.length.csv")%>%
  mutate(scientific=paste(family,genus,species))%>%
  glimpse()

metadata <- read.csv('data/tidy/montebello.synthesis.checked.metadata.csv')


# Create abundance of all recreational fished species ----
fished.species <- length %>%
  dplyr::left_join(master) %>%
  dplyr::mutate(fishing.type = ifelse(scientific %in%c("Serranidae Plectropomus spp"),"C/R",fishing.type))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Serranidae Plectropomus spp"), "450", minlegal.wa))%>%
  dplyr::filter(fishing.type %in% c("B/R","B/C/R","R","C/R"))%>% 
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


#Atkinsoni
atkinsoni.legal <- fished.species %>%
  dplyr::filter(species%in%c("atkinsoni")) %>%
  dplyr::filter(length>minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "legal size atkinsoni") %>%
  dplyr::glimpse()

atkinsoni.sublegal <- fished.species %>%
  dplyr::filter(species%in%c("atkinsoni")) %>%
  dplyr::filter(length<minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "sublegal size atkinsoni") %>%
  dplyr::glimpse() 

#plectropomus
plectropomus.legal <- fished.species %>%
  dplyr::filter(genus%in%c("Plectropomus")) %>%
  dplyr::filter(length>minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "legal size trout") %>%
  dplyr::glimpse()

plectropomus.sublegal <- fished.species %>%
  dplyr::filter(genus%in%c("Plectropomus")) %>%
  dplyr::filter(length<minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "sublegal size trout") %>%
  dplyr::glimpse() 

#nebulosus
nebulosus.legal <- fished.species %>%
  dplyr::filter(species%in%c("nebulosus")) %>%
  dplyr::filter(length>minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "legal size spango") %>%
  dplyr::glimpse()

nebulosus.sublegal <- fished.species %>%
  dplyr::filter(species%in%c("nebulosus")) %>%
  dplyr::filter(length<minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "sublegal size spango") %>%
  dplyr::glimpse() 

#lethrinids
lethrinid.legal <- fished.species %>%
  dplyr::filter(genus%in%c("Lethrinus")) %>%
  dplyr::filter(length>minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "legal size emperor") %>%
  dplyr::glimpse()

lethrinid.sublegal <- fished.species %>%
  dplyr::filter(genus%in%c("Lethrinus")) %>%
  dplyr::filter(length<minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "sublegal size emperor") %>%
  dplyr::glimpse() 

combined.length <- bind_rows(legal, sublegal, atkinsoni.legal, atkinsoni.sublegal, plectropomus.legal, plectropomus.sublegal,
                             nebulosus.legal, nebulosus.sublegal, lethrinid.legal, lethrinid.sublegal)

unique(combined.length$scientific)

dat.length <- combined.length %>%
  dplyr::right_join(metadata, by = c("sample")) %>% # add in all samples
  dplyr::select(sample,scientific,number) %>%
  tidyr::complete(nesting(sample), scientific) %>%
  replace_na(list(number = 0)) %>% #we add in zeros - in case we want to calculate abundance of species based on a length rule (e.g. greater than legal size)
  dplyr::ungroup()%>%
  dplyr::filter(!is.na(scientific)) %>% # this should not do anything
  dplyr::left_join(.,metadata) %>%
  dplyr::left_join(.,habitat) %>%
  dplyr::filter(successful.length%in%c("Yes")) %>%
  dplyr::mutate(scientific=as.character(scientific)) %>%
  dplyr::rename(response = scientific)%>%
  drop_na(fieldofview.open)%>%
  dplyr::glimpse()

saveRDS(dat.length, "data/tidy/dat.length.rds")

#Set and check predictor variables - these don't change so just doing for lengths
names(dat.length)
names(habitat)

pred.vars=c("depth","biota.unconsolidated", "biota.macroalgae", "biota.crinoids",
            "biogenic.reef", "biota.octocoral.black", "biota.consolidated", "biota.sponges", "biota.hydroids", "biota.stony.corals",
            "mean.relief", "sd.relief", "tpi", "roughness", "slope","detrended", "bathy_depth") 

# Check for correalation of predictor variables- remove anything highly correlated (>0.95)---
round(cor(dat.length[,pred.vars]),2)
#reef and sand
#roughness and slope
#depth and bathy_depth

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

# # Re-set the predictors for modeling----
pred.vars=c("depth","mean.relief","sd.relief","biogenic.reef","biota.macroalgae","biota.consolidated", "tpi", "roughness","detrended") 

#Export this to use in the next script? Or could just remove columns - but then have to re run later...