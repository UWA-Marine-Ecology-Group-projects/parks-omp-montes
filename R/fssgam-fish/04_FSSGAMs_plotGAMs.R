###
# Project: Parks - Abrolhos
# Data:    BOSS fish, habitat
# Task:    Plotting fish importance GAM relationships
# author:  Claude
# date:    Nov-Dec 2021
##

rm(list=ls())

library(dplyr)
library(tidyr)
library(gridExtra)
library(grid)
library(GlobalArchive)
library(stringr)
library(ggplot2)
library(gamm4)
library(ggmap)
library(rgdal)
library(raster)
library(png)
library(cowplot)

# set theme
# Theme-
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # legend.background = element_rect(fill="white"),
    legend.background = element_blank(),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=10),
    legend.title = element_blank(),
    legend.position = c(0.2, 0.8),
    text=element_text(size=10),
    strip.text.y = element_text(size = 10,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=10),
    axis.title.y=element_text(vjust=0.6, angle=90, size=10),
    axis.text.x=element_text(size=10),
    axis.text.y=element_text(size=10),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank())

# Set the study name
name <- "montes.synthesis" # for the study

## Set working directory----
working.dir <- getwd()
setwd(working.dir)
#OR Set manually once

# Bring in and format the raw data----
#MaxN
maxn <- read.csv("data/Tidy/montebello.synthesis.checked.maxn.csv")%>%
  mutate(scientific=paste(genus,species,sep=" "))%>%
  dplyr::select(campaignid, sample, family, species, genus, scientific, maxn)

names(maxn)

length <- read.csv("data/Tidy/montebello.synthesis.checked.length.csv")%>%
  mutate(scientific=paste(family,genus,species))%>%
  glimpse()

habitat <- readRDS("data/Tidy/merged_habitat.rds")%>%
  glimpse()

metadata <- read.csv('data/Tidy/montebello.synthesis.checked.metadata.csv')%>%
  glimpse()

# look at top species ----
maxn.sum<-maxn%>%
  group_by(scientific)%>%
  dplyr::summarise(maxn=sum(maxn))%>%
  ungroup()%>%
  top_n(15)%>%                   #Claude added this - what is the point of plotting 6 billion species you cannot read the names of...
  ungroup()

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

combined.maxn <- bind_rows(fished.maxn, species.maxn, 
                 ta.sr)%>%
  left_join(habitat) %>%
  left_join(metadata) %>%
  drop_na(fieldofview.open)%>%
  distinct()

#move onto lengths
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

dat <- bind_rows(combined.maxn, combined.length)

###   TEMPLATE   ###
###              ###
###              ###
###              ###
# Manually make the most parsimonious GAM models for each taxa ----
#### montes MaxN ####
unique(dat$scientific)

# MODEL Total abundance (relief) ----
dat.total <- dat %>% filter(scientific=="total.abundance")

gamm=gam(maxn~s(relief,k=3,bs='cr'), family=tw,data=dat.total)

# predict - relief ----
mod<-gamm
testdata <- expand.grid(relief=seq(min(dat$relief),max(dat$relief),length.out = 20)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.total = testdata%>%data.frame(fits)%>%
  group_by(relief)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Total abundance ----
# status ----
ggmod.total<- ggplot() +
  ylab("")+
  xlab("Mean relief")+
  geom_point(data=dat.total,aes(x=relief,y=maxn),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.total,aes(x=relief,y=maxn),alpha=0.5)+
  geom_line(data=predicts.total,aes(x=relief,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.total,aes(x=relief,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Total abundance") +
  theme(plot.title = element_text(hjust = 0))
ggmod.total


# Combine with cowplot
library(cowplot)

# view plots
plot.grid.abundance <- plot_grid(ggmod.total, NULL,NULL,
                       ggmod.species.relief, ggmod.species.tpi,NULL,
                       ggmod.target.biog, ggmod.target.detrended, ggmod.target.macroalgae,
                       ncol = 3, labels = c('a','','','b','c','','d','e','f'),align = "vh")
plot.grid.abundance

plot.grid.lengths <- plot_grid( ggmod.greater.biog, ggmod.greater.detrended, ggmod.greater.macroalgae,
                                 ggmod.legal.miniatus.biog, ggmod.legal.miniatus.depth,NULL,
                                NULL,NULL,NULL,
                                 ncol = 3, labels = c('g','h','i','j','k','','','',''),align = "vh")
plot.grid.lengths

plot.grid.species <- plot_grid(ggmod.coris.depth,NULL, NULL,
                                 ggmod.miniatus.biog, ggmod.miniatus.depth,NULL,
                                 ggmod.chromis.relief,NULL,NULL,
                                 ncol = 3, labels = c('l','','','m','n','','o','',''),align = "vh")
plot.grid.species

#Save plots
save_plot("plots/abrolhos.boss.gam.abundance.png", plot.grid.abundance,base_height = 9,base_width = 8.5)
save_plot("plots/abrolhos.boss.gam.species.png", plot.grid.species,base_height = 9,base_width = 8.5)
save_plot("plots/abrolhos.boss.gam.lengths.png", plot.grid.lengths,base_height = 9,base_width = 8.5)