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
name <- "2021-05_Abrolhos_BOSS" # for the study

## Set working directory----
working.dir <- getwd()
setwd(working.dir)
#OR Set manually once

# Bring in and format the raw data----
#MaxN
maxn   <- read.csv("data/Tidy/2021-05_Abrolhos_BOSS.complete.maxn.csv")
names(maxn)

metadata <- maxn %>%
  distinct(sample, latitude, longitude, date, time, location, status, site, 
           depth, observer, successful.count, successful.length)

length <- read.csv("data/Tidy/2021-05_Abrolhos_BOSS.complete.length.csv")%>%
  mutate(scientific=paste(family,genus,species))

allhab <- readRDS("data/Tidy/merged_habitat.rds")%>%
  ga.clean.names()%>%
  glimpse()
allhab <- allhab %>%
  dplyr::filter(method%in%c('BOSS'))%>%
  transform(kelps = kelps / totalpts) %>%
  transform(macroalgae = macroalgae / totalpts) %>%
  transform(sand = sand / totalpts) %>%
  transform(rock = rock / totalpts) %>%
  transform(biog = biog / totalpts) %>%
  glimpse()

# look at top species ----
maxn.sum <- maxn %>%
  mutate(scientific = paste(genus, species, sep = " ")) %>%
  group_by(scientific) %>%
  dplyr::summarise(maxn = sum(maxn)) %>%
  ungroup()

# Create total abundance and species richness ----
ta.sr <- maxn %>%
  dplyr::ungroup() %>%
  dplyr::group_by(scientific,sample) %>%
  dplyr::summarise(maxn = sum(maxn)) %>%
  tidyr::spread(scientific, maxn, fill = 0) %>%
  dplyr::mutate(total.abundance = rowSums(.[, 2:(ncol(.))], na.rm = TRUE )) %>% #Add in Totals
  dplyr::mutate(species.richness = rowSums(.[, 2:(ncol(.))] > 0)) %>% # double check these
  dplyr::select(sample, total.abundance, species.richness) %>%
  tidyr::gather(., "scientific", "maxn", 2:3) %>%
  dplyr::glimpse()

# Create abundance of all recreational fished species ----
url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?ts=5e6f36e2#gid=825736197"

master <- googlesheets4::read_sheet(url) %>%
  ga.clean.names() %>%
  filter(grepl('Australia', global.region)) %>% # Change country here
  dplyr::select(family,genus,species,fishing.type,australian.common.name,minlegal.wa) %>%
  distinct() %>%
  glimpse()

unique(master$fishing.type)

fished.species <- maxn %>%
  dplyr::left_join(master) %>%
  dplyr::mutate(fishing.type = ifelse(
    scientific %in%c("Serranidae Plectropomus spp"),"R", fishing.type)) %>%
  dplyr::filter(fishing.type %in% c("B/R", "B/C/R", "R", "C/R")) %>%
  dplyr::filter(!family%in%c("Monacanthidae", "Scorpididae", "Mullidae"))       # Brooke removed leatherjackets, sea sweeps and goat fish

unique(fished.species$scientific)

# Come back to maybe getting rid of some of these, but for now we continue on
fished.maxn <- fished.species %>%
  dplyr::ungroup() %>%
  dplyr::group_by(scientific,sample) %>%
  dplyr::summarise(maxn = sum(maxn)) %>%
  spread(scientific,maxn, fill = 0) %>%
  dplyr::mutate(targeted.abundance=rowSums(.[, 2:(ncol(.))], na.rm = TRUE )) %>% #Add in Totals
  dplyr::select(sample,targeted.abundance) %>%
  gather(.,"scientific","maxn",2:2) %>%
  dplyr::glimpse()

# Pick top species
species.maxn <- maxn %>%
  dplyr::filter(scientific %in% c("Pomacentridae Chromis westaustralis",
                                  "Labridae Coris auricularis",
                                  "Chaetodontidae Chaetodon assarius",
                                  "Lethrinidae Lethrinus miniatus"
  ))%>%
  dplyr::select(sample,scientific,maxn) %>%
  distinct()

combined.maxn <- bind_rows(fished.maxn, species.maxn, ta.sr) %>%
  left_join(allhab) %>%
  left_join(metadata) %>%
  distinct()

glimpse(combined.maxn)

master<-googlesheets4::read_sheet(url)%>%
  ga.clean.names()%>%
  filter(grepl('Australia', global.region))%>% # Change country here
  dplyr::select(family,genus,species,fishing.type,australian.common.name,minlegal.wa)%>%
  distinct()%>%
  glimpse()

unique(master$fishing.type)

fished.species <- length %>%
  dplyr::left_join(master) %>%
  dplyr::mutate(fishing.type = ifelse(scientific %in%c("Serranidae Plectropomus spp")
                                      ,"R",fishing.type))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Serranidae Plectropomus spp"), "450", minlegal.wa))%>%
  dplyr::filter(fishing.type %in% c("B/R","B/C/R","R","C/R"))%>%
  dplyr::filter(!family%in%c("Monacanthidae", "Scorpididae", "Mullidae")) # Brooke removed leatherjackets, sea sweeps and goat fish

unique(fished.species$scientific)

# Come back to maybe getting rid of some of these, but for now we continue on
# fished.maxn <- fished.species %>%
#   dplyr::ungroup() %>%
#   dplyr::group_by(scientific,sample) %>%
#   dplyr::summarise(maxn = sum(maxn)) %>%
#   spread(scientific,maxn, fill = 0) %>%
#   dplyr::mutate(targeted.abundance=rowSums(.[,2:(ncol(.))],na.rm = TRUE )) %>% #Add in Totals
#   dplyr::select(sample,targeted.abundance) %>%
#   gather(.,"scientific","maxn",2:2) %>%
#   dplyr::glimpse()

without.min.length <- fished.species %>%
  filter(is.na(minlegal.wa))%>%
  distinct(scientific) # only charlies don't have one
unique(without.min.length$scientific)

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

miniatus.legal <- fished.species %>%
  dplyr::filter(species%in%c("miniatus")) %>%
  dplyr::filter(length>minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "legal size red throat") %>%
  dplyr::glimpse()

miniatus.sublegal <- fished.species %>%
  dplyr::filter(species%in%c("miniatus")) %>%
  dplyr::filter(length<minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "sublegal size red throat") %>%
  dplyr::glimpse() # only two of these

combined.length <- bind_rows(legal, sublegal, miniatus.legal, miniatus.sublegal) # add pink snapper and other indicator species

unique(combined.length$scientific)

complete.length <- combined.length %>%
  dplyr::right_join(metadata, by = c("sample")) %>% # add in all samples
  dplyr::select(sample,scientific,number) %>%
  tidyr::complete(nesting(sample), scientific) %>%
  replace_na(list(number = 0)) %>% #we add in zeros - in case we want to calulate abundance of species based on a length rule (e.g. greater than legal size)
  dplyr::ungroup()%>%
  dplyr::filter(!is.na(scientific)) %>% # this should not do anything
  dplyr::left_join(.,metadata) %>%
  dplyr::left_join(.,allhab) %>%
  dplyr::filter(successful.length%in%c("Y")) %>%
  dplyr::mutate(scientific=as.character(scientific)) %>%
  dplyr::rename("maxn"="number")%>%
  dplyr::glimpse()

dat <- bind_rows(combined.maxn, complete.length)

# Manually make the most parsimonious GAM models for each taxa ----
#### Abrolhos MaxN ####
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

# MODEL Species richness (relief + tpi) ----
dat.species <- dat %>% filter(scientific=="species.richness")

gamm=gam(maxn~s(relief,k=3,bs='cr')+s(tpi, k = 3, bs = 'cr'), family=tw,data=dat.species)

# predict - relief ----
mod<-gamm
testdata <- expand.grid(relief=seq(min(dat$relief),max(dat$relief),length.out = 20),
                        tpi=mean(mod$model$tpi)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.species.relief = testdata%>%data.frame(fits)%>%
  group_by(relief)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
# predict - tpi ----
mod<-gamm
testdata <- expand.grid(tpi=seq(min(dat$tpi),max(dat$tpi),length.out = 20),
                        relief=mean(mod$model$relief)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.species.tpi = testdata%>%data.frame(fits)%>%
  group_by(tpi)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Species richness ----
# relief ----
ggmod.species.relief<- ggplot() +
  ylab("")+
  xlab("Mean relief")+
  geom_point(data=dat.species,aes(x=relief,y=maxn),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.species.relief,aes(x=relief,y=maxn),alpha=0.5)+
  geom_line(data=predicts.species.relief,aes(x=relief,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.species.relief,aes(x=relief,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Species richness") +
  theme(plot.title = element_text(hjust = 0))
ggmod.species.relief

ggmod.species.tpi<- ggplot() +
  ylab("Abundance")+
  xlab("TPI")+
  geom_point(data=dat.species,aes(x=tpi,y=maxn),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.species.tpi,aes(x=tpi,y=maxn),alpha=0.5)+
  geom_line(data=predicts.species.tpi,aes(x=tpi,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.species.tpi,aes(x=tpi,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.species.tpi

# MODEL Targeted abundance (biog + detrended + macroalgae) ----
dat.target <- dat %>% filter(scientific=="targeted.abundance")

gamm=gam(maxn~s(biog,k=3,bs='cr')+s(detrended, k = 3, bs = 'cr')+s(macroalgae, k = 3, bs = 'cr'), family=tw,data=dat.target)

# predict - biog ----
mod<-gamm
testdata <- expand.grid(biog=seq(min(dat$biog),max(dat$biog),length.out = 20),
                        detrended=mean(mod$model$detrended),
                        macroalgae=mean(mod$model$macroalgae)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.target.biog = testdata%>%data.frame(fits)%>%
  group_by(biog)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - detrended ----
mod<-gamm
testdata <- expand.grid(detrended=seq(min(dat$detrended),max(dat$detrended),length.out = 20),
                        biog=mean(mod$model$biog),
                        macroalgae=mean(mod$model$macroalgae)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.target.detrended = testdata%>%data.frame(fits)%>%
  group_by(detrended)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - macroalgae ----
mod<-gamm
testdata <- expand.grid(macroalgae=seq(min(dat$macroalgae),max(dat$macroalgae),length.out = 20),
                        biog=mean(mod$model$biog),
                        detrended=mean(mod$model$detrended)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.target.macroalgae = testdata%>%data.frame(fits)%>%
  group_by(macroalgae)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Targeted abundance ----
# biogenic reef ----
ggmod.target.biog<- ggplot() +
  ylab("")+
  xlab("Biogenic reef")+
  geom_point(data=dat.target,aes(x=biog,y=maxn),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.target.biog,aes(x=biog,y=maxn),alpha=0.5)+
  geom_line(data=predicts.target.biog,aes(x=biog,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.target.biog,aes(x=biog,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Targeted abundance") +
  theme(plot.title = element_text(hjust = 0))
ggmod.target.biog

# detrended bathy ----
ggmod.target.detrended<- ggplot() +
  ylab("")+
  xlab("Detrended bathymetry")+
  geom_point(data=dat.target,aes(x=detrended,y=maxn),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.target.detrended,aes(x=detrended,y=maxn),alpha=0.5)+
  geom_line(data=predicts.target.detrended,aes(x=detrended,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.target.detrended,aes(x=detrended,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.target.detrended

# macroalgae ----
ggmod.target.macroalgae<- ggplot() +
  ylab("")+
  xlab("Macroalgae")+
  geom_point(data=dat.target,aes(x=macroalgae,y=maxn),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.target.macroalgae,aes(x=macroalgae,y=maxn),alpha=0.5)+
  geom_line(data=predicts.target.macroalgae,aes(x=macroalgae,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.target.macroalgae,aes(x=macroalgae,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.target.macroalgae

# MODEL coris auricularis (depth) ----
dat.coris <- dat %>% filter(scientific=="Labridae Coris auricularis")

gamm=gam(maxn~s(depth,k=3,bs='cr'), family=tw,data=dat.coris)

# predict - biog ----
mod<-gamm
testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 20)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.coris.depth = testdata%>%data.frame(fits)%>%
  group_by(depth)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Coris auricularis ----
# depth ----
#load fish pic
c.a <- readPNG("data/images/Coris auricularis-3cmL.png")
c.a <- as.raster(c.a)

ggmod.coris.depth<- ggplot() +
  ylab("")+
  xlab("Depth")+
  geom_point(data=dat.coris,aes(x=depth,y=maxn),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.coris.depth,aes(x=depth,y=maxn),alpha=0.5)+
  geom_line(data=predicts.coris.depth,aes(x=depth,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.coris.depth,aes(x=depth,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Coris auricularis abundance") +
  theme(plot.title = element_text(hjust = 0))+
  annotation_raster(c.a, xmin=120, xmax=180, ymin=25, ymax=30)
ggmod.coris.depth

# MODEL Lethrinus miniatus (biog + depth) ----
dat.miniatus <- dat %>% filter(scientific=="Lethrinidae Lethrinus miniatus")

gamm=gam(maxn~s(biog,k=3,bs='cr')+s(depth, k = 3, bs = 'cr'), family=tw,data=dat.miniatus)

# predict - biog ----
mod<-gamm
testdata <- expand.grid(biog=seq(min(dat$biog),max(dat$biog),length.out = 20),
                        depth=mean(mod$model$depth)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.miniatus.biog = testdata%>%data.frame(fits)%>%
  group_by(biog)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - depth ----
mod<-gamm
testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 20),
                        biog=mean(mod$model$biog)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.miniatus.depth = testdata%>%data.frame(fits)%>%
  group_by(depth)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Lethrinus miniatus ----
# biogenic reef ----
#load fish pic
l.m <- readPNG("data/images/Lethrinus miniatus 3cm.png")
l.m <- as.raster(l.m)

ggmod.miniatus.biog<- ggplot() +
  ylab("")+
  xlab("Biogenic reef")+
  geom_point(data=dat.miniatus,aes(x=biog,y=maxn),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.miniatus.biog,aes(x=biog,y=maxn),alpha=0.5)+
  geom_line(data=predicts.miniatus.biog,aes(x=biog,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.miniatus.biog,aes(x=biog,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Lethrinus miniatus abundance") +
  theme(plot.title = element_text(hjust = 0))+
  annotation_raster(l.m, xmin=0.60, xmax=0.95, ymin=12.5, ymax=15.5)
ggmod.miniatus.biog

# depth ----
ggmod.miniatus.depth<- ggplot() +
  ylab("")+
  xlab("Depth")+
  geom_point(data=dat.target,aes(x=depth,y=maxn),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.miniatus.depth,aes(x=depth,y=maxn),alpha=0.5)+
  geom_line(data=predicts.miniatus.depth,aes(x=depth,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.miniatus.depth,aes(x=depth,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.miniatus.depth

# MODEL Chromis westaustralis (relief) ----
dat.chromis <- dat %>% filter(scientific=="Pomacentridae Chromis westaustralis")

gamm=gam(maxn~s(relief,k=3,bs='cr'), family=tw,data=dat.chromis)

# predict - relief ----
mod<-gamm
testdata <- expand.grid(relief=seq(min(dat$relief),max(dat$relief),length.out = 20)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.chromis.relief = testdata%>%data.frame(fits)%>%
  group_by(relief)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Chromis westaustralis ----
# depth ----
#add fish pic
c.w <- readPNG("data/images/Chromis westaustralis-3cmL.png")
c.w <- as.raster(c.w)

ggmod.chromis.relief<- ggplot() +
  ylab("")+
  xlab("Relief")+
  geom_point(data=dat.chromis,aes(x=relief,y=maxn),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.chromis.relief,aes(x=relief,y=maxn),alpha=0.5)+
  geom_line(data=predicts.chromis.relief,aes(x=relief,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.chromis.relief,aes(x=relief,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Chromis westaustralis abundance") +
  theme(plot.title = element_text(hjust = 0))+
  annotation_raster(c.w, xmin=1.94, xmax=2.54, ymin=100, ymax=110)
ggmod.chromis.relief

# MODEL Greater than legal size (biog + detrended + macroalgae) ----
dat.greater <- dat %>% filter(scientific=="greater than legal size")

gamm=gam(maxn~s(biog,k=3,bs='cr')+s(detrended, k = 3, bs = 'cr')+s(macroalgae, k = 3, bs = 'cr'), family=tw,data=dat.greater)

# predict - biog ----
mod<-gamm
testdata <- expand.grid(biog=seq(min(dat$biog),max(dat$biog),length.out = 20),
                        detrended=mean(mod$model$detrended),
                        macroalgae=mean(mod$model$macroalgae)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.greater.biog = testdata%>%data.frame(fits)%>%
  group_by(biog)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - detrended ----
mod<-gamm
testdata <- expand.grid(detrended=seq(min(dat$detrended),max(dat$detrended),length.out = 20),
                        biog=mean(mod$model$biog),
                        macroalgae=mean(mod$model$macroalgae)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.greater.detrended = testdata%>%data.frame(fits)%>%
  group_by(detrended)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - macroalgae ----
mod<-gamm
testdata <- expand.grid(macroalgae=seq(min(dat$macroalgae),max(dat$macroalgae),length.out = 20),
                        biog=mean(mod$model$biog),
                        detrended=mean(mod$model$detrended)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.greater.macroalgae = testdata%>%data.frame(fits)%>%
  group_by(macroalgae)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for greater than legal size  ----
# biogenic reef ----
ggmod.greater.biog<- ggplot() +
  ylab("")+
  xlab("Biogenic reef")+
  geom_point(data=dat.greater,aes(x=biog,y=maxn),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.greater.biog,aes(x=biog,y=maxn),alpha=0.5)+
  geom_line(data=predicts.greater.biog,aes(x=biog,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.greater.biog,aes(x=biog,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Greater than legal size") +
  theme(plot.title = element_text(hjust = 0))
ggmod.greater.biog

# detrended bathy ----
ggmod.greater.detrended<- ggplot() +
  ylab("")+
  xlab("Detrended bathymetry")+
  geom_point(data=dat.greater,aes(x=detrended,y=maxn),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.greater.detrended,aes(x=detrended,y=maxn),alpha=0.5)+
  geom_line(data=predicts.greater.detrended,aes(x=detrended,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.greater.detrended,aes(x=detrended,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.greater.detrended

# macroalgae ----
ggmod.greater.macroalgae<- ggplot() +
  ylab("")+
  xlab("Macroalgae")+
  geom_point(data=dat.greater,aes(x=macroalgae,y=maxn),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.greater.macroalgae,aes(x=macroalgae,y=maxn),alpha=0.5)+
  geom_line(data=predicts.greater.macroalgae,aes(x=macroalgae,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.greater.macroalgae,aes(x=macroalgae,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.greater.macroalgae

# MODEL Smaller than legal size (biog + detrended + macroalgae) ----
dat.smaller <- dat %>% filter(scientific=="smaller than legal size")

gamm=gam(maxn~s(biog,k=3,bs='cr')+s(detrended, k = 3, bs = 'cr')+s(macroalgae, k = 3, bs = 'cr'), family=tw,data=dat.smaller)

# predict - biog ----
mod<-gamm
testdata <- expand.grid(biog=seq(min(dat$biog),max(dat$biog),length.out = 20),
                        detrended=mean(mod$model$detrended),
                        macroalgae=mean(mod$model$macroalgae)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.smaller.biog = testdata%>%data.frame(fits)%>%
  group_by(biog)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - detrended ----
mod<-gamm
testdata <- expand.grid(detrended=seq(min(dat$detrended),max(dat$detrended),length.out = 20),
                        biog=mean(mod$model$biog),
                        macroalgae=mean(mod$model$macroalgae)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.smaller.detrended = testdata%>%data.frame(fits)%>%
  group_by(detrended)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - macroalgae ----
mod<-gamm
testdata <- expand.grid(macroalgae=seq(min(dat$macroalgae),max(dat$macroalgae),length.out = 20),
                        biog=mean(mod$model$biog),
                        detrended=mean(mod$model$detrended)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.smaller.macroalgae = testdata%>%data.frame(fits)%>%
  group_by(macroalgae)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for smaller than legal size  ----
# biogenic reef ----
ggmod.smaller.biog<- ggplot() +
  ylab("")+
  xlab("Biogenic reef")+
  geom_point(data=dat.smaller,aes(x=biog,y=maxn),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.smaller.biog,aes(x=biog,y=maxn),alpha=0.5)+
  geom_line(data=predicts.smaller.biog,aes(x=biog,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.smaller.biog,aes(x=biog,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Smaller than legal size") +
  theme(plot.title = element_text(hjust = 0))
ggmod.smaller.biog

# detrended bathy ----
ggmod.smaller.detrended<- ggplot() +
  ylab("")+
  xlab("Detrended bathymetry")+
  geom_point(data=dat.smaller,aes(x=detrended,y=maxn),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.smaller.detrended,aes(x=detrended,y=maxn),alpha=0.5)+
  geom_line(data=predicts.smaller.detrended,aes(x=detrended,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.smaller.detrended,aes(x=detrended,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.smaller.detrended

# macroalgae ----
ggmod.smaller.macroalgae<- ggplot() +
  ylab("Abundance")+
  xlab("Macroalgae")+
  geom_point(data=dat.smaller,aes(x=macroalgae,y=maxn),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.smaller.macroalgae,aes(x=macroalgae,y=maxn),alpha=0.5)+
  geom_line(data=predicts.smaller.macroalgae,aes(x=macroalgae,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.smaller.macroalgae,aes(x=macroalgae,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.smaller.macroalgae

# MODEL legal size lethrinus miniatus (biog + depth) ----
dat.legal.miniatus <- dat %>% filter(scientific=="legal size red throat")

gamm=gam(maxn~s(biog,k=3,bs='cr')+s(depth, k = 3, bs = 'cr'), family=tw,data=dat.legal.miniatus)

# predict - biog ----
mod<-gamm
testdata <- expand.grid(biog=seq(min(dat$biog),max(dat$biog),length.out = 20),
                        depth=mean(mod$model$depth)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.legal.miniatus.biog = testdata%>%data.frame(fits)%>%
  group_by(biog)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - detrended ----
mod<-gamm
testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 20),
                        biog=mean(mod$model$biog)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.legal.miniatus.depth = testdata%>%data.frame(fits)%>%
  group_by(depth)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for legal size lethrinus miniatus ----
# biogenic reef ----
ggmod.legal.miniatus.biog<- ggplot() +
  ylab("")+
  xlab("Biogenic reef")+
  geom_point(data=dat.legal.miniatus,aes(x=biog,y=maxn),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.legal.miniatus.biog,aes(x=biog,y=maxn),alpha=0.5)+
  geom_line(data=predicts.legal.miniatus.biog,aes(x=biog,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.legal.miniatus.biog,aes(x=biog,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Legal Lethrinus miniatus") +
  theme(plot.title = element_text(hjust = 0))+
  annotation_raster(l.m, xmin=0.6, xmax=0.95, ymin=8.25, ymax=10.25)
ggmod.legal.miniatus.biog

# depth ----
ggmod.legal.miniatus.depth<- ggplot() +
  ylab("")+
  xlab("Depth")+
  geom_point(data=dat.legal.miniatus,aes(x=depth,y=maxn),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.legal.miniatus.depth,aes(x=depth,y=maxn),alpha=0.5)+
  geom_line(data=predicts.legal.miniatus.depth,aes(x=depth,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.legal.miniatus.depth,aes(x=depth,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.legal.miniatus.depth

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