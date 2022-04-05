###
# Project: Parks - Montes
# Data:    BRUV fish, habitat
# Task:    Habitat GAM plots
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
library(reshape2)
library(patchwork)

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

## Set working directory----
working.dir <- getwd()
setwd(working.dir)
#OR Set manually once

# Load the dataset -
#habitat
dat <- readRDS("data/tidy/broad_merged_habitat.rds")%>%                                 # merged data from 'R/1_mergedata.R'
  dplyr::select(sample,biota.consolidated,
                biota.unconsolidated,mesophotic.reef,photic.reef,Total.Sum,depth_ga,
                detrended,roughness,slope,tpi)%>%
  dplyr::rename(depth=depth_ga)%>%
  glimpse()
  
colnames(dat)
dat <- melt(dat, measure.vars = c(2:5))%>%                                  # collect all taxa tags for univariate stats   
  rename(taxa = variable) %>%
  rename(response = value) %>%
  glimpse() 

# Manually make the most parsimonious GAM models for each taxa ----
#### Abrolhos habitat ####
unique(dat$taxa)
names(dat)

# MODEL Consolidated (detrended + roughness + tpi) ----
dat.consolidated <- dat %>% filter(taxa%in%"biota.consolidated")

mod=gam(cbind(response, (Total.Sum - response)) ~s(detrended, bs = 'cr', k = 5)+ 
          s(roughness, bs = 'cr', k = 5)+s(tpi, bs = 'cr', k = 5),
        family = binomial("logit"), method = "REML", data=dat.consolidated)

# predict - detrended ----
testdata <- expand.grid(detrended=seq(min(dat$detrended),max(dat$detrended),length.out = 100),
                        roughness=mean(mod$model$roughness),
                        tpi=mean(mod$model$tpi)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.consolidated.detrended = testdata%>%data.frame(fits)%>%
  group_by(detrended)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - roughness ----
testdata <- expand.grid(roughness=seq(min(dat$roughness),max(dat$roughness),length.out = 100),
                        detrended=mean(mod$model$detrended),
                        tpi=mean(mod$model$tpi)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.consolidated.roughness = testdata%>%data.frame(fits)%>%
  group_by(roughness)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - tpi ----
testdata <- expand.grid(tpi=seq(min(dat$tpi),max(dat$tpi),length.out = 100),
                        detrended=mean(mod$model$detrended),
                        roughness=mean(mod$model$roughness)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.consolidated.tpi = testdata%>%data.frame(fits)%>%
  group_by(tpi)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for consolidated ----
# detrended ----
ggmod.consolidated.detrended<- ggplot() +
  ylab("")+
  xlab("Detrended")+
  geom_point(data=dat.consolidated,aes(x=detrended,y=response/Total.Sum),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.consolidated.detrended,aes(x=detrended,y=response),alpha=0.5)+
  geom_line(data=predicts.consolidated.detrended,aes(x=detrended,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.consolidated.detrended,aes(x=detrended,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Consolidated") +
  theme(plot.title = element_text(hjust = 0))
ggmod.consolidated.detrended

# roughness ----
ggmod.consolidated.roughness<- ggplot() +
  ylab("")+
  xlab("Roughness")+
  geom_point(data=dat.consolidated,aes(x=roughness,y=response/Total.Sum),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.consolidated.roughness,aes(x=roughness,y=response),alpha=0.5)+
  geom_line(data=predicts.consolidated.roughness,aes(x=roughness,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.consolidated.roughness,aes(x=roughness,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.consolidated.roughness

# TPI ----
ggmod.consolidated.tpi<- ggplot() +
  ylab("")+
  xlab("TPI")+
  geom_point(data=dat.consolidated,aes(x=tpi,y=response/Total.Sum),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.consolidated.tpi,aes(x=tpi,y=response),alpha=0.5)+
  geom_line(data=predicts.consolidated.tpi,aes(x=tpi,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.consolidated.tpi,aes(x=tpi,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.consolidated.tpi

# MODEL Unconsolidated (depth + roughness) ----
dat.unconsolidated <- dat %>% filter(taxa%in%"biota.unconsolidated")

mod=gam(cbind(response, (Total.Sum - response)) ~ s(depth, bs = 'cr', k = 5)+
          s(roughness, bs = 'cr', k = 5),
        family = binomial("logit"), method = "REML", data=dat.unconsolidated)

# predict - depth ----
testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 100),
                        roughness=mean(mod$model$roughness)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.unconsolidated.depth = testdata%>%data.frame(fits)%>%
  group_by(depth)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - roughness ----
testdata <- expand.grid(roughness=seq(min(dat$roughness),max(dat$roughness),length.out = 100),
                        depth=mean(mod$model$roughness)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.unconsolidated.roughness = testdata%>%data.frame(fits)%>%
  group_by(roughness)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for unconsolidated ----
# depth ----
ggmod.unconsolidated.depth<- ggplot() +
  ylab("")+
  xlab("Depth")+
  geom_point(data=dat.unconsolidated,aes(x=depth,y=response/Total.Sum),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.unconsolidated.depth,aes(x=depth,y=response),alpha=0.5)+
  geom_line(data=predicts.unconsolidated.depth,aes(x=depth,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.unconsolidated.depth,aes(x=depth,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Unconsolidated") +
  theme(plot.title = element_text(hjust = 0))
ggmod.unconsolidated.depth

# roughness ----
ggmod.unconsolidated.roughness<- ggplot() +
  ylab("")+
  xlab("Roughness")+
  geom_point(data=dat.unconsolidated,aes(x=roughness,y=response/Total.Sum),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.unconsolidated.roughness,aes(x=roughness,y=response),alpha=0.5)+
  geom_line(data=predicts.unconsolidated.roughness,aes(x=roughness,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.unconsolidated.roughness,aes(x=roughness,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.unconsolidated.roughness

# MODEL Invertebrate reef (depth + roughness + tpi) ----
dat.mesophotic <- dat %>% filter(taxa%in%"mesophotic.reef")

mod=gam(cbind(response, (Total.Sum - response)) ~ s(depth, bs = 'cr', k = 5)+
        s(roughness, bs = 'cr', k = 5)+s(tpi, bs = 'cr', k = 5),
        family = binomial("logit"), method = "REML", data=dat.mesophotic)

# predict - depth ----
testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 100),
                        roughness=mean(mod$model$roughness),
                        tpi=mean(mod$model$tpi)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.mesophotic.depth = testdata%>%data.frame(fits)%>%
  group_by(depth)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - roughness ----
testdata <- expand.grid(roughness=seq(min(dat$roughness),max(dat$roughness),length.out = 100),
                        depth=mean(mod$model$depth),
                        tpi=mean(mod$model$tpi)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.mesophotic.roughness = testdata%>%data.frame(fits)%>%
  group_by(roughness)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - tpi ----
testdata <- expand.grid(tpi=seq(min(dat$tpi),max(dat$tpi),length.out = 100),
                        depth=mean(mod$model$depth),
                        roughness=mean(mod$model$roughness)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.mesophotic.tpi = testdata%>%data.frame(fits)%>%
  group_by(tpi)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for mesophotic ----
# depth ----
ggmod.mesophotic.depth<- ggplot() +
  ylab("")+
  xlab("Depth")+
  geom_point(data=dat.mesophotic,aes(x=depth,y=response/Total.Sum),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.mesophotic.depth,aes(x=depth,y=response),alpha=0.5)+
  geom_line(data=predicts.mesophotic.depth,aes(x=depth,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.mesophotic.depth,aes(x=depth,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Invertebrate reef") +
  theme(plot.title = element_text(hjust = 0))
ggmod.mesophotic.depth

# roughness ----
ggmod.mesophotic.roughness<- ggplot() +
  ylab("")+
  xlab("Roughness")+
  geom_point(data=dat.mesophotic,aes(x=roughness,y=response/Total.Sum),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.mesophotic.roughness,aes(x=roughness,y=response),alpha=0.5)+
  geom_line(data=predicts.mesophotic.roughness,aes(x=roughness,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.mesophotic.roughness,aes(x=roughness,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.mesophotic.roughness

# TPI ----
ggmod.mesophotic.tpi<- ggplot() +
  ylab("")+
  xlab("TPI")+
  geom_point(data=dat.mesophotic,aes(x=tpi,y=response/Total.Sum),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.mesophotic.tpi,aes(x=tpi,y=response),alpha=0.5)+
  geom_line(data=predicts.mesophotic.tpi,aes(x=tpi,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.mesophotic.tpi,aes(x=tpi,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.mesophotic.tpi

# MODEL Macroalgae/coral reef (depth + detrended + roughness) ----
dat.photic <- dat %>% filter(taxa%in%"photic.reef")

mod=gam(cbind(response, (Total.Sum - response)) ~ s(depth, bs = 'cr', k = 5) + 
          s(roughness, bs = 'cr', k = 5)+s(tpi, bs = 'cr', k = 5),
        family = binomial("logit"), method = "REML", data=dat.photic)

# predict - depth ----
testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 100),
                        roughness=mean(mod$model$roughness),
                        tpi=mean(mod$model$tpi)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.photic.depth = testdata%>%data.frame(fits)%>%
  group_by(depth)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - roughness ----
testdata <- expand.grid(roughness=seq(min(dat$roughness),max(dat$roughness),length.out = 100),
                        depth=mean(mod$model$depth),
                        tpi=mean(mod$model$tpi)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.photic.roughness = testdata%>%data.frame(fits)%>%
  group_by(roughness)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - tpi ----
testdata <- expand.grid(tpi=seq(min(dat$tpi),max(dat$tpi),length.out = 100),
                        depth=mean(mod$model$depth),
                        roughness=mean(mod$model$roughness)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.photic.tpi = testdata%>%data.frame(fits)%>%
  group_by(tpi)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for photic ----
# depth ----
ggmod.photic.depth<- ggplot() +
  ylab("")+
  xlab("Depth")+
  geom_point(data=dat.photic,aes(x=depth,y=response/Total.Sum),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.photic.depth,aes(x=depth,y=response),alpha=0.5)+
  geom_line(data=predicts.photic.depth,aes(x=depth,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.photic.depth,aes(x=depth,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Macroalgae/coral reef") +
  theme(plot.title = element_text(hjust = 0))
ggmod.photic.depth

# roughness ----
ggmod.photic.roughness<- ggplot() +
  ylab("")+
  xlab("Roughness")+
  geom_point(data=dat.photic,aes(x=roughness,y=response/Total.Sum),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.photic.roughness,aes(x=roughness,y=response),alpha=0.5)+
  geom_line(data=predicts.photic.roughness,aes(x=roughness,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.photic.roughness,aes(x=roughness,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.photic.roughness

# tpi ----
ggmod.photic.tpi<- ggplot() +
  ylab("")+
  xlab("TPI")+
  geom_point(data=dat.photic,aes(x=tpi,y=response/Total.Sum),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.photic.tpi,aes(x=tpi,y=response),alpha=0.5)+
  geom_line(data=predicts.photic.tpi,aes(x=tpi,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.photic.tpi,aes(x=tpi,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.photic.tpi

#Combined plots into grid

gg.grid <- ggmod.consolidated.detrended+ggmod.consolidated.roughness+ggmod.consolidated.tpi+
  ggmod.unconsolidated.depth+ggmod.unconsolidated.roughness+plot_spacer()+
  ggmod.mesophotic.depth+ggmod.mesophotic.roughness+ggmod.mesophotic.tpi+
  ggmod.photic.depth+ggmod.photic.roughness+ggmod.photic.tpi+
  plot_annotation(tag_levels = 'a')+plot_layout(ncol = 3,nrow = 4)
gg.grid

#save plots
save_plot("plots/montes.habitat.gam.plots.png", gg.grid,base_height = 9,base_width = 11)
