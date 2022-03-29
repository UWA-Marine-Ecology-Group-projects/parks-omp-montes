###
# Project: Parks - Montes synthesis
# Data:    BRUV fish, habitat
# Task:    Plotting fish importance GAM relationships
# author:  Claude
# date:    Dec 2021-Feb 2022
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
combined.maxn <- readRDS("data/tidy/dat.maxn.rds")%>%
  glimpse()

combined.length <- readRDS("data/tidy/dat.length.rds")%>%
  glimpse()

dat <- bind_rows(combined.maxn, combined.length)%>%
filter(number < 400)

# Manually make the most parsimonious GAM models for each taxa ----
#### montes MaxN ####

# MODEL Total abundance (depth + mean relief) ----
dat.total <- dat %>% filter(response=="total.abundance")

gamm=gam(number~s(detrended,k=3,bs='cr') + s(mean.relief,k=3,bs='cr')+ s(roughness,k=3,bs='cr'), family=tw,data=dat.total)

# predict - detrended ----
mod<-gamm
testdata <- expand.grid(detrended=seq(min(dat$detrended),max(dat$detrended),length.out = 20),
                        mean.relief=mean(mod$model$mean.relief),
                        roughness=mean(mod$model$roughness)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.total.detrended = testdata%>%data.frame(fits)%>%
  group_by(detrended)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - mean relief ----
mod<-gamm
testdata <- expand.grid(mean.relief=seq(min(dat$mean.relief),max(dat$mean.relief),length.out = 20),
                        detrended=mean(mod$model$detrended),
                        roughness=mean(mod$model$roughness)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.total.relief = testdata%>%data.frame(fits)%>%
  group_by(mean.relief)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - roughness ----
mod<-gamm
testdata <- expand.grid(roughness=seq(min(dat$roughness),max(dat$roughness),length.out = 20),
                        detrended=mean(mod$model$detrended),
                        mean.relief=mean(mod$model$mean.relief)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.total.roughness = testdata%>%data.frame(fits)%>%
  group_by(roughness)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Total abundance ----
# detrended ----
ggmod.total.detrended<- ggplot() +
  ylab("")+
  xlab("Detrended bathymetry")+
  geom_point(data=dat.total,aes(x=detrended,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.total.detrended,aes(x=detrended,y=maxn),alpha=0.5)+
  geom_line(data=predicts.total.detrended,aes(x=detrended,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.total.detrended,aes(x=detrended,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Total abundance") +
  theme(plot.title = element_text(hjust = 0))
ggmod.total.detrended

# mean relief ----
ggmod.total.relief<- ggplot() +
  ylab("")+
  xlab("Mean relief")+
  geom_point(data=dat.total,aes(x=mean.relief,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.total.relief,aes(x=mean.relief,y=maxn),alpha=0.5)+
  geom_line(data=predicts.total.relief,aes(x=mean.relief,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.total.relief,aes(x=mean.relief,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.total.relief

# mean relief ----
ggmod.total.roughness<- ggplot() +
  ylab("")+
  xlab("Roughness")+
  geom_point(data=dat.total,aes(x=roughness,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.total.roughness,aes(x=roughness,y=maxn),alpha=0.5)+
  geom_line(data=predicts.total.roughness,aes(x=roughness,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.total.roughness,aes(x=roughness,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.total.roughness

# MODEL Species richness (mean relief) ----
dat.species <- dat %>% filter(response=="species.richness")

gamm=gam(number~s(mean.relief,k=3,bs='cr'), family=tw,data=dat.species)

# predict - mean relief ----
mod<-gamm
testdata <- expand.grid(mean.relief=seq(min(dat$mean.relief),max(dat$mean.relief),length.out = 20)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.species.relief = testdata%>%data.frame(fits)%>%
  group_by(mean.relief)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Species richness ----
# mean relief ----
ggmod.species.relief<- ggplot() +
  ylab("")+
  xlab("Mean relief")+
  geom_point(data=dat.species,aes(x=mean.relief,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.species.relief,aes(x=mean.relief,y=maxn),alpha=0.5)+
  geom_line(data=predicts.species.relief,aes(x=mean.relief,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.species.relief,aes(x=mean.relief,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Species richness") +
  theme(plot.title = element_text(hjust = 0))
ggmod.species.relief

# MODEL Legals (mean relief + roughness) ----
dat.legal <- dat %>% filter(response=="greater than legal size")

gamm=gam(number~ s(mean.relief,k=3,bs='cr')+s(roughness,k=3,bs='cr'), family=tw,data=dat.legal)

# predict - roughness ----
mod<-gamm
testdata <- expand.grid(roughness=seq(min(dat$roughness),max(dat$roughness),length.out = 20),
                        mean.relief=mean(mod$model$mean.relief)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.legal.roughness = testdata%>%data.frame(fits)%>%
  group_by(roughness)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - mean relief ----
mod<-gamm
testdata <- expand.grid(mean.relief=seq(min(dat$mean.relief),max(dat$mean.relief),length.out = 20),
                        roughness=mean(mod$model$roughness)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.legal.relief = testdata%>%data.frame(fits)%>%
  group_by(mean.relief)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Legals ----
# mean relief ----
ggmod.legal.relief<- ggplot() +
  ylab("")+
  xlab("Mean relief")+
  geom_point(data=dat.legal,aes(x=mean.relief,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.legal.relief,aes(x=mean.relief,y=maxn),alpha=0.5)+
  geom_line(data=predicts.legal.relief,aes(x=mean.relief,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.legal.relief,aes(x=mean.relief,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Legal") +
  theme(plot.title = element_text(hjust = 0))
ggmod.legal.relief

# roughness ----
ggmod.legal.roughness<- ggplot() +
  ylab("")+
  xlab("Roughness")+
  geom_point(data=dat.legal,aes(x=roughness,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.legal.roughness,aes(x=roughness,y=maxn),alpha=0.5)+
  geom_line(data=predicts.legal.roughness,aes(x=roughness,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.legal.roughness,aes(x=roughness,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.legal.roughness

# MODEL Sublegals (mean relief + roughness) ----
dat.sublegal <- dat %>% filter(response=="smaller than legal size")

gamm=gam(number~s(mean.relief,k=3,bs='cr') + s(roughness,k=3,bs='cr'), family=tw,data=dat.sublegal)

# predict - mean relief ----
mod<-gamm
testdata <- expand.grid(mean.relief=seq(min(dat$mean.relief),max(dat$mean.relief),length.out = 20),
                        roughness=mean(mod$model$roughness)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sublegal.relief = testdata%>%data.frame(fits)%>%
  group_by(mean.relief)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - roughness ----
mod<-gamm
testdata <- expand.grid(roughness=seq(min(dat$roughness),max(dat$roughness),length.out = 20),
                        mean.relief=mean(mod$model$mean.relief)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sublegal.roughness = testdata%>%data.frame(fits)%>%
  group_by(roughness)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Sublegals ----
# depth ----
# mean relief ----
ggmod.sublegal.relief<- ggplot() +
  ylab("")+
  xlab("Mean relief")+
  geom_point(data=dat.sublegal,aes(x=mean.relief,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sublegal.relief,aes(x=mean.relief,y=maxn),alpha=0.5)+
  geom_line(data=predicts.sublegal.relief,aes(x=mean.relief,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sublegal.relief,aes(x=mean.relief,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Sublegal") +
  theme(plot.title = element_text(hjust = 0))
ggmod.sublegal.relief

# mean relief ----
ggmod.sublegal.roughness<- ggplot() +
  ylab("")+
  xlab("Roughness")+
  geom_point(data=dat.sublegal,aes(x=roughness,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sublegal.roughness,aes(x=roughness,y=maxn),alpha=0.5)+
  geom_line(data=predicts.sublegal.roughness,aes(x=roughness,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sublegal.roughness,aes(x=roughness,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.sublegal.roughness

# Combine with patchwork
library(patchwork)

# view plots
plot.grid.gam <- ggmod.total.detrended + ggmod.total.relief + ggmod.total.roughness +
                 ggmod.species.relief +  plot_spacer() +  plot_spacer() + 
                 ggmod.legal.relief + ggmod.legal.roughness + plot_spacer() +
                 ggmod.sublegal.relief + ggmod.sublegal.roughness + plot_spacer() +
                 plot_annotation(tag_levels = 'a') + plot_layout(ncol = 3,nrow = 4)
plot.grid.gam


#Save plots
save_plot("plots/montes.synthesis.gam.png", plot.grid.gam,base_height = 9,base_width = 8.5)
