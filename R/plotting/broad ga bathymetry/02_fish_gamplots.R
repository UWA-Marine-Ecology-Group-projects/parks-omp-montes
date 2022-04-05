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
filter(number < 400)                                                            #remove outiers from total abundance

# Manually make the most parsimonious GAM models for each taxa ----
#### montes MaxN ####

# MODEL Total abundance (depth + mean relief) ----
dat.total <- dat %>% filter(response=="total.abundance")

mod=gam(number~s(mean.relief,k=3,bs='cr'), family=tw,data=dat.total)

# predict - mean relief ----
testdata <- expand.grid(mean.relief=seq(min(dat$mean.relief),max(dat$mean.relief),length.out = 20)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.total.relief = testdata%>%data.frame(fits)%>%
  group_by(mean.relief)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Total abundance ----
# mean relief ----
ggmod.total.relief<- ggplot() +
  ylab("")+
  xlab("Mean relief")+
  geom_point(data=dat.total,aes(x=mean.relief,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.total.relief,aes(x=mean.relief,y=maxn),alpha=0.5)+
  geom_line(data=predicts.total.relief,aes(x=mean.relief,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.total.relief,aes(x=mean.relief,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Total abundance") +
  theme(plot.title = element_text(hjust = 0))
ggmod.total.relief

# MODEL Species richness (mean relief) ----
dat.species <- dat %>% filter(response=="species.richness")

mod=gam(number~s(mean.relief,k=3,bs='cr'), family=tw,data=dat.species)

# predict - mean relief ----
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

# MODEL Legals (mean relief) ----
dat.legal <- dat %>% filter(response=="greater than legal size")

mod=gam(number~ s(mean.relief,k=3,bs='cr'), family=tw,data=dat.legal)

# predict - mean relief ----
testdata <- expand.grid(mean.relief=seq(min(dat$mean.relief),max(dat$mean.relief),length.out = 20)) %>%
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

# MODEL Sublegals (detrended + mean.relief + tpi) ----
dat.sublegal <- dat %>% filter(response=="smaller than legal size")

mod=gam(number~s(detrended,k=3,bs='cr') + s(mean.relief,k=3,bs='cr') + s(tpi,k=3,bs='cr'), 
        family=tw,data=dat.sublegal)

# predict - detrended ----
testdata <- expand.grid(detrended=seq(min(dat$detrended),max(dat$detrended),length.out = 20),
                        mean.relief=mean(mod$model$mean.relief),
                        tpi=mean(mod$model$tpi)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sublegal.detrended = testdata%>%data.frame(fits)%>%
  group_by(detrended)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - mean relief ----
testdata <- expand.grid(mean.relief=seq(min(dat$mean.relief),max(dat$mean.relief),length.out = 20),
                        detrended=mean(mod$model$detrended),
                        tpi=mean(mod$model$tpi)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sublegal.relief = testdata%>%data.frame(fits)%>%
  group_by(mean.relief)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - tpi ----
testdata <- expand.grid(tpi=seq(min(dat$tpi),max(dat$tpi),length.out = 20),
                        detrended=mean(mod$model$detrended),
                        mean.relief=mean(mod$model$mean.relief)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sublegal.tpi = testdata%>%data.frame(fits)%>%
  group_by(tpi)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Sublegals ----
# depth ----
# detrended ----
ggmod.sublegal.detrended<- ggplot() +
  ylab("")+
  xlab("Detrended")+
  geom_point(data=dat.sublegal,aes(x=detrended,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sublegal.detrended,aes(x=detrended,y=maxn),alpha=0.5)+
  geom_line(data=predicts.sublegal.detrended,aes(x=detrended,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sublegal.detrended,aes(x=detrended,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Sublegal") +
  theme(plot.title = element_text(hjust = 0))
ggmod.sublegal.detrended

# mean relief ----
ggmod.sublegal.relief<- ggplot() +
  ylab("")+
  xlab("Mean relief")+
  geom_point(data=dat.sublegal,aes(x=mean.relief,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sublegal.relief,aes(x=mean.relief,y=maxn),alpha=0.5)+
  geom_line(data=predicts.sublegal.relief,aes(x=mean.relief,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sublegal.relief,aes(x=mean.relief,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.sublegal.relief

# tpi ----
ggmod.sublegal.tpi<- ggplot() +
  ylab("")+
  xlab("TPI")+
  geom_point(data=dat.sublegal,aes(x=tpi,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sublegal.tpi,aes(x=tpi,y=maxn),alpha=0.5)+
  geom_line(data=predicts.sublegal.tpi,aes(x=tpi,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sublegal.tpi,aes(x=tpi,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.sublegal.tpi

# Combine with patchwork
library(patchwork)

# view plots
plot.grid.gam <- ggmod.total.relief + plot_spacer() + plot_spacer() +
                 ggmod.species.relief +  plot_spacer() +  plot_spacer() + 
                 ggmod.legal.relief + plot_spacer() + plot_spacer() +
                 ggmod.sublegal.detrended + ggmod.sublegal.relief + ggmod.sublegal.tpi +
                 plot_annotation(tag_levels = 'a') + plot_layout(ncol = 3,nrow = 4)
plot.grid.gam


#Save plots
save_plot("plots/montes.synthesis.gam.png", plot.grid.gam,base_height = 9,base_width = 8.5)
