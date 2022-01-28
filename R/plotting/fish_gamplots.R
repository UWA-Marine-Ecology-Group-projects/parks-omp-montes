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

dat <- bind_rows(combined.maxn, combined.length)

# Manually make the most parsimonious GAM models for each taxa ----
#### montes MaxN ####

# MODEL Total abundance (depth + mean relief + detrended) ----
dat.total <- dat %>% filter(response=="total.abundance")

gamm=gam(number~s(depth,k=3,bs='cr') + s(mean.relief,k=3,bs='cr')+ s(detrended,k=3,bs='cr'), family=tw,data=dat.total)

# predict - depth ----
mod<-gamm
testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 20),
                        mean.relief=mean(mod$model$mean.relief),
                        detrended=mean(mod$model$detrended)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.total.depth = testdata%>%data.frame(fits)%>%
  group_by(depth)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - mean relief ----
mod<-gamm
testdata <- expand.grid(mean.relief=seq(min(dat$mean.relief),max(dat$mean.relief),length.out = 20),
                        depth=mean(mod$model$depth),
                        detrended=mean(mod$model$detrended)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.total.mean.relief = testdata%>%data.frame(fits)%>%
  group_by(mean.relief)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - detrended ----
mod<-gamm
testdata <- expand.grid(detrended=seq(min(dat$detrended),max(dat$detrended),length.out = 20),
                        depth=mean(mod$model$depth),
                        mean.relief=mean(mod$model$mean.relief)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.total.detrended = testdata%>%data.frame(fits)%>%
  group_by(detrended)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Total abundance ----
# depth ----
ggmod.total.depth<- ggplot() +
  ylab("")+
  xlab("Depth")+
  geom_point(data=dat.total,aes(x=depth,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.total.depth,aes(x=depth,y=maxn),alpha=0.5)+
  geom_line(data=predicts.total.depth,aes(x=depth,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.total.depth,aes(x=depth,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Total abundance") +
  theme(plot.title = element_text(hjust = 0))
ggmod.total.depth

# mean relief ----
ggmod.total.relief<- ggplot() +
  ylab("")+
  xlab("Mean relief")+
  geom_point(data=dat.total,aes(x=mean.relief,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.total.mean.relief,aes(x=mean.relief,y=maxn),alpha=0.5)+
  geom_line(data=predicts.total.mean.relief,aes(x=mean.relief,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.total.mean.relief,aes(x=mean.relief,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.total.relief

# detrended ----
ggmod.total.detrended<- ggplot() +
  ylab("")+
  xlab("Detrended bathymetry")+
  geom_point(data=dat.total,aes(x=detrended,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.total.detrended,aes(x=detrended,y=maxn),alpha=0.5)+
  geom_line(data=predicts.total.detrended,aes(x=detrended,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.total.detrended,aes(x=detrended,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.total.detrended

# MODEL Species richness (detrended + mean relief + roughness) ----
dat.species <- dat %>% filter(response=="species.richness")

gamm=gam(number~s(detrended,k=3,bs='cr') + s(mean.relief,k=3,bs='cr')+ s(roughness,k=3,bs='cr'), family=tw,data=dat.species)

# predict - detrended ----
mod<-gamm
testdata <- expand.grid(detrended=seq(min(dat$detrended),max(dat$detrended),length.out = 20),
                        mean.relief=mean(mod$model$mean.relief),
                        roughness=mean(mod$model$roughness)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.species.detrended = testdata%>%data.frame(fits)%>%
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

predicts.species.relief = testdata%>%data.frame(fits)%>%
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

predicts.species.roughness = testdata%>%data.frame(fits)%>%
  group_by(roughness)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Species richness ----
# detrended ----
ggmod.species.detrended<- ggplot() +
  ylab("")+
  xlab("Detrended bathymetry")+
  geom_point(data=dat.species,aes(x=detrended,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.species.detrended,aes(x=detrended,y=maxn),alpha=0.5)+
  geom_line(data=predicts.species.detrended,aes(x=detrended,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.species.detrended,aes(x=detrended,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Species richness") +
  theme(plot.title = element_text(hjust = 0))
ggmod.species.detrended

# mean relief ----
ggmod.species.relief<- ggplot() +
  ylab("")+
  xlab("Mean relief")+
  geom_point(data=dat.species,aes(x=mean.relief,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.species.relief,aes(x=mean.relief,y=maxn),alpha=0.5)+
  geom_line(data=predicts.species.relief,aes(x=mean.relief,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.species.relief,aes(x=mean.relief,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.species.relief

# roughness ----
ggmod.species.roughness<- ggplot() +
  ylab("")+
  xlab("Roughness")+
  geom_point(data=dat.species,aes(x=roughness,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.species.roughness,aes(x=roughness,y=maxn),alpha=0.5)+
  geom_line(data=predicts.species.roughness,aes(x=roughness,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.species.roughness,aes(x=roughness,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.species.roughness

# # MODEL Targeted abundance (depth + detrended + mean relief) ----
# dat.targeted <- dat %>% filter(response=="targeted.abundance")
# 
# gamm=gam(number~s(depth,k=3,bs='cr') + s(detrended,k=3,bs='cr')+ s(mean.relief,k=3,bs='cr'), family=tw,data=dat.targeted)
# 
# # predict - depth ----
# mod<-gamm
# testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 20),
#                         detrended=mean(mod$model$detrended),
#                         mean.relief=mean(mod$model$mean.relief)) %>%
#   distinct()%>%
#   glimpse()
# 
# fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
# 
# predicts.targeted.depth = testdata%>%data.frame(fits)%>%
#   group_by(depth)%>% #only change here
#   summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
#   ungroup()
# 
# # predict - detrended ----
# mod<-gamm
# testdata <- expand.grid(detrended=seq(min(dat$detrended),max(dat$detrended),length.out = 20),
#                         depth=mean(mod$model$depth),
#                         mean.relief=mean(mod$model$mean.relief)) %>%
#   distinct()%>%
#   glimpse()
# 
# fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
# 
# predicts.targeted.detrended = testdata%>%data.frame(fits)%>%
#   group_by(detrended)%>% #only change here
#   summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
#   ungroup()
# 
# # predict - mean relief ----
# mod<-gamm
# testdata <- expand.grid(mean.relief=seq(min(dat$mean.relief),max(dat$mean.relief),length.out = 20),
#                         depth=mean(mod$model$depth),
#                         detrended=mean(mod$model$detrended)) %>%
#   distinct()%>%
#   glimpse()
# 
# fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
# 
# predicts.targeted.relief = testdata%>%data.frame(fits)%>%
#   group_by(mean.relief)%>% #only change here
#   summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
#   ungroup()
# 
# # PLOTS for Targeted abundnace ----
# # depth ----
# ggmod.targeted.depth<- ggplot() +
#   ylab("")+
#   xlab("Depth")+
#   geom_point(data=dat.targeted,aes(x=depth,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
#   geom_line(data=predicts.targeted.depth,aes(x=depth,y=maxn),alpha=0.5)+
#   geom_line(data=predicts.targeted.depth,aes(x=depth,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
#   geom_line(data=predicts.targeted.depth,aes(x=depth,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
#   theme_classic()+
#   Theme1+
#   ggtitle("Targeted abundance") +
#   theme(plot.title = element_text(hjust = 0))
# ggmod.targeted.depth
# 
# # detrended ----
# ggmod.targeted.detrended<- ggplot() +
#   ylab("")+
#   xlab("Detrended bathymetry")+
#   geom_point(data=dat.targeted,aes(x=detrended,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
#   geom_line(data=predicts.targeted.detrended,aes(x=detrended,y=maxn),alpha=0.5)+
#   geom_line(data=predicts.targeted.detrended,aes(x=detrended,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
#   geom_line(data=predicts.targeted.detrended,aes(x=detrended,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
#   theme_classic()+
#   Theme1
# ggmod.targeted.detrended
# 
# # mean relief ----
# ggmod.targeted.relief<- ggplot() +
#   ylab("")+
#   xlab("Mean relief")+
#   geom_point(data=dat.targeted,aes(x=mean.relief,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
#   geom_line(data=predicts.targeted.relief,aes(x=mean.relief,y=maxn),alpha=0.5)+
#   geom_line(data=predicts.targeted.relief,aes(x=mean.relief,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
#   geom_line(data=predicts.targeted.relief,aes(x=mean.relief,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
#   theme_classic()+
#   Theme1
# ggmod.targeted.relief

# MODEL Legals (depth + mean relief) ----
dat.legal <- dat %>% filter(response=="greater than legal size")

gamm=gam(number~s(depth,k=3,bs='cr') + s(mean.relief,k=3,bs='cr'), family=tw,data=dat.legal)

# predict - depth ----
mod<-gamm
testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 20),
                        mean.relief=mean(mod$model$mean.relief)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.legal.depth = testdata%>%data.frame(fits)%>%
  group_by(depth)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - mean relief ----
mod<-gamm
testdata <- expand.grid(mean.relief=seq(min(dat$mean.relief),max(dat$mean.relief),length.out = 20),
                        depth=mean(mod$model$depth)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.legal.relief = testdata%>%data.frame(fits)%>%
  group_by(mean.relief)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Legals ----
# depth ----
ggmod.legal.depth<- ggplot() +
  ylab("")+
  xlab("Depth")+
  geom_point(data=dat.legal,aes(x=depth,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.legal.depth,aes(x=depth,y=maxn),alpha=0.5)+
  geom_line(data=predicts.legal.depth,aes(x=depth,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.legal.depth,aes(x=depth,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Legal") +
  theme(plot.title = element_text(hjust = 0))
ggmod.legal.depth

# mean relief ----
ggmod.legal.relief<- ggplot() +
  ylab("")+
  xlab("Mean relief")+
  geom_point(data=dat.legal,aes(x=mean.relief,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.legal.relief,aes(x=mean.relief,y=maxn),alpha=0.5)+
  geom_line(data=predicts.legal.relief,aes(x=mean.relief,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.legal.relief,aes(x=mean.relief,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.legal.relief

# MODEL Sublegals (depth + detrended + mean relief) ----
dat.sublegal <- dat %>% filter(response=="smaller than legal size")

gamm=gam(number~s(depth,k=3,bs='cr') + s(detrended,k=3,bs='cr')+ s(mean.relief,k=3,bs='cr'), family=tw,data=dat.sublegal)

# predict - depth ----
mod<-gamm
testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 20),
                        detrended=mean(mod$model$detrended),
                        mean.relief=mean(mod$model$mean.relief)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sublegal.depth = testdata%>%data.frame(fits)%>%
  group_by(depth)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - detrended ----
mod<-gamm
testdata <- expand.grid(detrended=seq(min(dat$detrended),max(dat$detrended),length.out = 20),
                        depth=mean(mod$model$depth),
                        mean.relief=mean(mod$model$mean.relief)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sublegal.detrended = testdata%>%data.frame(fits)%>%
  group_by(detrended)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - mean relief ----
mod<-gamm
testdata <- expand.grid(mean.relief=seq(min(dat$mean.relief),max(dat$mean.relief),length.out = 20),
                        depth=mean(mod$model$depth),
                        detrended=mean(mod$model$detrended)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sublegal.relief = testdata%>%data.frame(fits)%>%
  group_by(mean.relief)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Sublegals ----
# depth ----
ggmod.sublegal.depth<- ggplot() +
  ylab("")+
  xlab("Depth")+
  geom_point(data=dat.sublegal,aes(x=depth,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sublegal.depth,aes(x=depth,y=maxn),alpha=0.5)+
  geom_line(data=predicts.sublegal.depth,aes(x=depth,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sublegal.depth,aes(x=depth,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Sublegal") +
  theme(plot.title = element_text(hjust = 0))
ggmod.sublegal.depth

# detrended ----
ggmod.sublegal.detrended<- ggplot() +
  ylab("")+
  xlab("Detrended bathymetry")+
  geom_point(data=dat.sublegal,aes(x=detrended,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sublegal.detrended,aes(x=detrended,y=maxn),alpha=0.5)+
  geom_line(data=predicts.sublegal.detrended,aes(x=detrended,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sublegal.detrended,aes(x=detrended,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
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

# # MODEL Legal Spango (status) ----
# dat.spango <- dat %>% filter(response=="legal size spango")
# 
# gamm=gam(number~status, family=tw,data=dat.spango)
# 
# # predict - status ----
# mod<-gamm
# testdata <- expand.grid(status = c("Fished","No-take"))%>%
#   distinct()%>%
#   glimpse()
# 
# 
# fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
# 
# predicts.spango.status = testdata%>%data.frame(fits)%>%
#   group_by(status)%>% #only change here
#   summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
#   ungroup()
# 
# # PLOTS for Legal spango ----
# # status ----
# #load fish picture
# l.n <- readPNG("data/images/lethrinus nebulosus 3cm.png")
# l.n <- as.raster(l.n)
# 
# ggmod.spango.status<- ggplot(aes(x=status,y=maxn,fill=status,colour=status), data=predicts.spango.status) +
#   ylab("")+
#   xlab('Status')+
#   scale_fill_manual(labels = c("Fished", "No-take"),values=c("grey", "#1470ad"))+
#   scale_colour_manual(labels = c("Fished", "No-take"),values=c("black", "black"))+
#   scale_x_discrete(limits = rev(levels(predicts.spango.status$status)))+
#   geom_bar(stat = "identity")+
#   geom_errorbar(aes(ymin = maxn-se.fit,ymax = maxn+se.fit),width = 0.5) +
#   theme_classic()+
#   Theme1+
#   scale_y_continuous(expand = expansion(mult = c(0, .1)))+
#   theme(legend.position = "none")+
#   ggtitle("Legal Lethrinus nebulosus") +
#   theme(plot.title = element_text(hjust = 0))+
#   annotation_raster(l.n, xmin=1.7, xmax=2.5, ymin=0.8, ymax=1)
# ggmod.spango.status
# 
# # MODEL Sublegal trout (depth + mesophotic reef) ----
# dat.sublegaltrout <- dat %>% filter(response=="sublegal size trout")
# 
# gamm=gam(number~s(depth,k=3,bs='cr') + s(mesophotic.reef,k=3,bs='cr'), family=tw,data=dat.sublegaltrout)
# 
# # predict - depth ----
# mod<-gamm
# testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 20),
#                         mesophotic.reef=mean(mod$model$mesophotic.reef)) %>%
#   distinct()%>%
#   glimpse()
# 
# fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
# 
# predicts.sublegaltrout.depth = testdata%>%data.frame(fits)%>%
#   group_by(depth)%>% #only change here
#   summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
#   ungroup()
# 
# # predict - mesophotic reef ----
# mod<-gamm
# testdata <- expand.grid(mesophotic.reef=seq(min(dat$mesophotic.reef),max(dat$mesophotic.reef),length.out = 20),
#                         depth=mean(mod$model$depth)) %>%
#   distinct()%>%
#   glimpse()
# 
# fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
# 
# predicts.sublegaltrout.mesophotic = testdata%>%data.frame(fits)%>%
#   group_by(mesophotic.reef)%>% #only change here
#   summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
#   ungroup()
# 
# # PLOTS for Sublegal trout ----
# # depth ----
# #load fish pic
# p.spp <- readPNG("data/images/Plectropomus leopardus 3cm.png")
# p.spp <- as.raster(p.spp)
# 
# ggmod.sublegaltrout.depth<- ggplot() +
#   ylab("")+
#   xlab("Depth")+
#   geom_point(data=dat.sublegaltrout,aes(x=depth,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
#   geom_line(data=predicts.sublegaltrout.depth,aes(x=depth,y=maxn),alpha=0.5)+
#   geom_line(data=predicts.sublegaltrout.depth,aes(x=depth,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
#   geom_line(data=predicts.sublegaltrout.depth,aes(x=depth,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
#   theme_classic()+
#   Theme1+
#   ggtitle("Sublegal Plectropomus spp") +
#   theme(plot.title = element_text(hjust = 0))+
#   annotation_raster(p.spp, xmin=37.5, xmax=62.5, ymin=5.25, ymax=6.75)
# ggmod.sublegaltrout.depth
# 
# # mesophotic reef ----
# ggmod.sublegaltrout.mesophotic<- ggplot() +
#   ylab("")+
#   xlab("Mesophotic reef")+
#   geom_point(data=dat.sublegaltrout,aes(x=mesophotic.reef,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
#   geom_line(data=predicts.sublegaltrout.mesophotic,aes(x=mesophotic.reef,y=maxn),alpha=0.5)+
#   geom_line(data=predicts.sublegaltrout.mesophotic,aes(x=mesophotic.reef,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
#   geom_line(data=predicts.sublegaltrout.mesophotic,aes(x=mesophotic.reef,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
#   theme_classic()+
#   Theme1
# ggmod.sublegaltrout.mesophotic
# 
# # MODEL Legal atkinsoni (depth + mesophotic reef) ----
# dat.legalatkinsoni <- dat %>% filter(response=="legal size atkinsoni")
# 
# gamm=gam(number~s(depth,k=3,bs='cr') + s(mesophotic.reef,k=3,bs='cr'), family=tw,data=dat.legalatkinsoni)
# 
# # predict - depth ----
# mod<-gamm
# testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 20),
#                         mesophotic.reef=mean(mod$model$mesophotic.reef)) %>%
#   distinct()%>%
#   glimpse()
# 
# fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
# 
# predicts.legalatkinsoni.depth = testdata%>%data.frame(fits)%>%
#   group_by(depth)%>% #only change here
#   summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
#   ungroup()
# 
# # predict - mesophotic reef ----
# mod<-gamm
# testdata <- expand.grid(mesophotic.reef=seq(min(dat$mesophotic.reef),max(dat$mesophotic.reef),length.out = 20),
#                         depth=mean(mod$model$depth)) %>%
#   distinct()%>%
#   glimpse()
# 
# fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
# 
# predicts.legalatkinsoni.mesophotic = testdata%>%data.frame(fits)%>%
#   group_by(mesophotic.reef)%>% #only change here
#   summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
#   ungroup()
# 
# # PLOTS for Legal atkinsoni ----
# # depth ----
# #load fish pic
# l.a <- readPNG("data/images/Lethrinus atkinsoni 5cmL.png")
# l.a <- as.raster(l.a)
# 
# ggmod.legalatkinsoni.depth<- ggplot() +
#   ylab("")+
#   xlab("Depth")+
#   geom_point(data=dat.legalatkinsoni,aes(x=depth,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
#   geom_line(data=predicts.legalatkinsoni.depth,aes(x=depth,y=maxn),alpha=0.5)+
#   geom_line(data=predicts.legalatkinsoni.depth,aes(x=depth,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
#   geom_line(data=predicts.legalatkinsoni.depth,aes(x=depth,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
#   theme_classic()+
#   Theme1+
#   ggtitle("Legal Lethrinus atkinsoni") +
#   theme(plot.title = element_text(hjust = 0))+
#   annotation_raster(l.a, xmin=45, xmax=65, ymin=17.5, ymax=22.5)
# ggmod.legalatkinsoni.depth
# 
# # mesophotic reef ----
# ggmod.legalatkinsoni.mesophotic<- ggplot() +
#   ylab("")+
#   xlab("Mesophotic reef")+
#   geom_point(data=dat.legalatkinsoni,aes(x=mesophotic.reef,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
#   geom_line(data=predicts.legalatkinsoni.mesophotic,aes(x=mesophotic.reef,y=maxn),alpha=0.5)+
#   geom_line(data=predicts.legalatkinsoni.mesophotic,aes(x=mesophotic.reef,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
#   geom_line(data=predicts.legalatkinsoni.mesophotic,aes(x=mesophotic.reef,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
#   theme_classic()+
#   Theme1
# ggmod.legalatkinsoni.mesophotic
# 
# # MODEL Sublegal atkinsoni (depth + detrended + mean relief) ----
# dat.sublegalatkinsoni <- dat %>% filter(response=="sublegal size atkinsoni")
# 
# gamm=gam(number~s(depth,k=3,bs='cr') + s(detrended,k=3,bs='cr')+ s(mean.relief,k=3,bs='cr'), family=tw,data=dat.sublegalatkinsoni)
# 
# # predict - depth ----
# mod<-gamm
# testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 20),
#                         detrended=mean(mod$model$detrended),
#                         mean.relief=mean(mod$model$mean.relief)) %>%
#   distinct()%>%
#   glimpse()
# 
# fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
# 
# predicts.sublegalatkinsoni.depth = testdata%>%data.frame(fits)%>%
#   group_by(depth)%>% #only change here
#   summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
#   ungroup()
# 
# # predict - detrended ----
# mod<-gamm
# testdata <- expand.grid(detrended=seq(min(dat$detrended),max(dat$detrended),length.out = 20),
#                         depth=mean(mod$model$depth),
#                         mean.relief=mean(mod$model$mean.relief)) %>%
#   distinct()%>%
#   glimpse()
# 
# fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
# 
# predicts.sublegalatkinsoni.detrended = testdata%>%data.frame(fits)%>%
#   group_by(detrended)%>% #only change here
#   summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
#   ungroup()
# 
# # predict - mean relief ----
# mod<-gamm
# testdata <- expand.grid(mean.relief=seq(min(dat$mean.relief),max(dat$mean.relief),length.out = 20),
#                         depth=mean(mod$model$depth),
#                         detrended=mean(mod$model$detrended)) %>%
#   distinct()%>%
#   glimpse()
# 
# fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
# 
# predicts.sublegalatkinsoni.relief = testdata%>%data.frame(fits)%>%
#   group_by(mean.relief)%>% #only change here
#   summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
#   ungroup()
# 
# # PLOTS for Subegal atkinsoni ----
# # depth ----
# ggmod.sublegalatkinsoni.depth<- ggplot() +
#   ylab("")+
#   xlab("Depth")+
#   geom_point(data=dat.sublegalatkinsoni,aes(x=depth,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
#   geom_line(data=predicts.sublegalatkinsoni.depth,aes(x=depth,y=maxn),alpha=0.5)+
#   geom_line(data=predicts.sublegalatkinsoni.depth,aes(x=depth,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
#   geom_line(data=predicts.sublegalatkinsoni.depth,aes(x=depth,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
#   theme_classic()+
#   Theme1+
#   ggtitle("Sublegal Lethrinus atkinsoni") +
#   theme(plot.title = element_text(hjust = 0))+
#   annotation_raster(l.a, xmin=45, xmax=65, ymin=20, ymax=25)
# ggmod.sublegalatkinsoni.depth
# 
# # detrended ----
# ggmod.sublegalatkinsoni.detrended<- ggplot() +
#   ylab("")+
#   xlab("Detrended bathymetry")+
#   geom_point(data=dat.sublegalatkinsoni,aes(x=detrended,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
#   geom_line(data=predicts.sublegalatkinsoni.detrended,aes(x=detrended,y=maxn),alpha=0.5)+
#   geom_line(data=predicts.sublegalatkinsoni.detrended,aes(x=detrended,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
#   geom_line(data=predicts.sublegalatkinsoni.detrended,aes(x=detrended,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
#   theme_classic()+
#   Theme1
# ggmod.sublegalatkinsoni.detrended
# 
# # mean relief ----
# ggmod.sublegalatkinsoni.relief<- ggplot() +
#   ylab("")+
#   xlab("Mean relief")+
#   geom_point(data=dat.sublegalatkinsoni,aes(x=mean.relief,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
#   geom_line(data=predicts.sublegalatkinsoni.relief,aes(x=mean.relief,y=maxn),alpha=0.5)+
#   geom_line(data=predicts.sublegalatkinsoni.relief,aes(x=mean.relief,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
#   geom_line(data=predicts.sublegalatkinsoni.relief,aes(x=mean.relief,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
#   theme_classic()+
#   Theme1
# ggmod.sublegalatkinsoni.relief
# 
# # MODEL Lethrinus atkinsoni (depth + detrended + mean relief) ----
# dat.atkinsoni <- dat %>% filter(response=="Lethrinus atkinsoni")
# 
# gamm=gam(number~s(depth,k=3,bs='cr') + s(detrended,k=3,bs='cr')+ s(mean.relief,k=3,bs='cr'), family=tw,data=dat.atkinsoni)
# 
# # predict - depth ----
# mod<-gamm
# testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 20),
#                         detrended=mean(mod$model$detrended),
#                         mean.relief=mean(mod$model$mean.relief)) %>%
#   distinct()%>%
#   glimpse()
# 
# fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
# 
# predicts.atkinsoni.depth = testdata%>%data.frame(fits)%>%
#   group_by(depth)%>% #only change here
#   summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
#   ungroup()
# 
# # predict - detrended ----
# mod<-gamm
# testdata <- expand.grid(detrended=seq(min(dat$detrended),max(dat$detrended),length.out = 20),
#                         depth=mean(mod$model$depth),
#                         mean.relief=mean(mod$model$mean.relief)) %>%
#   distinct()%>%
#   glimpse()
# 
# fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
# 
# predicts.atkinsoni.detrended = testdata%>%data.frame(fits)%>%
#   group_by(detrended)%>% #only change here
#   summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
#   ungroup()
# 
# # predict - mean relief ----
# mod<-gamm
# testdata <- expand.grid(mean.relief=seq(min(dat$mean.relief),max(dat$mean.relief),length.out = 20),
#                         depth=mean(mod$model$depth),
#                         detrended=mean(mod$model$detrended)) %>%
#   distinct()%>%
#   glimpse()
# 
# fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
# 
# predicts.atkinsoni.relief = testdata%>%data.frame(fits)%>%
#   group_by(mean.relief)%>% #only change here
#   summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
#   ungroup()
# 
# # PLOTS for atkinsoni abundance ----
# # depth ----
# ggmod.atkinsoni.depth<- ggplot() +
#   ylab("")+
#   xlab("Depth")+
#   geom_point(data=dat.atkinsoni,aes(x=depth,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
#   geom_line(data=predicts.atkinsoni.depth,aes(x=depth,y=maxn),alpha=0.5)+
#   geom_line(data=predicts.atkinsoni.depth,aes(x=depth,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
#   geom_line(data=predicts.atkinsoni.depth,aes(x=depth,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
#   theme_classic()+
#   Theme1+
#   ggtitle("Lethrinus atkinsoni") +
#   theme(plot.title = element_text(hjust = 0))+
#   annotation_raster(l.a, xmin=45, xmax=65, ymin=30, ymax=37.5)
# ggmod.atkinsoni.depth
# 
# # detrended ----
# ggmod.atkinsoni.detrended<- ggplot() +
#   ylab("")+
#   xlab("Detrended bathymetry")+
#   geom_point(data=dat.atkinsoni,aes(x=detrended,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
#   geom_line(data=predicts.atkinsoni.detrended,aes(x=detrended,y=maxn),alpha=0.5)+
#   geom_line(data=predicts.atkinsoni.detrended,aes(x=detrended,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
#   geom_line(data=predicts.atkinsoni.detrended,aes(x=detrended,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
#   theme_classic()+
#   Theme1
# ggmod.atkinsoni.detrended
# 
# # mean relief ----
# ggmod.atkinsoni.relief<- ggplot() +
#   ylab("")+
#   xlab("Mean relief")+
#   geom_point(data=dat.atkinsoni,aes(x=mean.relief,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
#   geom_line(data=predicts.atkinsoni.relief,aes(x=mean.relief,y=maxn),alpha=0.5)+
#   geom_line(data=predicts.atkinsoni.relief,aes(x=mean.relief,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
#   geom_line(data=predicts.atkinsoni.relief,aes(x=mean.relief,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
#   theme_classic()+
#   Theme1
# ggmod.atkinsoni.relief
# 
# # MODEL Pomacentrus coelestis (depth + detrended + mean relief) ----
# dat.coelestis <- dat %>% filter(response=="Pomacentrus coelestis")
# 
# gamm=gam(number~s(depth,k=3,bs='cr') + s(detrended,k=3,bs='cr')+ s(mean.relief,k=3,bs='cr'), family=tw,data=dat.coelestis)
# 
# # predict - depth ----
# mod<-gamm
# testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 20),
#                         detrended=mean(mod$model$detrended),
#                         mean.relief=mean(mod$model$mean.relief)) %>%
#   distinct()%>%
#   glimpse()
# 
# fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
# 
# predicts.coelestis.depth = testdata%>%data.frame(fits)%>%
#   group_by(depth)%>% #only change here
#   summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
#   ungroup()
# 
# # predict - detrended ----
# mod<-gamm
# testdata <- expand.grid(detrended=seq(min(dat$detrended),max(dat$detrended),length.out = 20),
#                         depth=mean(mod$model$depth),
#                         mean.relief=mean(mod$model$mean.relief)) %>%
#   distinct()%>%
#   glimpse()
# 
# fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
# 
# predicts.coelestis.detrended = testdata%>%data.frame(fits)%>%
#   group_by(detrended)%>% #only change here
#   summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
#   ungroup()
# 
# # predict - mean relief ----
# mod<-gamm
# testdata <- expand.grid(mean.relief=seq(min(dat$mean.relief),max(dat$mean.relief),length.out = 20),
#                         depth=mean(mod$model$depth),
#                         detrended=mean(mod$model$detrended)) %>%
#   distinct()%>%
#   glimpse()
# 
# fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
# 
# predicts.coelestis.relief = testdata%>%data.frame(fits)%>%
#   group_by(mean.relief)%>% #only change here
#   summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
#   ungroup()
# 
# # PLOTS for Pomacentrus coelestis abundance ----
# # depth ----
# #load fish pic
# p.c <- readPNG("data/images/Pomacentrus coelestis-3cmL.png")
# p.c <- as.raster(p.c)
# 
# ggmod.coelestis.depth<- ggplot() +
#   ylab("")+
#   xlab("Depth")+
#   geom_point(data=dat.coelestis,aes(x=depth,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
#   geom_line(data=predicts.coelestis.depth,aes(x=depth,y=maxn),alpha=0.5)+
#   geom_line(data=predicts.coelestis.depth,aes(x=depth,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
#   geom_line(data=predicts.coelestis.depth,aes(x=depth,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
#   theme_classic()+
#   Theme1+
#   ggtitle("Pomacentrus coelestis") +
#   theme(plot.title = element_text(hjust = 0))+
#   annotation_raster(p.c, xmin=47.5, xmax=62.5, ymin=200, ymax=240)
# ggmod.coelestis.depth
# 
# # detrended ----
# ggmod.coelestis.detrended<- ggplot() +
#   ylab("")+
#   xlab("Detrended bathymetry")+
#   geom_point(data=dat.coelestis,aes(x=detrended,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
#   geom_line(data=predicts.coelestis.detrended,aes(x=detrended,y=maxn),alpha=0.5)+
#   geom_line(data=predicts.coelestis.detrended,aes(x=detrended,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
#   geom_line(data=predicts.coelestis.detrended,aes(x=detrended,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
#   theme_classic()+
#   Theme1
# ggmod.coelestis.detrended
# 
# # mean relief ----
# ggmod.coelestis.relief<- ggplot() +
#   ylab("")+
#   xlab("Mean relief")+
#   geom_point(data=dat.coelestis,aes(x=mean.relief,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
#   geom_line(data=predicts.coelestis.relief,aes(x=mean.relief,y=maxn),alpha=0.5)+
#   geom_line(data=predicts.coelestis.relief,aes(x=mean.relief,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
#   geom_line(data=predicts.coelestis.relief,aes(x=mean.relief,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
#   theme_classic()+
#   Theme1
# ggmod.coelestis.relief
# 
# # MODEL Chromis fumea (depth + detrended + mean relief) ----
# dat.fumea <- dat %>% filter(response=="Chromis fumea")
# 
# gamm=gam(number~s(depth,k=3,bs='cr') + s(detrended,k=3,bs='cr')+ s(mean.relief,k=3,bs='cr'), family=tw,data=dat.fumea)
# 
# # predict - depth ----
# mod<-gamm
# testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 20),
#                         detrended=mean(mod$model$detrended),
#                         mean.relief=mean(mod$model$mean.relief)) %>%
#   distinct()%>%
#   glimpse()
# 
# fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
# 
# predicts.fumea.depth = testdata%>%data.frame(fits)%>%
#   group_by(depth)%>% #only change here
#   summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
#   ungroup()
# 
# # predict - detrended ----
# mod<-gamm
# testdata <- expand.grid(detrended=seq(min(dat$detrended),max(dat$detrended),length.out = 20),
#                         depth=mean(mod$model$depth),
#                         mean.relief=mean(mod$model$mean.relief)) %>%
#   distinct()%>%
#   glimpse()
# 
# fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
# 
# predicts.fumea.detrended = testdata%>%data.frame(fits)%>%
#   group_by(detrended)%>% #only change here
#   summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
#   ungroup()
# 
# # predict - mean relief ----
# mod<-gamm
# testdata <- expand.grid(mean.relief=seq(min(dat$mean.relief),max(dat$mean.relief),length.out = 20),
#                         depth=mean(mod$model$depth),
#                         detrended=mean(mod$model$detrended)) %>%
#   distinct()%>%
#   glimpse()
# 
# fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
# 
# predicts.fumea.relief = testdata%>%data.frame(fits)%>%
#   group_by(mean.relief)%>% #only change here
#   summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
#   ungroup()
# 
# # PLOTS for Chromis fumea abundance ----
# # depth ----
# #load fish pic
# c.f <- readPNG("data/images/Pomacentridae-Dark.png")
# c.f <- as.raster(c.f)
# 
# ggmod.fumea.depth<- ggplot() +
#   ylab("")+
#   xlab("Depth")+
#   geom_point(data=dat.fumea,aes(x=depth,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
#   geom_line(data=predicts.fumea.depth,aes(x=depth,y=maxn),alpha=0.5)+
#   geom_line(data=predicts.fumea.depth,aes(x=depth,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
#   geom_line(data=predicts.fumea.depth,aes(x=depth,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
#   theme_classic()+
#   Theme1+
#   ggtitle("Chromis fumea") +
#   theme(plot.title = element_text(hjust = 0))+
#   annotation_raster(c.f, xmin=47.5, xmax=62.5, ymin=95, ymax=115)
# ggmod.fumea.depth
# 
# # detrended ----
# ggmod.fumea.detrended<- ggplot() +
#   ylab("")+
#   xlab("Detrended bathymetry")+
#   geom_point(data=dat.fumea,aes(x=detrended,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
#   geom_line(data=predicts.fumea.detrended,aes(x=detrended,y=maxn),alpha=0.5)+
#   geom_line(data=predicts.fumea.detrended,aes(x=detrended,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
#   geom_line(data=predicts.fumea.detrended,aes(x=detrended,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
#   theme_classic()+
#   Theme1
# ggmod.fumea.detrended
# 
# # mean relief ----
# ggmod.fumea.relief<- ggplot() +
#   ylab("")+
#   xlab("Mean relief")+
#   geom_point(data=dat.fumea,aes(x=mean.relief,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
#   geom_line(data=predicts.fumea.relief,aes(x=mean.relief,y=maxn),alpha=0.5)+
#   geom_line(data=predicts.fumea.relief,aes(x=mean.relief,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
#   geom_line(data=predicts.fumea.relief,aes(x=mean.relief,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
#   theme_classic()+
#   Theme1
# ggmod.fumea.relief

# Combine with cowplot
library(cowplot)

# view plots
plot.grid.gam <- plot_grid(ggmod.total.depth, ggmod.total.relief, ggmod.total.detrended,
                                 ggmod.species.detrended, ggmod.species.relief,ggmod.species.roughness,
                                 ggmod.legal.depth,ggmod.legal.relief, NULL,
                                 ggmod.sublegal.depth,ggmod.sublegal.detrended,ggmod.sublegal.relief,
                                 ncol = 3, labels = c('a','b','c','d','e','f', 'g','h','','i','j',"k"),align = "vh")
plot.grid.gam


#Save plots
save_plot("plots/montes.synthesis.gam.png", plot.grid.gam,base_height = 9,base_width = 8.5)
