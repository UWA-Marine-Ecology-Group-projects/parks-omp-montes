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
combined.maxn <- readRDS("data/tidy/dat.maxn.rds")%>%
  dplyr::select(-scientific)%>%
  glimpse()

combined.length <- readRDS("data/tidy/dat.length.rds")%>%
  glimpse()

dat <- bind_rows(combined.maxn, combined.length)

###   TEMPLATE   ###
###              ###
###              ###
###              ###
# Manually make the most parsimonious GAM models for each taxa ----
#### montes MaxN ####
unique(dat$scientific)

# MODEL Total abundance (depth + mean relief + tpi) ----
dat.total <- dat %>% filter(response=="total.abundance")

gamm=gam(number~s(depth,k=3,bs='cr') + s(mean.relief,k=3,bs='cr')+ s(tpi,k=3,bs='cr'), family=tw,data=dat.total)

# predict - depth ----
mod<-gamm
testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 20),
                        mean.relief=mean(mod$model$mean.relief),
                        tpi=mean(mod$model$tpi)) %>%
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
                        tpi=mean(mod$model$tpi)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.total.mean.relief = testdata%>%data.frame(fits)%>%
  group_by(mean.relief)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - tpi ----
mod<-gamm
testdata <- expand.grid(tpi=seq(min(dat$tpi),max(dat$tpi),length.out = 20),
                        depth=mean(mod$model$depth),
                        mean.relief=mean(mod$model$mean.relief)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.total.tpi = testdata%>%data.frame(fits)%>%
  group_by(tpi)%>% #only change here
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

# tpi ----
ggmod.total.tpi<- ggplot() +
  ylab("")+
  xlab("TPI")+
  geom_point(data=dat.total,aes(x=tpi,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.total.tpi,aes(x=tpi,y=maxn),alpha=0.5)+
  geom_line(data=predicts.total.tpi,aes(x=tpi,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.total.tpi,aes(x=tpi,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.total.tpi

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
  xlab("Detrended bathymetry")+
  geom_point(data=dat.species,aes(x=mean.relief,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.species.relief,aes(x=mean.relief,y=maxn),alpha=0.5)+
  geom_line(data=predicts.species.relief,aes(x=mean.relief,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.species.relief,aes(x=mean.relief,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.species.relief

# mean relief ----
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

# MODEL Targeted abundance (depth + mesophotic reef) ----
dat.targeted <- dat %>% filter(response=="targeted.abundance")

gamm=gam(number~s(depth,k=3,bs='cr') + s(mesophotic.reef,k=3,bs='cr'), family=tw,data=dat.targeted)

# predict - depth ----
mod<-gamm
testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 20),
                        mesophotic.reef=mean(mod$model$mesophotic.reef)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.targeted.depth = testdata%>%data.frame(fits)%>%
  group_by(depth)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - mesophotic reef ----
mod<-gamm
testdata <- expand.grid(mesophotic.reef=seq(min(dat$mesophotic.reef),max(dat$mesophotic.reef),length.out = 20),
                        depth=mean(mod$model$depth)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.targeted.mesophotic = testdata%>%data.frame(fits)%>%
  group_by(mesophotic.reef)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Targeted abundnace ----
# depth ----
ggmod.targeted.depth<- ggplot() +
  ylab("")+
  xlab("Depth")+
  geom_point(data=dat.targeted,aes(x=depth,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.targeted.depth,aes(x=depth,y=maxn),alpha=0.5)+
  geom_line(data=predicts.targeted.depth,aes(x=depth,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.targeted.depth,aes(x=depth,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Targeted abundance") +
  theme(plot.title = element_text(hjust = 0))
ggmod.targeted.depth

# mesophotic reef ----
ggmod.targeted.mesophotic<- ggplot() +
  ylab("")+
  xlab("Mesophotic reef")+
  geom_point(data=dat.targeted,aes(x=mesophotic.reef,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.targeted.mesophotic,aes(x=mesophotic.reef,y=maxn),alpha=0.5)+
  geom_line(data=predicts.targeted.mesophotic,aes(x=mesophotic.reef,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.targeted.mesophotic,aes(x=mesophotic.reef,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.targeted.mesophotic

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

# MODEL Sublegals (depth + detrended + mesophotic) ----
dat.sublegal <- dat %>% filter(response=="smaller than legal size")

gamm=gam(number~s(depth,k=3,bs='cr') + s(detrended,k=3,bs='cr')+ s(mesophotic.reef,k=3,bs='cr'), family=tw,data=dat.sublegal)

# predict - depth ----
mod<-gamm
testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 20),
                        detrended=mean(mod$model$detrended),
                        mesophotic.reef=mean(mod$model$mesophotic.reef)) %>%
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
                        mesophotic.reef=mean(mod$model$mesophotic.reef)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sublegal.detrended = testdata%>%data.frame(fits)%>%
  group_by(detrended)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - mesophotic reef ----
mod<-gamm
testdata <- expand.grid(mesophotic.reef=seq(min(dat$mesophotic.reef),max(dat$mesophotic.reef),length.out = 20),
                        depth=mean(mod$model$depth),
                        detrended=mean(mod$model$detrended)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sublegal.mesophotic = testdata%>%data.frame(fits)%>%
  group_by(mesophotic.reef)%>% #only change here
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
  xlab("Detrended")+
  geom_point(data=dat.sublegal,aes(x=detrended,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sublegal.detrended,aes(x=detrended,y=maxn),alpha=0.5)+
  geom_line(data=predicts.sublegal.detrended,aes(x=detrended,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sublegal.detrended,aes(x=detrended,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.sublegal.detrended

# mesophotic reef ----
ggmod.sublegal.mesophotic<- ggplot() +
  ylab("")+
  xlab("Mesophotic reef")+
  geom_point(data=dat.sublegal,aes(x=mesophotic.reef,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sublegal.mesophotic,aes(x=mesophotic.reef,y=maxn),alpha=0.5)+
  geom_line(data=predicts.sublegal.mesophotic,aes(x=mesophotic.reef,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sublegal.mesophotic,aes(x=mesophotic.reef,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.sublegal.mesophotic

# MODEL Legal Spango (status) ----
dat.spango <- dat %>% filter(response=="legal size spango")

gamm=gam(number~status, family=tw,data=dat.spango)

# predict - status ----
mod<-gamm
testdata <- expand.grid(status = c("Fished","No-take"))%>%
  distinct()%>%
  glimpse()


fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.spango.status = testdata%>%data.frame(fits)%>%
  group_by(status)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Legal spango ----
# status ----
ggmod.spango.status<- ggplot(aes(x=status,y=maxn,fill=status,colour=status), data=predicts.spango.status) +
  ylab("")+
  xlab('Status')+
  scale_fill_manual(labels = c("Fished", "No-take"),values=c("grey", "#1470ad"))+
  scale_colour_manual(labels = c("Fished", "No-take"),values=c("black", "black"))+
  scale_x_discrete(limits = rev(levels(predicts.spango.status$status)))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = maxn-se.fit,ymax = maxn+se.fit),width = 0.5) +
  theme_classic()+
  Theme1+
  scale_y_continuous(expand = expansion(mult = c(0, .1)))+
  theme(legend.position = "none")+
  ggtitle("Legal Lethrinus nebulosus") +
  theme(plot.title = element_text(hjust = 0))
ggmod.spango.status

# MODEL Sublegal trout (depth + mean relief) ----
dat.sublegaltrout <- dat %>% filter(response=="smaller than legal size")

gamm=gam(number~s(depth,k=3,bs='cr') + s(detrended,k=3,bs='cr')+ s(mesophotic.reef,k=3,bs='cr'), family=tw,data=dat.sublegal)

# predict - depth ----
mod<-gamm
testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 20),
                        detrended=mean(mod$model$detrended),
                        mesophotic.reef=mean(mod$model$mesophotic.reef)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sublegal.depth = testdata%>%data.frame(fits)%>%
  group_by(depth)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

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