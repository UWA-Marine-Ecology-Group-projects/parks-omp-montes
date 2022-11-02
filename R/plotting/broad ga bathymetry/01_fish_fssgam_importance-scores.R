###
# Project: Parks - Montes synthesis
# Data:    BRUV fish, habitat
# Task:    Plotting fish importance scores
# author:  Claude
# date:    Dec 2021-Feb 2022
##

rm(list=ls())

# Plotting defaults----
library(ggplot2)
library(dplyr)
library(ggtext)
library(tidyr)
library(cowplot)

## Set working directory----
working.dir <- getwd()
setwd(working.dir)
#OR Set manually once

datmaxn <- read.csv("output/fssgam - fish_broad/montebello.synthesis_all.var.imp.csv")%>%
  glimpse()

datlength <- read.csv("output/fssgam - fish_broad/montebello.synthesis_length_all.var.imp.csv")%>%
  glimpse()

#read in data - negative values manually added
#manually add in Xs for terms included in most parsimonious models
dat.taxa <-bind_rows(datmaxn,datlength)%>% #from local copy
  # dplyr::select(-sd.relief)%>% # BG turned this off
  rename(resp.var=X)%>%
  gather(key=predictor,value=importance,2:ncol(.))%>%
  mutate(label=NA)%>%
  mutate(resp.var=factor(resp.var, levels = c("smaller than legal size","greater than legal size","species.richness","total.abundance")))%>%
  mutate(predictor=factor(predictor, levels = c("depth", "mean.relief", "detrended", "roughness","biota.consolidated", "mesophotic.reef","photic.reef"
    # "depth","mean.relief","detrended","roughness","tpi","mesophotic.reef","photic.reef","status"
    )))%>%
  mutate(label=ifelse(predictor=="mean.relief"&resp.var=="total.abundance","X",label))%>%
  mutate(label=ifelse(predictor=="mean.relief"&resp.var=="species.richness","X",label))%>%
  mutate(label=ifelse(predictor=="mean.relief"&resp.var=="greater than legal size","X",label))%>%
  mutate(label=ifelse(predictor=="depth"&resp.var=="smaller than legal size","X",label))%>%
  glimpse()

# Theme-
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill="white"),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=8),
    legend.title = element_text(size=8, face="bold"),
    legend.position = "top",
    legend.direction="horizontal",
    text=element_text(size=10),
    strip.text.y = element_text(size = 10,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=10),
    axis.title.y=element_text(vjust=0.6, angle=90, size=10),
    axis.text.x=element_text(size=10,angle = 90, hjust=1,vjust=0.5),
    axis.text.y=element_text(size=10),#,face="italic"),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank())

# colour ramps-
re <- colorRampPalette(c("blue3", "white","red2"))(200)

# Labels-
legend_title<-"Importance"

# Plot gg.importance.scores ----
# gg.importance.scores <- ggplot(dat.taxa, aes(x=predictor,y=resp.var,fill=importance)) +
#    geom_tile(show.legend=T) +
#    scale_fill_gradientn(legend_title, colours=c(re), na.value = "grey98",
#                          limits = c(-1, 1))+
#   scale_y_discrete(labels=c("Smaller than legal size","Greater than legal size","Species richness","Total abundance"))+
#   scale_x_discrete(labels=c("Depth","Mean relief","Detrended","Roughness","TPI","Invertebrate reef","Macroalgae/coral reef","Status"))+
#    xlab(NULL)+
#    ylab(NULL)+
#    theme_classic()+
#    Theme1+
#    geom_text(aes(label=label))+
#   theme(axis.text.y = ggtext::element_markdown())
# gg.importance.scores

gg.importance.full <- ggplot(dat.taxa%>%dplyr::filter(resp.var%in%c("total.abundance", "species.richness")), 
                             aes(x=predictor,y=resp.var,fill=importance))+
  geom_tile(show.legend=T) +
  scale_fill_gradientn(legend_title,colours=c("white", re), na.value = "grey98",
                       limits = c(-1.01, 1))+
  scale_y_discrete(labels=c("Species richness","Total abundance"))+         # Tidy Taxa names
  labs(x = NULL, y = NULL, title = "Whole assemblage") +
  theme_classic()+
  Theme1+
  geom_text(aes(label=label)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.line.x = element_blank(),
        axis.text.y = ggtext::element_markdown(),
        plot.title = element_text(hjust = -0.43, vjust = -15)) # Looks crap here but title comes back in exported version
gg.importance.full

gg.importance.tar <- ggplot(dat.taxa%>%dplyr::filter(resp.var%in%c("greater than legal size", "smaller than legal size")), 
                            aes(x=predictor,y=resp.var,fill=importance))+
  geom_tile(show.legend=F) +
  scale_fill_gradientn(legend_title,colours=c("white", re), na.value = "grey98",
                       limits = c(-1.01, 1))+
  scale_y_discrete(labels=c("Smaller than legal size","Greater than legal size"))+         #Tidy Taxa names
  # scale_x_discrete(labels = c("Depth","Mean relief","Detrended","Roughness","TPI",
  #                             "Invertebrate reef","Macroalgae/coral reef","Status"))+   #Tidy predictor names
  
  scale_x_discrete(labels = c("Depth","Mean relief","Detrended","Roughness", "Consolidated (rock)",
                              "Invertebrate reef","Macroalgae/coral reef"))+   #Tidy predictor names
  
  labs(x = NULL, y = NULL, title = "Targeted assemblage") +
  theme_classic()+
  Theme1+
  theme(axis.text.y = ggtext::element_markdown(),
        plot.title = element_text(hjust = -0.45))+
  geom_text(aes(label=label))
gg.importance.tar

gg.importance.scores <- gg.importance.full / gg.importance.tar
gg.importance.scores

#save output - changed dimensions for larger text in report
save_plot("plots/montes.synthesis.fish.importance.png", gg.importance.scores,base_height = 5,base_width = 7)

# testing relationship direction
# library(mgcv)
# dat1 <- readRDS('data/tidy/dat.maxn.rds')%>%
#   glimpse()
# 
# dat2 <- readRDS('data/tidy/dat.length.rds')%>%
#   dplyr::mutate(status=as.factor(status))%>%
#   glimpse()
# str(dat2)
# 
# #total abundance
# dat.tot <- dat1 %>% filter(response=="total.abundance")
# 
# mod=gam(number~ status, family=tw, data=dat.tot) #s(mean.relief,k=3,bs = "cr")
# plot(mod,pages=1,all.terms = T)
# summary(mod)
# 
# #species richness
# dat.sr <- dat1 %>% filter(response=="species.richness")
# 
# mod=gam(number~ status, family=tw, data=dat.sr) #s(mean.relief,k=3,bs = "cr")
# plot(mod,pages=1,all.terms = T)
# summary(mod)
# 
# #greater than legal size
# dat.leg <- dat2 %>% filter(response=="greater than legal size")
# 
# mod=gam(number~ status, family=tw, data=dat.leg) #s(mean.relief,k=3,bs = "cr")
# plot(mod,pages=1,all.terms = T)
# summary(mod)
# 
# #smaller than legal size
# dat.sub <- dat2 %>% filter(response=="smaller than legal size")
# 
# mod=gam(number~ s(tpi,k=3,bs = "cr"), family=tw, data=dat.sub) #s(mean.relief,k=3,bs = "cr")
# plot(mod,pages=1,all.terms = T)
# 
