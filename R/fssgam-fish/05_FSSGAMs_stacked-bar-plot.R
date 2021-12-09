###
# Project: Parks - Abrolhos
# Data:    BOSS fish, habitat
# Task:    Plotting 10 most abundant species w/ cute pics
# author:  Claude
# date:    Nov-Dec 2021
##

# Set directories----
rm(list=ls())

# Study name ----
study <- "montebello.synthesis" 

# Libraries required
library(GlobalArchive)
library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(plyr)
library(ggmap)
library(rgdal)
library(raster)
library(png)
library(cowplot)

## Set your working directory ----
working.dir <- getwd()
setwd(working.dir)
#OR set manually once

theme_collapse<-theme(      
  panel.grid.major=element_line(colour = "white"), 
  panel.grid.minor=element_line(colour = "white", size = 0.25), 
  plot.margin= grid::unit(c(0, 0, 0, 0), "in"))

theme.larger.text<-theme(
  strip.text.x = element_text(size = 5,angle = 0),
  strip.text.y = element_text(size = 5),
  axis.title.x=element_text(vjust=-0.0, size=10),
  axis.title.y=element_text(vjust=0.0,size=10),
  axis.text.x=element_text(size=8),
  axis.text.y=element_text(size=8),
  legend.title = element_text(family="TN",size=8),
  legend.text = element_text(family="TN",size=8))

# read in maxn
maxn <- read.csv("data/Tidy/montebello.synthesis.checked.maxn.csv")%>%
  glimpse()

metadata <- read.csv('data/Tidy/montebello.synthesis.checked.metadata.csv')%>%
  glimpse()

# workout total maxn for each species ---
maxn.10<-maxn%>%
  mutate(scientific=paste(genus,species,sep=" "))%>%
  group_by(scientific)%>%
  dplyr::summarise(maxn=sum(maxn))%>%
  ungroup()%>%
  top_n(12)%>%
  dplyr::filter(!scientific%in%c('Unknown sp10', 'Unknown spp'))
  glimpse()


## Total frequency of occurance 
# I think we could remove this section - but maybe good to see sometimes
bar<-ggplot(maxn.10, aes(x=reorder(scientific,maxn), y=maxn)) +   
  geom_bar(stat="identity",position=position_dodge())+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  #scale_x_discrete(limits = rev(levels(scientific)))+
  #annotation_custom(lcpic, xmin=0.5, xmax=2.5, ymin=.75, ymax=1.5)+ 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_collapse
bar


# Load fish pictures for plotting ----
p.c <- readPNG("data/images/Pomacentrus coelestis-3cmL.png")
p.c <- as.raster(p.c)

c.f <- readPNG("data/images/Pomacentridae-Dark.png")
c.f <- as.raster(c.f)

l.a <- readPNG("data/images/Lethrinus atkinsoni 5cmL.png")
l.a <- as.raster(l.a)

c.fv <- readPNG("data/images/Carangoides fulvoguttatus-3cmL.png")
c.fv <- as.raster(c.fv)

p.p <- readPNG("data/images/Pentapodus porosus-3cmL.png")
p.p <- as.raster(p.p)

c.t <- readPNG("data/images/Labridae-Dark.png")
c.t <- as.raster(c.t)

l.c <- readPNG("data/images/Labridae-Dark.png")
l.c <- as.raster(l.c)

c.g <- readPNG("data/images/Carangoides gymnostethus 3cm.png")
c.g <- as.raster(c.g)

p.o <- readPNG("data/images/Pomacentridae-Dark.png")
p.o <- as.raster(p.o)

c.c <- readPNG("data/images/Caesio cuning-3cmL.png")
c.c <- as.raster(c.c)

## Top ten plot ----
bar.top.10<-ggplot(maxn.10, aes(x=reorder(scientific,maxn), y=maxn)) +   
  geom_bar(stat="identity",colour="black",fill="lightgrey",position=position_dodge())+
  ylim (0, 1250)+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  theme_bw()+
  theme(axis.text.y = element_text(face="italic"))+
  theme_collapse+
  theme.larger.text+
  annotation_raster(p.c, xmin=9.7,xmax=10.2,ymin=1200, ymax=1300)+
  annotation_raster(c.f, xmin=8.7,xmax=9.25,ymin=750, ymax=850)+
  annotation_raster(l.a, xmin=7.7, xmax=8.4, ymin=650, ymax=850)+
  annotation_raster(c.fv, xmin=6.5,xmax=7.5,ymin=600, ymax=900)+
  annotation_raster(p.p, xmin=5.7,xmax=6.3,ymin=515, ymax=690)+
  annotation_raster(c.t, xmin=4.7,xmax=5.3,ymin=425, ymax=600)+
  annotation_raster(l.c, xmin=3.65,xmax=4.25,ymin=405, ymax=580)+
  annotation_raster(c.g, xmin=2.5,xmax=3.5,ymin=365, ymax=660)+
  annotation_raster(p.o, xmin=1.725,xmax=2.275,ymin=350, ymax=450)+
  annotation_raster(c.c, xmin=0.7,xmax=1.3,ymin=300, ymax=530)
bar.top.10

#save out plot
ggsave("Plots/stacked.bar.plot.png",bar.top.10,dpi=600,width=6.0)
