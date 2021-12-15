###
# Project: Montes synthesis
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
maxn <- read.csv("data/tidy/montebello.synthesis.checked.maxn.csv")%>%
  glimpse()

metadata <- read.csv('data/tidy/montebello.synthesis.checked.metadata.csv')%>%
  glimpse()

# # workout total maxn for each species ---
# maxn.10<-maxn%>%
#   mutate(scientific=paste(genus,species,sep=" "))%>%
#   group_by(scientific)%>%
#   dplyr::summarise(maxn=sum(maxn))%>%
#   ungroup()%>%
#   top_n(12)%>%
#   dplyr::filter(!scientific%in%c('Unknown sp10', 'Unknown spp'))%>%
#   glimpse()
# 
# 
# ## Total frequency of occurance 
# # I think we could remove this section - but maybe good to see sometimes
# bar<-ggplot(maxn.10, aes(x=reorder(scientific,maxn), y=maxn)) +   
#   geom_bar(stat="identity",position=position_dodge())+
#   coord_flip()+
#   xlab("Species")+
#   ylab(expression(Overall~abundance~(Sigma~MaxN)))+
#   #scale_x_discrete(limits = rev(levels(scientific)))+
#   #annotation_custom(lcpic, xmin=0.5, xmax=2.5, ymin=.75, ymax=1.5)+ 
#   theme_bw()+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#   theme_collapse
# bar
# 
# 
# # Load fish pictures for plotting ----
# p.c <- readPNG("data/images/Pomacentrus coelestis-3cmL.png")
# p.c <- as.raster(p.c)
# 
# c.f <- readPNG("data/images/Pomacentridae-Dark.png")
# c.f <- as.raster(c.f)
# 
# l.a <- readPNG("data/images/Lethrinus atkinsoni 5cmL.png")
# l.a <- as.raster(l.a)
# 
# c.fv <- readPNG("data/images/Carangoides fulvoguttatus-3cmL.png")
# c.fv <- as.raster(c.fv)
# 
# p.p <- readPNG("data/images/Pentapodus porosus-3cmL.png")
# p.p <- as.raster(p.p)
# 
# c.t <- readPNG("data/images/Labridae-Dark.png")
# c.t <- as.raster(c.t)
# 
# l.c <- readPNG("data/images/Labridae-Dark.png")
# l.c <- as.raster(l.c)
# 
# c.g <- readPNG("data/images/Carangoides gymnostethus 3cm.png")
# c.g <- as.raster(c.g)
# 
# p.o <- readPNG("data/images/Pomacentridae-Dark.png")
# p.o <- as.raster(p.o)
# 
# c.c <- readPNG("data/images/Caesio cuning-3cmL.png")
# c.c <- as.raster(c.c)
# 
# ## Top ten plot ----
# bar.top.10<-ggplot(maxn.10, aes(x=reorder(scientific,maxn), y=maxn)) +   
#   geom_bar(stat="identity",colour="black",fill="lightgrey",position=position_dodge())+
#   ylim (0, 1250)+
#   coord_flip()+
#   xlab("Species")+
#   ylab(expression(Overall~abundance~(Sigma~MaxN)))+
#   theme_bw()+
#   theme(axis.text.y = element_text(face="italic"))+
#   theme_collapse+
#   theme.larger.text+
#   annotation_raster(p.c, xmin=9.7,xmax=10.2,ymin=1200, ymax=1300)+
#   annotation_raster(c.f, xmin=8.7,xmax=9.25,ymin=750, ymax=850)+
#   annotation_raster(l.a, xmin=7.7, xmax=8.4, ymin=650, ymax=850)+
#   annotation_raster(c.fv, xmin=6.5,xmax=7.5,ymin=600, ymax=900)+
#   annotation_raster(p.p, xmin=5.7,xmax=6.3,ymin=515, ymax=690)+
#   annotation_raster(c.t, xmin=4.7,xmax=5.3,ymin=425, ymax=600)+
#   annotation_raster(l.c, xmin=3.65,xmax=4.25,ymin=405, ymax=580)+
#   annotation_raster(c.g, xmin=2.5,xmax=3.5,ymin=365, ymax=660)+
#   annotation_raster(p.o, xmin=1.725,xmax=2.275,ymin=350, ymax=450)+
#   annotation_raster(c.c, xmin=0.7,xmax=1.3,ymin=300, ymax=530)
# bar.top.10

#Lets try subset - Shallow (0-20m) and deep (20-60m)
combined <- maxn %>%
  dplyr::left_join(metadata)%>%
  glimpse()

maxn.shallow.10 <- combined %>%
  mutate(scientific=paste(genus,species,sep=" "))%>%
  dplyr::filter(depth <= 20)%>%
  group_by(scientific)%>%
  dplyr::summarise(maxn=sum(maxn))%>%
  ungroup()%>%
  top_n(11)%>%
  dplyr::filter(!scientific%in%c('Unknown spp'))%>%
  glimpse()

#have a look
bar<-ggplot(maxn.shallow.10, aes(x=reorder(scientific,maxn), y=maxn)) +   
  geom_bar(stat="identity",position=position_dodge())+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_collapse
bar

maxn.deep.10 <- combined %>%
  mutate(scientific=paste(genus,species,sep=" "))%>%
  dplyr::filter(depth > 20)%>%
  group_by(scientific)%>%
  dplyr::summarise(maxn=sum(maxn))%>%
  ungroup()%>%
  top_n(11)%>%
  dplyr::filter(!scientific%in%c('Unknown sp10'))%>%
  glimpse()

#have a look
bar<-ggplot(maxn.deep.10, aes(x=reorder(scientific,maxn), y=maxn)) +   
  geom_bar(stat="identity",position=position_dodge())+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_collapse
bar

#plot deep fish
#load deep fish
#1 Chromis fumea
c.f <- readPNG("data/images/Pomacentridae-Dark.png")
c.f <- as.raster(c.f)

#2 Pentapodus porosus
p.p <- readPNG("data/images/Pentapodus porosus-3cmL.png")
p.p <- as.raster(p.p)

#3 Pomacentrus coelestis
p.c <- readPNG("data/images/Pomacentrus coelestis-3cmL.png")
p.c <- as.raster(p.c)

#4 Cirrhilabrus temminckii
c.t <- readPNG("data/images/Labridae-Dark.png")
c.t <- as.raster(c.t)

#5 Pristotis obtusirostris
p.o <- readPNG("data/images/Pomacentridae-Dark.png")
p.o <- as.raster(p.o)

#6 Carangoides gymnostethus 
c.g <- readPNG("data/images/Carangoides gymnostethus 3cm.png")
c.g <- as.raster(c.g)

#7 Lethrinus ravus
l.r <- readPNG("data/images/Lethrinidae-Dark.png")
l.r <- as.raster(l.r)

#8 Leptojulis cyanopleura
l.c <- readPNG("data/images/Labridae-Dark.png")
l.c <- as.raster(l.c)

#9 Nemipterus spp
n.spp <- readPNG("data/images/Nemipterus_bathybius_nb_GIBBONS.png")
n.spp <- as.raster(n.spp)

#10 Alepes vari
a.v <- readPNG("data/images/Pseudocaranx dentex-3cm.png")
a.v <- as.raster(a.v)

#plot final bar plot
bar.deep.top.10<-ggplot(maxn.deep.10, aes(x=reorder(scientific,maxn), y=maxn)) +   
  geom_bar(stat="identity",colour="black",fill="lightgrey",position=position_dodge())+
  ylim (0, 600)+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  theme_bw()+
  theme(axis.text.y = element_text(face="italic"))+
  theme_collapse+
  theme.larger.text+
  annotation_raster(c.f, xmin=9.75,xmax=10.25,ymin=530, ymax=600)+              #1
  annotation_raster(p.p, xmin=8.8,xmax=9.2,ymin=450, ymax=540)+                 #2
  annotation_raster(p.c, xmin=7.75, xmax=8.25, ymin=410, ymax=480)+             #3
  annotation_raster(c.t, xmin=6.775,xmax=7.275,ymin=395, ymax=520)+             #4
  annotation_raster(p.o, xmin=5.75,xmax=6.25,ymin=340, ymax=410)+               #5
  annotation_raster(c.g, xmin=4.7,xmax=5.3,ymin=310, ymax=420)+                 #6
  annotation_raster(l.r, xmin=3.7,xmax=4.3,ymin=275, ymax=390)+                 #7
  annotation_raster(l.c, xmin=2.775,xmax=3.275,ymin=230, ymax=355)+             #8
  annotation_raster(n.spp, xmin=1.8,xmax=2.2,ymin=230, ymax=320)+               #9
  annotation_raster(a.v, xmin=0.75,xmax=1.25,ymin=220, ymax=320)+               #10
  ggtitle("Deep assemblage (20-65m)") +
  theme(plot.title = element_text(hjust = 0))
bar.deep.top.10

#plot shallow fish
#load shallow fish
#1 Pomacentrus coelestis
p.c <- readPNG("data/images/Pomacentrus coelestis-3cmL.png")
p.c <- as.raster(p.c)

#2 Lethrinus atkinsoni
l.a <- readPNG("data/images/Lethrinus atkinsoni 5cmL.png")
l.a <- as.raster(l.a)

#3 Carangoides fulvoguttatus
c.fv <- readPNG("data/images/Carangoides fulvoguttatus-3cmL.png")
c.fv <- as.raster(c.fv)

#4 Herklotsichthys spp
h.spp <- readPNG("data/images/Decapterus_spp_nb_TAYLOR.png")
h.spp <- as.raster(h.spp)

#5 Thalassoma lunare
t.l <- readPNG("data/images/Thalassoma lunare-3cmL.png")
t.l <- as.raster(t.l)

#6 Chromis fumea
c.f <- readPNG("data/images/Pomacentridae-Dark.png")
c.f <- as.raster(c.f)

#7 Acanthurus grammoptilus
a.g <- readPNG("data/images/Acanthurus grammoptilus-3cmL.png")
a.g <- as.raster(a.g)

#8 Caesio cuning
c.c <- readPNG("data/images/Caesio cuning-3cmL.png")
c.c <- as.raster(c.c)

#9 Pentapodus emeryii
p.e <- readPNG("data/images/Pentapodus emeryii-3cmL.png")
p.e <- as.raster(p.e)

#10 Pomacentrus nagasakiensis
p.n <- readPNG("data/images/Pomacentrus milleri 5cmL.png")
p.n <- as.raster(p.n)

#plot final bar plot
bar.shallow.top.10<-ggplot(maxn.shallow.10, aes(x=reorder(scientific,maxn), y=maxn)) +   
  geom_bar(stat="identity",colour="black",fill="lightgrey",position=position_dodge())+
  ylim (0, 850)+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  theme_bw()+
  theme(axis.text.y = element_text(face="italic"))+
  theme_collapse+
  theme.larger.text+
  annotation_raster(p.c, xmin=9.7,xmax=10.2,ymin=800, ymax=880)+                #1
  annotation_raster(l.a, xmin=8.6,xmax=9.3,ymin=500, ymax=620)+                 #2
  annotation_raster(c.fv, xmin=7.6, xmax=8.5, ymin=400, ymax=570)+              #3
  annotation_raster(h.spp, xmin=6.75,xmax=7.25,ymin=290, ymax=370)+             #4
  annotation_raster(t.l, xmin=5.8,xmax=6.2,ymin=260, ymax=400)+                 #5
  annotation_raster(c.f, xmin=4.8,xmax=5.2,ymin=210, ymax=290)+                 #6
  annotation_raster(a.g, xmin=3.75,xmax=4.25,ymin=210, ymax=330)+               #7
  annotation_raster(c.c, xmin=2.75,xmax=3.25,ymin=200, ymax=320)+               #8
  annotation_raster(p.e, xmin=1.8,xmax=2.2,ymin=190, ymax=320)+                 #9
  annotation_raster(p.n, xmin=0.75,xmax=1.25,ymin=190, ymax=270)+               #10
  ggtitle("Shallow assemblage (0-20m)") +
  theme(plot.title = element_text(hjust = 0))
bar.shallow.top.10

#save out plot
ggsave("plots/stacked.bar.plot.deep.png",bar.deep.top.10,dpi=600,width=6.0)
ggsave("plots/stacked.bar.plot.shallow.png",bar.shallow.top.10,dpi=600,width=6.0)

plot.grid.top10 <- plot_grid(bar.shallow.top.10,bar.deep.top.10,
                               ncol = 2)
plot.grid.top10

#looks like butt
# ggsave("plots/stacked.bar.plot.grid.png",plot.grid.top10,dpi=600,width=6.0)



#test to see if splitting 20-40 & 40-60 looks any good
bar<-ggplot(maxn.shallow.10, aes(x=reorder(scientific,maxn), y=maxn)) +   
  geom_bar(stat="identity",position=position_dodge())+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_collapse+
  ggtitle("Shallow (0-20m)") +
  theme(plot.title = element_text(hjust = 0))
bar

maxn.med.10 <- combined %>%
  mutate(scientific=paste(genus,species,sep=" "))%>%
  dplyr::filter(depth  > 20 & depth < 40)%>%
  group_by(scientific)%>%
  dplyr::summarise(maxn=sum(maxn))%>%
  ungroup()%>%
  top_n(11)%>%
  dplyr::filter(!scientific%in%c('Unknown sp10'))%>%
  glimpse()

#have a look
bar<-ggplot(maxn.med.10, aes(x=reorder(scientific,maxn), y=maxn)) +   
  geom_bar(stat="identity",position=position_dodge())+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_collapse+
  ggtitle("Medium (20-40m)") +
  theme(plot.title = element_text(hjust = 0))
bar

maxn.deeptest.10 <- combined %>%
  mutate(scientific=paste(genus,species,sep=" "))%>%
  dplyr::filter(depth  > 40)%>%
  group_by(scientific)%>%
  dplyr::summarise(maxn=sum(maxn))%>%
  ungroup()%>%
  top_n(10)%>%
  dplyr::filter(!scientific%in%c('Unknown sp10'))%>%
  glimpse()

#have a look
bar<-ggplot(maxn.deeptest.10, aes(x=reorder(scientific,maxn), y=maxn)) +   
  geom_bar(stat="identity",position=position_dodge())+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_collapse+
  ggtitle("Deep (40-65m)") +
  theme(plot.title = element_text(hjust = 0))
bar
