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
library(dplyr)
library(ggplot2)
library(stringr)
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
  dplyr::mutate(scientific = paste(genus, species, sep = " "))%>%
  glimpse()

metadata <- read.csv("data/tidy/montebello.synthesis.checked.metadata.csv")

wgscrs <- "+proj=longlat +datum=WGS84"
cwatr <- st_read("data/spatial/shape/coastal-waters_polygon.shp")
cwatr <- st_transform(cwatr, wgscrs)

maxn.sf <- left_join(maxn, metadata) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = wgscrs)

maxn.sf <- maxn.sf %>%
  dplyr::mutate(longitude = unlist(map(maxn.sf$geometry, 1)),
                latitude = unlist(map(maxn.sf$geometry, 2)),
                state = lengths(st_intersects(maxn.sf, cwatr)) > 0) %>%
  glimpse()

state.maxn <- as.data.frame(maxn.sf) %>%
  dplyr::select(-geometry) %>%
  dplyr::filter(state %in% "FALSE") %>%
  dplyr::mutate(scientific = paste(family,genus,species, sep = " "))%>%
  glimpse()

test <- maxn %>%
  dplyr::mutate(scientific = paste(family,genus,species, sep = " "))%>%
  glimpse()

species <- as.data.frame(unique(test$scientific)) 

# #get stats to use for the little section in report
# sum(test$maxn) #16935 fish
# 
# length(unique(test$sample)) #200
# 
# length(unique(test$family)) #63
# length(unique(test$genus)) #161
# length(unique(test$scientific)) #385
# length(unique(test$species)) #325

# For only state data
sum(state.maxn$maxn) # 6277 fish

length(unique(state.maxn$sample)) # 88

length(unique(state.maxn$family)) # 51
length(unique(state.maxn$genus)) # 122
length(unique(state.maxn$scientific)) # 267
length(unique(state.maxn$species)) # 233


length <- read.csv(file = 'data/tidy/montebello.synthesis.expanded.length.csv')

mass <- read.csv(file = 'data/tidy/montebello.synthesis.complete.mass.csv')

metadata <- read.csv('data/tidy/montebello.synthesis.checked.metadata.csv')%>%
  glimpse()

# workout total maxn for each species ---
maxn.10<-maxn%>%
  mutate(scientific=paste(genus,species,sep=" "))%>%
  group_by(scientific)%>%
  dplyr::summarise(maxn=sum(maxn))%>%
  ungroup()%>%
  top_n(12)%>%
  dplyr::filter(!scientific%in%c('Unknown sp10', 'Unknown spp'))%>%
  glimpse()

#Lets try subset - Shallow (0-20m) and deep (20-60m)
# combined <- maxn %>%
#   dplyr::left_join(metadata)%>%
#   glimpse()

#shallow (0-20m)
# maxn.shallow.10 <- combined %>%
#   mutate(scientific=paste(genus,species,sep=" "))%>%
#   dplyr::filter(depth <= 20)%>%
#   group_by(scientific)%>%
#   dplyr::summarise(maxn=sum(maxn))%>%
#   ungroup()%>%
#   top_n(11)%>%
#   dplyr::filter(!scientific%in%c('Unknown spp'))%>%
#   glimpse()

maxn.shallow.10 <- state.maxn%>%
  mutate(scientific = paste(genus, species, sep = " "))%>%
  dplyr::filter(depth <= 30)%>%
  group_by(scientific)%>%
  dplyr::summarise(maxn = sum(maxn))%>%
  ungroup()%>%
  top_n(10)%>%
  dplyr::filter(!scientific %in% c('Unknown spp'))%>%
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

maxn.deep.10 <- state.maxn %>%
  mutate(scientific=paste(genus,species,sep=" "))%>%
  dplyr::filter(depth > 30)%>%
  dplyr::filter(depth <= 70)%>%
  group_by(scientific)%>%
  dplyr::summarise(maxn=sum(maxn))%>%
  ungroup()%>%
  top_n(10)%>%
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

#1 Cirrhilabrus temminckii
c.t <- readPNG("data/images/Labridae-Dark.png")
c.t <- as.raster(c.t)

#2 Pentapodus porosus
p.p <- readPNG("data/images/Pentapodus porosus-3cmL.png")
p.p <- as.raster(p.p)

#3 Lethrinus ravus
l.r <- readPNG("data/images/Lethrinidae-Dark.png")
l.r <- as.raster(l.r)

#4 Carangoides gymnostethus 
c.g <- readPNG("data/images/Carangoides gymnostethus 3cm.png")
c.g <- as.raster(c.g)

#5 Coris caudimacula
c.c <- readPNG("data/images/Labridae-Dark.png")
c.c <- as.raster(c.c)

#6 Carangoides fulvoguttatus
c.f <- readPNG("data/images/Carangoides fulvoguttatus-3cmL.png")
c.f <- as.raster(c.f)

#7 Nemipterus spp
n.spp <- readPNG("data/images/Nemipterus_bathybius_nb_GIBBONS.png")
n.spp <- as.raster(n.spp)

l.p <- as.raster(readPNG("data/images/Lethrinus punctulatus-3cmL.png"))
l.p <- as.raster(l.p)

#9 Pentapodus nagasakiensis
p.n <- readPNG("data/images/Pentapodus porosus-3cmL.png")
p.n <- as.raster(p.n)

#10 Leptojulis cyanopleura
l.c <- readPNG("data/images/Labridae-Dark.png")
l.c <- as.raster(l.c)

# #2 Chromis fumea
# c.fu <- readPNG("data/images/Pomacentridae-Dark.png")
# c.fu <- as.raster(c.fu)

#3 Pomacentrus coelestis
# p.c <- readPNG("data/images/Pomacentrus coelestis-3cmL.png")
# p.c <- as.raster(p.c)

#5 Pristotis obtusirostris
# p.o <- readPNG("data/images/Pomacentridae-Dark.png")
# p.o <- as.raster(p.o)

#10 Alepes vari
# a.v <- readPNG("data/images/Pseudocaranx dentex-3cm.png")
# a.v <- as.raster(a.v)

#plot final bar plot
bar.deep.top.10 <- ggplot(maxn.deep.10%>%mutate(scientific=str_replace_all(.$scientific,          #is alepes vari fished?
  c("gymnostethus"="gymnostethus*","ravus"="ravus*"))), aes(x=reorder(scientific,maxn), y=maxn)) +   
  geom_bar(stat="identity",colour="black",fill="lightgrey",position=position_dodge())+
  ylim (0, 320)+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  theme_bw()+
  theme(axis.text.y = element_text(face="italic"))+
  theme_collapse+
  theme.larger.text+
  annotation_raster(c.t, xmin = 9.775, xmax = 10.275, ymin=270, ymax=320)+          #1
  annotation_raster(p.p, xmin = 8.75, xmax = 9.25, ymin = 235, ymax = 280)+         #2
  annotation_raster(l.r, xmin = 7.65, xmax = 8.35, ymin=190, ymax=250)+             #3
  annotation_raster(c.g, xmin = 6.6, xmax = 7.4, ymin=190, ymax=270)+               #4
  annotation_raster(c.c, xmin = 5.75, xmax = 6.25, ymin=160, ymax=210)+             #5
  annotation_raster(c.f, xmin = 4.6, xmax = 5.4, ymin = 130, ymax = 210)+          #6
  annotation_raster(n.spp, xmin = 3.7, xmax = 4.3, ymin=130, ymax=200)+             #7
  annotation_raster(l.p, xmin = 2.65, xmax = 3.35, ymin = 120, ymax=190)+           #8
  annotation_raster(p.n, xmin = 1.75, xmax = 2.25, ymin = 115, ymax = 180)+         #9
  annotation_raster(l.c, xmin = 0.7, xmax = 1.3, ymin = 110, ymax = 180)+           #10
  ggtitle("Deep assemblage (30 - 70 m)") +
  theme(plot.title = element_text(hjust = 0))
bar.deep.top.10

#plot shallow fish
#load shallow fish
#1 Pomacentrus coelestis
p.c <- readPNG("data/images/Pomacentrus coelestis-3cmL.png")
p.c <- as.raster(p.c)

#2 Pristotis obtusirostris
p.o <- readPNG("data/images/Pomacentridae-Dark.png")
p.o <- as.raster(p.o)

#3 Alepes vari
a.v <- readPNG("data/images/Carangidae-LargeReproduction.png")
a.v <- as.raster(a.v)

#4 Lethrinus atkinsoni
l.a <- readPNG("data/images/Lethrinus atkinsoni 5cmL.png")
l.a <- as.raster(l.a)

#5 Cirrhilabrus temminckii
c.t <- readPNG("data/images/Labridae-Dark.png")
c.t <- as.raster(c.t)

#6 Caesio cuning
c.c <- readPNG("data/images/Caesio cuning-3cmL.png")
c.c <- as.raster(c.c)

#7 Pentapodus emeryii
p.e <- readPNG("data/images/Pentapodus emeryii-3cmL.png")
p.e <- as.raster(p.e)

#7 Acanthurus olivaceus
a.o <- readPNG("data/images/Acanthurus grammoptilus-3cmL.png")
a.o <- as.raster(a.o)

#9 Acanthurus grammoptilus
a.g <- readPNG("data/images//Acanthurus grammoptilus-3cmL.png")
a.g <- as.raster(a.g)

#10 Carangoides fulvoguttatus
c.f <- readPNG("data/images/Carangoides fulvoguttatus-3cmL.png")
c.f <- as.raster(c.f)

#plot final bar plot
bar.shallow.top.10<-ggplot(maxn.shallow.10%>%mutate(scientific=str_replace_all(.$scientific,          
c("atkinsoni"="atkinsoni*","fulvoguttatus"="fulvoguttatus*","Herklotsichthys spp"="Herklotsichthys spp*"))), aes(x=reorder(scientific,maxn), y=maxn)) +   
  geom_bar(stat="identity",colour="black",fill="lightgrey",position=position_dodge())+
  ylim (0, 600)+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  theme_bw()+
  theme(axis.text.y = element_text(face="italic"))+
  theme_collapse+
  theme.larger.text+
  annotation_raster(p.c, xmin=9.65,xmax=10.25,ymin=340, ymax= 440)+                #1
  annotation_raster(p.o, xmin=8.7,xmax=9.2,ymin=200, ymax= 310)+                 #2
  annotation_raster(a.v, xmin=7.5, xmax=8.5, ymin=190, ymax= 420)+              #3
  annotation_raster(l.a, xmin=6.65,xmax=7.35,ymin=130, ymax= 270)+             #4
  annotation_raster(c.t, xmin=5.8,xmax=6.2,ymin=120, ymax= 240)+                 #5
  annotation_raster(c.c, xmin=4.7,xmax=5.3,ymin=70, ymax= 220)+                 #6
  annotation_raster(p.e, xmin=3.75,xmax=4.25,ymin=50, ymax= 210)+               #7
  annotation_raster(a.o, xmin=2.75,xmax=3.25,ymin=50, ymax= 180)+               #8
  annotation_raster(a.g, xmin=1.7,xmax=2.3,ymin=50, ymax= 180)+                 #9
  annotation_raster(c.f, xmin=0.55,xmax=1.45,ymin=50, ymax= 210)+               #10
  ggtitle("Shallow assemblage (0 - 30 m)") +
  theme(plot.title = element_text(hjust = 0))

bar.shallow.top.10<-ggplot(maxn.shallow.10%>%mutate(scientific=str_replace_all(.$scientific,          
                                                                               c("atkinsoni"="atkinsoni*","fulvoguttatus"="fulvoguttatus*","Herklotsichthys spp"="Herklotsichthys spp*"))), aes(x=reorder(scientific,maxn), y=maxn)) +   
  geom_bar(stat="identity",colour="black",fill="lightgrey",position=position_dodge())+
  ylim (0, 400)+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  theme_bw()+
  theme(axis.text.y = element_text(face="italic"))+
  theme_collapse+
  theme.larger.text+
  annotation_raster(p.c, xmin=9.65,xmax=10.25,ymin=340, ymax= 400)+                #1
  annotation_raster(p.o, xmin=8.7,xmax=9.2,ymin=200, ymax= 260)+                 #2
  annotation_raster(a.v, xmin=7.5, xmax=8.5, ymin=190, ymax= 330)+              #3
  annotation_raster(l.a, xmin=6.65,xmax=7.35,ymin=130, ymax= 220)+             #4
  annotation_raster(c.t, xmin=5.8,xmax=6.2,ymin=120, ymax= 190)+                 #5
  annotation_raster(c.c, xmin=4.7,xmax=5.3,ymin=70, ymax= 170)+                 #6
  annotation_raster(p.e, xmin=3.75,xmax=4.25,ymin=50, ymax= 150)+               #7
  annotation_raster(a.o, xmin=2.75,xmax=3.25,ymin=50, ymax= 140)+               #8
  annotation_raster(a.g, xmin=1.7,xmax=2.3,ymin=50, ymax= 140)+                 #9
  annotation_raster(c.f, xmin=0.55,xmax=1.45,ymin=50, ymax= 170)+               #10
  ggtitle("Shallow assemblage (0 - 30 m)") +
  theme(plot.title = element_text(hjust = 0))
bar.shallow.top.10


#save out plot
ggsave("plots/stacked.bar.plot.deep.png",bar.deep.top.10,dpi=600,width=6.0)
ggsave("plots/stacked.bar.plot.shallow.png",bar.shallow.top.10,dpi=600,width=6.0)

plot.grid.top10 <- plot_grid(bar.shallow.top.10,bar.deep.top.10,
                               ncol = 2)
plot.grid.top10


#targeted species top 10 abundance
# Read in life history
url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?ts=5e6f36e2#gid=825736197"

master <- googlesheets4::read_sheet(url)%>%
  ga.clean.names()%>%
  filter(grepl('Australia', global.region))%>%
  filter(grepl('NW', marine.region))%>%
  dplyr::select(family,genus,species,iucn.ranking,fishing.mortality,fishing.type,australian.common.name,minlegal.wa)%>% 
  distinct()%>%
  glimpse()

fished.species <- state.maxn %>%
  dplyr::left_join(master) %>%
  dplyr::mutate(fishing.type = ifelse(scientific %in%c("Plectropomus spp","Scomberomorus spp","Sillago spp",
                                                       "Herklotsichthys spp","Lethrinus spp")
                                      ,"R",fishing.type))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Plectropomus spp"), "450", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Scomberomorus spp"), "900", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Lethrinus spp"), "280", minlegal.wa))%>%
  dplyr::filter(fishing.type %in% c("B/R","B/C/R","R","C/R"))%>% 
  dplyr::filter(!species %in% c('Loxodon macrorhinus'))%>%          #removed Loxodon macrorhinus - maybe remove herklotsichthys if it cooks everything
  glimpse()

# workout total maxn for each species ---
maxn.fished.10<-fished.species %>%
  group_by(scientific)%>%
  dplyr::summarise(maxn=sum(maxn))%>%
  ungroup()%>%
  top_n(10)%>%
  glimpse()

#have a look
bar<-ggplot(maxn.fished.10, aes(x=reorder(scientific,maxn), y=maxn)) +   
  geom_bar(stat="identity",position=position_dodge())+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_collapse
bar

#1 - Lethrinus atkinsoni
# already loaded

#2 - Herklotsichthys spp
# already loaded

#3 - Plectropomus spp
p.s <- as.raster(readPNG("data/images/Plectropomus leopardus 3cm.png"))

#4 - Lethrinus nebulosus
l.n <- as.raster(readPNG("data/images/lethrinus nebulosus 3cm.png"))

#5 - Lethrinus punctulatus
l.p <- as.raster(readPNG("data/images/Lethrinus punctulatus-3cmL.png"))

#6 - Lutjanus lemniscatus
l.l <- as.raster(readPNG("data/images/Lutjanus lemniscatus 5cmL.png"))

#7 - Symphorus nematophorus
s.n <- as.raster(readPNG("data/images/Lutjanidae-Dark.png"))

#8 - Choerodon cyanodus
c.c <- as.raster(readPNG("data/images/Choerodon cyanodus-5cmL.png"))

#9 - Scomberomorus spp
s.spp <- as.raster(readPNG("data/images/Scombridae-Dark.png"))

#10 - Choerodon cauteroma
c.ca <- as.raster(readPNG("data/images/Choerodon cauteroma-5cmL.png"))

#plot final bar plot
bar.fished.10<-ggplot(maxn.fished.10, aes(x=reorder(scientific,maxn), y=maxn)) +   
  geom_bar(stat="identity",colour="black",fill="lightgrey",position=position_dodge())+
  ylim (0, 775)+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  theme_bw()+
  theme(axis.text.y = element_text(face="italic"))+
  theme_collapse+
  theme.larger.text+
  annotation_raster(l.a, xmin=9.7,xmax=10.3,ymin=640, ymax=780)+          #1
  annotation_raster(h.spp, xmin=8.8,xmax=9.2,ymin=290, ymax=350)+               #2
  annotation_raster(p.s, xmin=7.6, xmax=8.4, ymin=190, ymax=360)+         #3
  annotation_raster(l.n, xmin=6.55,xmax=7.45,ymin=150, ymax=310)+               #4
  annotation_raster(l.p, xmin=5.6,xmax=6.3,ymin=140, ymax=270)+                #5
  annotation_raster(l.l, xmin=4.7,xmax=5.3,ymin=130, ymax=260)+                 #6
  annotation_raster(s.n, xmin=3.55,xmax=4.45,ymin=120, ymax=280)+                 #7
  annotation_raster(c.c, xmin=2.65,xmax=3.35,ymin=110, ymax=250)+              #8
  annotation_raster(s.spp, xmin=1.55,xmax=2.45,ymin=100, ymax=350)+                #9
  annotation_raster(c.ca, xmin=0.65,xmax=1.35,ymin=90, ymax=230)                 #10






# ggtitle("10 most abundant species") +
# theme(plot.title = element_text(hjust = 0))
# bar.fished.10

#save out plot
ggsave("plots/abundant.targets.bar.png",bar.fished.10,dpi=600,width=6.0, height = 6.0)
