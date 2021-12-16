###
# Project: Parks - Abrolhos
# Data:    BOSS fish, habitat
# Task:    Plotting fish importance scores
# author:  Claude
# date:    Nov-Dec 2021
##

rm(list=ls())

# Plotting defaults----
library(ggplot2)
library(dplyr)

## Set working directory----
working.dir <- getwd()
setwd(working.dir)
#OR Set manually once

#read in data - negative values manually added
#manually add in Xs for terms included in most parsimonious models
dat.taxa <-read.csv("output/fssgam - fish/montebello.synthesis_combined.imp.csv")%>% #from local copy
 # rename(resp.var=response)%>%
  gather(key=predictor,value=importance,2:ncol(.))%>%
  mutate(label=NA)%>%
  mutate(resp.var = factor(resp.var, levels = c("Chromis fumea","Pomacentrus coelestis","Lethrinus atkinsoni",
                                                 "sublegal size atkinsoni","legal size atkinsoni",
                                                 "sublegal size trout","legal size trout",
                                                 "legal size spango",
                                                 "smaller than legal size","greater than legal size",
                                                 "targeted.abundance","species.richness","total.abundance")))%>%  #change order of response variables
  mutate(predictor = factor(predictor, levels = c("photic.reef","mesophotic.reef","biota.unconsolidated","detrended",
                                                  "roughness","tpi","depth","mean.relief","sd.relief","status")))%>%
  mutate(label=ifelse(predictor=="depth"&resp.var=="targeted.abundance","X",label))%>%
  mutate(label=ifelse(predictor=="mesophotic.reef"&resp.var=="targeted.abundance","X",label))%>%
  mutate(label=ifelse(predictor=="roughness"&resp.var=="Lethrinus atkinsoni","X",label))%>%
  mutate(label=ifelse(predictor=="sd.relief"&resp.var=="Lethrinus atkinsoni","X",label))%>%
  mutate(label=ifelse(predictor=="detrended"&resp.var=="Pomacentrus coelestis","X",label))%>%
  mutate(label=ifelse(predictor=="roughness"&resp.var=="Pomacentrus coelestis","X",label))%>%
  mutate(label=ifelse(predictor=="sd.relief"&resp.var=="Pomacentrus coelestis","X",label))%>%
  mutate(label=ifelse(predictor=="detrended"&resp.var=="Chromis fumea","X",label))%>%
  mutate(label=ifelse(predictor=="mean.relief"&resp.var=="Chromis fumea","X",label))%>%
  mutate(label=ifelse(predictor=="depth"&resp.var=="total.abundance","X",label))%>%
  mutate(label=ifelse(predictor=="mean.relief"&resp.var=="total.abundance","X",label))%>%
  mutate(label=ifelse(predictor=="tpi"&resp.var=="total.abundance","X",label))%>%
  mutate(label=ifelse(predictor=="mean.relief"&resp.var=="species.richness","X",label))%>%
  mutate(label=ifelse(predictor=="detrended"&resp.var=="species.richness","X",label))%>%
  mutate(label=ifelse(predictor=="roughness"&resp.var=="species.richness","X",label))%>%
  mutate(label=ifelse(predictor=="depth"&resp.var=="greater than legal size","X",label))%>%
  mutate(label=ifelse(predictor=="mean.relief"&resp.var=="greater than legal size","X",label))%>%
  mutate(label=ifelse(predictor=="depth"&resp.var=="legal size atkinsoni","X",label))%>%
  mutate(label=ifelse(predictor=="mesophotic.reef"&resp.var=="legal size atkinsoni","X",label))%>%
  mutate(label=ifelse(predictor=="status"&resp.var=="legal size spango","X",label))%>%
  mutate(label=ifelse(predictor=="depth"&resp.var=="smaller than legal size","X",label))%>%
  mutate(label=ifelse(predictor=="detrended"&resp.var=="smaller than legal size","X",label))%>%
  mutate(label=ifelse(predictor=="mesophotic.reef"&resp.var=="smaller than legal size","X",label))%>%
  mutate(label=ifelse(predictor=="depth"&resp.var=="sublegal size atkinsoni","X",label))%>%
  mutate(label=ifelse(predictor=="detrended"&resp.var=="sublegal size atkinsoni","X",label))%>%
  mutate(label=ifelse(predictor=="mean.relief"&resp.var=="sublegal size atkinsoni","X",label))%>%
  mutate(label=ifelse(predictor=="depth"&resp.var=="sublegal size trout","X",label))%>%
  mutate(label=ifelse(predictor=="mean.relief"&resp.var=="sublegal size trout","X",label))%>%
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
    axis.text.y=element_text(size=10,face="italic"),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank())

# colour ramps-
re <- colorRampPalette(c("blue3", "white","red2"))(200)

# Labels-
legend_title<-"Importance"

# Plot gg.importance.scores ----
gg.importance.scores <- ggplot(dat.taxa, aes(x=predictor,y=resp.var,fill=importance)) +
   geom_tile(show.legend=T) +
   scale_fill_gradientn(legend_title, colours=c(re), na.value = "grey98",
                         limits = c(-1, 1))+
      scale_y_discrete( labels=c("Chromis fumea abundance",'Pomacentrus coelestis abundance',"Lethrinus atkinsoni abundance",
                                 "Sublegal Lethrinus atkinsoni","Legal Lethrinus atkinsoni",
                                 "Sublegal Plectropomus spp","Legal Plectropomus spp","Legal Lethrinus nebulosus",
                                 "Sublegal","Legal",
                                 "Targeted abundance","Species richness","Total abundance"))+         #Tidy Taxa names
      scale_x_discrete(labels = c("Photic reef", "Mesophotic reef",  "Detrended bathymetry", "Roughness", "TPI", "Depth",
                                 'Mean relief', "SD relief","Status"))+   #Tidy predictor names
   xlab(NULL)+
   ylab(NULL)+
   theme_classic()+
   Theme1+
   geom_text(aes(label=label))
gg.importance.scores

#save output - changed dimensions for larger text in report
save_plot("plots/montes.synthesis.fish.importance.png", gg.importance.scores,base_height = 6.75,base_width = 6.275)
