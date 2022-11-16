###
# Project: Parks - Abrolhos
# Data:    BOSS fish, habitat
# Task:    Habitat importance scores
# author:  Claude
# date:    Nov-Dec 2021
##

rm(list=ls())

# Plotting defaults----
library(ggplot2)
library(dplyr)

dat.taxa <-read.csv("output/fssgam-habitat_broad/_montebello.synthesis_all.var.imp.csv")%>% #from local copy
  rename(resp.var=X)%>%
  gather(key=predictor,value=importance,2:ncol(.))%>%
  mutate(label=NA)%>%
  dplyr::filter(!resp.var%in%c("photic.reef"))%>%
  mutate(label=ifelse(resp.var=="biota.consolidated"&predictor=="depth_ga","X",label))%>%
  mutate(label=ifelse(resp.var=="biota.consolidated"&predictor=="roughness","X",label))%>%
  mutate(label=ifelse(resp.var=="biota.consolidated"&predictor=="tpi","X",label))%>%
  mutate(label=ifelse(resp.var=="biota.unconsolidated"&predictor=="depth_ga","X",label))%>%
  mutate(label=ifelse(resp.var=="biota.unconsolidated"&predictor=="roughness","X",label))%>%
  mutate(label=ifelse(resp.var=="mesophotic.reef"&predictor=="depth_ga","X",label))%>%
  mutate(label=ifelse(resp.var=="mesophotic.reef"&predictor=="roughness","X",label))%>%
  mutate(label=ifelse(resp.var=="mesophotic.reef"&predictor=="tpi","X",label))%>%
  mutate(label=ifelse(resp.var=="biota.stony.corals"&predictor=="detrended","X",label))%>%
  mutate(label=ifelse(resp.var=="biota.macroalgae"&predictor=="detrended","X",label))%>%
  mutate(label=ifelse(resp.var=="biota.macroalgae"&predictor=="roughness","X",label))%>%
  dplyr::mutate(importance = ifelse(resp.var == "biota.consolidated" & 
                                      predictor == "tpi", importance * -1, importance)) %>%
  dplyr::mutate(importance = ifelse(resp.var == "biota.consolidated" & 
                                      predictor == "depth_ga", importance * -1, importance)) %>%
  dplyr::mutate(importance = ifelse(resp.var == "biota.macroalgae" & 
                                      predictor == "roughness", importance * -1, importance)) %>%
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
   scale_y_discrete( labels=c("Rock","Macroalgae", "Hard coral", "Sand", "Soft coral/sponges"))+
   scale_x_discrete(labels = c("Depth","Detrended","Roughness", "TPI"))+
   xlab(NULL)+
   ylab(NULL)+
   theme_classic()+
   Theme1+
   geom_text(aes(label=label))
gg.importance.scores

#save plots
save_plot("plots/montes.habitat.importance.scores.png", gg.importance.scores,base_height = 6,base_width = 8)
