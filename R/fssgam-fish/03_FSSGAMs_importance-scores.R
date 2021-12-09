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
dat.taxa <-read.csv("output/fssgam - fish/2021-05_Abrolhos_BOSS_combined_imp.scores.csv")%>% #from local copy
  rename(resp.var=response)%>%
  gather(key=predictor,value=importance,2:ncol(.))%>%
  mutate(label=NA)%>%
  mutate(label=ifelse(predictor=="biog"&resp.var=="targeted.abundance","X",label))%>%
  mutate(label=ifelse(predictor=="detrended"&resp.var=="targeted.abundance","X",label))%>%
  mutate(label=ifelse(predictor=="macroalgae"&resp.var=="targeted.abundance","X",label))%>%
  mutate(label=ifelse(predictor=="depth"&resp.var=="Labridae Coris auricularis","X",label))%>%
  mutate(label=ifelse(predictor=="biog"&resp.var=="Lethrinidae Lethrinus miniatus","X",label))%>%
  mutate(label=ifelse(predictor=="depth"&resp.var=="Lethrinidae Lethrinus miniatus","X",label))%>%
  mutate(label=ifelse(predictor=="location"&resp.var=="Lethrinidae Lethrinus miniatus","X",label))%>%
  mutate(label=ifelse(predictor=="relief"&resp.var=="Pomacentridae Chromis westaustralis","X",label))%>%
  mutate(label=ifelse(predictor=="relief"&resp.var== "total.abundance","X",label))%>%
  mutate(label=ifelse(predictor=="location"&resp.var== "species.richness","X",label))%>%
  mutate(label=ifelse(predictor=="relief"&resp.var== "species.richness","X",label))%>%
  mutate(label=ifelse(predictor=="tpi"&resp.var== "species.richness","X",label))%>%
  mutate(label=ifelse(predictor=="biog"&resp.var== "greater than legal size","X",label))%>%
  mutate(label=ifelse(predictor=="depth"&resp.var== "greater than legal size","X",label))%>%
  mutate(label=ifelse(predictor=="location"&resp.var== "greater than legal size","X",label))%>%
  mutate(label=ifelse(predictor=="biog"&resp.var== "legal size red throat","X",label))%>%
  mutate(label=ifelse(predictor=="depth"&resp.var== "legal size red throat","X",label))%>%
  mutate(label=ifelse(predictor=="location"&resp.var== "legal size red throat","X",label))%>%
  mutate(label=ifelse(predictor=="biog"&resp.var== "smaller than legal size","X",label))%>%
  mutate(label=ifelse(predictor=="detrended"&resp.var== "smaller than legal size","X",label))%>%
  mutate(label=ifelse(predictor=="macroalgae"&resp.var== "smaller than legal size","X",label))%>%
  mutate(resp.var = factor(resp.var, levels = c("Pomacentridae Chromis westaustralis","Lethrinidae Lethrinus miniatus", "Labridae Coris auricularis",
                                                "legal size red throat", "smaller than legal size", "greater than legal size","targeted.abundance",
                                                "species.richness","total.abundance")))%>%
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
     scale_y_discrete( labels=c("Chromis westaustralis abundance","Lethrinus miniatus abundance",
                                "Coris auricularis abundance","Legal size Lethrinus miniatus","Smaller than legal size","Greater than legal size","Targeted abundance","Species richness","Total abundance"))+
  scale_x_discrete(labels = c("Biogenic", "Depth", "Detrended", "Location", "Macroalgae", "Relief", "Sand", "Slope", 'TPI'))+
   xlab(NULL)+
   ylab(NULL)+
   theme_classic()+
   Theme1+
   geom_text(aes(label=label))
gg.importance.scores

#save output - changed dimensions for larger text in report
save_plot("plots/abrolhos.fish.importance.png", gg.importance.scores,base_height = 6.75,base_width = 6.275)
