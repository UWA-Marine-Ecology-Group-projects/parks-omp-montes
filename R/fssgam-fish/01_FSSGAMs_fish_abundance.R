###
# Project: Parks - Abrolhos
# Data:    BOSS fish, habitat
# Task:    Modelling fish abundance w/ FSSGAM
# author:  Claude
# date:    Nov-Dec 2021
##

# Part 1-FSS modeling----
rm(list=ls())

## librarys----
detach("package:plyr", unload=TRUE)#will error - don't worry
library(tidyr)
library(dplyr)
options(dplyr.width = Inf) #enables head() to display all coloums
library(mgcv)
library(MuMIn)
library(car)
library(doBy)
library(gplots)
library(RColorBrewer)
library(doParallel) #this can removed?
library(doSNOW)
library(gamm4)
library(RCurl) #needed to download data from GitHub
library(GlobalArchive)
library(ggplot2)
library(devtools)
library(FSSgam)

## set study name
study <- "montebello.synthesis" 
name <- study

## Set your working directory ----
working.dir <- getwd()
setwd(working.dir)

maxn <- read.csv("data/Tidy/montebello.synthesis.checked.maxn.csv")%>%
  mutate(scientific=paste(genus,species,sep=" "))%>%
  dplyr::select(campaignid, sample, family, species, genus, scientific, maxn)

names(maxn)

length <- read.csv("data/Tidy/montebello.synthesis.checked.length.csv")%>%
  glimpse()

habitat <- readRDS("data/Tidy/merged_habitat.rds")%>%
  glimpse()

metadata <- read.csv('data/Tidy/montebello.synthesis.checked.metadata.csv')%>%
  glimpse()

# look at top species ----
  maxn.sum<-maxn%>%
    group_by(scientific)%>%
    dplyr::summarise(maxn=sum(maxn))%>%
    ungroup()%>%
    top_n(15)%>%                   #Claude added this - what is the point of plotting 6 billion species you cannot read the names of...
    ungroup()

  ## Total frequency of occurance
  ggplot(maxn.sum, aes(x=reorder(scientific,maxn), y=maxn)) +   
    geom_bar(stat="identity",position=position_dodge())+
    coord_flip()+
    xlab("Species")+
    ylab(expression(Overall~abundance~(Sigma~MaxN)))+
    #Theme1+
    theme(axis.text.y = element_text(face="italic"))+
    #theme_collapse+
    scale_y_continuous(expand = expand_scale(mult = c(0, .1)))#+

# Create total abundance and species richness ----
ta.sr <- maxn %>%
  dplyr::ungroup() %>%
  dplyr::group_by(scientific,sample) %>%
  dplyr::summarise(maxn = sum(maxn)) %>%
  tidyr::spread(scientific,maxn, fill = 0) %>%
   dplyr::mutate(total.abundance=rowSums(.[,2:(ncol(.))],na.rm = TRUE )) %>% #Add in Totals
   dplyr::mutate(species.richness=rowSums(.[,2:(ncol(.))] > 0)) %>% # double check these
   dplyr::select(sample,total.abundance,species.richness) %>%
   tidyr::gather(.,"response","number",2:3) %>%
  dplyr::glimpse()

# Create abundance of all recreational fished species ----
url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?ts=5e6f36e2#gid=825736197"

master<-googlesheets4::read_sheet(url)%>%
  ga.clean.names()%>%
  filter(grepl('Australia', global.region))%>% # Change country here
  dplyr::select(family,genus,species,fishing.type,australian.common.name)%>%
  distinct()%>%
  glimpse()

unique(master$fishing.type)

fished.species <- maxn %>%
  dplyr::left_join(master) %>%
  dplyr::mutate(fishing.type = ifelse(scientific %in%c("Serranidae Plectropomus spp")
                                      ,"R",fishing.type))%>%
  dplyr::filter(fishing.type %in% c("B/R","B/C/R","R","C/R"))%>% 
  dplyr::filter(!species %in% c('Loxodon macrorhinus'))%>%          #removed Loxodon macrorhinus
  glimpse()
  
unique(fished.species$scientific)

# Come back to maybe getting rid of some of these, but for now we continue on
fished.maxn <- fished.species %>%
  dplyr::ungroup() %>%
  dplyr::group_by(scientific,sample) %>%
  dplyr::summarise(maxn = sum(maxn)) %>%
  spread(scientific,maxn, fill = 0) %>%
  dplyr::mutate(targeted.abundance=rowSums(.[,2:(ncol(.))],na.rm = TRUE )) %>% #Add in Totals
  dplyr::select(sample,targeted.abundance) %>%
  gather(.,"response","number",2:2) %>%
  dplyr::glimpse()

# Pick top 3 species                      second most abundant species is 'unknown spp'
species.maxn <- maxn %>%
  dplyr::filter(scientific %in% c("Pomacentrus coelestis",
                                  "Chromis fumea",
                                  "Lethrinus atkinsoni"
  ))%>%
  dplyr::select(sample,scientific,maxn) %>%
  dplyr::rename('number' = maxn)%>%
  dplyr::mutate(response = scientific)%>%
  distinct()

dat <- bind_rows(fished.maxn, species.maxn, 
                           ta.sr)%>%
  left_join(habitat) %>%
   left_join(metadata) %>%
  drop_na(fieldofview.open)%>%
  distinct()

# Set predictor variables---
names(maxn)
names(habitat)

pred.vars=c("depth","biota.unconsolidated", "biota.macroalgae", "biota.crinoids",
            "reef", "biota.octocoral.black", "biota.consolidated", "biota.sponges", "biota.hydroids", "biota.stony.corals",
            "mean.relief", "sd.relief", "tpi", "roughness", "slope","aspect","detrended","lineartrend") 
  

# Check for correalation of predictor variables- remove anything highly correlated (>0.95)---
round(cor(dat[,pred.vars]),2)
#linear trend and depth
#reef and sand
#roughness and slope
#linear trend and sand

# Plot of likely transformations - thanks to Anna Cresswell for this loop!
par(mfrow=c(3,2))
for (i in pred.vars) {
  x<-dat[ ,i]
  x = as.numeric(unlist(x))
  hist((x))#Looks best
  plot((x),main = paste(i))
  hist(sqrt(x))
  plot(sqrt(x))
  hist(log(x+1))
  plot(log(x+1))
}

# Review of individual predictors - we have to make sure they have an even distribution---
#If the data are squewed to low numbers try sqrt>log or if squewed to high numbers try ^2 of ^3

#remove linear trend, sand and slope
#also remove all 'non-reef' predictors


# # Re-set the predictors for modeling----
pred.vars=c("depth","mean.relief","sd.relief","reef", "tpi", "roughness","aspect","detrended") 

# Check to make sure Response vector has not more than 90% zeros----
unique.vars=unique(as.character(dat$response))

unique.vars.use=character()
for(i in 1:length(unique.vars)){
  temp.dat=dat[which(dat$response==unique.vars[i]),]
  if(length(which(temp.dat$number==0))/nrow(temp.dat)<0.9){
    unique.vars.use=c(unique.vars.use,unique.vars[i])}
}

unique.vars.use   

# Run the full subset model selection----
savedir <- "Output/fssgam - fish"
resp.vars=unique.vars.use
use.dat=as.data.frame(dat)
str(use.dat)

factor.vars=c("status")# Status as a Factor with two levels
out.all=list()
var.imp=list()

# Loop through the FSS function for each Taxa----
for(i in 1:length(resp.vars)){
  use.dat=as.data.frame(dat[which(dat$response==resp.vars[i]),])
  use.dat$location <- as.factor(use.dat$location)
  use.dat$campaignid <- as.factor(use.dat$campaignid)
  Model1=gam(number~s(depth,k=5,bs='cr'),# + 
               #s(campaignid,bs='re') +s(location,bs='re'),
             family=tw(),  data=use.dat)
  
  model.set=generate.model.set(use.dat=use.dat,
                               test.fit=Model1,
                               # factor.smooth.interactions = TRUE,
                               # smooth.smooth.interactions = c("depth"),
                               pred.vars.cont=pred.vars,
                               pred.vars.fact=factor.vars,
                               #linear.vars="depth",
                               k=5#,
                              # null.terms="s(campaignid ,bs='re')+s(location,bs='re')"
                               )
  out.list=fit.model.set(model.set,
                         max.models=600,
                         parallel=T)
  names(out.list)
  
  out.list$failed.models # examine the list of failed models
  mod.table=out.list$mod.data.out  # look at the model selection table
  mod.table=mod.table[order(mod.table$AICc),]
  mod.table$cumsum.wi=cumsum(mod.table$wi.AICc)
  out.i=mod.table[which(mod.table$delta.AICc<=2),]
  out.all=c(out.all,list(out.i))
  # var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) #Either raw importance score
  var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) #Or importance score weighted by r2
  
  # plot the best models
  for(m in 1:nrow(out.i)){
    best.model.name=as.character(out.i$modname[m])
    
    png(file = paste(savedir, paste(name, m, resp.vars[i], "mod_fits.png", sep = "_"), sep = "/"))
    if(best.model.name!="null"){
      par(mfrow=c(3,1),mar=c(9,4,3,1))
      best.model=out.list$success.models[[best.model.name]]
      plot(best.model,all.terms=T,pages=1,residuals=T,pch=16)
      mtext(side=2,text=resp.vars[i],outer=F)}  
    dev.off()
  }
}

# Model fits and importance---
names(out.all)=resp.vars
names(var.imp)=resp.vars
all.mod.fits=do.call("rbind",out.all)
all.var.imp=do.call("rbind",var.imp)
write.csv(all.mod.fits[ , -2], file = paste(savedir, paste(name, "all.mod.fits.csv", sep = "_"), sep = "/"))
write.csv(all.var.imp, file = paste(savedir, paste(name, "all.var.imp.csv", sep = "_"), sep = "/"))

# Generic importance plots-
heatmap.2(all.var.imp,notecex=0.4,  dendrogram ="none",
          col=colorRampPalette(c("white","yellow","red"))(10),
          trace="none",key.title = "",keysize=2,
          notecol="black",key=T,
          sepcolor = "black",margins=c(12,14), lhei=c(4,15),Rowv=FALSE,Colv=FALSE)
