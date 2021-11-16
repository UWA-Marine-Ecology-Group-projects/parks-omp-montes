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
library(FSSgam)
library(GlobalArchive)
library(ggplot2)

## set study name
study <- "montebello.synthesis" 
name <- study

## Set your working directory ----
working.dir<-dirname(rstudioapi::getActiveDocumentContext()$path)

## Save these directory names to use later----
tidy.dir<- 'H:/GitHub/parks-omp-montes/Tidy data'

## Load the data sets -
setwd(tidy.dir)
dir()

length <- read.csv("montebello.synthesis.checked.length.csv")%>%
  mutate(scientific=paste(family,genus,species))

habitat <- read.csv("montebello.synthesis.complete.habitat.csv")%>%
  mutate(reef = biota.crinoids+biota.invertebrate.complex+biota.macroalgae+biota.octocoral.black+
           biota.consolidated+biota.sponges+biota.hydroids+biota.stony.corals)
names(habitat)

metadata <- read.csv('montebello.synthesis.checked.metadata.csv')


# Create abundance of all recreational fished species ----
url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?ts=5e6f36e2#gid=825736197"

master<-googlesheets4::read_sheet(url)%>%
  ga.clean.names()%>%
  filter(grepl('Australia', global.region))%>% # Change country here
  dplyr::select(family,genus,species,fishing.type,australian.common.name,minlegal.wa)%>%
  distinct()%>%
  glimpse()

unique(master$fishing.type)

fished.species <- length %>%
  dplyr::left_join(master) %>%
  dplyr::mutate(fishing.type = ifelse(scientific %in%c("Serranidae Plectropomus spp"),"C/R",fishing.type))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Serranidae Plectropomus spp"), "450", minlegal.wa))%>%
  dplyr::filter(fishing.type %in% c("B/R","B/C/R","R","C/R"))%>% 
  dplyr::filter(!species %in% c('Loxodon macrorhinus'))%>%          #removed Loxodon macrorhinus
  glimpse()

unique(fished.species$scientific)

# Come back to maybe getting rid of some of these, but for now we continue on

without.min.length <- fished.species %>%
  filter(is.na(minlegal.wa))%>%
  distinct(scientific)  #there are heaps here without min legal sizes

legal <- fished.species %>%
  tidyr::replace_na(list(minlegal.wa=0)) %>%
  dplyr::filter(length>minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "greater than legal size") %>%
  dplyr::glimpse()

sublegal <- fished.species %>%
  dplyr::filter(length<minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "smaller than legal size") %>%
  dplyr::glimpse()


#Atkinsoni

atkinsoni.legal <- fished.species %>%
  dplyr::filter(species%in%c("atkinsoni")) %>%
  dplyr::filter(length>minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "legal size atkinsoni") %>%
  dplyr::glimpse()

atkinsoni.sublegal <- fished.species %>%
  dplyr::filter(species%in%c("atkinsoni")) %>%
  dplyr::filter(length<minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "sublegal size atkinsoni") %>%
  dplyr::glimpse() 

#plectropomus

plectropomus.legal <- fished.species %>%
  dplyr::filter(genus%in%c("Plectropomus")) %>%
  dplyr::filter(length>minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "legal size trout") %>%
  dplyr::glimpse()

plectropomus.sublegal <- fished.species %>%
  dplyr::filter(genus%in%c("Plectropomus")) %>%
  dplyr::filter(length<minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "sublegal size trout") %>%
  dplyr::glimpse() 

#nebulosus

nebulosus.legal <- fished.species %>%
  dplyr::filter(species%in%c("nebulosus")) %>%
  dplyr::filter(length>minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "legal size spango") %>%
  dplyr::glimpse()

nebulosus.sublegal <- fished.species %>%
  dplyr::filter(species%in%c("nebulosus")) %>%
  dplyr::filter(length<minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "sublegal size spango") %>%
  dplyr::glimpse() 


combined.length <- bind_rows(legal, sublegal, atkinsoni.legal, atkinsoni.sublegal, plectropomus.legal, plectropomus.sublegal,
                             nebulosus.legal, nebulosus.sublegal)

unique(combined.length$scientific)

complete.length <- combined.length %>%
  dplyr::right_join(metadata, by = c("sample")) %>% # add in all samples
  dplyr::select(sample,scientific,number) %>%
  tidyr::complete(nesting(sample), scientific) %>%
  replace_na(list(number = 0)) %>% #we add in zeros - in case we want to calculate abundance of species based on a length rule (e.g. greater than legal size)
  dplyr::ungroup()%>%
  dplyr::filter(!is.na(scientific)) %>% # this should not do anything
  dplyr::left_join(.,metadata) %>%
  dplyr::left_join(.,habitat) %>%
  dplyr::filter(successful.length%in%c("Yes")) %>%
  dplyr::mutate(scientific=as.character(scientific)) %>%
  dplyr::glimpse()

# Set predictor variables---
names(complete.length)
names(habitat)

pred.vars=c("depth","biota.invertebrate.complex","biota.unconsolidated","biota.macroalgae", "biota.crinoids", "reef", "mean.relief", "biota.octocoral.black",
            "sd.relief", "biota.consolidated", "biota.sponges", "biota.hydroids", "biota.stony.corals") 

dat <- complete.length%>%
  drop_na(reef)

# Check for correalation of predictor variables- remove anything highly correlated (>0.95)---
round(cor(dat[,pred.vars]),2)
# nothing is highly correlated 

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


# # Re-set the predictors for modeling----
pred.vars=c("depth","mean.relief","sd.relief","reef") 

# Check to make sure Response vector has not more than 80% zeros----
#some of the species have more than 80% 0s - sublegal spangos, trout legal and sublegal

unique.vars=unique(as.character(dat$scientific))

unique.vars.use=character()
for(i in 1:length(unique.vars)){
  temp.dat=dat[which(dat$scientific==unique.vars[i]),]
  if(length(which(temp.dat$number==0))/nrow(temp.dat)<0.8){
    unique.vars.use=c(unique.vars.use,unique.vars[i])}
}

unique.vars.use   


# Run the full subset model selection----
setwd("H:/GitHub/parks-omp-montes/Output")
resp.vars=unique.vars.use
use.dat=as.data.frame(dat)
str(use.dat)

name<- paste(study,"length",sep="_")

factor.vars=c("status")# Status as a Factor with two levels
out.all=list()
var.imp=list()

# Loop through the FSS function for each Taxa----
for(i in 1:length(resp.vars)){
  use.dat=as.data.frame(dat[which(dat$scientific==resp.vars[i]),])
  use.dat$location <- as.factor(use.dat$location)
  use.dat$campaignid <- as.factor(use.dat$campaignid)
  
  Model1=gam(number~s(mean.relief,k=5,bs='cr') + 
               s(campaignid,bs='re') +
               s(location,bs='re'),
             family=tw(),  data=use.dat)
  
  model.set=generate.model.set(use.dat=use.dat,
                               test.fit=Model1,
                               factor.smooth.interactions = TRUE,
                               # smooth.smooth.interactions = TRUE,
                               pred.vars.cont=pred.vars,
                               pred.vars.fact=factor.vars,
                               #linear.vars="depth",
                               k=5,
                               null.terms="s(campaignid ,bs='re')+s(location,bs='re')"
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
    
    png(file=paste(name,m,resp.vars[i],"mod_fits.png",sep="_"))
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
write.csv(all.mod.fits[,-2],file=paste(name,"all.mod.fits.csv",sep="_"))
write.csv(all.var.imp,file=paste(name,"all.var.imp.csv",sep="_"))

# Generic importance plots-
heatmap.2(all.var.imp,notecex=0.4,  dendrogram ="none",
          col=colorRampPalette(c("white","yellow","red"))(10),
          trace="none",key.title = "",keysize=2,
          notecol="black",key=T,
          sepcolor = "black",margins=c(12,12), lhei=c(4,15),Rowv=FALSE,Colv=FALSE)


# Part 2 - custom plot of importance scores----

# Load the importance score dataset produced above
# dat.taxa <-read.csv(text=getURL("https://raw.githubusercontent.com/beckyfisher/FSSgam/master/case_study2_model_out/clams_all.var.imp.csv"))%>% #from github
dat.taxa <-read.csv("clams_all.var.imp.csv")%>% #from local copy
  rename(resp.var=X)%>%
  gather(key=predictor,value=importance,2:ncol(.))%>%
  glimpse()


# Plotting defaults----
library(ggplot2)
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
re <- colorRampPalette(c("mistyrose", "red2","darkred"))(200)

# Labels-
legend_title<-"Importance"

# Annotations-
dat.taxa.label<-dat.taxa%>%
  mutate(label=NA)%>%
  mutate(label=ifelse(predictor=="Distance"&resp.var=="BDS","X",ifelse(predictor=="Status"&resp.var=="BDS","X",ifelse(predictor=="sqrt.X500um"&resp.var=="BDS","X",label))))%>%
  mutate(label=ifelse(predictor=="lobster"&resp.var=="BMS","X",label))%>%
  mutate(label=ifelse(predictor=="sqrt.X4mm"&resp.var=="CPN","X",ifelse(predictor=="lobster"&resp.var=="CPN","X",label)))%>%
  glimpse()

# Plot gg.importance.scores ----
gg.importance.scores <- ggplot(dat.taxa.label, aes(x=predictor,y=resp.var,fill=importance))+
  geom_tile(show.legend=T) +
  scale_fill_gradientn(legend_title,colours=c("white", re), na.value = "grey98",
                       limits = c(0, max(dat.taxa.label$importance)))+
  scale_x_discrete(limits=c("Distance",
                            "Status",
                            "lobster",
                            "snapper",
                            "fetch",
                            "org",
                            "sqrt.X4mm",
                            "sqrt.X2mm",
                            "sqrt.X1mm",
                            "sqrt.X500um"),
                   labels=c(
                     "Distance",
                     "Status",
                     "Lobster",
                     "Snapper",
                     "Fetch (km)",
                     "Organic content",
                     "Grain size: 4mm",
                     "            2mm",
                     "            1mm",
                     "            500um"
                   ))+
  scale_y_discrete(limits = c("CPN",
                              "BMS",
                              "BDS"),
                   labels=c("P. novizelandiae",
                            "M. striata",
                            "D. subrosea"))+
  xlab(NULL)+
  ylab(NULL)+
  theme_classic()+
  Theme1+
  geom_text(aes(label=label))
gg.importance.scores


# Part 3 - plots of the most parsimonious models----

### now  make a nice plot of the most interesting models-----
library(gridExtra)
library(grid)
# Theme-
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # legend.background = element_rect(fill="white"),
    legend.background = element_blank(),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=15),
    legend.title = element_blank(),
    legend.position = c(0.2, 0.8),
    text=element_text(size=15),
    strip.text.y = element_text(size = 15,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=15),
    axis.title.y=element_text(vjust=0.6, angle=90, size=15),
    axis.text.x=element_text(size=15),
    axis.text.y=element_text(size=15),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank())


# Bring in and format the raw data----
setwd("~/GitHub/FSSgam")
name<-"clams"

# Load the dataset - from github
# dat <-read.csv(text=getURL("https://raw.githubusercontent.com/beckyfisher/FSSgam/master/case_study2_dataset.csv?token=AOSO6uyYhat9-Era46nbjALQpTydsTskks5ZY3vhwA%3D%3D"))%>%
# Load the dataset - from local files
dat <-read.csv("case_study2_dataset.csv")%>%
  rename(response=Abundance)%>%
  #   Transform variables
  mutate(sqrt.X4mm=sqrt(X4mm))%>%
  mutate(sqrt.X2mm=sqrt(X2mm))%>%
  mutate(sqrt.X1mm=sqrt(X1mm))%>%
  mutate(sqrt.X500um=sqrt(X500um))%>%
  mutate(distance=as.numeric(as.character(Distance)))%>%
  na.omit()%>%
  glimpse()




# Manually make the most parsimonious GAM models for each taxa ----
setwd("~/GitHub/FSSgam/case_study2_model_out")


# MODEL Bivalve.Dosina.subrosea 500um + distance x Status ----
dat.bds<-dat%>%filter(Taxa=="BDS")
gamm=gam(response~s(sqrt.X500um,k=3,bs='cr')+s(distance,k=1,bs='cr', by=Status)+ s(Location,Site,bs="re")+ Status, family=tw(),data=dat.bds)

# predict - status from MODEL Bivalve.Dosina.subrosea----
mod<-gamm
testdata <- expand.grid(distance=mean(mod$model$distance),
                        sqrt.X500um=mean(mod$model$sqrt.X500um),
                        Location=(mod$model$Location),
                        Site=(mod$model$Site),
                        Status = c("Fished","No-take"))%>%
  distinct()%>%
  glimpse()


fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
#head(fits,2)
predicts.bds.status = testdata%>%data.frame(fits)%>%
  group_by(Status)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
write.csv(predicts.bds.status,"predicts.csv") #there is some BUG in dplyr - that this fixes
predicts.bds.status<-read.csv("predicts.csv")%>%
  glimpse()

# predict - distance.x.status from MODEL Bivalve.Dosina.subrosea----
mod<-gamm
testdata <- expand.grid(distance=seq(min(dat$distance),max(dat$distance),length.out = 20),
                        sqrt.X500um=mean(mod$model$sqrt.X500um),
                        Location=(mod$model$Location),
                        Site=(mod$model$Site),
                        Status = c("Fished","No-take"))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
#head(fits,2)
predicts.bds.distance.x.status = testdata%>%data.frame(fits)%>%
  group_by(distance,Status)%>% #only change here
  # group_by(sqrt.X500um,Status)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
write.csv(predicts.bds.distance.x.status,"predicts.csv") #there is some BUG in dplyr - that this fixes
predicts.bds.distance.x.status<-read.csv("predicts.csv")%>%
  glimpse()

# predict 500um from MODEL Bivalve.Dosina.subrosea----
mod<-gamm
testdata <- expand.grid(sqrt.X500um=seq(min(dat$sqrt.X500um),max(dat$sqrt.X500um),length.out = 20),
                        distance=mean(mod$model$distance),
                        Location=(mod$model$Location),
                        Site=(mod$model$Site),
                        Status = c("Fished","No-take"))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
#head(fits,2)
predicts.bds.500um = testdata%>%data.frame(fits)%>%
  group_by(sqrt.X500um)%>% #only change here
  # group_by(sqrt.X500um,Status)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
write.csv(predicts.bds.500um,"predicts.csv") #there is some BUG in dplyr - that this fixes
predicts.bds.500um<-read.csv("predicts.csv")%>%
  glimpse()

# MODEL Bivalve.Myadora.striata  Lobster----
dat.bms<-dat%>%filter(Taxa=="BMS")
head(dat.bms,2)
gamm=gam(response~s(lobster,k=3,bs='cr')+ s(Location,Site,bs="re"), family=tw(),data=dat.bms)

# predict - lobster from model for Bivalve.Myadora.striata ----
mod<-gamm
testdata <- expand.grid(lobster=seq(min(dat$lobster),max(dat$lobster),length.out = 20),
                        Location=(mod$model$Location),
                        Site=(mod$model$Site),
                        Status = c("Fished","No-take"))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
#head(fits,2)
predicts.bms.lobster = testdata%>%data.frame(fits)%>%
  group_by(lobster)%>% #only change here
  # group_by(sqrt.X500um,Status)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
write.csv(predicts.bms.lobster,"predicts.csv") #there is some BUG in dplyr - that this fixes
predicts.bms.lobster<-read.csv("predicts.csv")%>%
  glimpse()

# MODEL Decapod.P.novazelandiae 4mm + Lobster----
dat.cpn<-dat%>%filter(Taxa=="CPN")
head(dat.cpn,2)
gamm=gam(response~s(sqrt.X4mm,k=3,bs='cr')+s(lobster,k=3,bs='cr')+ s(Location,Site,bs="re"), family=tw(),data=dat.cpn)

# predict - sqrt.X4mm from model for Decapod.P.novazelandiae ----
mod<-gamm
testdata <- expand.grid(sqrt.X4mm=seq(min(dat$sqrt.X4mm),max(dat$sqrt.X4mm),length.out = 20),
                        lobster=mean(mod$model$lobster),
                        Location=(mod$model$Location),
                        Site=(mod$model$Site),
                        Status = c("Fished","No-take"))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
head(fits,2)
predicts.cpn.4mm = testdata%>%data.frame(fits)%>%
  group_by(sqrt.X4mm)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
write.csv(predicts.cpn.4mm,"predicts.csv") #there is some BUG in dplyr - that this fixes
predicts.cpn.4mm<-read.csv("predicts.csv")%>%
  glimpse()

# predict - lobster from model for Decapod.P.novazelandiae ----
mod<-gamm
testdata <- expand.grid(lobster=seq(min(dat$lobster),max(dat$lobster),length.out = 20),
                        sqrt.X4mm=mean(mod$model$sqrt.X4mm),
                        Location=(mod$model$Location),
                        Site=(mod$model$Site),
                        Status = c("Fished","No-take"))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
#head(fits,2)
predicts.cpn.lobster = testdata%>%data.frame(fits)%>%
  group_by(lobster)%>% #only change here
  # group_by(sqrt.X500um,Status)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
write.csv(predicts.cpn.lobster,"predicts.csv") #there is some BUG in dplyr - that this fixes
predicts.cpn.lobster<-read.csv("predicts.csv")%>%
  glimpse()

# PLOTS for Bivalve.Dosina.subrosea 500um + distance x Status ----
ggmod.bds.status<- ggplot(aes(x=Status,y=response,fill=Status,colour=Status), data=predicts.bds.status) +
  ylab(" ")+
  xlab('Status')+
  #   ggtitle(substitute(italic(name)))+
  scale_fill_manual(labels = c("Fished", "No-take"),values=c("red", "black"))+
  scale_colour_manual(labels = c("Fished", "No-take"),values=c("red", "black"))+
  scale_x_discrete(limits = rev(levels(predicts.bds.status$Status)))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = response-se.fit,ymax = response+se.fit),width = 0.5) +
  theme_classic()+
  Theme1+
  annotate("text", x = -Inf, y=Inf, label = "(a)",vjust = 1, hjust = -.1,size=5)+
  annotate("text", x = -Inf, y=Inf, label = "   Dosinia subrosea",vjust = 1, hjust = -.1,size=5,fontface="italic")
ggmod.bds.status

ggmod.bds.distance.x.status<- ggplot(aes(x=distance,y=response,colour=Status), data=dat.bds) +
  ylab(" ")+
  xlab('Distance (m)')+
  #   ggtitle(substitute(italic(name)))+
  scale_color_manual(labels = c("Fished", "No-take"),values=c("red", "black"))+
  geom_jitter(width = 0.25,height = 0,alpha=0.75, size=2,show.legend=FALSE)+
  # geom_point(alpha=0.75, size=2)+
  geom_line(data=predicts.bds.distance.x.status,show.legend=FALSE)+
  geom_line(data=predicts.bds.distance.x.status,aes(y=response - se.fit),linetype="dashed",show.legend=FALSE)+
  geom_line(data=predicts.bds.distance.x.status,aes(y=response + se.fit),linetype="dashed",show.legend=FALSE)+
  theme_classic()+
  Theme1+
  annotate("text", x = -Inf, y=Inf, label = "(b)",vjust = 1, hjust = -.1,size=5)
ggmod.bds.distance.x.status

ggmod.bds.500um<- ggplot() +
  ylab(" ")+
  xlab('Grain size: 500 um (sqrt)')+
  #   ggtitle(substitute(italic(name)))+
  scale_color_manual(labels = c("Fished", "No-take"),values=c("red", "black"))+
  #   geom_jitter(width = 0.25,height = 0)+
  geom_point(data=dat.bds,aes(x=sqrt.X500um,y=response,colour=Status),  alpha=0.75, size=2,show.legend=FALSE)+
  geom_line(data=predicts.bds.500um,aes(x=sqrt.X500um,y=response),alpha=0.5)+
  geom_line(data=predicts.bds.500um,aes(x=sqrt.X500um,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.bds.500um,aes(x=sqrt.X500um,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  annotate("text", x = -Inf, y=Inf, label = "(c)",vjust = 1, hjust = -.1,size=5)
ggmod.bds.500um

# PLOTS Bivalve M.striata lobster ----
ggmod.bms.lobster<- ggplot() +
  ylab("Abundance")+
  xlab(bquote('Density of legal lobster (no./25' *m^-2*')'))+
  scale_color_manual(labels = c("Fished", "SZ"),values=c("red", "black"))+
  geom_point(data=dat.bms,aes(x=lobster,y=response,colour=Status),  alpha=0.75, size=2,show.legend=FALSE)+
  geom_line(data=predicts.bms.lobster,aes(x=lobster,y=response),alpha=0.5)+
  geom_line(data=predicts.bms.lobster,aes(x=lobster,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.bms.lobster,aes(x=lobster,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  annotate("text", x = -Inf, y=Inf, label = "(d)",vjust = 1, hjust = -.1,size=5)+
  annotate("text", x = -Inf, y=Inf, label = "   Myadora striata",vjust = 1, hjust = -.1,size=5,fontface="italic")+
  geom_blank(data=dat.bms,aes(x=lobster,y=response*1.05))#to nudge data off annotations
ggmod.bms.lobster

# PLOTS Decapod.P.novazelandiae 4mm + lobster ----
ggmod.cpn.lobster<- ggplot() +
  ylab(" ")+
  xlab(bquote('Density of legal lobster (no./25' *m^-2*')'))+
  scale_color_manual(labels = c("Fished", "SZ"),values=c("red", "black"))+
  geom_point(data=dat.cpn,aes(x=lobster,y=response,colour=Status),  alpha=0.75, size=2,show.legend=FALSE)+
  geom_line(data=predicts.cpn.lobster,aes(x=lobster,y=response),alpha=0.5)+
  geom_line(data=predicts.cpn.lobster,aes(x=lobster,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.cpn.lobster,aes(x=lobster,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  annotate("text", x = -Inf, y=Inf, label = "(e)",vjust = 1, hjust = -.1,size=5)+
  annotate("text", x = -Inf, y=Inf, label = "  Pagurus novizelandiae",vjust = 1, hjust = -.1,size=5,fontface="italic")+
  geom_blank(data=dat.cpn,aes(x=lobster,y=response*1.05))#to nudge data off annotations
ggmod.cpn.lobster

ggmod.cpn.4mm<- ggplot() +
  ylab(" ")+
  xlab('Grain size: 4 mm (sqrt)')+
  scale_color_manual(labels = c("Fished", "No-take"),values=c("red", "black"))+
  geom_point(data=dat.cpn,aes(x=sqrt.X4mm,y=response,colour=Status),  alpha=0.75, size=2,show.legend=FALSE)+
  geom_line(data=predicts.cpn.4mm,aes(x=sqrt.X4mm,y=response),alpha=0.5)+
  geom_line(data=predicts.cpn.4mm,aes(x=sqrt.X4mm,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.cpn.4mm,aes(x=sqrt.X4mm,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  annotate("text", x = -Inf, y=Inf, label = "(f)",vjust = 1, hjust = -.1,size=5)+
  annotate("text", x = -Inf, y=Inf, label = " ",vjust = 1, hjust = -.1,size=5,fontface="italic")
ggmod.cpn.4mm

# combined.plot using grid() and gridExtra()------
blank <- grid.rect(gp=gpar(col="white"))

# To see what they will look like use grid.arrange() - make sure Plot window is large enough! - or will error!
grid.arrange(ggmod.bds.status,ggmod.bds.distance.x.status,ggmod.bds.500um,
             ggmod.bms.lobster,blank,blank,
             ggmod.cpn.lobster,ggmod.cpn.4mm,blank,nrow=3,ncol=3)

# Use arrangeGrob ONLY - as we can pass this to ggsave! Note use of raw ggplot's
combine.plot<-arrangeGrob(ggmod.bds.status,ggmod.bds.distance.x.status,ggmod.bds.500um,
                          ggmod.bms.lobster,blank,blank,
                          ggmod.cpn.lobster,ggmod.cpn.4mm,blank,nrow=3,ncol=3)

ggsave(combine.plot,file="Langlois_gamm.plot.png", width = 30, height = 30,units = "cm")












