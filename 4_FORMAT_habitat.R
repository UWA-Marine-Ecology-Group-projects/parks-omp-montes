# Clear memory ----
rm(list=ls())

# Libraries required ----
# To connect to GlobalArchive
library(devtools)
install_github("UWAMEGFisheries/GlobalArchive") #to check for updates
library(GlobalArchive)
# To connect to life.history
library(httpuv)
library(googlesheets4)
# To tidy data
library(tidyr)
library(plyr)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)
library(fst)

# Search and replace function ----
# gsr <- function(Source, Search, Replace) { 
#   if (length(Search) != length(Replace))     stop("Search and Replace Must Have Equal Number of Items\n") 
#   Changed <- as.character(Source) 
#   for (i in 1:length(Search)) 
#   { 
#     cat("Replacing: ", Search[i], " With: ", Replace[i], "\n")
#     Changed <- replace(Changed, Changed == Search[i], Replace[i])   } 
#   cat("\n")    
#   Changed 
# }

# Study name---
study<-"montebello.synthesis"  ## change for your project

## Set your working directory ----
working.dir<-dirname(rstudioapi::getActiveDocumentContext()$path) # to directory of current file - or type your own

## Save these directory names to use later----
staging.dir<-paste(working.dir,"Staging",sep="/") 
download.dir<-paste(working.dir,"Downloads",sep="/")
tidy.dir<-paste(working.dir,"Tidy data",sep="/")
plots.dir=paste(working.dir,"Plots",sep="/")
error.dir=paste(working.dir,"Errors to check",sep="/")
checking.dir <- paste(working.dir,"Data to be checked",sep="/")

# Read in the data----
setwd(checking.dir)
dir()

# Read in metadata----

metadata<-read_csv(file="montebello.synthesis_metadata.csv")%>%
  dplyr::mutate(id=paste(campaignid,sample,sep="."))%>%
  glimpse()


# Read in habitat----
setwd(working.dir)
habitat<-ga.list.files("_Habitat.point.score.txt")%>% # list all files ending in "_Habitat.point.score.csv"
  purrr::map_df(~ga.read.files_txt(.))%>%
  glimpse()

habitat[is.na(habitat)] = '0'

str(habitat)

names(habitat)%>%sort()

# # Change habitat names to broad -----
# # # Read in name changes to update names----
# url <- 'https://docs.google.com/spreadsheets/d/1c5aic_D7wmvNLcpbjihyThnL5SCUCgPZY-J20AK5ZyE/edit#gid=0'
# 
# updated.names <- googlesheets4::read_sheet(url)%>%
#   mutate(new.name=tolower(new.name))%>%
#   glimpse()
# 
# # Update names----
# colnames(habitat)<- gsr(colnames(habitat),updated.names$current.name,updated.names$new.name) # changes old names to new names
# names(habitat)%>%sort()
# habitat[is.na(habitat)] <- 0 # changes NAs to zeros
cols = c(2:19) 
# names(habitat)%>%sort()
# habitat[,cols] = apply(habitat[,cols], 2, function(x) as.numeric(as.character(x))); # changes all columns to numeric
# habitat<-habitat%>%replace_na(cols = 0)

habitat[cols] <- sapply(habitat[cols], as.numeric)
str(habitat)

names(habitat)

habitat<-habitat%>%
  rename( relief.0 = relief.0.flat.substrate.sandy.rubble.with.few.features.0.substrate.slope.)%>%
  rename( relief.1 = relief.1.some.relief.features.amongst.mostly.flat.substrate.sand.rubble.45.degree.substrate.slope.)%>%
  rename( relief.2 = relief.2.mostly.relief.features.amongst.some.flat.substrate.or.rubble.45.substrate.slope.)%>%
  rename( relief.3 = relief.3.good.relief.structure.with.some.overhangs.45.substrate.slope.)%>%
  rename( relief.4 = relief.4.high.structural.complexity.fissures.and.caves.vertical.wall.90.substrate.slope.)%>%
  rename( relief.5 = relief.5.exceptional.structural.complexity.numerous.large.holes.and.caves.vertical.wall.90.substrate.slope.)%>%
  glimpse()


 # select(sample,campaignid,project,relief.0,relief.1,relief.2,relief.3,relief.4,relief.5,fieldofview.facing.down,fieldofview.facing.up,fieldofview.limited,fieldofview.open,biota.ascidians,biota.bryozoa,biota.consolidated,biota.crinoids,biota.hydrocoral,biota.hydroids,biota.invertebrate.complex,biota.macroalgae,biota.mangrove,biota.octocoral.black,biota.seagrasses,biota.sponges,biota.stony.corals,biota.true.anemones,biota.unconsolidated,biota.zoanthids)

# Create %fov----
fov.percent.cover<-habitat%>%
  select(campaignid,sample,starts_with("fieldofview"))%>%
  mutate_all(funs(replace(.,is.na(.),0)))%>%
  mutate(Total.Sum=rowSums(.[,3:(ncol(.))],na.rm = TRUE ))%>%
  group_by(campaignid,sample)%>%
  mutate_at(vars(starts_with("fieldofview.")),funs(./Total.Sum*100))%>%
  dplyr::select(-Total.Sum)%>%
  glimpse()

# Create relief----
relief.mean.and.sd<-habitat%>%
  select(campaignid,sample,starts_with("relief."))%>%
  gather(key=relief,value=score,-campaignid,-sample)%>%
  filter(score>0)%>%
  mutate(relief.rank=ifelse(relief=="relief.0",0,ifelse(relief=="relief.1",1,ifelse(relief=="relief.2",2,ifelse(relief=="relief.3",3,ifelse(relief=="relief.4",4,ifelse(relief=="relief.5",5,relief)))))))%>%
  .[rep(seq.int(1,nrow(.)), .$score), 1:5]%>% # ensure number of columns is correct
  mutate(relief.rank=as.numeric(relief.rank))%>%
  group_by(campaignid,sample) %>%
  dplyr::summarise(mean.relief= mean (relief.rank), sd.relief= sd (relief.rank))%>%
  glimpse()

# CREATE catami_broad------
broad.percent.cover<-habitat%>%
  select(campaignid,sample,starts_with("biota."))%>%
  # mutate(Total.Sum=rowSums(.[,3:(ncol(.))],na.rm = TRUE ))%>%
  # group_by(campaignid,sample)%>%
  # mutate_at(vars(starts_with("biota.")),funs(./Total.Sum*100))%>%
  # select(-Total.Sum)%>%
  # data.frame()%>%
  glimpse()

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
broad.percent.cover[is.nan(broad.percent.cover)] <- 0 
broad.percent.cover%>%glimpse()

# Write final habitat data----
setwd(tidy.dir)
dir()

habitat.relief.fov<-relief.mean.and.sd%>%
  full_join(fov.percent.cover,by=c("campaignid","sample"))%>%
  full_join(broad.percent.cover,by=c("campaignid","sample"))%>%
  mutate(id=paste(campaignid,sample,sep="."))%>%
  semi_join(metadata)%>%
  left_join(metadata)%>%
  glimpse()

names(habitat.relief.fov)<-str_replace_all(names(habitat.relief.fov),c("biota."=""))

write.csv(habitat.relief.fov, file=paste(study,"complete.habitat.csv",sep = "."), row.names=FALSE)
write.fst(habitat.relief.fov,"complete.habitat.fst")

