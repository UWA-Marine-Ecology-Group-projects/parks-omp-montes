#Claude has edited this and it is pretty poxy
#need to fix up

# Set directories----
rm(list=ls())

# Study name ----
study <- "montes.synthesis" 

# Libraries required
library(devtools)
# install_github("UWAMEGFisheries/GlobalArchive") #to check for updates
library(GlobalArchive)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(googlesheets4)
library(tidyverse)
library(sf)
library(purrr)

## Set your working directory ----
working.dir <- getwd()
setwd(working.dir)
#OR set manually once
maxn <- read_csv("data/tidy/montebello.synthesis.checked.maxn.csv")%>%
  mutate(scientific=paste(genus,species,sep=" "))%>%
  glimpse()

# Read in metadata ----
metadata<-read_csv("data/tidy/montebello.synthesis.checked.metadata.csv")%>%
  dplyr::select(sample,latitude,longitude,date,time,depth)

maxn <- maxn %>%
  left_join(metadata)

wgscrs <- "+proj=longlat +datum=WGS84"
cwatr <- st_read("data/spatial/shape/coastal-waters_polygon.shp")
cwatr <- st_transform(cwatr, wgscrs)

maxn.sf <- maxn %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = wgscrs)
maxn.sf <- maxn.sf %>%
  dplyr::mutate(longitude = unlist(map(maxn.sf$geometry, 1)),
                latitude = unlist(map(maxn.sf$geometry, 2)),
                state = lengths(st_intersects(maxn.sf, cwatr)) > 0) %>%
  glimpse()

maxn <- as.data.frame(maxn.sf) %>%
  dplyr::select(-geometry) %>%
  dplyr::filter(state %in% "FALSE") %>%
  glimpse()

# mass <- read.csv("data/tidy/montebello.synthesis.complete.mass.csv")%>%
#   glimpse()

# Read in life history
url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?ts=5e6f36e2#gid=825736197"

master <- googlesheets4::read_sheet(url)%>%
  ga.clean.names()%>%
  filter(grepl('Australia', global.region))%>%
  filter(grepl('NW', marine.region))%>%
  dplyr::select(family,genus,species,iucn.ranking,fishing.mortality,fishing.type,australian.common.name)%>% 
  distinct()%>%
  mutate(scientific = paste(genus,species, sep = " "))%>%
  glimpse()

unique(master$fishing.type)
names(master)

length(unique(metadata$sample))

total.number.fish <- sum(maxn$maxn) # 6277

#species list
species.list <- as.data.frame(unique(maxn$scientific))
species.list$scientific <- species.list$`unique(maxn$scientific)`

fished.species <- species.list %>%
  left_join(master)%>%
  dplyr::mutate(fishing.type = ifelse(scientific %in%c("Plectropomus spp","Scomberomorus spp","Sillago spp",
                                                       "Herklotsichthys spp","Lethrinus spp")
                                      ,"R",fishing.type))%>%
  dplyr::filter(fishing.type%in%c("R","C/R","B/C","B/C/R","C","B/R" ))%>%
  dplyr::select(scientific,australian.common.name, fishing.type)%>%
  glimpse()
unique(fished.species$scientific)

write.csv(fished.species, file = "output/fssgam - fish/montes.fished.species.csv", row.names = F)

iucn.species <- species.list %>%
  left_join(master) %>%
  dplyr::filter(iucn.ranking %in% c("Vulnerable", "Endangered", 
                                    "Near Threatened", "Critically Endangered")) %>%
  distinct() %>%
  dplyr::select(scientific, australian.common.name, iucn.ranking) %>%
  glimpse()

write.csv(iucn.species, file = paste0("data/tidy/", study, "_endangered.species.table.csv"),
          row.names = F)

# total.number.measured <- length%>%
#   filter(length>0)
# sum(total.number.measured$number) # 7575
# 7575/13596 # 55%

# total.measurable <- maxn%>%filter(!successful.length%in%c("No"))%>%filter(successful.count%in%c("Yes"))
# sum(total.measurable$maxn)
# 
# no.lengths <- length %>% filter(number>0)
# videos.not.measured <- anti_join(metadata,no.lengths, by = c("sample", "latitude", "longitude"))
# 
# fish.to.measure <- semi_join(maxn,videos.not.measured)
# sum(fish.to.measure$maxn)


###### NEED TO READ IN LUMPED COMMON NAMES FOR PSEUDOCARANX
###### WILL ALSO NEED TO ADD INTO CHECKEM AND VISUALISER!!!!!!!
# 
# # Create Species list ----
# species.table <- maxn%>%
#   group_by(family,genus,species,scientific)%>%
#   summarise_at(vars("maxn"),funs(sum,mean,sd,se=sd(.)/sqrt(n())))%>%
#   ungroup()%>%
#   mutate(mean=round(mean,digits=2))%>%
#   mutate(sd=round(sd,digits=2))%>%
#   mutate(se=round(se,digits=2))%>%
#   mutate(genus.species=paste(genus,species,sep=" "))%>%
#   arrange(family)%>%
#   left_join(master)%>%
#   dplyr::select(-c(scientific))%>%
#   dplyr::mutate(mean.relative.abundance.per.deployment.plus.minus.SE=paste(mean,"+/-",se,sep=" "))%>%
#   dplyr::rename(total.relative.abundance = sum)%>%
#   ungroup()
# 
# unique(species.table$fishing.type)
# 
# ubiquity <- maxn%>%
#   filter(maxn>0) %>%
#   group_by(family,genus,species,scientific)%>%
#   summarise(no.of.deployments=n())%>%
#   ungroup() %>%
#   mutate(ubiquity=(no.of.deployments/200)*100)
# 
# cleaned<-species.table%>%
#   dplyr::select(family,genus.species,australian.common.name,fishing.type,iucn.ranking)%>%
#   ## fix up variables
#   mutate(fishing.type=ifelse(fishing.type%in%c("C/R","C","B/C"),"Commercial","")) %>%
#   dplyr::filter(iucn.ranking %in% c('Near Threatened', "Endangered", "Critically Endangered", "Vulnerable"))
#   # left_join(arch)
# ## Make names nicer for table
# write.csv(cleaned,"data/tidy/endangered.species.table.csv")

