###
# Project: Parks - Abrolhos
# Data:    BOSS fish, habitat
# Task:    Modelling fish abundance w/ FSSGAM
# author:  Claude & Kingsley
# date:    Nov-Dec 2021
##

# Merge habitat and spatial covariates
rm(list=ls())

library(reshape2)
library(raster)
library(sp)
library(ggplot2)
library(dplyr)

## Set your working directory ----
working.dir <- getwd()
setwd(working.dir)

#read in habitat
habitat <- read.csv("data/Tidy/montebello.synthesis.complete.habitat.csv")%>%
  mutate(reef = biota.crinoids+biota.invertebrate.complex+biota.macroalgae+biota.octocoral.black+
           biota.consolidated+biota.sponges+biota.hydroids+biota.stony.corals)      #i am not sure reef should be made at this point or later in scripts

# get spatial covariates (made in 'R/wrangling/A1_spatial_layers.R')
tifs  <- list.files("Output/spatial_covariates/", "*.tif", full.names = TRUE)
preds <- stack(tifs)


## extract bathy derivatives for modelling
# spatial setup
wgscrs <- CRS("+proj=longlat +datum=WGS84 +south")
utmcrs <- CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")

# align crs and check samples over bathy and extract terrain data
allhab_sp <- SpatialPointsDataFrame(coords = habitat[21:20], data = habitat, 
                                    proj4string = wgscrs)
allhab_t  <- spTransform(allhab_sp, CRS = utmcrs)
plot(preds[[1]])
plot(allhab_t, add=T)
habt_df   <- as.data.frame(allhab_t, xy = T)
habi_df   <- cbind(habt_df, raster::extract(preds, allhab_t))

names(habi_df)

#save out the habitat with spatial covariates
saveRDS(habi_df, "data/Tidy/merged_habitat.rds")
