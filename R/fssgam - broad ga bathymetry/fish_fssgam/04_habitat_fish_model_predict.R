###
# Project: Parks - Montes omp
# Data:    BRUVS + Habitat data
# Task:    Habitat-Fish modelling + Prediction
# author:  Kingsley Griffin & Claude
# date:    April 2022
##

rm(list=ls())

library(reshape2)
library(mgcv)
library(ggplot2)
library(viridis)
library(raster)
library(dplyr)
library(stringr)
library(sp)

# read in
dat1 <- readRDS("data/tidy/dat.maxn.rds")%>%
  glimpse()
dat2 <- readRDS("data/tidy/dat.length.rds")
fabund <- bind_rows(dat1,dat2)  %>%                      # merged fish data used for fssgam script
        dplyr::filter(number<400)
prel   <- readRDS("output/spatial_predictions_broad/predicted_relief_raster_ga.rds")           # predicted relief from 'R/habitat/5_krige_relief.R'
str(prel)

tifs  <- list.files("output/spatial_covariates/", "*.tif", full.names = TRUE)
preds <- stack(tifs)

preddf <- as.data.frame(preds, xy = TRUE, na.rm = TRUE)

# join habitat and relief predictions, fix column names
predsp             <- SpatialPointsDataFrame(coords = cbind(preddf$x, preddf$y), data = preddf)
preddf$mean.relief      <- extract(prel, predsp)
preddf             <- na.omit(preddf)                                           #maybe wrong, filtering out all rows with na relief
preddf <- preddf %>%
  dplyr::rename(depth = layer_ga_Z, detrended = layer_ga_detrended, roughness = layer_ga_roughness,
                slope = layer_ga_slope, tpi = layer_ga_tpi)%>%
  glimpse()
fabund <- fabund %>%
  dplyr::rename(depthx = depth, depth = depth_ga)%>%
  glimpse()

# reduce predictor space to fit survey area (needed later)
fishsp <- SpatialPointsDataFrame(coords = cbind(fabund$longitude.1, 
                                                fabund$latitude.1), 
                                 data = fabund)
sbuff  <- buffer(fishsp, 10000)

# use formula from top model from FSSGam model selection
#total abundance
m_totabund <- gam(number ~ s(mean.relief, k = 3, bs = "cr"), 
               data = fabund%>%dplyr::filter(response%in%"total.abundance"), 
               method = "REML", family = tw())
summary(m_totabund)

#species richness
m_richness <- gam(number ~ s(mean.relief, k = 3, bs = "cr"), 
                     data = fabund%>%dplyr::filter(response%in%"species.richness"), 
                     method = "REML", family = tw())
summary(m_richness)

#greater than legal size
m_legal <- gam(number ~ s(mean.relief, k = 3, bs = "cr"),  
                  data = fabund%>%dplyr::filter(response%in%"greater than legal size"), 
                  method = "REML", family = tw())
summary(m_legal)

#smaller than legal size
m_sublegal <- gam(number ~ s(detrended, k = 3, bs = "cr")+s(mean.relief, k = 3, bs = "cr")+
                    s(tpi, k = 3, bs = "cr"),
               data = fabund%>%dplyr::filter(response%in%"smaller than legal size"), 
               method = "REML", family = tw())
summary(m_sublegal)


# predict, rasterise and plot
preddf <- cbind(preddf, 
                "p_totabund" = predict(m_totabund, preddf, type = "response"),
                "p_richness" = predict(m_richness, preddf, type = "response"),
                "p_legal" = predict(m_legal, preddf, type = "response"),
                "p_sublegal" = predict(m_sublegal, preddf, type = "response"))

prasts <- rasterFromXYZ(preddf[, c(1, 2, 9:12)],res = c(261, 277)) 
plot(prasts)

# subset to 10km from sites only
sprast <- mask(prasts, sbuff)
plot(sprast)

# tidy and output data
spreddf <- as.data.frame(sprast, xy = TRUE, na.rm = TRUE)

summary(spreddf)

#saveRDS(preddf, "output/broad_fish_predictions.rds")
saveRDS(spreddf, "output/spatial_predictions_broad/site_fish_predictions.rds")
