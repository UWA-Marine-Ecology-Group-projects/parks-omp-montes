###
# Project: Parks - Abrolhos Post-Survey
# Data:    BRUVS, BOSS Habitat data
# Task:    Habitat-Fish modelling + Prediction
# author:  Kingsley Griffin
# date:    Nov-Dec 2021
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
fabund <- bind_rows(dat1,dat2)                        # merged fish data used for fssgam script

prel   <- readRDS("output/spatial_predictions/predicted_relief_raster.rds")           # predicted relief from 'R/habitat/5_krige_relief.R'
str(prel)

tifs  <- list.files("output/spatial_covariates/", "*.tif", full.names = TRUE)
preds <- stack(tifs)

preddf <- as.data.frame(preds, xy = TRUE, na.rm = TRUE)

# join habitat and relief predictions, fix column names
predsp             <- SpatialPointsDataFrame(coords = cbind(preddf$x, preddf$y), data = preddf)
preddf$relief      <- extract(prel, predsp)
preddf             <- na.omit(preddf)
preddf$depth       <- preddf$layer_depth
preddf$detrended   <- preddf$layer_detrended
preddf$roughness   <- preddf$layer_roughness
preddf$tpi         <- preddf$layer_tpi
preddf$mean.relief <- preddf$relief
fabund$depthx      <- fabund$depth
fabund$depth       <- fabund$bathy_depth
head(preddf)

# reduce predictor space to fit survey area (needed later)
fishsp <- SpatialPointsDataFrame(coords = cbind(fabund$longitude.1, 
                                                fabund$latitude.1), 
                                 data = fabund)
sbuff  <- buffer(fishsp, 10000)

# use formula from top model from FSSGam model selection
#total abundance
m_totabund <- gam(number ~ s(depth, k = 3, bs = "cr") + s(mean.relief, k = 3, bs = "cr")+ s(tpi, k = 3, bs = "cr"), 
               data = fabund%>%dplyr::filter(response%in%"total.abundance"), 
               method = "REML", family = tw())
summary(m_totabund)

m_richness <- gam(number ~ s(detrended, k = 3, bs = "cr")+s(mean.relief, k = 3, bs = "cr")+s(roughness, k = 3, bs = "cr"),  # not necessarily the top model
                     data = fabund%>%dplyr::filter(response%in%"species.richness"), 
                     method = "REML", family = tw())
summary(m_richness)

m_legal <- gam(number ~ s(depth, k = 3, bs = "cr")+s(mean.relief, k = 3, bs = "cr"),  # not necessarily the top model
                  data = fabund%>%dplyr::filter(response%in%"greater than legal size"), 
                  method = "REML", family = tw())
summary(m_legal)

m_sublegal <- gam(number ~ s(depth, k = 3, bs = "cr")+s(detrended, k = 3, bs = "cr")+s(mean.relief, k = 3, bs = "cr"),  # not necessarily the top model
               data = fabund%>%dplyr::filter(response%in%"smaller than legal size"), 
               method = "REML", family = tw())
summary(m_sublegal)


# predict, rasterise and plot
preddf <- cbind(preddf, 
                "p_totabund" = predict(m_totabund, preddf, type = "response"),
                "p_richness" = predict(m_richness, preddf, type = "response"),
                "p_legal" = predict(m_legal, preddf, type = "response"),
                "p_sublegal" = predict(m_sublegal, preddf, type = "response"))

prasts <- rasterFromXYZ(preddf[, c(1, 2, 14:17)], res = c(30, 30)) 
plot(prasts)

# subset to 10km from sites only
sprast <- mask(prasts, sbuff)
plot(sprast)

# tidy and output data
spreddf <- as.data.frame(sprast, xy = TRUE, na.rm = TRUE)

summary(spreddf)

#saveRDS(preddf, "output/broad_fish_predictions.rds")
saveRDS(spreddf, "output/site_fish_predictions.rds")


