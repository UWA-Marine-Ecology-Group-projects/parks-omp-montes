###
# Project: Parks - Montes Post-Survey
# Data:    BRUVS, BOSS Habitat data
# Task:    Habitat modelling
# author:  Kingsley Griffin
# date:    Jan 22
##

rm(list = ls())

library(reshape2)
library(mgcv)
library(ggplot2)
library(viridis)
library(raster)
library(dplyr)

# read in
habi  <- readRDS("data/tidy/broad_merged_habitat.rds")                               # merged data from 'R/1_mergedata.R'
tifs  <- list.files("output/spatial_covariates/", "layer_ga*", full.names = TRUE)
preds <- stack(tifs)
preddf <- as.data.frame(preds, xy = TRUE, na.rm = TRUE)%>%
  dplyr::rename(detrended=layer_ga_detrended, roughness = layer_ga_roughness, slope = layer_ga_slope,
                tpi = layer_ga_tpi, depth_ga = layer_ga_Z)%>%
  dplyr::mutate(depth_ga = abs(depth_ga))%>%
  glimpse()

# reduce predictor space to fit survey area
# preddf <- preddf[preddf$Depth > min(habi$Depth), ]
# preddf <- preddf[preddf$Depth < 200, ]
habisp <- SpatialPointsDataFrame(coords = cbind(habi$longitude, 
                                                habi$latitude), data = habi)
sbuff  <- buffer(habisp, 10000)

# # visualise patterns
# covs <- c("Depth", "slope", "roughness", "tpi", "tri", "detrended")             # all covariates
# habs <- c("kelps", "macroalgae", "sponge", "sand", "rock", "biogenic")          # all habitats
# habl <- habi[, colnames(habi) %in% c(covs, habs, "totalpts")]
# habl <- melt(habl, measure.vars = covs)
# habl <- melt(habl, measure.vars = habs)
# head(habl)
# colnames(habl) <- c("totalpts", "covariate", "value", "habitat", "count")
# ggplot(habl, aes(value, (count/totalpts) * 100)) + 
#   geom_point() + geom_smooth() + 
#   facet_grid(habitat ~ covariate, scales = "free")

# use formula from top model from '2_modelselect.R'
# Macroalgae
m_macro <- gam(cbind(biota.macroalgae, Total.Sum - biota.macroalgae) ~ 
                  s(detrended, k = 5, bs = "cr") + 
                  s(roughness, k = 5, bs = "cr"), 
                data = habi, method = "REML", family = binomial("logit"))
summary(m_macro)
gam.check(m_macro)
vis.gam(m_macro)

# Coral
m_coral <- gam(cbind(biota.stony.corals, Total.Sum - biota.stony.corals) ~ 
                 s(detrended, k = 5, bs = "cr"), 
               data = habi, method = "REML", family = binomial("logit"))
summary(m_coral)
gam.check(m_coral)
# vis.gam(m_coral)

# Soft coral/sponge reef
m_meso <- gam(cbind(mesophotic.reef, Total.Sum - mesophotic.reef) ~ 
                s(depth_ga,     k = 5, bs = "cr")  + 
                s(detrended, k = 5, bs = "cr") +
                s(roughness, k = 5, bs = "cr") +
                s(tpi, k = 5, bs = "cr"), 
              data = habi, method = "REML", family = binomial("logit"))
summary(m_meso)
gam.check(m_meso)
vis.gam(m_meso)

#macroalgae
m_macro <- gam(cbind(biota.macroalgae, Total.Sum - biota.macroalgae) ~ 
                 s(depth_ga,     k = 5, bs = "cr")  + 
                 s(detrended, k = 5, bs = "cr") + 
                 s(roughness, k = 5, bs = "cr") +
                 s(tpi, k = 5, bs = "cr"), 
          data = habi, method = "REML", family = binomial("logit"))
summary(m_macro)
gam.check(m_macro)
vis.gam(m_macro)

#stony corals
# m_stony <- gam(cbind(biota.stony.corals, Total.Sum - biota.stony.corals) ~ 
#                 s(depth_ga,     k = 5, bs = "cr")  + 
#                 s(detrended, k = 5, bs = "cr") + 
#                 # s(roughness, k = 5, bs = "cr"),
#                 s(tpi, k = 5, bs = "cr"),
#               data = habi, method = "REML", family = binomial("logit"))
# summary(m_stony)
# gam.check(m_stony)
# vis.gam(m_stony)

#rock
m_rock <- gam(cbind(biota.consolidated, Total.Sum - biota.consolidated) ~ 
                s(depth_ga,     k = 5, bs = "cr")  + 
                s(detrended, k = 5, bs = "cr") + 
                s(roughness, k = 5, bs = "cr") +
                s(tpi, k = 5, bs = "cr"), 
              data = habi, method = "REML", family = binomial("logit"))
summary(m_rock)
gam.check(m_rock)
vis.gam(m_rock)

#sand
m_sand <- gam(cbind(biota.unconsolidated, Total.Sum - biota.unconsolidated) ~ 
                s(depth_ga,     k = 5, bs = "cr")  + 
                s(detrended, k = 5, bs = "cr") + 
                s(roughness, k = 5, bs = "cr") +
                s(tpi, k = 5, bs = "cr"),
              data = habi, method = "REML", family = binomial("logit"))
summary(m_sand)
gam.check(m_sand)
vis.gam(m_sand)

# predict, rasterise and plot
preddf <- cbind(preddf, 
                "pcoral" = predict(m_coral, preddf, type = "response"),
                "pmacro" = predict(m_macro, preddf, type = "response"),
                "pmeso" = predict(m_meso, preddf, type = "response"),
                "prock" = predict(m_rock, preddf, type = "response"),
                "psand" = predict(m_sand, preddf, type = "response"))

prasts <- rasterFromXYZ(preddf)
prasts$dom_tag <- which.max(prasts[[6:10]])
plot(prasts[[6:11]])

# subset to 10km from sites only
sprast <- mask(prasts, sbuff)
sprast <- sprast[[6:11]]
plot(sprast)

# # categorise by dominant tag
# sprast$dom_tag <- apply(sprast[5:8], 1,
#                         FUN = function(x){names(which.max(x))})
# sprast$dom_tag <- sub('.', '', sprast$dom_tag)
# head(sprast)

# write to tifs to reduce file sizes for git
crs(sprast) <- "+proj=longlat +datum=WGS84"

saveRDS(sprast, file = "output/fssgam-habitat_broad/montes-habitat-spatial_WGS84.rds")

writeRaster(sprast, "output/spatial_predictions_broad/parks-montes.tif", 
            bylayer = TRUE, suffix = names(sprast), overwrite = TRUE)


# Convert it to a shapefile
dom_rast <- sprast %>%
  as.data.frame(xy = T, na.rm = T) %>%
  dplyr::select(x, y, dom_tag) %>%
  rast(type = "xyz", crs = "epsg:4326")
plot(dom_rast)
# Don't think I need this step?
pred_stars <- st_as_stars(dom_rast)

dom.habs <- st_as_sf(pred_stars, as_points = FALSE, merge = TRUE) %>%
  dplyr::mutate(dom_tag = dplyr::recode(dom_tag,
                "1" = "coral",
                "2" = "macroalgae",
                "3" = "inverts",
                "4" = "rock",
                "5" = "sand"))
plot(dom.habs)

name <- "parks-montes"

st_write(dom.habs, paste0("output/spatial_predictions_broad/", name, "_predicted-dominant-habitat.shp"),
         append = F)

# Write national habitat map to a shapefile for Tim
natmap <- rast("data/spatial/rasters/ecosystem-types-19class-naland.tif") %>%
  st_as_stars()

natmap.classes <- read.csv("data/spatial/rasters/Ecosystem_categories-final-2021.csv") %>%
  dplyr::mutate(habitat.type = as.character(category.number)) %>%
  dplyr::select(habitat.type, Ecosystems) %>%
  glimpse()

natmap.sf <- st_as_sf(natmap, as_points = FALSE, merge = TRUE) %>%
  dplyr::rename(habitat.type = `ecosystem-types-19class-naland.tif`) %>%
  dplyr::mutate(habitat.type = as.character(habitat.type)) %>%
  left_join(natmap.classes) %>%
  dplyr::rename(code = habitat.type,
                class = Ecosystems)

st_write(natmap.sf, "data/spatial/shape/national-ecosystem-types.shp",
         append = F,layer_options = "ENCODING=UTF-8")

# Not sure whats going on with this bit
# spreddf <- as.data.frame(sprast, xy = T, na.rm = T)
# spreddf$dom_tag <- dplyr::recode(spreddf$dom_tag,
#                                  "1" = "Hard coral",
#                                  "2" = "Macroalgae",
#                                  "3" = "Soft coral/sponges",
#                                  "4" = "Rock",
#                                  "5" = "Sand")
# test <- spreddf[,c(1:2,8)]
# test$dom_tag <- as.factor(test$dom_tag)
# test$ndom_tag <- as.numeric(test$dom_tag)
# head(test)
# testraster <- rasterFromXYZ(test[,c("x", "y", "ndom_tag")], 
#                             crs = "+proj=longlat +datum=WGS84")
# plot(testraster)
# testraster[] = factor(levels(test$dom_tag)[testraster[]])
# plot(testraster)
# 
# saveRDS(testraster, file = "output/fssgam-habitat_broad/montes-dominant-habitat_WGS84.rds")
# 
# 
# writeRaster(prasts[[6:11]], "output/spatial_predictions_broad/broad-layer.tif", 
#             bylayer = TRUE, suffix = names(prasts[[6:11]]), overwrite = TRUE)
# 
# writeRaster(sprast, "output/spatial_predictions_broad/layer.tif", 
#             bylayer = TRUE, suffix = names(sprast), overwrite = TRUE)

