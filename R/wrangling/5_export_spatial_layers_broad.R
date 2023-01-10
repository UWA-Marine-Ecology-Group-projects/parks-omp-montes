
###
# Project: Parks - Montebello
# Data:    Bathymetry Data
# Task:    Prepare spatial layers for modelling
# author:  Kingsley Griffin
# date:    Nov-Dec 2021
##

rm(list = ls())

library(sp)
library(raster)
library(sf)
library(stars)
library(starsExtra)

# set a few standards
wgscrs  <- CRS("+proj=longlat +datum=WGS84")
sppcrs  <- CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")     # crs for sp objects

# read in and merge GA coarse bathy tiles from https://ecat.ga.gov.au/geonetwork/srv/eng/catalog.search#/metadata/67703
cbaths <- list.files("data/spatial/rasters/large", "*tile", full.names = TRUE)
cbathy <- lapply(cbaths, function(x){read.table(file = x, header = TRUE, sep = ",")})
cbathy <- do.call("rbind", lapply(cbathy, as.data.frame)) 
cbathy <- cbathy[cbathy$Z <= 0, ]
bath_r <- rasterFromXYZ(cbathy)
plot(bath_r)

cropex <- extent(114, 116.8, -22, -18.5)
bath_crop <- crop(bath_r,cropex)
plot(bath_crop)
# cropbath_df <- as.data.frame(bath_crop, xy = T)
writeRaster(bath_crop, 'data/spatial/rasters/large/ga_bathy_largerextent.tif', overwrite = T)

# bring in some drop data to check and refine area etc
habitat <- read.csv("data/Tidy/montebello.synthesis.complete.habitat.csv")
habsp   <- SpatialPointsDataFrame(coords = habitat[22:21], data = habitat)
plot(habsp, add = T)
habuff  <- buffer(habsp, 100000)
plot(habuff, add = T)

# reduce bathy area
bath_r <- crop(bath_r, extent(habuff))
plot(bath_r)
plot(habsp, add = T)
# 
# # aggregate raster to reduce size and plotting time etc
# aggbath  <- aggregate(bath_r, 10)
# abath_df <- as.data.frame(aggbath, xy = TRUE, fun = max, na.rm = TRUE)
# saveRDS(abath_df, 'output/ga_bathy_trim.rds')
# 
# # trim down coastal waters line
# cwatr <- st_read("data/spatial/shp/amb_coastal_waters_limit.shp")
# cwatr <- st_crop(cwatr, c(xmin = 107, xmax = 117, ymin = -31, ymax = -21))      # crop to general project area
# saveRDS(cwatr, 'output/coastal_waters_limit_trimmed.rds')

# retain finer bathy near survey area - mainly useful for speedy plotting
# habuff   <- buffer(habsp, 10000)
fbath    <- crop(bath_r, extent(habuff))
fbath_df <- as.data.frame(fbath, xy = TRUE)
saveRDS(fbath_df, 'output/spatial_covariates/ga_bathy_fine.rds')

# # get fine bathy from the merged data in: https://ecat.ga.gov.au/geonetwork/srv/eng/catalog.search#/metadata/144600
# mbathy <- raster('data/spatial/raster/North_West_Shelf_DEM_v2_Bathymetry_2020_30m_MSL_cog.tif')
# 
# # use habitat utm to crop this finer bathy down to a manageable size rather than 4gb
# proj4string(habsp) <- wgscrs
# habsp_t   <- spTransform(habsp, sppcrs)
# habt_buff <- buffer(habsp_t, 2000)
# fbathy    <- crop(mbathy, habt_buff)
# fbathy[fbathy$North_West_Shelf_DEM_v2_Bathymetry_2020_30m_MSL_cog > 0] <- NA
# plot(fbathy)
# plot(habsp_t, add = T)
# names(fbathy) <- "depth"
# 
# rm(mbathy) # get this connection closed

# # transform bathy to projected coords for modelling
proj4string(bath_r) <- wgscrs
# bath_utm <- projectRaster(bath_r, crs = sppcrs)
# plot(bath_utm)
# 
# # reduce bathy area further to keep lightweight
# fbath_t <- crop(fbath_t, extent(c(105000, 165000, 6880000, 7000000)))

# calculate terrain on fine bathy
preds <- terrain(bath_r, neighbors = 8,
                 opt = c("slope",  "TPI", "roughness"), unit = "degrees")
preds <- stack(bath_r, preds)
plot(preds)
# detrend bathy to highlight local topo
zstar <- st_as_stars(bath_r)
detre <- detrend(zstar, parallel = 8)
detre <- as(object = detre, Class = "Raster")
names(detre) <- c("detrended", "lineartrend")
preds <- stack(preds, detre[[1]])
plot(preds)

# saveRDS(preds, "output/spatial_covariates.rds")
# in this project, the rds is too large for git so we're using individual layers instead
writeRaster(preds, "output/spatial_covariates/layer_ga.tif", 
            bylayer = TRUE, suffix = names(preds), overwrite = TRUE)

# clear workspace of large rasters etc
rm(list=ls())
