#comparing broad bathymetry and composite bathymetry
library(raster)
library(ggplot2)
library(patchwork)


working.dir <- getwd()
setwd(working.dir)

broad <- readRDS("output/spatial_covariates/ga_bathy_fine.rds")
broad <- rasterFromXYZ(broad)
broad
# plot(broad)

fine <- raster("output/spatial_covariates/layer_depth.tif")
# plot(fine)
fine <- as.data.frame(fine, xy = T)


fine.plot <- ggplot()+
  geom_raster(data = fine,aes(x = x, y = y, fill = layer_depth))+
  labs(title = "Fine composite bathymetry")
fine.plot

broad.plot <- ggplot()+geom_raster(data = broad,aes(x = x, y = y, fill = Z))+
  labs(title = "Broad GA bathymetry")
broad.plot

compare <- fine.plot + broad.plot
ggsave("plots/spatial/compare-bathy.png")
