###
# Project: parks - montes
# Data:    250m geoscience australia bathymetry
# Task:    Cross sections
# author:  Claude
# date:    October 2022
##

rm(list = ls())

# Load libraries
library(dplyr)
library(sf)
library(geosphere)
library(rnaturalearth)
library(ggplot2)
# library(metR)
library(stringr)
library(patchwork)
library(terra)
library(ggnewscale)
library(GlobalArchive)
library(tidyverse)
library(viridis)


# Bathymetry cross section
cbaths <- list.files("data/spatial/rasters/large", "*tile", full.names = TRUE)  # XYZ ascii files in gitignore
cbath <- lapply(cbaths, function(x){read.table(file = x, header = TRUE, sep = ",")})
cbath <- do.call("rbind", lapply(cbath, as.data.frame))

summary(cbath)

# onslow -21.631844, 115.109194
# Tyral rocks -20.269470915212963/115.3909506041739

points <- data.frame(x = c(115.109194, 115.3909506041739),                                      # Set your start and end lat and longs
                     y = c(-21.631844, -20.269470915212963), id = 1)

tran <- sfheaders::sf_linestring(obj = points,                                  # Cast to sf line
                                 x = "x",
                                 y = "y",
                                 linestring_id = "id")

wgscrs <- "+proj=longlat +datum=WGS84"
st_crs(tran) <- wgscrs
plot(tran)

tranv <- vect(tran)                                                             # Ito terra package vector
dep <- terra::rast(cbath)                                                              # Terra spatraster
plot(dep)

cbathy <- terra::extract(dep, tranv, xy = T, ID = F)

summary(cbath)

filtered.dat <- cbath %>%
  filter(X > 114) %>%
  filter(X < 120) %>%
  filter(Y > -23) %>%
  filter(Y < -20)

summary(filtered.dat)
  
# BG trying to get a straight line
bath_cross <- cbath %>%
  dplyr::filter(abs(X - 115.3909) == min(abs(X - 115.3909)),
                Z > -200) %>%
  dplyr::mutate("distance.from.coast" = distHaversine(cbind(X, Y), 
                                                      c(115.3909, -21.51375))) %>% # Coastline at -21.51375
  st_as_sf(coords = c("X", "Y"), crs = wgscrs)



gdacrs <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"

plot(bath_cross)
aus <- st_read("data/spatial/shape/cstauscd_r.mif")
st_crs(aus) <- gdacrs                                                           
aus <- st_transform(aus, wgscrs)
aus <- aus[aus$FEAT_CODE %in% "mainland", ]
aus <- st_union(aus)                                                            # Joins the states together
plot(aus)
ausout <- st_cast(aus, "MULTILINESTRING")                                       # 2 different polygon forms - this one not filled
plot(ausout)

bath_sf <- bath_cross %>%
  dplyr::mutate(x = unlist(map(bath_cross$geometry, 1)),                        # Get the lats and longs from geometry list
                y = unlist(map(bath_cross$geometry, 2)),
                land = lengths(st_intersects(bath_cross, aus)) > 0) %>%         # If it is on the land on or not
  glimpse()

bath_df1 <- as.data.frame(bath_sf) %>%
  dplyr::select(-geometry) %>%
  dplyr::rename(depth = "Z") %>%
  dplyr::mutate(distance.from.coast = as.numeric(distance.from.coast/1000)) %>% # To km
  dplyr::mutate(distance.from.coast = ifelse(land %in% "FALSE", 
                                             distance.from.coast*-1,            # Make distance from coast negative if its ocean
                                             distance.from.coast)) %>%          
  # dplyr::filter(depth > -250) %>%                                               # Out to the shelf break
  dplyr::filter(distance.from.coast < 10) %>%
  glimpse()

paleo <- data.frame(depth = c(-118, -94, -63, -41),                             # Manually set depths for old coastlines
                    label = c("20-30 Ka", "15-17 Ka", "12-13 Ka", "9-10 Ka"))

for (i in 1:nrow(paleo)) {                                                      # Loop to get closest depth to the old coastline depth
  temp <- bath_df1 %>%
    dplyr::filter(abs(bath_df1$depth - paleo$depth[i]) == min(abs(bath_df1$depth - paleo$depth[i]))) %>%
    dplyr::select(depth, distance.from.coast) %>%
    slice(1)
  
  if (i == 1) {
    dat <- temp
  } 
  else {
    dat <- bind_rows(dat, temp)
  }}

paleo$distance.from.coast <- dat$distance.from.coast                            # Lol

p3 <- ggplot() +
  geom_rect(aes(xmin = min(bath_df1$distance.from.coast), xmax = max(bath_df1$distance.from.coast), ymin =-Inf, ymax = 0), fill = "#12a5db", alpha = 0.5) +
  annotate(geom = "segment", x = -102, xend = -102, y = -26, yend = 0, 
           colour = "red", size = 0.7) +
  annotate(geom = "segment", x = -190, xend = -190, y = -150, yend = 0,
           colour = "#b9e6fb", size = 0.7) +
  geom_line(data = bath_df1, aes(y = depth, x = distance.from.coast)) +
  geom_ribbon(data = bath_df1, aes(ymin = -Inf, ymax = depth, x = distance.from.coast), fill = "tan") +
  geom_segment(data = paleo, aes(x = distance.from.coast, xend = distance.from.coast + 20, 
                                 y = depth, yend = depth), linetype = 2, alpha = 0.5) +
  geom_text(data = paleo, aes(x = distance.from.coast + 28, y = depth, label = label), size = 3) +
  theme_classic() +
  scale_x_continuous(expand = c(0,0)) +
  labs(x = "Distance from coast (km)", y = "Elevation (m)") +
  annotate(geom = "text", x = c(-137, -80, 0), y = c(-10, 65, 10), 
           label = c("Tryal Rocks", "Barrow Island", "Pilbara coast"), size = 2.5) # Exciting labels for features

p3

png(filename = "plots/tryalrocks_cross_section_north-south.png", width = 8, height = 4,
    units = "in", res = 200)
p3
dev.off()
