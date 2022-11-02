###
# Project: parks - montes
# Data:    250m geoscience australia bathymetry
# Task:    Cross sections
# author:  Claude
# date:    October 2022
##

# Load libraries
library(dplyr)
library(sf)
# library(rgeos)
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

# onslow -21.631844, 115.109194
# Tyral rocks -20.269470915212963/115.3909506041739

points <- data.frame(x = c(115.109194, 115.3909506041739),                                      # Set your start and end lat and longs
                     y = c(-21.631844, -20.269470915212963), id = 1)

tran <- sfheaders::sf_linestring(obj = points,                                  # Cast to sf line
                                 x = "x",
                                 y = "y",
                                 linestring_id = "id")
plot(tran)

wgscrs <- "+proj=longlat +datum=WGS84"
st_crs(tran) <- wgscrs

tranv <- vect(tran)                                                             # Ito terra package vector
dep <- terra::rast(cbath)                                                              # Terra spatraster
plot(dep)

cbathy <- terra::extract(dep, tranv, xy = T, ID = F)

# BG trying to get a straight line
# bath_cross <- cbath %>%
#   dplyr::filter(abs(Y - -20.269) == min(abs(Y - -20.269))) %>%
#                 # Z > -220) %>% 
#   st_as_sf(coords = c("X", "Y"), crs = wgscrs)

bath_cross <- st_as_sf(x = cbathy, coords = c("x", "y"), crs = wgscrs)          # Cast to sf object
plot(bath_cross)
aus <- st_read("data/spatial/shape/cstauscd_r.mif")
st_crs(aus) <- st_crs(aumpa)
aus <- st_transform(aus, wgscrs)
aus <- aus[aus$FEAT_CODE %in% "mainland", ]
aus <- st_union(aus)                                                            # Joins the states together
plot(aus)
ausout <- st_cast(aus, "MULTILINESTRING")                                       # 2 different polygon forms - this one not filled
plot(ausout)

bath_sf <- bath_cross %>%
  dplyr::mutate("distance.from.coast" = st_distance(bath_cross, ausout),        # Distance from coast in m
                x = unlist(map(bath_cross$geometry, 1)),                        # Get the lats and longs from geometry list
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
  dplyr::filter(depth > -250) %>%                                               # Out to the shelf break
  glimpse()

paleo <- data.frame(depth = c(-118, -94, -63, -41),                             # Manually set detpsh for old coastlines
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
  geom_line(data = bath_df1, aes(y = depth, x = distance.from.coast)) +
  geom_ribbon(data = bath_df1, aes(ymin = -Inf, ymax = depth, x = distance.from.coast), fill = "tan") +
  theme_classic() +
  scale_x_continuous(expand = c(0,0)) +
  labs(x = "Distance from coast (km)", y = "Elevation (m)") +
  # geom_segment(data = paleo, aes(x = distance.from.coast, xend = distance.from.coast + 20, # Old coastline lines
  #                                y = depth, yend = depth), linetype = 2, alpha = 0.5) +
  # geom_text(data = paleo, aes(x = distance.from.coast + 26, y = depth, label = label), size = 3) +  # And annotation text
  annotate(geom = "text", x = c(-20, - 35), y = 10, label = c("Whaleback Island", "Daw Island"), size = 2.5) # Exciting labels for features

png(filename = "plots/eastern-recherche-cross-section.png", width = 8, height = 4,
    units = "in", res = 200)
p3
dev.off()