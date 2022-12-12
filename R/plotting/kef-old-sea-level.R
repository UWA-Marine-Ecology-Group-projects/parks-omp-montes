###
# Project: Parks Montes
# Data:    Geoscience Australia 250m res bathy
# Task:    Generate exploratory site plots
# author:  Claude Spencer
# date:    November 2022
##

# CONTENTS
# 5. Key Ecological Features (p5)
# 7. Old sea level map (p7)

# Clear your environment
rm(list = ls())

# Load libraries
library(dplyr)
library(sf)
library(rgeos)
library(rnaturalearth)
library(ggplot2)
library(metR)
library(stringr)
library(patchwork)
library(terra)
library(ggnewscale)
library(GlobalArchive)
library(tidyverse)
library(viridis)

# Set your study name
name <- "Montebello"                                                              # Change here

# Set CRS for transformations
wgscrs <- "+proj=longlat +datum=WGS84"
gdacrs <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"

# Set cropping extent - larger than most zoomed out plot
e <- ext(114.5, 116.5, -22, -19)

# Load necessary spatial files
sf_use_s2(F)                                                                    # Switch off spatial geometry for cropping
# Australian outline and state and commonwealth marine parks
aus    <- st_read("data/spatial/shape/cstauscd_r.mif") %>%                 # Geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
  dplyr::filter(FEAT_CODE %in% c("mainland", "island"))

st_crs(aus) <- gdacrs
ausc <- sf::st_crop(aus, e)

# Commonwealth parks
aumpa  <- st_read("data/spatial/shape/AustraliaNetworkMarineParks.shp")         # All aus mpas
mpa <- st_crop(aumpa, e)                                                        # Crop to the study area
# Reorder levels so everything plots nicely
unique(mpa$ZoneName)

# State parks
# simplify zone names
wampa  <- st_read("data/spatial/shape/WA_MPA_2018.shp")                         # all wa mpas
mb_mp <- wampa[wampa$NAME %in% c("Montebello Islands","Barrow Island"),]        # just wa parks nearby

mb_mp$ZoneName <- dplyr::recode(mb_mp$ZoneName,
                                "Special Purpose Zone (Mining Exclusion)" =
                                  "Special Purpose Zone\n(Mining Exclusion)")

mb_mp$waname <- gsub("( \\().+(\\))", "", mb_mp$ZONE_TYPE)
mb_mp$waname <- gsub(" [1-4]", "", mb_mp$waname)
# ab_mpa$waname[ab_mpa$ZONE_TYPE == unique(ab_mpa$ZONE_TYPE)[14]] <-
#   c("Special Purpose Zone\n(Habitat Protection)")
mb_mp$waname <- dplyr::recode(mb_mp$waname,
                              "General Use" = "General Use Zone",
                              # "MMA" = "Marine Management Area",
                              # "Recreation Area" = "Recreation Zone",
                              # "Conservation Area" = "Sanctuary Zone",
                              "Special Purpose Zone (Shore Based Activities)" =
                                "Special Purpose Zone\n(Shore Based Activities)")
mb_mp <- mb_mp %>%
  dplyr::mutate(waname=ifelse(NAME%in%"Barrow Island"&
                                TYPE%in%"Marine Park","Sanctuary Zone",waname))%>%
  dplyr::filter(!waname%in%"Unassigned")

# Terrestrial parks
terrnp <- st_read("data/spatial/shape/Legislated_Lands_and_Waters_DBCA_011.shp") %>%  # Terrestrial reserves
  dplyr::filter(leg_catego %in% c("Nature Reserve", "National Park"))
terrnp <- st_crop(terrnp, xmin = 115, xmax = 116.5, ymin = -21, ymax = -19)       # Crop to the study area - using a different extent as this is on land

# Key Ecological Features
kef <- st_read("data/spatial/shape/AU_DOEE_KEF_2015.shp")
kef <- st_crop(kef, xmin = 114.5, xmax = 116.5, ymin = -22, ymax = -19)                                                       # Crop
unique(kef$NAME)
plot(kef)
# Simplify names for plot legend
kef$NAME <- dplyr::recode(kef$NAME, "Ancient coastline at 125 m depth contour" = "Ancient coastline",
                          "Canyons linking the Cuvier Abyssal Plain and the Cape Range Peninsula" = "Cape Range and Cuvier Plain Canyons",
                          "Continental Slope Demersal Fish Communities" = "Continental slope fish")
# Reorder levels so everything plots nicely
kef$NAME <- factor(kef$NAME, levels = c("Cape Range and Cuvier Plain Canyons",  # Doesn't come up in final plot
                                        "Continental slope fish",
                                        "Ancient coastline"))

# Coastal waters limit
cwatr <- st_read("data/spatial/shape/amb_coastal_waters_limit.shp")       # Coastal waters limit
cwatr <- st_crop(cwatr, xmin = 114.5, xmax = 116.5, ymin = -22, ymax = -19)

# 5. Key Ecological Features (p5)
kef_fills <- scale_fill_manual(values = c("Ancient coastline" = "#ffff6d",
                                          "Continental slope fish" = "#ffb677"))

terr_fills <- scale_fill_manual(values = c("National Park" = "#c4cea6",         # Set the colours for terrestrial parks
                                           "Nature Reserve" = "#e4d0bb"),
                                guide = "none")

nmpa_cols <- scale_color_manual(values = c("Multiple Use Zone" = "#b9e6fb"))

wampa_cols <- scale_colour_manual(values = c("Sanctuary Zone" = "#bfd054",
                                           "General Use Zone" = "#bddde1",
                                           "Recreation Zone" = "#f4e952",
                                           "Special Purpose Zone" = "#7f66a7"))

p5 <- ggplot() +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA, show.legend = F) +
  labs(fill = "Terrestrial Managed Areas") +
  terr_fills +
  new_scale_fill() +
  geom_sf(data = kef, aes(fill = NAME), alpha = 0.7, color = NA) +
  kef_fills +
  geom_sf(data = aumpa, fill = NA, alpha = 1, aes(color = ZoneName), show.legend = F, size = 0.4) +
  nmpa_cols +
  new_scale_colour() +
  geom_sf(data = mb_mp, 
          fill = NA, alpha = 1, aes(color = waname), show.legend = F, size = 0.4) +
  wampa_cols + 
  new_scale_color() +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.5) +
  labs(x = NULL, y = NULL,  fill = "Key Ecological Features") +
  guides(fill = guide_legend(order = 1)) +
  annotate(geom = "text", label = "Barrow Island", 
           x = (115.4023 + 0.15), y = -20.7804, size = 3) +
  annotate(geom = "point", x = 115.4023, y = -20.7804) +
  annotate(geom = "text", x = c(115.390), y = c(-20.269), label = c("Tryal Rocks"), size = 2) +
  coord_sf(xlim = c(114.75,116.25), ylim = c(-21.2,-20)) +                      # Change here
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

p5

png(filename = paste(paste0('plots/', name) , 'key-ecological-features.png', 
                     sep = "-"), units = "in", res = 200, width = 8, height = 6)
p5
dev.off()


# 7. Old sea level map (p7)
# Bathymetry data
cbaths <- list.files("data/spatial/rasters/large", "*tile", full.names = TRUE)
cbathy <- lapply(cbaths, function(x){read.table(file = x, header = TRUE, sep = ",")})
cbathy <- do.call("rbind", lapply(cbathy, as.data.frame))                       # All bathy in tiles as a dataframe
bath_r <- rast(cbathy)
crs(bath_r) <- wgscrs
bath_r <- crop(bath_r, e)
bath_df <- as.data.frame(bath_r, xy = T, na.rm = T)                             # Dataframe - cropped and above 0 use for bath cross section
bath_r <- clamp(bath_r, upper = 0, value = FALSE)                               # Only data below 0
bathy <- as.data.frame(bath_r, xy = T, na.rm = T)

depth_fills <- scale_fill_manual(values = c("#b8d9a9","#8dbc80", "#5d9d52"),
                                 labels = c("9-10 Ka", "15-17 Ka", "20-30 Ka"),
                                 name = "Coastline age")

# Convert back to a raster and smooth it out
# build basic plot elements
p7 <- ggplot() +
  geom_tile(data = bathy %>% dplyr::filter(Z < -100), aes(x = x, y = y, fill = Z)) +
  scale_fill_gradient2(low = "royalblue4", mid = "lightskyblue1", high = "white", name = "Depth (m)") +
  new_scale_fill() +
  geom_contour_filled(data = bathy, aes(x = x, y = y, z = Z,
                                        fill = after_stat(level)),
                      breaks = c(0, -40, -70, -125)) +
  depth_fills +
  new_scale_fill() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey62", size = 0.2) +
  new_scale_fill() +
  geom_sf(data = mpa %>% dplyr::filter(!ZoneName %in% "National Park Zone"), 
          colour = "#b9e6fb", size = 0.4, fill = NA) +
  # geom_sf(data = npz,
  #         colour = "#7bbc63", size = 0.55, fill = NA) +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 0.7, size = 0.5) +
  annotate(geom = "segment", x = 115.3909, xend = 115.3909, y = -22, yend = -20,
           linetype = 2, alpha = 0.4) +
  coord_sf(xlim = c(114.75,116.25), ylim = c(-21.2,-20)) +                            # Change here
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#b8d9a9", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank()) #  #b8d9a9
# p7



png(filename = paste(paste0('plots/', name) , 'old-sea-levels.png', 
                     sep = "-"), units = "in", res = 200, width = 8, height = 6)
p7
dev.off()
