###
# Project: Parks Montes
# Data:    Geoscience Australia 250m res bathy
# Task:    Generate exploratory site plots
# author:  Claude Spencer
# date:    November 2022
##

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
name <- "Abrolhos"                                                              # Change here

# Set CRS for transformations
wgscrs <- "+proj=longlat +datum=WGS84"
gdacrs <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"

# Set cropping extent - larger than most zoomed out plot
e <- ext(112, 116, -30, -26)

# Load necessary spatial files
sf_use_s2(F)                                                                    # Switch off spatial geometry for cropping
# Australian outline and state and commonwealth marine parks
aus    <- st_read("data/spatial/shapefiles/cstauscd_r.mif") %>%                 # Geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
  dplyr::filter(FEAT_CODE %in% c("mainland", "island"))
st_crs(aus) <- gdacrs
ausc <- st_crop(aus, e)

# Commonwealth parks
aumpa  <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")    # All aus mpas
mpa <- st_crop(aumpa, e)                                                        # Crop to the study area
# Reorder levels so everything plots nicely
aumpa$ZoneName <- factor(aumpa$ZoneName, levels = c("Multiple Use Zone", 
                                                    "Special Purpose Zone",
                                                    "National Park Zone"))
npz <- mpa[mpa$ZoneName %in% "National Park Zone", ]                            # Just National Park Zones

# State parks
wampa <- st_read("data/spatial/shapefiles/WA_MPA_2020.shp")
st_crs(wampa) <- gdacrs
# Simplify names for plot legend
wampa$waname <- gsub("( \\().+(\\))", "", wampa$ZONE_TYPE)
wampa$waname <- gsub(" [1-4]", "", wampa$waname)
wampa$waname[wampa$NAME == "Hamelin Pool"]     <- "Marine Nature Reserve"
wampa$waname[wampa$NAME == "Abrolhos Islands"] <- "Fish Habitat Protection Area"
wampa$waname <- dplyr::recode(wampa$waname, 
                              "General Use" = "General Use Zone",
                              "Special Purpose Zone (Shore Based Activities)" = 
                                "Special Purpose Zone\n(Shore Based Activities)",
                              "Special Purpose Zone (Seagrass Protection) (IUCN IV)" = 
                                "Special Purpose Zone")

wampa <- st_crop(wampa, e)                                                      # Crop to the study area
wasanc <- wampa[wampa$ZONE_TYPE %in% "Sanctuary Zone (IUCN IA)", ]

# Terrestrial parks
terrnp <- st_read("data/spatial/shapefiles/Legislated_Lands_and_Waters_DBCA_011.shp") %>%  # Terrestrial reserves
  dplyr::filter(leg_catego %in% c("Nature Reserve", "National Park"))
terrnp <- st_crop(terrnp, xmin = 113, ymin = -30, xmax = 116, ymax = -26)       # Crop to the study area - using a different extent as this is on land

# Key Ecological Features
kef <- st_read("data/spatial/shapefiles/AU_DOEE_KEF_2015.shp")
kef <- st_crop(kef, e)                                                          # Crop
unique(kef$NAME)
# Simplify names for plot legend
kef$NAME <- dplyr::recode(kef$NAME,"Perth Canyon and adjacent shelf break, and other west coast canyons" = "West coast canyons",                 
                          "Commonwealth marine environment within and adjacent to the west coast inshore lagoons" = "West coast lagoons",                
                          "Ancient coastline at 90-120m depth" = "Ancient coastline",                                                   
                          "Western demersal slope and associated fish communities" = "Western demersal fish",                               
                          "Western rock lobster" = "Western rock lobster",
                          "Commonwealth marine environment surrounding the Houtman Abrolhos Islands" = "Abrolhos Islands")
# Reorder levels so everything plots nicely
kef$NAME <- factor(kef$NAME, levels = c("Western rock lobster", "Western demersal fish", "Wallaby Saddle", 
                                        "Abrolhos Islands", "Ancient coastline", 
                                        "West coast canyons", "West coast lagoons"))

# Coastal waters limit
cwatr <- st_read("data/spatial/shapefiles/amb_coastal_waters_limit.shp")       # Coastal waters limit
cwatr <- st_crop(cwatr, e)


# 5. Key Ecological Features (p5)
kef_fills <- scale_fill_manual(values = c("Ancient coastline" = "#ff6db6",                             
                                          "Western rock lobster" = "#6db6ff",
                                          "West coast canyons" = "#dbb865",
                                          "West coast lagoons" = "#188e8e",
                                          "Abrolhos Islands" = "#2bf446",
                                          "Western demersal fish" = "#016dda",
                                          "Wallaby Saddle" = "#940000"))

p5 <- ggplot() +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA, show.legend = F) +
  labs(fill = "Terrestrial Managed Areas") +
  terr_fills +
  new_scale_fill() +
  geom_sf(data = kef, aes(fill = NAME), alpha = 0.7, color = NA) +
  kef_fills +
  geom_sf(data = mpa, fill = NA, alpha = 1, aes(color = ZoneName), show.legend = F, size = 0.4) +
  nmpa_cols +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.2) +
  labs(x = NULL, y = NULL,  fill = "Key Ecological Features") +
  guides(fill = guide_legend(order = 1)) +
  coord_sf(xlim = c(112, 116), ylim = c(-30, -26)) +                            # Change here
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

png(filename = paste(paste0('plots/spatial/', name) , 'key-ecological-features.png', 
                     sep = "-"), units = "in", res = 200, width = 8, height = 6)
p5
dev.off()