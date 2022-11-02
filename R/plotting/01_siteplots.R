###
# Project: ** Add here **
# Data:    Geoscience Australia 250m res bathy
# Task:    Generate exploratory site plots
# author:  Claude Spencer
# date:    ** Add here **
##

# CONTENTS
# 1. Exploratory bathymetry plots (p1)
# 2. National Reef Model plot (p2)
# 3. Location overview plot - includes parks zones and an aus inset (p3)
# 4. Site zoom plot - including sampling points (p4)
# 5. Key Ecological Features (p5)
# 6. Bathymetry cross section (p6)
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
e <- ext(115, 116.17, -21, -19)

# Load necessary spatial files
sf_use_s2(F)                                                                    # Switch off spatial geometry for cropping
# Australian outline and state and commonwealth marine parks
aus    <- st_read("data/spatial/shape/cstauscd_r.mif") %>%                 # Geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
  dplyr::filter(FEAT_CODE %in% c("mainland", "island"))

st_crs(aus) <- gdacrs
ausc <- st_crop(aus, e)

# Commonwealth parks
aumpa  <- st_read("data/spatial/shape/AustraliaNetworkMarineParks.shp")    # All aus mpas
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

# Bathymetry data
cbaths <- list.files("data/spatial/rasters/raw bathymetry", "*tile", full.names = TRUE)
cbathy <- lapply(cbaths, function(x){read.table(file = x, header = TRUE, sep = ",")})
cbathy <- do.call("rbind", lapply(cbathy, as.data.frame))                       # All bathy in tiles as a dataframe
bath_r <- rast(cbathy)
crs(bath_r) <- wgscrs
bath_r <- crop(bath_r, e)
bath_df <- as.data.frame(bath_r, xy = T, na.rm = T)                             # Dataframe - cropped and above 0 use for bath cross section
bath_r <- clamp(bath_r, upper = 0, value = FALSE)                               # Only data below 0
bathy <- as.data.frame(bath_r, xy = T, na.rm = T)

# Generate hillshading
slope  <- terrain(bath_r, v = 'slope', unit = 'degrees')                        # Slope 
aspect <- terrain(bath_r, v = 'aspect', unit = 'degrees')                       # Aspect
hill   <- shade(slope, aspect, angle = 70, direction = 0)                       # Hill shading
hill  <- as.data.frame(hill, xy = T, na.rm = T)                                 # To a dataframe for plotting

# 1. Exploratory bathymetry plots (p1)
p1 <- ggplot() +
  geom_tile(data = hill,aes(x = x, y = y, fill = lyr1), alpha = 1) +
  scale_fill_gradient(low = "white", high = "black", guide = "none") +
  new_scale_fill() +
  geom_tile(data = bathy, aes(x = x, y = y, fill = Z), alpha = 0.7) +
  scale_fill_viridis() +
  geom_contour(data = bathy, aes(x = x, y = y, z = Z), 
               breaks = c(-30, -70, -200, -700),                                # Add here as needed
               colour = "white", size = 0.1) +
  geom_sf(data = ausc, fill = "seashell2", colour = "black", size = 0.1) +
  geom_sf(data = npz, aes(color = ZoneName), fill = NA, size = 0.4) +
  scale_color_manual(values = c("Habitat Protection Zone" = "#fff8a3",
                                "National Park Zone" = "#7bbc63",
                                "Multiple Use Zone" = "#b9e6fb")) +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 1, size = 0.4) +
  coord_sf(xlim = c(112, 116), ylim = c(-30, -26)) +                            # Change here
  labs(y = "Latitude", x = "Longitude")+
  theme_minimal()
png(filename = paste(paste0('plots/spatial/', name) , 'exploratory-site-plot.png', 
                     sep = "-"), height = 4, width = 10,
    res = 300, units = "in")
p1
dev.off()

# 2. National Reef Model plot (p2)
nrm <- rast("data/spatial/rasters/ecosystem-types-19class-naland.tif")
nrm <- crop(nrm, e)

# Load the classes to match to the raster
nrm_class <- read.csv("data/spatial/rasters/Ecosystem_categories-final-2021.csv") %>%
  ga.clean.names() %>%
  dplyr::rename(classname = category.number) %>%
  dplyr::select(classname, exp.ecosystem.names) %>%
  glimpse()

nrm_df <- as.data.frame(nrm, xy = TRUE, na.rm = TRUE) %>%                       # Join and convert to a dataframe
  dplyr::rename(classname = "ecosystem-types-19class-naland") %>%
  dplyr::left_join(nrm_class) %>%
  dplyr::mutate(exp.ecosystem.names = gsub("\\.", " ", exp.ecosystem.names)) %>%
  glimpse()

unique(nrm_df$exp.ecosystem.names)                                              # Manually set colours for plotting
nrm_fills <- scale_fill_manual(values = c(
  "Shelf unvegetated soft sediments" = "cornsilk1",
  "Mesophotic coral reefs" = "orange",
  "Shallow coral reefs less than 30 m depth" = "coral2",
  "Shelf vegetated sediments" = "seagreen3"))

terr_fills <- scale_fill_manual(values = c("National Park" = "#c4cea6",          # Set the colours for terrestrial parks
                                           "Nature Reserve" = "#e4d0bb"),
                                guide = "none")

# assign mpa colours - full levels are saved at end of script for future ref
nmpa_cols <- scale_color_manual(values = c("Habitat Protection Zone" = "#fff8a3",
                                           "National Park Zone" = "#7bbc63",
                                           "Multiple Use Zone" = "#b9e6fb"))

p2 <- ggplot() +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA) +
  terr_fills +
  new_scale_fill() +
  geom_tile(data = nrm_df, aes(x, y, fill = exp.ecosystem.names)) +
  nrm_fills +
  geom_contour(data = bathy, aes(x = x, y = y, z = Z),
               breaks = c(-30, -70, -200, - 700, - 7000), colour = "black", alpha = 1, size = 0.18) +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 1, size = 0.6) +
  geom_sf(data = npz, fill = NA, aes(colour = ZoneName), size = 0.4) +
  nmpa_cols +
  labs(color = "Australian Marine Parks") +
  new_scale_color() +
  coord_sf(xlim = c(112, 116), ylim = c(-30, -26)) +                            # Change here
  labs(fill = "Habitat classification", x = NULL, y = NULL) +
  theme_minimal()

png(filename = paste(paste0('plots/spatial/', name) , 'national-reef-model.png', 
                     sep = "-"), width = 10, height = 4,
    units = "in", res = 300)
p2
dev.off()

# 3. Location overview plot - includes parks zones and an aus inset (p3)
# assign mpa colours - full levels are saved at end of script for future ref
nmpa_fills <- scale_fill_manual(values = c("National Park Zone" = "#7bbc63",
                                           "Multiple Use Zone" = "#b9e6fb",
                                           "Special Purpose Zone" = "#6daff4",
                                           "Habitat Protection Zone" = "#fff8a3"
))

wampa_fills <- scale_fill_manual(values = c("Fish Habitat Protection Area" = "#fac86b",
                                            "Reef Observation Area" = "#ddccff",
                                            "Sanctuary Zone" = "#bfd054",
                                            "General Use Zone" = "#bddde1",
                                            "Recreation Zone" = "#f4e952",
                                            "Special Purpose Zone" = "#c5bcc9",
                                            "Marine Nature Reserve" = "#bfd054"
))

p3 <- ggplot() +
  geom_contour_filled(data = bathy, aes(x = x, y = y, z = Z,
                                        fill = after_stat(level)),
                      breaks = c(-30, -70, -200, - 700, -2000 , -4000,-6000)) +
  geom_contour(data = bathy, aes(x = x, y = y, z = Z),
               breaks = c(-30, -70, -200, - 700, -2000 , -4000,-6000), colour = "white", alpha = 3/5, size = 0.1) +
  scale_fill_grey(start = 1, end = 0.5, guide = "none") +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
  new_scale_fill() +
  geom_sf(data = wampa, aes(fill = waname), alpha = 2/5, colour = NA) +
  wampa_fills +
  labs(fill = "State Marine Parks") +
  new_scale_fill() +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA) +
  labs(fill = "State Managed Areas") +
  terr_fills +
  new_scale_fill() +
  geom_sf(data = mpa, aes(fill = ZoneName), alpha = 4/5, colour = NA) +
  nmpa_fills +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.2) +
  labs(x = NULL, y = NULL, fill = "Australian Marine Parks") +
  guides(fill = guide_legend(order = 1)) +
  coord_sf(xlim = c(112, 116), ylim = c(-30, -26)) +                            # Change here
  theme_minimal()
p3

# inset map
p3.1 <- ggplot(data = aus) +
  geom_sf(fill = "seashell1", colour = "grey90", size = 0.05, alpha = 4/5) +
  geom_sf(data = aumpa, alpha = 5/6, colour = "grey85", size = 0.02) +
  coord_sf(xlim = c(108, 125), ylim = c(-37, -13)) +
  annotate("rect", xmin = 108.9, xmax = 115.0607, ymin = -29.4, ymax = -24.2,   # Change here 
           colour = "grey25", fill = "white", alpha = 1/5, size = 0.2) +
  theme_bw() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "grey70"))
p3.1

# plot both 
p3.1 + p3 + plot_layout(widths = c(0.8, 2.2))

ggsave(paste(paste0('plots/spatial/', name) , 'broad-site-plot.png', 
             sep = "-"), dpi = 200, width = 10, height = 6)

# 4. Site zoom plot - including sampling points (p4)
bossmet <- read.csv("data/tidy/2021-05_Abrolhos_BOSS.checked.metadata.csv") %>%
  dplyr::mutate(method = "Drop camera") %>%
  glimpse()
bruvmet <- read.csv("data/tidy/2021-05_Abrolhos_stereo-BRUVs.checked.metadata.csv") %>%
  dplyr::mutate(method = "BRUV") %>%
  glimpse()
metadata <- bind_rows(bossmet, bruvmet)

p4 <- ggplot() +
  geom_contour_filled(data = bathy, aes(x = x, y = y, z = Z,
                                        fill = after_stat(level)),
                      breaks = c(0, -30, -70, -200, -700, -2000, -4000, -10000), alpha = 4/5) +
  scale_fill_grey(start = 1, end = 0.5 , guide = "none") +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
  new_scale_fill() +  
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA) +
  labs(fill = "State Managed Areas") +
  terr_fills +
  new_scale_fill() +
  geom_sf(data = npz, aes(fill = ZoneName), alpha = 3/5, colour = NA) +
  nmpa_fills +
  labs(x = NULL, y = NULL, fill = "Australian Marine Park") +
  geom_contour(data = bathy, aes(x = x, y = y, z = Z), 
               breaks = c(0, -30, -70, -200, - 700, - 9000), colour = "white", alpha = 1, size = 0.2) +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.2) +
  geom_point(data = metadata, aes(longitude, latitude, colour = method),
             alpha = 3/5, shape = 10) +
  scale_colour_manual(values = c("BRUV" = "indianred4",
                                 "Drop Camera" = "seagreen4")) +
  labs(colour = "Sample", x = NULL, y = NULL) +
  coord_sf(xlim = c(112, 116), ylim = c(-30, -26)) +                            # Change here
  theme_minimal()

png(filename = paste(paste0('plots/spatial/', name) , 'sampling-locations.png', 
                     sep = "-"), units = "in", res = 200, width = 8, height = 6)
p4
dev.off()

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

# 6. Bathymetry cross section (p6)
sf_use_s2(T)
bath_cross <- cbathy %>%
  dplyr::filter(abs(Y - -28) == min(abs(Y - -28)),
                Y < 116, Z > -250) %>% 
  st_as_sf(coords = c("X", "Y"), crs = wgscrs)

auss <- st_transform(aus, wgscrs)
auss <- auss[auss$FEAT_CODE %in% "mainland", ]
auss <- st_union(auss)
ausout <- st_cast(auss, "MULTILINESTRING")

bath_sf <- bath_cross %>%
  dplyr::mutate("distance.from.coast" = st_distance(bath_cross, ausout),
                x = unlist(map(bath_cross$geometry, 1)),
                y = unlist(map(bath_cross$geometry, 2)),
                land = lengths(st_intersects(bath_cross, auss)) > 0) %>%
  glimpse()

bath_slice <- as.data.frame(bath_sf) %>%
  dplyr::select(-geometry) %>%
  dplyr::rename(depth = "Z") %>%
  dplyr::mutate(distance.from.coast = as.numeric(distance.from.coast/1000)) %>%
  dplyr::mutate(distance.from.coast = ifelse(land %in% "FALSE", distance.from.coast*-1, distance.from.coast)) %>%
  glimpse()

paleo <- data.frame(depth = c(-118, -94, -63, -41),
                    label = c("20-30 Ka", "15-17 Ka", "12-13 Ka", "9-10 Ka"))

for (i in 1:nrow(paleo)) {
  temp <- bath_slice %>%
    dplyr::filter(abs(bath_slice$depth - paleo$depth[i]) == min(abs(bath_slice$depth - paleo$depth[i]))) %>%
    dplyr::select(depth, distance.from.coast) %>%
    slice(1)
  
  if (i == 1) {
    dat <- temp
  } 
  else {
    dat <- bind_rows(dat, temp)
  }
}

paleo$distance.from.coast <- dat$distance.from.coast
rm("temp", "dat")

p6 <- ggplot() +
  geom_rect(aes(xmin = min(bath_slice$distance.from.coast), xmax = 9, ymin =-Inf, ymax = 0), fill = "#12a5db", alpha = 0.5) +
  geom_line(data = bath_slice, aes(y = depth, x = distance.from.coast)) +
  geom_ribbon(data = bath_slice, aes(ymin = -Inf, ymax = depth, x = distance.from.coast), fill = "tan") +
  theme_classic() +
  scale_x_continuous(expand = c(0,0), limits = c(min(bath_slice$distance.from.coast), 10)) +
  labs(x = "Distance from coast (km)", y = "Elevation (m)") +
  geom_segment(data = paleo, aes(x = distance.from.coast, xend = distance.from.coast + 20, 
                                 y = depth, yend = depth), linetype = 2, alpha = 0.5) +
  geom_text(data = paleo, aes(x = distance.from.coast + 26, y = depth, label = label), size = 3)

png(filename = paste(paste0('plots/spatial/', name) , 'bathymetry-cross-section.png', 
                     sep = "-"), units = "in", res = 200, width = 8, height = 6)
p6
dev.off()

# 7. Old sea level map (p7)
depth_fills <- scale_fill_manual(values = c("#b8d9a9","#8dbc80", "#5d9d52"),
                                 labels = c("9-10 Ka", "15-17 Ka", "20-30 Ka"),
                                 name = "Coastline age")

# Convert back to a raster and smooth it out
# build basic plot elements
p7 <- ggplot() +
  geom_tile(data = bathy, aes(x = x, y = y, fill = Z)) +
  scale_fill_gradient2(low = "royalblue4", mid = "lightskyblue1", high = "white", name = "Depth (m)") +
  new_scale_fill() +
  geom_contour_filled(data = bathy, aes(x = x, y = y, z = Z,
                                        fill = after_stat(level)),
                      breaks = c(0, -40, -70, -125)) +
  depth_fills +
  new_scale_fill() +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey62", size = 0.2) +
  new_scale_fill() +
  geom_sf(data = mpa%>%dplyr::filter(!ZoneName %in% "National Park Zone"), 
          colour = "grey61", size = 0.2, fill = NA) +
  geom_sf(data = npz, 
          colour = "#7bbc63", size = 0.55, fill = NA) +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 0.7, size = 0.3) +
  coord_sf(xlim = c(112, 116), ylim = c(-30, -26)) +                            # Change here
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal()+
  theme(panel.background = element_rect(fill = "#b8d9a9"))
png(filename = paste(paste0('plots/spatial/', name) , 'old-sea-levels.png', 
                     sep = "-"), units = "in", res = 200, width = 8, height = 6)
p7
dev.off()