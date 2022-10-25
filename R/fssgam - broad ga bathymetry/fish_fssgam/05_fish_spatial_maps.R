###
# Project: Parks - Montes omp
# Data:    BRUVS + Habitat data
# Task:    Fish spatial prediction figures
# author:  Kingsley Griffin & Claude
# date:    April 2022
##

rm(list=ls())

# library(reshape2)
library(ggplot2)
library(viridis)
library(raster)
library(patchwork)
library(sf)
library(terra)

#read in fish prediction data
spreddf <- readRDS("output/spatial_predictions_broad/site_fish_predictions.rds")   
spreddf <- rast(spreddf, type = "xyz")
plot(spreddf)
cwatrp <- vect("data/spatial/shape/coastal-waters_polygon.shp")
spreddf <- mask(spreddf, cwatrp, inverse = T)
plot(spreddf)
spreddf <- as.data.frame(spreddf, xy = T, na.rm = T)

# bring in spatial layers
#National marine parks
aumpa  <- st_read("data/spatial/shape/AustraliaNetworkMarineParks.shp")           # all aus mpas
nw_mpa <- aumpa[aumpa$ResName %in% c("Montebello"), ]                             # just Abrolhos Aus MP
# mb_npz <- nw_mpa[nw_mpa$ZoneName == "National Park Zone", ]
# ab_npz$parkid <- c(1:3)

#State marine parks
wampa  <- st_read("data/spatial/shape/WA_MPA_2018.shp")
mb_mpa <- wampa[wampa$NAME %in% "Montebello Islands" ,]

wgscrs <- CRS("+proj=longlat +datum=WGS84")
sppcrs <- CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")       # crs for sp objects
# nw_mpa <- st_transform(nw_mpa, sppcrs)

cwatr <- st_read("data/spatial/shape/amb_coastal_waters_limit.shp")

aus    <- st_read("data/spatial/shape/cstauscd_r.mif")                            # geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/

aus    <- st_read("data/spatial/shape/cstauscd_r.mif")                     # geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
aus <- aus[aus$FEAT_CODE %in% c("mainland", "island"),]
st_crs(aus) <- st_crs(aumpa)

montes <- aus[aus$GROUP_NAME %in% c("MONTEBELLO ISLANDS"), ]                      # just montes
st_crs(montes) <- st_crs(aumpa)
# montes <- st_transform(montes, sppcrs)

#bring in bathy for contour lines
bathy <- raster("data/spatial/raster/ga_bathy_largerextent.tif")                # bathymetry trimmed to project area
proj4string(bathy) <- wgscrs
# bathy <- projectRaster(bathy, crs = sppcrs)
bathdf <- as.data.frame(bathy, xy = T)
colnames(bathdf)[3] <- "Depth"

#wa MPA colours
mb_mpa$waname <- gsub("( \\().+(\\))", "", mb_mpa$ZONE_TYPE)
mb_mpa$waname <- gsub(" [1-4]", "", mb_mpa$waname)

mb_mpa$waname <- dplyr::recode(mb_mpa$waname, 
                               "General Use" = "General Use Zone",
                               # "MMA" = "Marine Management Area",
                               # "Recreation Area" = "Recreation Zone",
                               # "Conservation Area" = "Sanctuary Zone",
                               "Special Purpose Zone (Shore Based Activities)" = 
                               "Special Purpose Zone\n(Shore Based Activities)")

wampa_cols <- scale_color_manual(values = c("Sanctuary Zone" = "#7bbc63",      #changed to NPZ color as test from #bfd054
                                           #"Marine Nature Reserve" = "#bfd054",
                                           #"Conservation Area" = "#b3a63d",
                                           # "Habitat Protection Zone" = "#fffbcc",# State MPA colours
                                           # "Fish Habitat Protection Area" = "#fbff85",
                                           # "National Park Zone" = "#a4d194",
                                           "General Use Zone" = "#bddde1",
                                           "Recreation Zone" = "#f4e952",
                                           "Special Purpose Zone" = "#c5bcc9"
                                           # "Special Purpose Zone\n(Shore Based Activities)" = "#ba3030"
                                           # "Special Purpose Zone\n(Habitat Protection)" = "#f0ac41",
                                           #"Reef Observation Area" = "#ddccff",
                                           #"Marine Management Area" = "#b7cfe1"
))

wampa_fills <- scale_fill_manual(values = c("Sanctuary Zone" = "#bfd054",
                                            #"Marine Nature Reserve" = "#bfd054",
                                            #"Conservation Area" = "#b3a63d",
                                            # "Habitat Protection Zone" = "#fffbcc",# State MPA colours
                                            #"Fish Habitat Protection Area" = "#fbff85",
                                            # "National Park Zone" = "#a4d194",
                                            "General Use Zone" = "#bddde1",
                                            "Recreation Zone" = "#f4e952",
                                            "Special Purpose Zone" = "#c5bcc9"
                                            # "Special Purpose Zone\n(Shore Based Activities)" = "#ba3030"
                                            # "Special Purpose Zone\n(Habitat Protection)" = "#f0ac41",
                                            #"Reef Observation Area" = "#ddccff",
                                            #"Marine Management Area" = "#b7cfe1"
))

# plotting broad maps
#total abundance
sf_use_s2(T)

dep_ann <- data.frame(x = c(115.340000003, 115.219999997, 115.415000005, 115.582000000), 
                      y = c(-20.599999997, -20.179999997, -20.270000003, -20.144999998), label = c("30m","70m", "30m","70m")) # updated BG

p11 <- ggplot() +
  
  geom_contour_filled(data = bathdf, aes(x = x, y = y, z = Depth,
                                         fill = after_stat(level)),
                      breaks = c(0, -30, -70, -200, -700)) +
  scale_fill_grey(start = 1, end = 0.5, guide = "none") +
  # geom_contour(data = bathdf, aes(x, y, z = Depth),
  #              breaks = c(0, -30, -70, -200, -700), colour = "white",
  #              alpha = 1, size = 0.1) +
  
  new_scale_fill()+
  
  geom_tile(data = spreddf, aes(x, y, fill = p_totabund)) +
  scale_fill_viridis(direction = -1) +
  geom_contour(data = bathdf, aes(x, y, z = Depth), color = "black",
  breaks = c(-30, -70, -200), size = 0.2) +
  geom_sf(data = nw_mpa, fill = NA, colour = "#b9e6fb", size = 1) +
  # geom_sf(data = mb_mpa%>%dplyr::filter(waname%in%"Sanctuary Zone"), fill = NA, 
  #         aes(color = waname), size = 0.4, show.legend = F) +
  theme_minimal() +
  wampa_cols+
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = cwatr, colour = "red", size = 1) +
  geom_text(data = dep_ann, aes(x , y, label = label), inherit.aes = F, size = 2.8, colour = "black")+
  # annotate("text", x = c(327004.392,313992.301, 334705), 
  #          y = c(7721238.518, 7767602.728,7757846.68), label = c("30m","70m", "30m"), size = 2)+
  # coord_sf(xlim = c(315000, 360000), ylim = c(7720000, 7768000)) +
  # coord_sf(xlim = c(min(spreddf$x), max(spreddf$x)), ylim = c(min(spreddf$y), max(spreddf$y))) +
  coord_sf(xlim = c(115.22, 116), ylim = c(-20.83627, -20)) + # Updated BG
  scale_x_continuous(breaks = seq(115.2, 116, by = 0.2)) + # Added BG
  labs(x = NULL, y = NULL, fill = "Total Abundance", title = "Whole assemblage")
p11


#species richness
p21 <- ggplot() +
  
  geom_contour_filled(data = bathdf, aes(x = x, y = y, z = Depth,
                                         fill = after_stat(level)),
                      breaks = c(0, -30, -70, -200, -700)) +
  scale_fill_grey(start = 1, end = 0.5, guide = "none") +
  geom_contour(data = bathdf, aes(x, y, z = Depth),
               breaks = c(0, -30, -70, -200, -700), colour = "white",
               alpha = 1, size = 0.1) +
  
  new_scale_fill()+
  
  geom_tile(data = spreddf, aes(x, y, fill = p_richness)) +
  scale_fill_viridis(direction = -1) +
  # geom_contour(data = bathdf, aes(x, y, z = Depth), color = "black",
  #              breaks = c(-30, -70, -200), size = 0.2) +
  geom_sf(data = nw_mpa, fill = NA, colour = "#b9e6fb", size = 1) +
  # geom_sf(data = mb_mpa%>%dplyr::filter(waname%in%"Sanctuary Zone"), fill = NA, 
  #         aes(color = waname), size = 0.4, show.legend = F) +
  theme_minimal() +
  wampa_cols+
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = cwatr, colour = "red", size = 1) +
  # geom_sf(data = montes, fill = "seashell2", colour = "grey80", size = 0.1) +
  # annotate("text", x = c(327004.392,313992.301, 334705), 
  #          y = c(7721238.518, 7767602.728,7757846.68), label = c("30m","70m", "30m"), size = 2)+
  # coord_sf(xlim = c(min(spreddf$x), max(spreddf$x)), ylim = c(min(spreddf$y), max(spreddf$y))) +\
  geom_text(data = dep_ann, aes(x , y, label = label), inherit.aes = F, size = 2.8, colour = "black")+
  coord_sf(xlim = c(115.22, 116), ylim = c(-20.83627, -20)) + # Updated BG
  scale_x_continuous(breaks = seq(115.2, 116, by = 0.2)) + # Added BG
  labs(x = NULL, y = NULL, fill = "Species Richness")

p21

# greater than legal size

p31 <- ggplot() +
  
  geom_contour_filled(data = bathdf, aes(x = x, y = y, z = Depth,
                                         fill = after_stat(level)),
                      breaks = c(0, -30, -70, -200, -700)) +
  scale_fill_grey(start = 1, end = 0.5, guide = "none") +
  geom_contour(data = bathdf, aes(x, y, z = Depth),
               breaks = c(0, -30, -70, -200, -700), colour = "white",
               alpha = 1, size = 0.1) +
  
  new_scale_fill()+
  
  geom_tile(data = spreddf, aes(x, y, fill = p_legal)) +
  scale_fill_viridis(direction = -1) +
  geom_contour(data = bathdf, aes(x, y, z = Depth), color = "black",
               breaks = c(-30, -70, -200), size = 0.2) +
  geom_sf(data = nw_mpa, fill = NA, colour = "#b9e6fb", size = 1) +
  # geom_sf(data = mb_mpa%>%dplyr::filter(waname%in%"Sanctuary Zone"), fill = NA, 
  #         aes(color = waname), size = 0.4, show.legend = F) +
  theme_minimal() +
  wampa_cols+
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = cwatr, colour = "red", size = 1) +
  # annotate("text", x = c(327004.392,313992.301, 334705), 
  #          y = c(7721238.518, 7767602.728,7757846.68), label = c("30m","70m", "30m"), size = 2)+
  # coord_sf(xlim = c(315000, 360000), ylim = c(7720000, 7768000)) +
  # coord_sf(xlim = c(min(spreddf$x), max(spreddf$x)), ylim = c(min(spreddf$y), max(spreddf$y))) +
  geom_text(data = dep_ann, aes(x , y, label = label), inherit.aes = F, size = 2.8, colour = "black")+
  coord_sf(xlim = c(115.22, 116), ylim = c(-20.83627, -20)) + # Updated BG
  scale_x_continuous(breaks = seq(115.2, 116, by = 0.2)) + # Added BG
  labs(x = NULL, y = NULL, fill = "Legal", title = "Targeted assemblage")

p31

#smaller than legal size

p41 <- ggplot() +
  
  geom_contour_filled(data = bathdf, aes(x = x, y = y, z = Depth,
                                         fill = after_stat(level)),
                      breaks = c(0, -30, -70, -200, -700)) +
  scale_fill_grey(start = 1, end = 0.5, guide = "none") +
  geom_contour(data = bathdf, aes(x, y, z = Depth),
               breaks = c(0, -30, -70, -200, -700), colour = "white",
               alpha = 1, size = 0.1) +
  
  new_scale_fill()+
  
  geom_tile(data = spreddf, aes(x, y, fill = p_sublegal)) +
  scale_fill_viridis(direction = -1) +
  geom_contour(data = bathdf, aes(x, y, z = Depth), color = "black",
               breaks = c(-30, -70, -200), size = 0.2) +
  geom_sf(data = nw_mpa, fill = NA, colour = "#b9e6fb", size = 1) +
  # geom_sf(data = mb_mpa%>%dplyr::filter(waname%in%"Sanctuary Zone"), fill = NA, 
  #         aes(color = waname), size = 0.4, show.legend = F) +
  theme_minimal() +
  wampa_cols+
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = cwatr, colour = "red", size = 1) +
  # annotate("text", x = c(327004.392,313992.301, 334705), 
  #          y = c(7721238.518, 7767602.728,7757846.68), label = c("30m","70m", "30m"), size = 2)+
  # coord_sf(xlim = c(min(spreddf$x), max(spreddf$x)), ylim = c(min(spreddf$y), max(spreddf$y))) +
  geom_text(data = dep_ann, aes(x , y, label = label), inherit.aes = F, size = 2.8, colour = "black")+
  coord_sf(xlim = c(115.22, 116), ylim = c(-20.83627, -20)) + # Updated BG
  scale_x_continuous(breaks = seq(115.2, 116, by = 0.2)) + # Added BG
  labs(x = NULL, y = NULL, fill = "Sublegal")

p41

gg.predictions <- p11 + p21 + p31 + p41 & theme(legend.justification = "left")
png(filename = "plots/site_fish_predictions.png", width = 10, height = 8,
    res = 300, units = "in")
gg.predictions
dev.off()

