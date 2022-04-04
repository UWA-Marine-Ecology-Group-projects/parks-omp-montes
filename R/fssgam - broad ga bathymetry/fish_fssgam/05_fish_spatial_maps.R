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

#read in fish prediction data
spreddf <- readRDS("output/spatial_predictions_broad/site_fish_predictions.rds")   

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
sppcrs <- CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")     # crs for sp objects
nw_mpa <- st_transform(nw_mpa, sppcrs)

aus    <- st_read("data/spatial/shape/cstauscd_r.mif")                            # geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
montes <- aus[aus$GROUP_NAME %in% c("MONTEBELLO ISLANDS"), ]                      # just montes
st_crs(montes) <- wgscrs
montes <- st_transform(montes, sppcrs)

max(spreddf$x) #362426.3
min(spreddf$x) #314141.3
max(spreddf$y) #7774100
min(spreddf$y) #7716761

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

p11 <- ggplot() +
  geom_tile(data = spreddf, aes(x, y, fill = p_totabund)) +
  scale_fill_viridis(direction = -1) +
  geom_sf(data = nw_mpa, fill = NA, colour = "#b9e6fb", size = 0.8) +
  geom_sf(data = mb_mpa%>%dplyr::filter(waname%in%"Sanctuary Zone"), fill = NA, aes(color = waname), size = 0.8, show.legend = F) +
  theme_minimal() +
  wampa_cols+
  geom_sf(data = montes, fill = "seashell2", colour = "grey80", size = 0.1) +
  coord_sf(xlim = c(315000, 360000), ylim = c(7720000, 7770000)) +
  labs(x = NULL, y = NULL, fill = "Total Abundance")
p11


#species richness
p21 <- ggplot() +
  geom_tile(data = spreddf, aes(x, y, fill = p_richness)) +
  scale_fill_viridis(direction = -1) +
  geom_sf(data = nw_mpa, fill = NA, colour = "#b9e6fb", size = 0.8) +
  geom_sf(data = mb_mpa%>%dplyr::filter(waname%in%"Sanctuary Zone"), fill = NA, aes(color = waname), size = 0.8, show.legend = F) +
  theme_minimal() +
  wampa_cols+
  geom_sf(data = montes, fill = "seashell2", colour = "grey80", size = 0.1) +
  coord_sf(xlim = c(315000, 360000), ylim = c(7720000, 7770000))+
  # scale_x_continuous(breaks = c(113.2,113.4,113.6))+
  labs(x = NULL, y = NULL, fill = "Species Richness")

p21

# greater than legal size

p31 <- ggplot() +
  geom_tile(data = spreddf, aes(x, y, fill = p_legal)) +
  scale_fill_viridis(direction = -1) +
  geom_sf(data = nw_mpa, fill = NA, colour = "#b9e6fb", size = 0.8) +
  geom_sf(data = mb_mpa%>%dplyr::filter(waname%in%"Sanctuary Zone"), fill = NA, aes(color = waname), size = 0.8, show.legend = F) +
  theme_minimal() +
  wampa_cols+
  geom_sf(data = montes, fill = "seashell2", colour = "grey80", size = 0.1) +
  coord_sf(xlim = c(315000, 360000), ylim = c(7720000, 7770000)) +
  #scale_x_continuous(breaks = c(113.2,113.4,113.6))+
  labs(x = NULL, y = NULL, fill = "Legal")

p31

#smaller than legal size

p41 <- ggplot() +
  geom_tile(data = spreddf, aes(x, y, fill = p_sublegal)) +
  scale_fill_viridis(direction = -1) +
  geom_sf(data = nw_mpa, fill = NA, colour = "#b9e6fb", size = 0.8) +
  geom_sf(data = mb_mpa%>%dplyr::filter(waname%in%"Sanctuary Zone"), fill = NA, aes(color = waname), size = 0.8, show.legend = F) +
  theme_minimal() +
  wampa_cols+
  geom_sf(data = montes, fill = "seashell2", colour = "grey80", size = 0.1) +
  coord_sf(xlim = c(315000, 360000), ylim = c(7720000, 7770000)) +
  #scale_x_continuous(breaks = c(113.2,113.4,113.6))+
  labs(x = NULL, y = NULL, fill = "Sublegal")

p41

gg.predictions <- p11+p21+p31+p41 & theme(legend.justification = "left")
gg.predictions

ggsave("plots/site_fish_predictions.png", gg.predictions,width = 9, height = 8, dpi = 160)
