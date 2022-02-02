###
# Project: Parks - Abrolhos Post-Survey
# Data:    BOSS Fish data
# Task:    Fish figures - predictions
# author:  Kingsley Griffin
# date:    Nov-Dec 2021
##

rm(list=ls())

# library(reshape2)
library(ggplot2)
library(viridis)
library(raster)
library(patchwork)
library(sf)

#read in fish prediction data
spreddf <- readRDS("output/site_fish_predictions.rds")   

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

max(spreddf$x) #361746.1
min(spreddf$x) #313956.1
max(spreddf$y) #7772921
min(spreddf$y) #7717571

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
#make points higher than 250 equal to 250 - outliers in total abundance data from crazy tpi values
summary(spreddf$p_totabund)

#testing to see if removing outliers looks better
# spreddf.tot <- spreddf %>%
#   dplyr::filter(p_totabund<200)%>%
#   glimpse()
spreddf$p_totabund <- ifelse(spreddf$p_totabund > 200,200,spreddf$p_totabund)
hist(spreddf.tot$p_totabund)

p11 <- ggplot() +
  geom_tile(data = spreddf, aes(x, y, fill = p_totabund)) +
  scale_fill_viridis(direction = -1) +
  geom_sf(data = nw_mpa, fill = NA, colour = "#b9e6fb", size = 0.8) +
  geom_sf(data = mb_mpa%>%dplyr::filter(waname%in%"Sanctuary Zone"), fill = NA, aes(color = waname), size = 0.8, show.legend = F) +
  theme_minimal() +
  wampa_cols+
  coord_sf(xlim=c(313956.1,361746.1),ylim = c(7717571,7772921))+
  labs(x = NULL, y = NULL, fill = "Total Abundance")#+theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
p11


#species richness
p21 <- ggplot() +
  geom_tile(data = spreddf, aes(x, y, fill = p_richness)) +
  scale_fill_viridis(direction = -1) +
  geom_sf(data = nw_mpa, fill = NA, colour = "#b9e6fb", size = 0.8) +
  geom_sf(data = mb_mpa%>%dplyr::filter(waname%in%"Sanctuary Zone"), fill = NA, aes(color = waname), size = 0.8, show.legend = F) +
  theme_minimal() +
  wampa_cols+
  coord_sf(xlim=c(313956.1,361746.1),ylim = c(7717571,7772921))+
  #scale_x_continuous(breaks = c(113.2,113.4,113.6))+
  labs(x = NULL, y = NULL, fill = "Species Richness")#+theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

p21

# greater than legal size
summary(spreddf$p_legal)
spreddf$p_legal <- ifelse(spreddf$p_legal > 80,80,spreddf$p_legal)
hist(spreddf$p_legal)

p31 <- ggplot() +
  geom_tile(data = spreddf, aes(x, y, fill = p_legal)) +
  scale_fill_viridis(direction = -1) +
  geom_sf(data = nw_mpa, fill = NA, colour = "#b9e6fb", size = 0.8) +
  geom_sf(data = mb_mpa%>%dplyr::filter(waname%in%"Sanctuary Zone"), fill = NA, aes(color = waname), size = 0.8, show.legend = F) +
  theme_minimal() +
  wampa_cols+
  coord_sf(xlim=c(313956.1,361746.1),ylim = c(7717571,7772921))+
  #scale_x_continuous(breaks = c(113.2,113.4,113.6))+
  labs(x = NULL, y = NULL, fill = "Legal")#+theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

p31

#smaller than legal size
summary(spreddf$p_sublegal)
spreddf$p_sublegal <- ifelse(spreddf$p_sublegal > 20,20,spreddf$p_sublegal)
hist(spreddf$p_sublegal)

p41 <- ggplot() +
  geom_tile(data = spreddf, aes(x, y, fill = p_sublegal)) +
  scale_fill_viridis(direction = -1) +
  geom_sf(data = nw_mpa, fill = NA, colour = "#b9e6fb", size = 0.8) +
  geom_sf(data = mb_mpa%>%dplyr::filter(waname%in%"Sanctuary Zone"), fill = NA, aes(color = waname), size = 0.8, show.legend = F) +
  theme_minimal() +
  wampa_cols+
  coord_sf(xlim=c(313956.1,361746.1),ylim = c(7717571,7772921))+
  #scale_x_continuous(breaks = c(113.2,113.4,113.6))+
  labs(x = NULL, y = NULL, fill = "Sublegal")#+theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

p41

gg.predictions <- p11+p21+p31+p41 & theme(legend.justification = "left", aspect.ratio=1)
gg.predictions

ggsave("plots/site_fish_predictions.png", gg.predictions,width = 11, height = 8, dpi = 160)
