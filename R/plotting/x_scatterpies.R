###
# Project: MAC HUB South-west Corner
# Data:    BRUVS, BOSS
# Task:    Scatterpies
# author:  Claude
# date:    April 2022
##

rm(list=ls())

library(dplyr)
library(ggplot2)
library(scatterpie)
library(viridis)
library(sf)
library(raster)
library(ggnewscale)
library(metR)
library(cowplot)

working.dir <- getwd()
setwd(working.dir)

#define crs
wgscrs <- CRS("+proj=longlat +datum=WGS84")
sppcrs <- CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")     # crs for sp objects

# bring in marine parks
aumpa  <- st_read("data/spatial/shape/AustraliaNetworkMarineParks.shp")%>%           # all aus mpas
  dplyr::filter(ResName%in%"Montebello")
#bring in state MP
wampa  <- st_read("data/spatial/shape/WA_MPA_2018.shp")

# get aus outline data
aus    <- st_read("data/spatial/shape/cstauscd_r.mif")                     # geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
aus    <- aus[aus$FEAT_CODE == c("mainland","island"), ]
st_crs(aus)         <- st_crs(aumpa)

dat <- readRDS("data/tidy/broad_merged_habitat.rds")%>%
  dplyr::select(longitude.1, latitude.1, biota.consolidated, 
                biota.unconsolidated, mesophotic.reef, photic.reef, 
                biota.macroalgae, biota.stony.corals)%>%                        #put these in to try them out
  dplyr::mutate(grouping = factor(1:194))%>%
  dplyr::rename("Invertebrate reef" = mesophotic.reef,
                "Macroalgae/coral reef" = photic.reef,
                "Rock" = biota.consolidated,
                "Macroalgae" = biota.macroalgae,
                "Coral" = biota.stony.corals,
                "Sand" = biota.unconsolidated)%>%
  glimpse()



#bring in bathy for contour lines
bathy <- raster("data/spatial/raster/ga_bathy_largerextent.tif")                # bathymetry trimmed to project area
bathdf <- as.data.frame(bathy, xy = T)%>%
  dplyr::rename("longitude.1" = x, "latitude.1" = y)
colnames(bathdf)[3] <- "Depth"
# assign commonwealth zone colours
nmpa_cols <- scale_fill_manual(values = c("National Park Zone" = "#7bbc63",
                                          "Habitat Protection Zone" = "#fff8a3",
                                          "Multiple Use Zone" = "#b9e6fb",
                                          "Special Purpose Zone\n(Mining Exclusion)" = "#368ac1")) 

# state colours
wampa_cols <- scale_fill_manual(values = c("Sanctuary Zone" = "#bfd054",
                                           "General Use Zone" = "#bddde1",
                                           "Recreation Zone" = "#f4e952"))

#class colours 
hab_cols <- scale_fill_manual(values = c("Rock" = "grey40",
                                         "Invertebrate reef" = "plum",
                                         "Macroalgae/coral reef" = "darkgoldenrod4",
                                         # "Seagrasses" = "forestgreen",
                                         "Sand" = "wheat"))

#depth colours 
depth_cols <- scale_fill_manual(values = c("#a7cfe0","#9acbec","#98c4f7", "#a3bbff"),guide = "none")
#shallow to deep

#make the plot
gg.scatterpie <- ggplot() + 
  geom_contour_filled(data = bathdf, aes(longitude.1, latitude.1, z = Depth, fill = after_stat(level)), color = "black",
                      breaks = c(-30, -70, -200,-700,-10000), size = 0.1) +
  # annotate("text", x = c(114.40,114.467,114.72,114.945), y = -33.85, label = c("700m","200m","70m","30m"), size = 2)+
  scale_fill_grey(start = 0.5, end = 0.8) +
  # depth_cols+
  new_scale_fill()+
  # geom_sf(data = aus, fill = "seashell2", colour = "black", size = 0.1) +
  # geom_sf(data = wampa,fill = "#bfd054", alpha = 2/5, color = NA)+
  # wampa_cols+
  # labs(fill = "State Marine Parks")+
  # new_scale_fill()+
  # geom_sf(data = aumpa, fill = "#7bbc63",alpha = 2/5, color = NA) +
  # labs(fill = "Australian Marine Parks")+
  # nmpa_cols+
  # new_scale_fill()+
  geom_scatterpie(aes(x=longitude.1, y=latitude.1, group=grouping), data=dat,
                  cols = c("Invertebrate reef","Macroalgae/coral reef","Rock",
                           "Sand"),
                  pie_scale = 0.45, color = NA) +
  labs(fill = "Habitat",x = 'Longitude', y = 'Latitude')+
  hab_cols+
  # coord_sf(xlim = c(min(dat$longitude.1),max(dat$longitude.1)), 
           # ylim = c(min(dat$latitude.1), max(dat$latitude.1)))+
  coord_sf(xlim = c(314179, 359788), ylim = c(7719520, 7770980))+
  theme_minimal()+
  theme(panel.background = element_rect(fill = "#b9d1d6"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
gg.scatterpie

save_plot("plots/spatial/scatterpies.png", gg.scatterpie,base_height = 6.5,base_width = 7)
