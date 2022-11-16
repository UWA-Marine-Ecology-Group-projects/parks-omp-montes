###
# Project: Parks Montes
# Data:    BRUVS, BOSS
# Task:    Scatterpies
# author:  Claude
# date:    November 2022
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

#define crs
wgscrs <- CRS("+proj=longlat +datum=WGS84")
sppcrs <- CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")     # crs for sp objects

# bring in state waters
cwatr  <- st_read("data/spatial/shape/amb_coastal_waters_limit.shp")            # coastal waters line
cwatr <- st_crop(cwatr, c(xmin = 113, xmax = 118, ymin = -23, ymax = -20))

# bring in marine parks
aumpa  <- st_read("data/spatial/shape/AustraliaNetworkMarineParks.shp")%>%           # all aus mpas
  dplyr::filter(ResName%in%"Montebello")
#bring in state MP
wampa  <- st_read("data/spatial/shape/WA_MPA_2018.shp")
mb_mp <- wampa[wampa$NAME %in% c("Montebello Islands","Barrow Island"),]        # just wa parks nearby

# simplify zone names
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

wa_mp <- mb_mp %>%
  dplyr::filter(waname%in%"Sanctuary Zone")

# get aus outline data
aus    <- st_read("data/spatial/shape/cstauscd_r.mif")                     # geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
aus <- aus[aus$FEAT_CODE %in% c("mainland", "island"),]
st_crs(aus) <- st_crs(aumpa)

dat <- readRDS("data/tidy/broad_merged_habitat.rds")%>%
  dplyr::select(longitude.1, latitude.1, biota.consolidated, 
                biota.unconsolidated, mesophotic.reef, photic.reef, 
                biota.macroalgae, biota.stony.corals)%>%                        #put these in to try them out
  dplyr::mutate(grouping = factor(1:194))%>%
  dplyr::rename("Soft coral/sponges" = mesophotic.reef,
                "Macroalgae/coral reef" = photic.reef,
                "Rock" = biota.consolidated,
                "Macroalgae" = biota.macroalgae,
                "Hard coral" = biota.stony.corals,
                "Sand" = biota.unconsolidated)%>%
  glimpse()

#bring in bathy for contour lines
bathy <- raster("data/spatial/rasters/large/ga_bathy_largerextent.tif")                # bathymetry trimmed to project area
bathdf <- as.data.frame(bathy, xy = T)%>%
  dplyr::rename("longitude.1" = x, "latitude.1" = y)
colnames(bathdf)[3] <- "Depth"
# assign commonwealth zone colours
nmpa_cols <- scale_color_manual(values = c("National Park Zone" = "#7bbc63",
                                          "Habitat Protection Zone" = "#fff8a3",
                                          "Multiple Use Zone" = "#b9e6fb",
                                          "Special Purpose Zone\n(Mining Exclusion)" = "#368ac1"), guide = "none") 

# state colours
wampa_cols <- scale_color_manual(values = c("Sanctuary Zone" = "#bfd054",
                                           "General Use Zone" = "#bddde1",
                                           "Recreation Zone" = "#f4e952"))

#class colours 
hab_cols <- scale_fill_manual(values = c("Rock" = "grey40",
                                         "Sand" = "wheat",
                                         "Macroalgae" = "darkgoldenrod4",
                                         "Hard coral" = "coral2",
                                         "Soft coral/sponges" = "plum"))

#depth colours 
depth_cols <- scale_fill_manual(values = c("#a7cfe0","#9acbec","#98c4f7", "#a3bbff"),guide = "none")
#shallow to deep

#make the plot
gg.scatterpie <- ggplot() + 
  geom_contour_filled(data = bathdf, aes(x = longitude.1, y = latitude.1, z = Depth,
                                         fill = after_stat(level)),
                      breaks = c(0, -30, -70, -200, -700)) +
  scale_fill_grey(start = 1, end = 0.5, guide = "none") +
  geom_contour(data = bathdf, aes(longitude.1, latitude.1, z = Depth),
               breaks = c(0, -30, -70, -200, -700), colour = "white",
               alpha = 1, size = 0.1) +
  
  new_scale_fill()+
  geom_sf(data = wa_mp,fill = "#bfd054", alpha = 2/5, color = "black", size = 0.5)+
  geom_sf(data = aumpa, fill = "#b9e6fb", alpha = 3/5, color = "black", size = 0.5) +
  geom_sf(data = aus, fill = "seashell2", colour = "black", size = 0.1) +
  geom_scatterpie(aes(x = longitude.1, y = latitude.1, group = grouping), data = dat,
                  cols = c("Soft coral/sponges","Macroalgae","Hard coral","Rock",
                           "Sand"),
                  pie_scale = 0.6, color = NA) +
  labs(fill = "Habitat",x = 'Longitude', y = 'Latitude')+
  hab_cols+
  annotate("text", x = c(115.34, 115.25, 115.582), y = c(-20.6, -20.18, -20.145), label = c("30m","70m", "70m"), size = 2.5)+
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.5) +
  coord_sf(xlim = c(115.2517, 116), ylim = c(-20.83627, -20)) + # Updated BG
 
  theme_minimal()+
  theme(panel.background = element_rect(fill = "#e0e0e0"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  annotate(geom = "text", x = c(115.390), y = c(-20.269), label = c("Tryal Rocks"), size = 2.5) +
  theme(legend.position = "bottom", 
        panel.background = element_rect(colour = NA))
# gg.scatterpie

png(filename = "plots/scatterpies.png", 
    height = 6, width = 7, units = "in", res = 300)
gg.scatterpie
dev.off()

