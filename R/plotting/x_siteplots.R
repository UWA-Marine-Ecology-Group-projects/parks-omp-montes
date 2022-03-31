
###
# Project: MAC HUB South-west Corner
# Data:    BRUVS, BOSS
# Task:    Overview maps
# author:  Kingsley Griffin
# date:    Mar 2022
##

rm(list = ls())

library(dplyr)
library(sf)
library(rgeos)
library(rnaturalearth)
library(ggplot2)
library(metR)
# library(googlesheets4)
library(stringr)
library(patchwork)
library(raster)
library(ggnewscale)

working.dir <- getwd()
setwd(working.dir)

# get data and sort spatial boundaries
aus    <- st_read("data/spatial/shape/cstauscd_r.mif")%>%                     # geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
  dplyr::filter(FEAT_CODE %in% c("mainland","island"))
# dirkh  <- aus[aus$ISLAND_NAME == "DIRK HARTOG ISLAND", ]                      # just dirk hartog island
# aus    <- aus[aus$FEAT_CODE == "mainland", ]
aumpa  <- st_read("data/spatial/shape/AustraliaNetworkMarineParks.shp")    # all aus mpas
wampa  <- st_read("data/spatial/shape/WA_MPA_2018.shp")                    # all wa mpas
# nb_mp  <- wampa[wampa$NAME %in% c("Ngari Capes"), ]                             # just wa parks nearby
# rg_nmp <- aumpa[aumpa$NetName %in% c("South-west", "North-west"), ]             # regional nat parks networks
# nb_nmp <- rg_nmp[rg_nmp$ResName %in% c("South-west Corner", "Geographe"), ]     # just nat parks nearby
# nb_npz <- nb_nmp[nb_nmp$ZoneName == "National Park Zone", ]
# wanew  <- st_read("data/spatial/shapefiles/test1.shp")                          # zones in ngari capes
terrnp <- st_read(
  "data/spatial/shape/Legislated_Lands_and_Waters_DBCA_011.shp")           # terrestrial reserves
# jacmap <- raster("data/spatial/rasters/ecosystem-types-19class-naland.tif")     # jac's aus habitat map
# cropex <- extent(112, 116, -35, -32)
# jacmap <- crop(jacmap, cropex)
# jacmap <- projectRaster(jacmap, crs = sppcrs, method = "ngb")
cwatr  <- st_read("data/spatial/shape/amb_coastal_waters_limit.shp")       # coastal waters line
cwatr <- st_crop(cwatr, c(xmin = 114.8, xmax = 116, ymin = -21.0, ymax = -19.5))      # crop down coastal waters line to general project area
# bathdf <- readRDS("output/spatial_covariates/ga_bathy_fine.rds")            # bathymetry trimmed to project area
# colnames(bathdf)[3] <- "Depth"
bath_r <- raster("data/spatial/raster/WA_500m_bathy.tif")
plot(bath_r)
st_crs(aus)         <- st_crs(aumpa)

metadata <- read.csv('data/tidy/montebello.synthesis.checked.metadata.csv') %>%                     # get sampling data
  glimpse()

# simplify zone names
nb_nmp$ZoneName <- dplyr::recode(nb_nmp$ZoneName,
                                 "Special Purpose Zone (Mining Exclusion)" =
                                   "Special Purpose Zone\n(Mining Exclusion)")

nb_mp$waname <- gsub("( \\().+(\\))", "", nb_mp$ZONE_TYPE)
nb_mp$waname <- gsub(" [1-4]", "", nb_mp$waname)
# ab_mpa$waname[ab_mpa$ZONE_TYPE == unique(ab_mpa$ZONE_TYPE)[14]] <- 
#   c("Special Purpose Zone\n(Habitat Protection)")
nb_mp$waname[nb_mp$NAME == "Ngari Capes"]     <- "General Use"
nb_mp$waname <- dplyr::recode(nb_mp$waname, 
                               "General Use" = "General Use Zone",
                               # "MMA" = "Marine Management Area",
                               # "Recreation Area" = "Recreation Zone",
                               # "Conservation Area" = "Sanctuary Zone",
                               "Special Purpose Zone (Shore Based Activities)" = 
                                "Special Purpose Zone\n(Shore Based Activities)")


# reduce terrestrial parks
terrnp <- terrnp[terrnp$leg_catego %in% c("Nature Reserve", "National Park"), ] # exclude state forests etc
terrnp <- st_crop(terrnp, xmin = 113, ymin = -36, xmax = 116, ymax = -33)       # just swc
# plot(terrnp["leg_catego"])

# assign commonwealth zone colours
nmpa_cols <- scale_fill_manual(values = c("National Park Zone" = "#7bbc63",
                                          "Habitat Protection Zone" = "#fff8a3",
                                          "Multiple Use Zone" = "#b9e6fb",
                                          "Special Purpose Zone\n(Mining Exclusion)" = "#368ac1"))

# state colours
wampa_cols <- scale_fill_manual(values = c("Sanctuary Zone" = "#bfd054",
                                           "General Use Zone" = "#bddde1",
                                           "Recreation Zone" = "#f4e952"))

# state terrestrial parks colours
waterr_cols <- scale_fill_manual(values = c("National Park" = "#c4cea6",
                                           "Nature Reserve" = "#e4d0bb"))

# build basic plot elements
p1 <- ggplot() +
  # geom_raster(data = bathdf, aes(x, y, fill = Depth), alpha = 0.9) +
  # scale_fill_gradient(low = "black", high = "grey70") +
  geom_contour_filled(data = bathdf, aes(x = x, y = y, z = Depth,
                                         fill = after_stat(level)),
                      breaks = c(0, -40, -70, -120, -7000)) +
  # geom_contour(data = bathdf, aes(x = x, y = y, z = Depth),
  # binwidth = 250, colour = "white", alpha = 3/5, size = 0.1) +
  scale_fill_grey(start = 1, end = 0.5, guide = "none") +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  new_scale_fill() +
  # geom_sf(data = nb_mp, aes(fill = waname), alpha = 2/5, colour = NA) +
  # geom_sf(data = wanew, aes(fill = waname), alpha = 2/5, colour = NA) +
  # wampa_cols +
  # labs(fill = "State Marine Parks") +
  # new_scale_fill() +
  # geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA) +
  # labs(fill = "Terrestrial Managed Areas") +
  # waterr_cols +
  # new_scale_fill() +
  # geom_sf(data = nb_nmp, aes(fill = ZoneName), alpha = 4/5, colour = NA) +
  # nmpa_cols + 
  # geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.2) +
  # geom_contour(data = bathdf, aes(x, y, z = Depth),
  #              breaks = c(0, -40, -70, -120), colour = "white", 
  #              alpha = 1, size = 0.1) +
  # labs(x = NULL, y = NULL, fill = "Australian Marine Parks") +
  # guides(fill = guide_legend(order = 1)) +
  annotate("rect", xmin = min(metadata$longitude), xmax = max(metadata$longitude), 
           ymin = min(metadata$latitude), ymax = max(metadata$latitude),
           colour = "grey15", fill = "white", alpha = 0.2, size = 0.1) +
  coord_sf(xlim = c(114.75,116.25), ylim = c(-21.2,-20))+
  # coord_sf(xlim = c(min(bathdf$x), max(bathdf$x)), ylim = c(min(bathdf$y), max(bathdf$y))) +
  theme_minimal()
p1

# inset map
p2 <- ggplot(data = aus) +
  geom_sf(fill = "seashell1", colour = "grey90", size = 0.05, alpha = 4/5) +
  geom_sf(data = rg_nmp, alpha = 5/6, colour = "grey85", size = 0.02) +
  # geom_sf(data = ab_mpa, alpha = 4/5, colour = "grey85") +
  coord_sf(xlim = c(108, 125), ylim = c(-37, -13)) +
  annotate("rect", xmin = 114.3, xmax = 115.8, ymin = -34.5, ymax = -33.2,
           colour = "grey25", fill = "white", alpha = 1/5, size = 0.2) +
  theme_bw() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "grey70"))
# p2

# plot both 
p2 + p1 + plot_layout(widths = c(0.8, 2.2))

ggsave("plots/overview_map.png", dpi = 200, width = 10, height = 6)


# site zoom plots
# reduce zone levels for these plots
# assign commonwealth zone colours
s_nmpa_cols <- scale_fill_manual(values = c("National Park Zone" = "#7bbc63",
                                            "Special Purpose Zone\n(Mining Exclusion)" = "#368ac1"))

# state colours
s_wampa_cols <- scale_fill_manual(values = c("Sanctuary Zone" = "#bfd054",
                                             "General Use Zone" = "#bddde1"))


# make closer plot
# trim down bathy for nicer contour labels
sitebathy <- bathdf[bathdf$Depth > -500, ]                               # trim to reduce legend
sitebathy <- sitebathy[sitebathy$x > 114.4 & sitebathy$x < 115, ]
sitebathy <- sitebathy[sitebathy$y > -34.1 & sitebathy$y < -33.7, ]

p3 <- ggplot() +
  # # geom_raster(data = bathdf, aes(x, y, fill = Depth), alpha = 0.9) +
  # geom_contour_filled(data = bathdf, aes(x = x, y = y, z = Depth), 
  #              binwidth = 50, colour = "white", alpha = 4/5, size = 0.1) +
  # scale_fill_gradient(low = "black", high = "grey70", guide = "none") +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  new_scale_fill() +
  geom_sf(data = nb_mp, aes(fill = waname), alpha = 3/5, colour = NA) +
  geom_sf(data = wanew, aes(fill = waname), alpha = 3/5, colour = NA) +
  s_wampa_cols +
  labs(fill = "State Marine Parks") +
  new_scale_fill() +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 3/5, colour = NA) +
  waterr_cols +
  labs(fill = "Terrestrial Managed Areas") +
  new_scale_fill() +
  geom_sf(data = nb_nmp, aes(fill = ZoneName), alpha = 3/5, colour = NA) +
  s_nmpa_cols + 
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.2) +
  geom_contour(data = bathdf, aes(x = x, y = y, z = Depth), 
               binwidth = 50, colour = "white", alpha = 4/5, size = 0.1) +
  geom_text_contour(data = sitebathy, aes(x = x, y = y, z = Depth), 
                    binwidth = 50, size = 2.5, label.placer = label_placer_n(1)) +
  geom_point(data = habitat, aes(longitude, latitude, colour = method), 
             alpha = 3/5, shape = 10) +
  scale_colour_manual(values = c("BRUV" = "indianred4",
                                 "Drop Camera" = "seagreen4")) +
  labs(x = NULL, y = NULL, fill = "Australian Marine Parks", colour = "Sample") +
  guides(fill = guide_legend(order = 1)) +
  annotate("rect", xmin = 114.7, xmax = 114.95, ymin = -34.14, ymax = -34.01,
           colour = "grey15", fill = "white", alpha = 0.1, size = 0.1) +
  coord_sf(xlim = c(114.4, 115.1), ylim = c(-34.15, -33.65)) +
  theme_minimal()
p3

ggsave("plots/site_overview_map.png", dpi = 200, width = 10, height = 6)

# jac's map, eh
# sort out the classes
jlevs  <- ratify(jacmap)
jclass <- levels(jlevs)[[1]]
jclass[["class"]] <- c("shelf.unvegetated.soft.sediments",
                       "Upper.slope.unvegetated.soft.sediments",
                       "Mid.slope.sediments",
                       "Lower.slope.reef.and.sediments",
                       "Abyssal.reef.and.sediments",
                       "Seamount.soft.sediments",
                       "Shelf.incising.and.other.canyons",
                       "Shelf.vegetated.sediments",
                       "Shallow.coral.reefs.less.than.30.m.depth",
                       "Shallow.rocky.reefs.less.than.30.m.depth",
                       "Mesophotic.coral.reefs",
                       "Mesophotic.rocky.reefs",
                       "Rariophotic.shelf.reefs",
                       "Upper.slope.rocky.reefs.shelf.break.to.700.m.depth",
                       "Mid.slope.reef",
                       "Artificial.reefs.pipelines.and.cables")                 # the class names rather than number
levels(jacmap) <- jclass
jmap_df <- as.data.frame(jacmap, xy = TRUE, na.rm = TRUE)
colnames(jmap_df)[3] <- "classname"
jmap_df$classname <- gsub("\\.", " ", jmap_df$classname)                        # replace . with space in names


# plot
waterr_cols <- scale_fill_manual(values = c("National Park" = "#c4cea6",
                                            "Nature Reserve" = "#e4d0bb"),
                                 guide = "none")

jmap_df$classname <- dplyr::recode(jmap_df$classname, "shelf unvegetated soft sediments" =
                              "Shelf unvegetated soft sediments")

jcls_cols <- scale_fill_manual(values = c(
  # "Shallow coral reefs less than 30 m depth" = "coral2", 
  "Shallow rocky reefs less than 30 m depth" = "darkgoldenrod1", 
  "Mesophotic rocky reefs" = "khaki4",
  "Shelf vegetated sediments" = "seagreen3",
  "Shelf unvegetated soft sediments" = "cornsilk1",
  "Rariophotic shelf reefs" = "steelblue3",
  "Upper slope rocky reefs shelf break to 700 m depth" = "indianred3",
  "Upper slope unvegetated soft sediments" = "wheat1", 
  "Mid slope sediments" = "navajowhite1"))

nb_wasz <- wanew[wanew$waname == "Sanctuary Zone", ]

p6 <- ggplot() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA) +
  waterr_cols +
  new_scale_fill() + 
  geom_tile(data = jmap_df, aes(x, y, fill = classname)) +
  jcls_cols +
  geom_sf(data = nb_npz, colour = "#7bbc63", alpha = 3/5, fill = NA) +
  geom_sf(data = nb_wasz, colour = "#7bbc63", alpha = 3/5, fill = NA) +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.2) +
  labs(x = NULL, y = NULL, fill = "Habitat classification") +
  theme_minimal() +
  coord_sf(xlim = c(114.4, 115.1), ylim = c(-34.15, -33.65))
p6

ggsave("plots/overall_jmonk_natmap.png",
       width = 10, height = 6, dpi = 160)





# # saving for later
# # assign commonwealth zone colours
# nmpa_cols <- scale_fill_manual(values = c("National Park Zone" = "#7bbc63",
#                                           "Habitat Protection Zone" = "#fff8a3",# Commonwealth MPA colours
#                                           # "Habitat Protection Zone (Reefs)" = "#fbff85",
#                                           "Multiple Use Zone" = "#b9e6fb",
#                                           # "Recreational Use Zone" = "#ffb36b",
#                                           # "Sanctuary Zone" = "#f7c0d8",
#                                           # "Special Purpose Zone" = "#6daff4",
#                                           # "Special Purpose Zone (Trawl)" = "#3e8ec4",
#                                           "Special Purpose Zone (Mining Exclusion)" = "#368ac1"
# ))
# 
# # state colours
# wampa_cols <- scale_fill_manual(values = c("Sanctuary Zone" = "#bfd054",
#                                            # "Marine Nature Reserve" = "#bfd054",
#                                            # "Conservation Area" = "#b3a63d",
#                                            # "Habitat Protection Zone" = "#fffbcc",# State MPA colours
#                                            # "Fish Habitat Protection Area" = "#fbff85",
#                                            # "National Park Zone" = "#a4d194",
#                                            "General Use Zone" = "#bddde1",
#                                            "Recreation Zone" = "#f4e952"
#                                            # "Special Purpose Zone" = "#c5bcc9",
#                                            # "Special Purpose Zone\n(Shore Based Activities)" = "#ba3030"
#                                            # "Special Purpose Zone\n(Habitat Protection)" = "#f0ac41",
#                                            # "Reef Observation Area" = "#ddccff",
#                                            # "Marine Management Area" = "#b7cfe1"
# ))

