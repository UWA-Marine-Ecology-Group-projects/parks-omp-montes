###
# Project: Parks Montes synthesis
# Data:    BRUVS, BOSS
# Task:    Overview maps
# author:  Claude Spencer
# date:    Mar 2022
##

rm(list = ls())

library(dplyr)
library(sf)
# library(rgeos)
library(rnaturalearth)
library(ggplot2)
library(metR)
# library(googlesheets4)
library(stringr)
library(patchwork)
library(raster)
library(terra)
library(ggnewscale)

# get data and sort spatial boundaries
aus    <- st_read("data/spatial/shape/cstauscd_r.mif")%>%                       # geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
  dplyr::filter(FEAT_CODE %in% c("mainland","island"))
aumpa  <- st_read("data/spatial/shape/AustraliaNetworkMarineParks.shp")         # all aus mpas
wampa  <- st_read("data/spatial/shape/WA_MPA_2018.shp")                         # all wa mpas
mb_mp <- wampa[wampa$NAME %in% c("Montebello Islands","Barrow Island"),]        # just wa parks nearby
rg_nmp <- aumpa[aumpa$NetName %in% c("South-west", "North-west"), ]             # regional nat parks networks
nb_npz <- rg_nmp[rg_nmp$ZoneName == "National Park Zone", ]
terrnp <- st_read(
  "data/spatial/shape/Legislated_Lands_and_Waters_DBCA_011.shp")                # terrestrial reserves
jacmap <- raster("data/spatial/rasters/ecosystem-types-19class-naland.tif")     # jac's aus habitat map
cropex <- extent(113, 118, -23, -19)
jacmap <- crop(jacmap, cropex)
plot(jacmap)
# jacmap <- projectRaster(jacmap, crs = sppcrs, method = "ngb")
cwatr  <- st_read("data/spatial/shape/amb_coastal_waters_limit.shp")            # coastal waters line
cwatr <- st_crop(cwatr, c(xmin = 113, xmax = 118, ymin = -23, ymax = -20))      # crop down coastal waters line to general project area
# bathy <- raster("data/spatial/rasters/large/ga_bathy_largerextent.tif")                # bathymetry trimmed to project area
bathy <- rast("data/spatial/rasters/large/Australian_Bathymetry_and_Topography_2023_250m_MSL_cog.tif") %>%
  crop(ext(114.8, 116, -22, -19.5)) %>%
  clamp(upper = 0, values = F)
plot(bathy)
bathdf <- as.data.frame(bathy, xy = T)
colnames(bathdf)[3] <- "Depth"

testmp <- aumpa %>%
  dplyr::filter(ResName %in% "Montebello") %>%
  st_transform(4326)
test <- mask(bathy, testmp) %>%
  trim()
plot(test)
summary(test)

st_crs(aus)         <- st_crs(aumpa)

metadata <- read.csv('data/tidy/montebello.synthesis.checked.metadata.csv') %>% # get sampling data
  glimpse()

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
  dplyr::mutate(waname = ifelse(NAME %in% "Barrow Island" &
                              TYPE %in% "Marine Park","Sanctuary Zone", 
                              ifelse(NAME %in% "Barrow Island" & 
                                       TYPE %in% "Marine Management Area", "Marine Management Area", waname))) %>%
  dplyr::filter(!waname%in%"Unassigned")

wa_mp <- mb_mp %>%
  dplyr::filter(waname%in%"Sanctuary Zone")

# # reduce terrestrial parks
terrnp <- terrnp[terrnp$leg_catego %in% c("Nature Reserve", "National Park"), ] # exclude state forests etc
terrnp <- st_crop(terrnp, 
                  xmin = 114.75, ymin = -21.2, xmax = 116.25, ymax = -20)       # just montes

plot(terrnp["leg_catego"])

# assign commonwealth zone colours
nmpa_cols <- scale_fill_manual(values = c(#"National Park Zone" = "#7bbc63",
                                          #"Habitat Protection Zone" = "#fff8a3",
                                          "Multiple Use Zone" = "#b9e6fb"))
                                          #"Special Purpose Zone\n(Mining Exclusion)" = "#368ac1"

# state colours
wampa_cols <- scale_fill_manual(values = c("Sanctuary Zone" = "#bfd054",
                                           "General Use Zone" = "#bddde1",
                                            "Recreation Zone" = "#f4e952",
                                           "Special Purpose Zone" = "#7f66a7",
                                           "Marine Management Area" = "#71a6d1"))

# state terrestrial parks colours
waterr_cols <- scale_fill_manual(values = c(#"National Park" = "#c4cea6",
                                           "Nature Reserve" = "#c4cea6")) #e4d0bb

#nature reserve, section 5(1)(h) Reserve, Conservation park

# build basic plot elements
p1 <- ggplot() +
  # geom_raster(data = bathdf, aes(x, y, fill = Depth), alpha = 0.9) +
  # scale_fill_gradient(low = "black", high = "grey70") +
  geom_contour_filled(data = bathdf, aes(x = x, y = y, z = Depth,
                                         fill = after_stat(level)),
                      breaks = c(0, -30, -70, -200, -700, -2000)) +
  scale_fill_grey(start = 1, end = 0.5, guide = "none") +
  geom_contour(data = bathdf, aes(x, y, z = Depth),
               breaks = c(0, -30, -70, -200, -700, -2000), colour = "white",
               alpha = 1, size = 0.1) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  new_scale_fill() +
  geom_sf(data = mb_mp, aes(fill = waname), alpha = 3/5, colour = NA) +
  wampa_cols +
  labs(fill = "State Marine Parks") +
  new_scale_fill() +
  geom_sf(data = terrnp %>% dplyr::filter(leg_catego%in%c("National Park","Nature Reserve")), 
          aes(fill = leg_catego), alpha = 4/5, colour = NA) +
  labs(fill = "Terrestrial Managed Areas") +
  waterr_cols +
  new_scale_fill() +
  geom_sf(data = aumpa, aes(fill = ZoneName), alpha = 4/5, colour = NA) +
  nmpa_cols +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.7) +
  labs(x = 'Longitude', y = 'Latitude', fill = "Australian Marine Parks") +
  guides(fill = guide_legend(order = 1)) +
  annotate("rect", xmin = min(metadata$longitude), xmax = max(metadata$longitude),
           ymin = min(metadata$latitude), ymax = max(metadata$latitude),
           colour = "goldenrod1", fill = "white", alpha = 0.2, size = 0.4) +
  coord_sf(xlim = c(114.75,116.25), ylim = c(-21.2,-20))+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  annotate(geom = "text", x = c(115.390), y = c(-20.269), label = c("Tryal Rocks"), size = 2)
# Tyral rocks -20.269470915212963/115.3909506041739

# p1

# inset map
p2 <- ggplot(data = aus) +
  geom_sf(fill = "seashell1", colour = "grey90", size = 0.05, alpha = 4/5) +
  geom_sf(data = rg_nmp, alpha = 5/6, colour = "grey85", size = 0.02) +
  # geom_sf(data = ab_mpa, alpha = 4/5, colour = "grey85") +
  coord_sf(xlim = c(108, 125), ylim = c(-37, -13)) +
  annotate("rect", xmin = 114.75, xmax = 116.25, ymin = -21.2, ymax = -20,
           colour = "grey25", fill = "white", alpha = 1/5, size = 0.2) +
  theme_bw() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "grey70"))
# p2

# plot both 
p2 + p1 + plot_layout(widths = c(0.8, 2.2))

ggsave("plots/overview_map.png", dpi = 300, width = 10, height = 5)

# Samples per campaign per zone
unique(metadata$campaignid)

parks <- st_read("data/spatial/shape/western-australia_marine-parks-all.shp")

test <- metadata %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_intersection(parks) %>%
  group_by(campaignid, epbc, zone) %>%
  summarise(n = n()) %>%
  glimpse()

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
sitebathy <- bathdf[bathdf$Depth > -201, ]                               # trim to reduce legend
sitebathy <- sitebathy[sitebathy$x > 114.75 & sitebathy$x < 116.25, ]
sitebathy <- sitebathy[sitebathy$y > -21.2 & sitebathy$y < -19.8, ]
# coord_sf(xlim = c(114.75,116.25), ylim = c(-21.2,-20))+

depth_cols <- scale_fill_manual(values = c("#a3bbff","#98c4f7","#9acbec", "#a7cfe0"),guide = "none")

dep_ann <- data.frame(x = c(115.340000003, 115.219999997, 115.415000005, 115.582000000), 
                      y = c(-20.599999997, -20.179999997, -20.270000003, -20.144999998), label = c("30m","70m", "Tryal Rocks","70m")) # updated BG


#"#a7cfe0","#9acbec","#98c4f7", "#a3bbff"
p3 <- ggplot() +
  # # geom_raster(data = bathdf, aes(x, y, fill = Depth), alpha = 0.9) +
  geom_contour_filled(data = bathdf, aes(x, y, z = Depth,
                                         fill = after_stat(level)),
                      breaks = c(0, -30, -70, -200, -700)) +
  scale_fill_grey(start = 1, end = 0.5, guide = "none") +
  geom_contour(data = bathdf, aes(x, y, z = Depth),
               breaks = c(0, -30, -70, -200, -700), colour = "white",
               alpha = 1, size = 0.1) +
  # geom_contour_filled(data = sitebathy, aes(x = x, y = y, z = Depth, fill = after_stat(level)),
  #                     breaks = c(-30,-70,-200), colour = "white", alpha = 1, size = 0.3) +
  # scale_fill_grey(start = 0.6, end = 0.5, guide = "none") +
  # depth_cols+
  new_scale_fill()+
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  new_scale_fill() +
  geom_sf(data = mb_mp, aes(fill = waname), alpha = 3/5,  size = 0.5) +
  wampa_cols +
  labs(fill = "State Marine Parks") +
  new_scale_fill() +
  geom_sf(data = aumpa, aes(fill = ZoneName), alpha = 3/5, size = 0.5) +
  nmpa_cols +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.8) +
  # annotate("text", x = c(115.24,115.283,115.52,115.21), 
           # y = c(-20.425,-20.55,-20.2453, -20.27), label = c("50m","50m","50m","50m"), size = 2)+
  geom_point(data = metadata, aes(longitude, latitude), colour = "indianred4",
             alpha = 4/5, shape = 10) +
  labs(x = NULL, y = NULL, fill = "Australian Marine Parks") +
  guides(fill = guide_legend(order = 1)) +
  geom_text(data = dep_ann, aes(x , y, label = label), inherit.aes = F, size = 2.8, colour = "black")+
  # coord_sf(xlim = c(min(metadata$longitude),max(metadata$longitude)), 
  #          ylim = c(min(metadata$latitude), max(metadata$latitude)))+
  coord_sf(xlim = c(115.24, 116), ylim = c(-20.83627, -20)) + # Updated BG
  theme_minimal()+
  theme(panel.background = element_rect(fill = "#CCCCCC", colour = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
png(filename = "plots/site_overview_map1.png", res = 200, units = "in",
    width = 10, height = 6)
p3
dev.off()

# ggsave("plots/site_overview_map1.png", dpi = 200, width = 10, height = 6)

# jac's map, eh
# sort out the classes
jlevs  <- ratify(jacmap)
jclass <- levels(jlevs)[[1]]
jclass[["class"]] <- c("shelf.unvegetated.soft.sediments",
                       "Upper.slope.unvegetated.soft.sediments",
                       "Mid.slope.sediments",
                       "Lower.slope.reef.and.sediments",
                       "Shelf.incising.and.other.canyons",
                       "Shelf.vegetated.sediments",
                       "Shallow.coral.reefs.less.than.30.m.depth",
                       "Mesophotic.coral.reefs",
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

# state colours
wampa_outs <- scale_colour_manual(values = c("Sanctuary Zone" = "#bfd054",
                                           "General Use Zone" = "#bddde1",
                                           "Recreation Zone" = "#f4e952",
                                           "Special Purpose Zone" = "#7f66a7"))

jmap_df$classname <- dplyr::recode(jmap_df$classname, "shelf unvegetated soft sediments" =
                              "Shelf unvegetated soft sediments")

jcls_fills <- scale_fill_manual(values = c(
  "Shallow coral reefs less than 30 m depth" = "coral2", #
  # "Shallow rocky reefs less than 30 m depth" = "darkgoldenrod1", 
  # "Mesophotic rocky reefs" = "khaki4",
  "Shelf vegetated sediments" = "seagreen3",
  "Shelf unvegetated soft sediments" = "cornsilk1",
  "Rariophotic shelf reefs" = "steelblue3",
  "Upper slope rocky reefs shelf break to 700 m depth" = "indianred3",
  "Upper slope unvegetated soft sediments" = "wheat1", 
  "Mid slope sediments" = "navajowhite1",
  # "Shelf incising and other canyons" = "peru",
  "Mesophotic coral reefs" = "orange",
  # "Mid slope reef" = "sienna",
  "Artificial reefs pipelines and cables" = "saddlebrown"))

jcls_cols <- scale_color_manual(values = c(
  "Shallow coral reefs less than 30 m depth" = "coral2", #"coral2",
  # "Shallow rocky reefs less than 30 m depth" = "darkgoldenrod1", 
  # "Mesophotic rocky reefs" = "khaki4",
  "Shelf vegetated sediments" = "seagreen3",
  "Shelf unvegetated soft sediments" = "cornsilk1",
  "Rariophotic shelf reefs" = "steelblue3",
  "Upper slope rocky reefs shelf break to 700 m depth" = "indianred3",
  "Upper slope unvegetated soft sediments" = "wheat1", 
  "Mid slope sediments" = "navajowhite1",
  # "Shelf incising and other canyons" = "peru",
  "Mesophotic coral reefs" = "orange",
  # "Mid slope reef" = "sienna",
  "Artificial reefs pipelines and cables" = "saddlebrown"))

p6 <- ggplot() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA) +
  waterr_cols +
  new_scale_fill() + 
  geom_tile(data = jmap_df, aes(x, y, fill = classname, color = classname), ) +
  jcls_cols+
  jcls_fills+
  labs(x = NULL, y = NULL, fill = "Habitat classification", color = "Habitat classification") +
  new_scale_color() +
  new_scale_fill() +
  geom_contour(data = bathdf, aes(x, y, z = Depth),
               breaks = c(-30, -70, -200, -700, -2000), colour = "black",
               alpha = 1, size = 0.1) +
  geom_sf(data = mb_mp, aes(color = waname), alpha = 3/5, fill = NA, show.legend = F) +
  wampa_outs +
  new_scale_color() +
  geom_sf(data = aumpa, colour = "#b9e6fb", alpha = 3/5, fill = NA, size = 1) +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 1) +
  theme_minimal() +
  coord_sf(xlim = c(114.75,116.25), ylim = c(-21.2,-20))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
png(filename = "plots/overall_jmonk_natmap.png",
       width = 10, height = 6, res = 160, units = "in")
p6
dev.off()


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

