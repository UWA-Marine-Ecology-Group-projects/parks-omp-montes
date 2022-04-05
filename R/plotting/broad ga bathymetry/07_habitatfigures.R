###
# Project: Parks - Montes Post-Survey
# Data:    BRUVS, BOSS Habitat data
# Task:    Habitat figures
# author:  Kingsley Griffin
# date:    Jan 22
##

rm(list = ls())

library(reshape2)
library(ggplot2)
library(viridis)
library(raster)
library(patchwork)
library(sf)
library(ggnewscale)

# spatial setup and bring in layers
wgscrs <- CRS("+proj=longlat +datum=WGS84")
sppcrs <- CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")       # crs for sp objects

#National marine parks
aumpa  <- st_read("data/spatial/shape/AustraliaNetworkMarineParks.shp")           # all aus mpas
nw_mpa <- aumpa[aumpa$ResName %in% c("Montebello"), ]                             # just Abrolhos Aus MP
nw_mpa <- st_transform(nw_mpa, sppcrs)

aus    <- st_read("data/spatial/shape/cstauscd_r.mif")                            # geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
montes <- aus[aus$GROUP_NAME %in% c("MONTEBELLO ISLANDS"), ]                      # just montes
st_crs(montes) <- wgscrs
montes <- st_transform(montes, sppcrs)
# aus    <- aus[aus$FEAT_CODE == "mainland", ]

aumpa  <- st_read("data/spatial/shape/AustraliaNetworkMarineParks.shp")           # all aus mpas
mb_mpa <- aumpa[aumpa$ResName %in% c("Montebello"), ]                             # just Montes Aus MP
wampa  <- st_read("data/spatial/shape/WA_MPA_2018.shp")
mb_mpa <- wampa[wampa$NAME %in% "Montebello Islands" ,]

mb_mpa <- st_transform(mb_mpa, sppcrs)

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

# read in outputs from 'R/4_habitat_model.R'
rlist  <- list.files("output/spatial_predictions_broad/", "*.tif", full.names = T)
prasts <- stack(rlist)
spreddf <- as.data.frame(prasts, xy = TRUE, na.rm = TRUE)

# preddf <- readRDS("output/broad_habitat_predictions.rds")
# spreddf <- readRDS("output/site_habitat_predictions.rds")                       # site predictions only
spreddf$dom_tag <- as.factor(spreddf$layer_dom_tag)
spreddf$dom_tag <- dplyr::recode(spreddf$dom_tag,
                          "1" = "Invertebrate reef",
                          "2" = "Macroalgae/coral reef",
                          "3" = "Rock",
                          "4" = "Sand")

# fig 1: categorical habitat maps
# assign mpa colours
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

hab_cols <- scale_fill_manual(values = c(#"Macroalgae" = "darkgoldenrod4",
                                         #"Coral" = "coral2",
                                         "Rock" = "grey40",
                                         "Sand" = "wheat",
                                         "Invertebrate reef" = "plum",
                                         "Macroalgae/coral reef" = "cadetblue2"
))

p1 <- ggplot() +
  geom_tile(data = spreddf%>%dplyr::filter(dom_tag%in%c("Rock","Sand","Invertebrate reef","Macroalgae/coral reef")), 
            aes(x, y, fill = dom_tag)) +
  hab_cols +
  geom_sf(data = montes, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = nw_mpa, fill = NA, colour = "#b9e6fb", size = 0.5) +
  geom_sf(data = mb_mpa%>%dplyr::filter(waname%in%"Sanctuary Zone"),
          fill = NA, aes(color = waname), size = 0.5, show.legend = F) +
  wampa_cols+
  labs(x = NULL, y = NULL, fill = NULL) +
  guides(colour = "none") +
  coord_sf(xlim = c(315000, 360000), ylim = c(7720000, 7770000)) +
  theme_minimal()
p1

ggsave("plots/site_dominant_habitat.pdf", width = 9, height = 6, dpi = 1000)

# fig 2: habitat multiplot
# melt classes for faceting
widehabit <- melt(spreddf, measure.vars = c(6:9))
widehabit$variable <- dplyr::recode(widehabit$variable,                         #wide habitat but its long :P
                                    layer_pphotic = "Macroalgae/coral reef",
                                    layer_pmeso = "Invertebrate reef",
                                    layer_prock = "Rock",
                                    layer_psand = "Sand")

# # coord_sf(xlim = c(115.2, 116), ylim = c(-21, -20)) +
# smb_mpa <- st_crop(mb_mpa, extent(c(-21, 116, -20, 115.2)))
# plot(mb_mpa["ZoneName"])
# plot(smb_mpa["ZoneName"])

p2 <- ggplot() +
  geom_tile(data = widehabit, aes(x, y, fill = value)) +
  scale_fill_viridis(direction = -1, limits = c(0, max(widehabit$value))) +
  geom_sf(data = nw_mpa, fill = NA, colour = "#b9e6fb", size = 0.5) +
  geom_sf(data = mb_mpa%>%dplyr::filter(waname%in%"Sanctuary Zone"),
          fill = NA, aes(color = waname), size = 0.5, show.legend = F) +
  geom_sf(data = montes, fill = "seashell2", colour = "grey80", size = 0.1) +
  labs(x = NULL, y = NULL, fill = "Occurrence (p)") +
  coord_sf(xlim = c(315000, 360000), ylim = c(7720000, 7770000)) +
  theme_minimal() +
  wampa_cols+
  facet_wrap(~variable)

p2

ggsave("plots/site_habitat_predicted.png", width = 9, height = 8, dpi = 160)

# # fig 3: biogenic reef
# p3 <- ggplot(spreddf[widehabit$sitens == 1, ], aes(x, y)) +
#   geom_tile(aes(fill = pbiogenic)) +
#   scale_fill_viridis(direction = -1, limits = c(0, max(spreddf$pbiogenic))) +
#   labs(x = NULL, y = NULL) +
#   coord_equal() +
#   guides(fill = "none") +
#   theme_minimal()
# 
# p32 <- ggplot(spreddf[widehabit$sitens == 0, ], aes(x, y)) +
#   geom_tile(aes(fill = pbiogenic)) +
#   scale_fill_viridis(direction = -1, limits = c(0, max(spreddf$pbiogenic))) +
#   labs(x = NULL, y = NULL, fill = "Biogenic\nReef (p)") +
#   coord_equal() +
#   theme_minimal()
# 
# p3 + p32 + plot_layout(widths = c(0.46, 0.54))
# ggsave("plots/site_biogenicreef_p.png", width = 10, height = 6, dpi = 160)

# fig 4: predicted relief
pcelldf <- readRDS('output/spatial_predictions_broad/predicted_relief_site_ga.rds')
# pcelldf$sitens <- ifelse(pcelldf$y > 6940000, 1, 0)
pcelldf$prelief[pcelldf$prelief < 0] <- 0

p4 <- ggplot() +
  geom_tile(data = pcelldf, aes(x, y, fill = prelief)) +
  labs(fill = "Relief score", color = "Relief score")+
  scale_fill_viridis(option = "C", direction = -1, 
                     limits = c(0, max(pcelldf$prelief))) +
  geom_sf(data = nw_mpa, fill = NA, colour = "#b9e6fb", size = 0.5) +
  geom_sf(data = mb_mpa%>%dplyr::filter(waname%in%"Sanctuary Zone"),
          fill = NA, aes(color = waname), size = 0.5, show.legend = F) +
  wampa_cols+
  geom_sf(data = montes, fill = "seashell2", colour = "grey80", size = 0.1) +
  labs(x= NULL, y = NULL) +
  theme_minimal()+
  coord_sf(xlim = c(315000, 360000), ylim = c(7720000, 7770000))
p4

ggsave("plots/site_relief_p.png", width = 10, height = 6, dpi = 160)

# fig 4.1.2: spatial random effect

p5 <- ggplot() +
  geom_tile(data = pcelldf, aes(x, y, fill = p_sp)) +
  scale_fill_viridis(option = "B", 
                     limits = c(min(pcelldf$p_sp), max(pcelldf$p_sp))) +
  geom_sf(data = nw_mpa, fill = NA, colour = "#b9e6fb", size = 0.5) +
  geom_sf(data = mb_mpa%>%dplyr::filter(waname%in%"Sanctuary Zone"),
          fill = NA, aes(color = waname), size = 0.5, show.legend = F) +
  wampa_cols+
  geom_sf(data = montes, fill = "seashell2", colour = "grey80", size = 0.1) +
  # geom_point(data = habi, aes(x, y), 
  #            alpha = 0.7, colour = "grey70", size = 1, shape = 3) +
  labs(x= NULL, y = NULL, fill = "Spatial dependence") +
  theme_minimal()+
  coord_sf(xlim = c(315000, 360000), ylim = c(7720000, 7770000))
p5

ggsave("plots/site_relief_spatialeffect.png", 
       width = 10, height = 6, dpi = 160)

# jac's map, eh
# sort out the classes
jlevs <- ratify(jacmap)
jclass <- levels(jlevs)[[1]]
jclass[["class"]] <- c("shelf.unvegetated.soft.sediments",
                       "Upper.slope.unvegetated.soft.sediments", 
                       "Mid.slope.sediments",
                       "Lower.slope.reef.and.sediments",
                       "Abyssal.reef.and.sediments", 
                       "Seamount.soft.sediments", 
                       "Shelf.vegetated.sediments", 
                       "Shallow.coral.reefs.less.than.30.m.depth", 
                       "Mesophotic.coral.reefs", 
                       "Rariophotic.shelf.reefs", 
                       "Upper.slope.rocky.reefs.shelf.break.to.700.m.depth", 
                       "Mid.slope.reef", 
                       "Artificial.reefs.pipelines.and.cables") # the class names
levels(jacmap) <- jclass
jmap_df <- as.data.frame(jacmap, xy = TRUE)
colnames(jmap_df)[3] <- "class"

# set up dfs
jmap_nth <- jmap_df[(jmap_df$y > 6985000 & jmap_df$y < 7000000) & 
                      (jmap_df$x > 100000 & jmap_df$x < 140000), ]

jmap_sth <- jmap_df[(jmap_df$y > 6880000 & jmap_df$y < 6900000) & 
                      (jmap_df$x > 125000 & jmap_df$x < 170000), ]

# plot

jcls_cols <- scale_fill_manual(values = c("Upper.slope.unvegetated.soft.sediments" = "wheat4", 
                                          "shelf.unvegetated.soft.sediments" = "wheat2",
                                          "Shallow.coral.reefs.less.than.30.m.depth" = "coral2", 
                                          "Mesophotic.coral.reefs" = "darkorange3",
                                          "Rariophotic.shelf.reefs" = "steelblue2"))

p6 <- ggplot() + 
  geom_tile(data = jmap_nth, aes(x, y, fill = class)) +
  jcls_cols +
  geom_sf(data = ab_npz[ab_npz$parkid == 3, ], fill = NA, colour = "#7bbc63") +
  labs(x= NULL, y = NULL, fill = NULL) +
  guides(fill = "none") +
  theme_minimal()

p62 <- ggplot() + 
  geom_tile(data = jmap_sth, aes(x, y, fill = class)) +
  jcls_cols +
  geom_sf(data = ab_npz[ab_npz$parkid == 2, ], fill = NA, colour = "#7bbc63") +
  labs(x= NULL, y = NULL, fill = NULL) +
  theme_minimal()

p6 + p62 + plot_layout(widths = c(0.5, 0.44))
ggsave("plots/site_jmonk_natmap.png", 
       width = 10, height = 6, dpi = 160)
