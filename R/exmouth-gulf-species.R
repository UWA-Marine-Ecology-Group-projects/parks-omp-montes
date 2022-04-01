metadata <- read.csv("data/2014 OpCode track.csv")%>%
  dplyr::filter(CampaignID%in%"2014-05_Exmouth.to.Dampier.shallow_stereoBRUVs")%>%
  glimpse()

locs <- metadata %>%
  dplyr::select(OpCode, Longitude, Latitude)%>%
  glimpse()

points <- read.csv("data/2014-05_Exmouth.to.Dampier.shallow_stereoBRUVs_Points.csv")%>%
  dplyr::mutate(scientific=paste(Genus, Species, sep = " "))%>%
  glimpse()

aus    <- st_read("data/spatial/shape/cstauscd_r.mif")%>%                       # geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
  dplyr::filter(FEAT_CODE %in% c("mainland","island"))


#species of interest
#lethrinus ravus
species <- points %>%
  dplyr::filter(scientific %in% c("Lethrinus ravus", "Macropharyngodon negrosensis"))%>%
  left_join(locs)%>%
  glimpse()

p1 <- ggplot()+
  geom_point(data = species, aes(x = Longitude, y = Latitude, color = scientific))+
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1)+
  coord_sf(xlim = c(114.0,114.8),ylim = c(-22.6,-21.4))+
  theme_minimal()
p1

#macropharyngodon negrosensis
neg <- points %>%
  dplyr::filter(scientific %in% "Macropharyngodon negrosensis")%>%
  left_join(locs)%>%
  glimpse()

