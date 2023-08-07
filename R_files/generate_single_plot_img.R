library(leaflet)
library(leaflet.extras)
library(DT)
library(scales)
library(tidyverse)
library(sf)
library(htmltools)
library(googlesheets4)
library(mapview)
library(sp)

setwd("/Users/marco/GitHub/graslandvielfalt/R_files")
source("./config_plot_map.R")

#municipalities <- st_read("./gadm41_CHE.gpkg", layer = "ADM_ADM_3")

#plots <- read_csv("./2023-joinedPlotSelection_v2.csv") %>%
#  get_municipality(., municipalities, what = c("NAME_1", "NAME_3")) %>%
#  rename(canton = NAME_1,
#         municipality = NAME_3) %>%
#  mutate(priority = gsub("A", "", priority)) %>%
#  arrange(priority, canton, municipality, elevation) %>%
#  group_by(municipality) %>%
#  mutate(ID = paste0(priority, "-",
#                     toupper(substr(canton, 1, 2)), "-", 
#                     toupper(substr(municipality, 1, 2)), "-", 
#                     row_number())) %>%
#  ungroup() %>%
#  select(ID, elevation, canton, municipality, mgroup, LU1980, LU2000, LU2020, LNF_Code, everything()) %>%
#  arrange(priority, elevation)

#write_csv(plots, "./2023-joinedPlotSelection_v3.csv")

donePlots <- read_csv("./2023-donePlots.csv") %>%
  filter(Done == 1)

# Read the data from the Google Sheet
gsheet <- read_sheet("https://docs.google.com/spreadsheets/d/1rIDiZIn6EFSC1ifOlfHDeNpUIWRYYqZDQa5PPJdeaog/edit#gid=123", sheet = "Sheet1") %>%
  mutate_all(as.character) %>%
  filter(done_veg == 1)


plots <- read_csv("./2023-joinedPlotSelection_v3.csv") %>%
  filter(!priority %in% c("MP5", "MP6", "MP7")) %>%
  mutate(link = paste0("http://www.google.ch/maps/place/", Latitude, ",", Longitude)) %>%
  left_join(gsheet %>% select("ID", "date_veg", "coord_inprec", "moss"), by = "ID") %>%
  mutate(sID = sub("^[^-]*-", "", ID))

#gps <- read_tsv("./gps/2023-05-23-1116-bkp2.txt") %>% as.data.frame()
#write_csv(gps, "./gps/2023-05-23-1116-bkp2-to-clean.csv")
gps <- read_csv("./gps/2023-05-23-1116-bkp2-clean_v3.csv") %>%
  mutate(sID = toupper(ifelse(nchar(Punkt_ID_G4B) <= 6,
                                       gsub("^(.{2})(.{2})", "\\1-\\2-", Punkt_ID_G4B),
                             sub("^[^-]*-", "", Punkt_ID_G4B)))) %>%
  select(-ID) %>%
  left_join(plots, by = "sID")

plots <- plots %>%
  left_join(gps %>% select(sID, Korrtyp), by = "sID")

#be <- rgdal::readOGR("/Users/marco/kDocuments_Marco/PhD/server/1_original_data/shapefiles/be_bewirtschaftungseinheit_view.shp")

#poly <- be %>%
#  get_polygons(plots = plots, shapefile = ., radius_m = 500)
#
#writeOGR(poly, dsn = "./2023-plots-with-be-poly.geojson", 
#         layer = ogrListLayers("/Users/marco/kDocuments_Marco/PhD/server/1_original_data/shapefiles/be_bewirtschaftungseinheit_view.shp")[1],
#         driver = "GeoJSON")

poly <- rgdal::readOGR("./2023-plots-with-be-poly.geojson")



###### SAVING AN IMAGE OF EVERY LOCATION ###### 

# Cluster the points based on distance
coords <- plots[, c("Longitude", "Latitude")]

# Maximum distance in meters
max_distance <- 3800

# Helper function to convert coordinates to Cartesian coordinates in meters
coord2cartesian <- function(coords) {
  lat_rad <- coords$Latitude * pi / 180
  x <- coords$Longitude * 6378137 * pi / 180
  y <- log(tan((90 + coords$Latitude) * pi / 360)) * 6378137
  return(data.frame(Longitude = x, Latitude = y))
}

# Convert coordinates to Cartesian coordinates in meters
coords_cartesian <- coord2cartesian(coords)

# Perform DBSCAN clustering
db_clusters <- dbscan(coords_cartesian, eps = max_distance, minPts = 2); table(db_clusters$cluster)

# Create the mini-map
mini_map <- leaflet(options = leafletOptions(minZoom = 0, maxZoom = 13)) %>%
  setView(lng = 8.2, lat = 46.8, zoom = 8)

# Generate and save images for each cluster
coords$cluster <- db_clusters$cluster
unique_clusters <- unique(db_clusters$cluster)

for (i in unique_clusters) { #unique(db_clusters$cluster)
  cluster_coords <- coords %>%
    filter(cluster == i)
  
  # Calculate bounding box
  min_lng <- min(cluster_coords$Longitude)
  max_lng <- max(cluster_coords$Longitude)
  min_lat <- min(cluster_coords$Latitude)
  max_lat <- max(cluster_coords$Latitude)
  
  # Calculate zoom level based on bounding box dimensions
  lng_diff <- max_lng - min_lng
  lat_diff <- max_lat - min_lat
  zoom <- 14 #ifelse(min(18, floor(-log2(max(lng_diff, lat_diff)) + 8)) > 14, 14, min(18, floor(-log2(max(lng_diff, lat_diff)) + 8)))
  
  # Set view to bounding box
  cluster_map <- m %>%
    hideGroup("Done Plots") %>%
    setView(lng = (min_lng + max_lng) / 2, lat = (min_lat + max_lat) / 2, zoom = zoom) %>%
    addMiniMap(mini_map, width = 150, height = 100, position = "bottomleft", zoomLevelFixed = 6,
               tiles = "https://wmts20.geo.admin.ch/1.0.0/ch.swisstopo.pixelkarte-farbe/default/current/3857/{z}/{x}/{y}.jpeg") %>%
    addLabelOnlyMarkers(
      data = plots,
      lng = ~Longitude,
      lat = ~Latitude,
      label = ~as.character(ID),
      labelOptions = labelOptions(noHide = T)
    )
  
  mapshot(cluster_map, file = paste0("./plot_maps/cluster_", i, ".png"), remove_controls = TRUE, delay = 2, vwidth = 1500, vheight = 1000)
  
}


###### CREATING THE MUNICIPALITY MAP ############################################

#municipalities <- read_csv("./2023-joinedPlotSelection_v2.csv") %>%
#  get_municipality(., municipalities, what = c("NAME_1", "NAME_3")) %>%
#  rename(canton = NAME_1,
#         municipality = NAME_3) %>%
#  select(canton, municipality)


