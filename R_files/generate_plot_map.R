library(leaflet)
library(leaflet.extras)
library(DT)
library(scales)
library(tidyverse)
library(sf)
library(htmltools)

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

plots <- read_csv("./2023-joinedPlotSelection_v3.csv") %>%
  filter(!priority %in% c("MP5", "MP6", "MP7")) %>%
  mutate(link = paste0("http://www.google.ch/maps/place/", Latitude, ",", Longitude))

#be <- rgdal::readOGR("/Users/marco/kDocuments_Marco/PhD/server/1_original_data/shapefiles/be_bewirtschaftungseinheit_view.shp")

#poly <- be %>%
#  get_polygons(plots = plots, shapefile = ., radius_m = 500)
#
#writeOGR(poly, dsn = "./2023-plots-with-be-poly.geojson", 
#         layer = ogrListLayers("/Users/marco/kDocuments_Marco/PhD/server/1_original_data/shapefiles/be_bewirtschaftungseinheit_view.shp")[1],
#         driver = "GeoJSON")

poly <- rgdal::readOGR("./2023-plots-with-be-poly.geojson")

########################################################################################################################################
### Create plot table #################################################################################################################
########################################################################################################################################
  
(t <- DT::datatable(plots %>% filter(!ID %in% donePlots$ID) %>%
                      mutate(ID = paste0('<a target="_parent" href=', .$link, '>', .$ID, ' </a>', sep = "")) %>%
                      select(-Latitude, -Longitude, -link, -P.Test.CO2., -Nutzungsid),
                    class = "display nowrap",
                    escape = F,
                    rownames = FALSE))

htmltools::save_html(t, file="2023-plot-table.html")


########################################################################################################################################
### Create a leaflet map with the Swiss Topographic Map as a basemap ###################################################################
########################################################################################################################################

# Define a color palette with distinct colors
pal <- colorFactor(
  palette = c("red", "orange", "yellow", "green", "blue", "purple", "magenta"),
  domain = plots$priority
)

# Create a leaflet map with the Swiss Topographic Map as a basemap
#(m <- leaflet(plots) %>%
#    addTiles(urlTemplate = "https://wmts20.geo.admin.ch/1.0.0/ch.swisstopo.pixelkarte-farbe/default/current/3857/{z}/{x}/{y}.jpeg",
#             attribution = '&copy; <a href="https://www.geo.admin.ch/de/about-swiss-geoportal/impressum.html#copyright">swisstopo</a>') %>% 
#    
#    # Add a button for each category in the priority variable
#    addLayersControl(
#      overlayGroups = c(unique(plots$priority), "Done Plots", "Bewirtschaftungseinheiten"), 
#      options = layersControlOptions(collapsed = TRUE)
#    ) %>%
#    
#    addCircleMarkers(data = plots, 
#                     lat = ~Latitude, lng = ~Longitude,
#                     popup = ~paste(ID, round(elevation, 0), sep = " - "),
#                     radius = 8, stroke = FALSE, fillOpacity = 1, color = ~pal(priority),
#                     group = ~priority) %>%
#    
#    addLegend(pal = pal, values = plots$priority,
#              position = "bottomright", title = "Value") %>%
#    addScaleBar(position = "bottomleft") %>%
#    setView(lng = 9, lat = 46.4, zoom = 8) %>%
#  addPolygons(data = poly, 
#                fill = FALSE, 
#                color = "darkorange", 
#                opacity = 0.9,
#                group = "Bewirtschaftungseinheiten") %>%
#  addAwesomeMarkers(data = donePlots,
#                    lat = ~Latitude, lng = ~Longitude,
#                    icon = ~awesomeIcons(
#                      icon = "leaf",
#                      markerColor = "green",
#                      iconColor = "white",
#                      library = "fa"
#                    ),
#                    labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE),
#                    label = lapply(donePlots$ID, HTML),
#                    clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = FALSE),
#                    group = "Done Plots") %>% 
#  hideGroup("Bewirtschaftungseinheiten") #"MP3", "MP4", "MP5", "MP6", "MP7", "P2", "P3"
#)

# Filter the points with LU2020 column set to true
BFF <- plots %>% filter(LU2020 == TRUE)
non_BFF <- plots %>% filter(LU2020 == FALSE | is.na(LU2020))

(m <- leaflet(plots) %>%
    addTiles(urlTemplate = "https://wmts20.geo.admin.ch/1.0.0/ch.swisstopo.pixelkarte-farbe/default/current/3857/{z}/{x}/{y}.jpeg",
             attribution = '&copy; <a href="https://www.geo.admin.ch/de/about-swiss-geoportal/impressum.html#copyright">swisstopo</a>',
             group = "Swiss Topographic Map") %>%
    addTiles(urlTemplate = "https://wmts20.geo.admin.ch/1.0.0/ch.swisstopo.swissimage/default/current/3857/{z}/{x}/{y}.jpeg",
             attribution = '&copy; <a href="https://www.geo.admin.ch/de/about-swiss-geoportal/impressum.html#copyright">swisstopo</a>',
             group = "Satellite View") %>%
    addCircleMarkers(data = BFF, 
                     lat = ~Latitude, lng = ~Longitude,
                     popup = ~paste(ID, round(elevation, 0), sep = " - "),
                     radius = 8, stroke = FALSE, fillOpacity = 1, color = ~pal(priority),
                     group = "BFF") %>%
    addCircleMarkers(data = non_BFF, 
                     lat = ~Latitude, lng = ~Longitude,
                     popup = ~paste(ID, round(elevation, 0), sep = " - "),
                     radius = 8, stroke = FALSE, fillOpacity = 1, color = ~pal(priority),
                     group = "non_BFF") %>%
    addLegend(pal = pal, values = plots$priority,
              position = "bottomright", title = "Value") %>%
    addScaleBar(position = "bottomleft") %>%
    setView(lng = 9, lat = 46.4, zoom = 8) %>%
    addPolygons(data = poly, 
                fill = FALSE, 
                color = "darkorange", 
                opacity = 0.9,
                group = "Bewirtschaftungseinheiten") %>%
    addAwesomeMarkers(data = donePlots,
                      lat = ~Latitude, lng = ~Longitude,
                      icon = ~awesomeIcons(
                        icon = "leaf",
                        markerColor = "green",
                        iconColor = "white",
                        library = "fa"
                      ),
                      labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE),
                      label = lapply(donePlots$ID, HTML),
                      clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = FALSE),
                      group = "Done Plots") %>% 
    addWMSTiles(
      baseUrl = "https://wms.geo.admin.ch/",
      layers = "ch.bav.haltestellen-oev",
      group = "Public Transport Stops",
      options = WMSTileOptions(format = "image/png", transparent = TRUE)
    ) %>%
    addLayersControl(
      baseGroups = c("Swiss Topographic Map", "Satellite View"),
      overlayGroups = c("BFF", "non_BFF", "Done Plots", "Bewirtschaftungseinheiten", "Public Transport Stops"),
      options = layersControlOptions(collapsed = TRUE)
    ) %>%
    hideGroup(c("Bewirtschaftungseinheiten", "Public Transport Stops"))
    
)

m <- addFullscreenControl(m)

map <- addControlGPS(m, options = gpsOptions(position = "topleft", activate = TRUE, 
                                               autoCenter = TRUE, maxZoom = 10, 
                                               setView = TRUE))
activateGPS(map)


# Add fullscreen button

htmlwidgets::saveWidget(map, file=paste("./2023-plot-map.html", sep = ""))


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


