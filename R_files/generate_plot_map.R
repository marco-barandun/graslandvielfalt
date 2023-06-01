library(leaflet)
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
plots <- read_csv("./2023-joinedPlotSelection_v3.csv")

donePlots <- read_csv("./2023-donePlots.csv") %>%
  filter(!is.na(Done))

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
  
(t <- DT::datatable(plots,
                    class = "display nowrap",
                    escape = F,
                    rownames = FALSE))

#htmltools::save_html(t, file="2023-plot-table.html")


########################################################################################################################################
### Create a leaflet map with the Swiss Topographic Map as a basemap ###################################################################
########################################################################################################################################

# Define a color palette with distinct colors
pal <- colorFactor(
  palette = c("red", "orange", "yellow", "green", "blue", "purple", "magenta"),
  domain = plots$priority
)

# Create a leaflet map with the Swiss Topographic Map as a basemap
(m <- leaflet(plots) %>%
    addTiles(urlTemplate = "https://wmts20.geo.admin.ch/1.0.0/ch.swisstopo.pixelkarte-farbe/default/current/3857/{z}/{x}/{y}.jpeg",
             attribution = '&copy; <a href="https://www.geo.admin.ch/de/about-swiss-geoportal/impressum.html#copyright">swisstopo</a>') %>% 
    
    # Add a button for each category in the priority variable
    addLayersControl(
      overlayGroups = c(unique(plots$priority), "Done Plots", "Bewirtschaftungseinheiten"), 
      options = layersControlOptions(collapsed = TRUE)
    ) %>%
    
    addCircleMarkers(data = plots, 
                     lat = ~Latitude, lng = ~Longitude,
                     popup = ~paste(ID, round(elevation, 0), sep = " - "),
                     radius = 8, stroke = FALSE, fillOpacity = 1, color = ~pal(priority),
                     group = ~priority) %>%
    
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
  hideGroup("Bewirtschaftungseinheiten") #"MP3", "MP4", "MP5", "MP6", "MP7", "P2", "P3"
)



(m <- leaflet(plots) %>%
    addTiles(urlTemplate = "https://wmts20.geo.admin.ch/1.0.0/ch.swisstopo.pixelkarte-farbe/default/current/3857/{z}/{x}/{y}.jpeg",
             attribution = '&copy; <a href="https://www.geo.admin.ch/de/about-swiss-geoportal/impressum.html#copyright">swisstopo</a>',
             group = "Swiss Topographic Map") %>%
    addTiles(urlTemplate = "https://wmts20.geo.admin.ch/1.0.0/ch.swisstopo.swissimage/default/current/3857/{z}/{x}/{y}.jpeg",
             attribution = '&copy; <a href="https://www.geo.admin.ch/de/about-swiss-geoportal/impressum.html#copyright">swisstopo</a>',
             group = "Satellite View") %>%
    addLayersControl(
      baseGroups = c("Swiss Topographic Map", "Satellite View"),
      overlayGroups = c(unique(plots$priority), "Done Plots", "Bewirtschaftungseinheiten"),
      options = layersControlOptions(collapsed = TRUE)
    ) %>%
    addCircleMarkers(data = plots, 
                     lat = ~Latitude, lng = ~Longitude,
                     popup = ~paste(ID, round(elevation, 0), sep = " - "),
                     radius = 8, stroke = FALSE, fillOpacity = 1, color = ~pal(priority),
                     group = ~priority) %>%
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
    hideGroup(c("Bewirtschaftungseinheiten", "MP5", "MP6", "MP7", "P3"))
)






htmlwidgets::saveWidget(m, file=paste("./2023-plot-map.html", sep = ""))


###### CREATING THE MUNICIPALITY MAP ############################################

municipalities <- read_csv("./2023-joinedPlotSelection_v2.csv") %>%
  get_municipality(., municipalities, what = c("NAME_1", "NAME_3")) %>%
  rename(canton = NAME_1,
         municipality = NAME_3) %>%
  select(canton, municipality)


