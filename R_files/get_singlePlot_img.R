# Load required libraries
library(rgdal)
library(sf)
library(ggplot2)
library(ggsn)
library(grid)
library(rgdal)
library(webshot)
library(sp)
library(tidyverse)
library(googlesheets4)
library(leaflet)

setwd("/Users/marco/GitHub/graslandvielfalt/R_files")

# Read the data from the Google Sheet
gsheet <- read_sheet("https://docs.google.com/spreadsheets/d/1rIDiZIn6EFSC1ifOlfHDeNpUIWRYYqZDQa5PPJdeaog/edit#gid=123", sheet = "Sheet1") %>%
  mutate_all(as.character) %>%
  filter(done_veg == 1)

plots <- read_csv("./2023-joinedPlotSelection_v3.csv") %>%
  filter(!priority %in% c("MP5", "MP6", "MP7")) %>%
  mutate(link = paste0("http://www.google.ch/maps/place/", Latitude, ",", Longitude)) %>%
  inner_join(gsheet %>% select("ID", "date_veg", "coord_inprec", "moss"), by = "ID") %>%
  mutate(sID = sub("^[^-]*-", "", ID))

# Load the shapefile using rgdal and convert it to sf
#be <- rgdal::readOGR("/Users/marco/kDocuments_Marco/PhD/old/server/1_original_data/shapefiles")
#
#poly <- be %>%
#  get_polygons(plots = plots, shapefile = ., radius_m = 1)
#
#writeOGR(poly, dsn = "./2023-plots-with-be-poly-singlePlot.geojson", 
#         layer = ogrListLayers("/Users/marco/kDocuments_Marco/PhD/old/server/1_original_data/shapefiles/be_bewirtschaftungseinheit_view.shp")[1],
#         driver = "GeoJSON")

poly <- rgdal::readOGR("./2023-plots-with-be-poly-singlePlot.geojson")

# Function to remove individual files and then the empty folder
remove_folder_with_files <- function(folder_path) {
  # Remove individual files
  files_in_folder <- list.files(path = folder_path, full.names = TRUE)
  file.remove(files_in_folder)
  
  # Remove the empty folder
  unlink(folder_path, recursive = TRUE)
}

# Function to create and capture images of each polygon
capture_polygon_images <- function(polygon_data, plots_data, output_directory) {
  
  plots_list <- plots_data$ID
  plots_done <- str_replace(str_replace(list.files("/Users/marco/kDocuments_Marco/data/G4B/singlePlot_maps/"), ".jpg", ""), "polygon_", "")
  plots_todo <- rev(setdiff(plots_list, plots_done))
  
  for (i in plots_todo) {
    
    # Convert current point to an sf object and specify CRS if necessary
    current_point_data <- plots_data %>%
      filter(ID == i)
    
    # Get the current point coordinates
    current_point_longitude <- current_point_data$Longitude
    current_point_latitude <- current_point_data$Latitude
    
    point_coords_sp <- SpatialPoints(matrix(c(current_point_longitude, current_point_latitude), ncol = 2),
                                     proj4string = CRS("+proj=longlat +datum=WGS84"))
    
    # Find the polygons that contain the points using the over() function
    current_polygon <- poly[!is.na(sp::over(poly, point_coords_sp)), ]
    
    if (nrow(current_polygon) == 0) {
      print(paste0("Skipping ", i, " - Point outside polygons."))
      next  # Skip to the next iteration
    }
    
    # Get the centroid of the polygon
    centroid <- coordinates(current_polygon)
    
    # Create a new leaflet map with swisstopo background
    my_map <- leaflet() %>%
      addTiles(urlTemplate = "https://wmts20.geo.admin.ch/1.0.0/ch.swisstopo.pixelkarte-farbe/default/current/3857/{z}/{x}/{y}.jpeg",
               attribution = '&copy; <a href="https://www.geo.admin.ch/de/about-swiss-geoportal/impressum.html#copyright">swisstopo</a>',
               group = "Swiss Topographic Map") %>%
      addScaleBar(position = "bottomleft") %>% 
      
      addPolygons(data = current_polygon,
                  fill = FALSE, 
                  color = "darkorange", 
                  opacity = 0.9,
                  group = "Bewirtschaftungseinheiten") %>%
      addCircleMarkers(data = plots_data, 
                       lat = ~Latitude, lng = ~Longitude,
                       radius = 8, stroke = FALSE, fillOpacity = 1, color = "red") %>%
      addCircleMarkers(data = current_point_data, 
                       lat = ~Latitude, lng = ~Longitude,
                       radius = 8, stroke = FALSE, fillOpacity = 1, color = "darkorange") %>%
      setView(lng = mean(centroid[, 1]),
              lat = mean(centroid[, 2]),
              zoom = 20)
    
    # Save the map as an HTML widget
    output_filename <- paste0(output_directory, "/polygon_", i, ".html")
    htmlwidgets::saveWidget(my_map, output_filename, selfcontained = FALSE)
    
    # Capture the HTML widget as an image
    output_image <- paste0(output_directory, "/polygon_", i, ".jpg")
    webshot(output_filename, output_image, delay = 3, cliprect = "viewport", vwidth = 1080, vheight = 1080)
    
    print(paste0("DONE WITH ", i))
    
    # Remove the HTML file
    file.remove(output_filename)
    # Remove the folder with HTML files and other intermediate files
    remove_folder_with_files(paste0(output_directory, "/polygon_", i, "_files"))
  }
  print(paste("Images generated for", length(plots_done <- str_replace(str_replace(list.files("/Users/marco/kDocuments_Marco/data/G4B/singlePlot_maps/"), ".jpg", ""), "polygon_", "")), 
              "plots out of", length(plots_list)))
}

# Specify the output directory where the images will be saved
output_directory <- "/Users/marco/kDocuments_Marco/data/G4B/singlePlot_maps"

# Call the function to capture images of each polygon
capture_polygon_images(poly, plots, output_directory)


