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
library(rgdal)
library(data.table)  # Load data.table package
library(terra)
library(sf)
library(dplyr)

plots <- read_csv("/Users/marco/GitHub/graslandvielfalt/R_files/2023-joinedPlotSelection_v3.csv") %>%
  filter(!priority %in% c("MP5", "MP6", "MP7"))

plots_buffered <- st_buffer(st_as_sf(plots, coords = c("Longitude", "Latitude"), crs = 4326), dist = 5000) %>%
  st_union() %>%
  st_make_valid()

# Read the CSV file and filter duplicate coordinates
kph <- read_csv("/Users/marco/GitHub/GitHub_G4B/2023_re-survey/2_generated_data/old/Coord_Kamp_Peter_Hohl.csv") %>%
  filter(origin == "dorothea_pflanzen" | origin == "peter_pflanzen") %>%
  filter(year > 2000) %>%
  distinct(x_coord, y_coord, .keep_all = TRUE) %>%
  st_as_sf(coords = c("x_coord", "y_coord"), crs = 21781) %>%
  st_transform(crs = 4326) %>%
  st_make_valid() %>%
  st_filter(plots_buffered)

# Read the shapefile
be <- st_read("/Users/marco/kDocuments_Marco/PhD/old/server/1_original_data/shapefiles/be_bewirtschaftungseinheit_view.shp") %>%
  st_make_valid()

be_subset <- be %>%
  st_filter(plots_buffered)

# Create a buffer around the points
buffered_points <- kph %>%
  st_buffer(dist = 10) %>%
  st_filter(be_wb)

arealstat <- fread("/Users/marco/kDocuments_Marco/data/G4B/arealstatistik/ag-b-00.03-37-area-csv.csv") %>%
  mutate(index = 1:nrow(arealstat)) %>%
  dplyr::select(E_COORD, N_COORD, AS18_72) %>%
  rast()

p <- kph %>%
  filter(!rn %in% buffered_points$rn) %>%
  st_transform(crs = 2056) %>%
  mutate(AS18_72 = extract(arealstat, .)[, 2]) %>%
  st_transform(crs = 4326) %>%
  filter(AS18_72 %in% c(44, 47, 48, 50:60))

mapview(p)

final_ds <- p %>%
  mutate(Latitude = st_coordinates(.)[, "Y"],
         Longitude = st_coordinates(.)[, "X"]) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  dplyr::select(Latitude, Longitude, everything()) %>%
  filter(elevation < 2100) %>%
  mutate(priority = ifelse(elevation < 1839, "T1", "T2"))

write_csv(final_ds, "/Users/marco/GitHub/graslandvielfalt/R_files/tobia-non-grassland-plots.csv")

 mapview(p, color = p$elevation)
