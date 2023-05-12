library(leaflet)
library(DT)
library(scales)
library(tidyverse)
library(sf)

setwd("/Users/marco/GitHub/wiesenbiodiversitaet/R_files")

get_municipality <- function(coords_df, shapefile, what) {
  
  # Remove points that are located in the ocean
  occs <- sp::SpatialPointsDataFrame(coords = coords_df %>% dplyr::select(Longitude, Latitude), 
                                     data = coords_df) ##check columns for long/lat
  
  shapefile_sp <- as(shapefile, "Spatial")
  
  raster::crs(occs) <- raster::crs(shapefile_sp)
  ovr <- sp::over(occs, shapefile_sp) %>% ###overlay world and points
    dplyr::select(what)
  
  ds <- cbind(coords_df, ovr)
  
}

municipalities <- st_read("/Users/marco/kDocuments_Marco/PhD/server/1_original_data/gadm41_CHE.gpkg",
                          layer = "ADM_ADM_3")

plots <- read_csv("./2023-joinedPlotSelection_v2.csv") %>%
  get_municipality(., municipalities, what = c("NAME_1", "NAME_3")) %>%
  rename(canton = NAME_1,
         municipality = NAME_3) %>%
  mutate(priority = gsub("A", "", priority)) %>%
  arrange(priority, canton, municipality, elevation) %>%
  group_by(municipality) %>%
  mutate(ID = paste0(priority, "-",
                     toupper(substr(canton, 1, 2)), "-", 
                     toupper(substr(municipality, 1, 2)), "-", 
                     row_number())) %>%
  ungroup() %>%
  select(ID, elevation, canton, municipality, mgroup, LU1980, LU2000, LU2020, LNF_Code, everything()) %>%
  arrange(priority, elevation)

write_csv(plots, "./2023-joinedPlotSelection_v3.csv")
  
(t <- DT::datatable(plots,
                    class = "display nowrap",
                    escape = F,
                    rownames = FALSE))

htmltools::save_html(t, file="2023-plot-table.html")

# Create a leaflet map with the Swiss Topographic Map as a basemap
m <- leaflet() %>%
  addTiles(urlTemplate = "https://wmts20.geo.admin.ch/1.0.0/ch.swisstopo.pixelkarte-farbe/default/current/3857/{z}/{x}/{y}.jpeg",
           attribution = '&copy; <a href="https://www.geo.admin.ch/de/about-swiss-geoportal/impressum.html#copyright">swisstopo</a>') %>% 
  addCircleMarkers(data = plots, 
             lat = ~Latitude, lng = ~Longitude,
             popup = ~paste(ID, round(elevation, 0), sep = " - "),
             radius = 6, stroke = FALSE, fillOpacity = 1, color = ~priority) %>%
  setView(lng = 9, lat = 46.4, zoom = 8)

htmlwidgets::saveWidget(m, file=paste("./2023-plot-map.html", sep = ""))
