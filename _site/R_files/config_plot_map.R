get_polygons <- function(plots, shapefile, radius_m) {
  
  require(sf)
  require(leaflet)
  require(nngeo)
  require(s2)
  
  shapefile <- sf::st_as_sf(shapefile)
  
  # Check validity of geometries in the shapefile
  if (!all(st_is_valid(shapefile))) {
    shapefile <- st_make_valid(shapefile)
  }
  
  # Convert the plots dataframe to an sf object
  plots_sf <- st_as_sf(plots, coords = c("Longitude", "Latitude"))
  
  # Assign CRS to plots_sf
  st_crs(plots_sf) <- st_crs(shapefile)
  
  # Check CRS and transform if needed
  if (!identical(st_crs(be), st_crs(plots_sf))) {
    plots_sf <- st_transform(plots_sf, crs = st_crs(be)[1])
  }
  
  # Create buffer around points
  buffered_plots <- st_buffer(plots_sf, dist = radius_m)
  
  # Find indices of intersecting polygons
  intersects <- st_intersects(shapefile, buffered_plots, sparse = TRUE)
  indices <- which(sapply(intersects, length) > 0)
  
  # Subset be using indices
  filtered_be <- be[indices, ]
  

}

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