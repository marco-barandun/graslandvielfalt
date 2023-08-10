library(tidyverse)
library(mapview)
library(sf)


setwd("/Users/marco/GitHub/graslandvielfalt/R_files")

gps <- read_csv("./gps/2023-05-23-1116-bkp2-clean_v4.csv") %>%
  mutate(sID = toupper(ifelse(nchar(Punkt_ID_G4B) <= 6,
                              gsub("^(.{2})(.{2})", "\\1-\\2-", Punkt_ID_G4B),
                              sub("^[^-]*-", "", Punkt_ID_G4B))))

gps <- st_as_sf(gps, coords = c("X-Koordinate", "Y-Koordinate"), crs = 2056)

mapview(gps, 
        cex = 3)
