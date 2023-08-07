library(tidyverse)
library(mefa)

setwd("/Users/marco/GitHub/GitHub_G4B/2023_re-survey/")
source("./3_scripts/config_1_readVegedaz_marco.R")
hohl_2002_griwa_orth <- readVegedaz("./1_original_data/Data_Markus_Hohl_versionNFP48/Markus_extracted/Griwa_Orth_2002.tab")
hohl_2003_griwa_orth <- readVegedaz("./1_original_data/Data_Markus_Hohl_versionNFP48/Markus_extracted/Griwa_Orth_2003.tab")
hohl_2002_tuj_orth <- readVegedaz("./1_original_data/Data_Markus_Hohl_versionNFP48/Markus_extracted/Tuj_Orth_2002.tab")
hohl_2003_tuj_orth <- readVegedaz("./1_original_data/Data_Markus_Hohl_versionNFP48/Markus_extracted/Tuj_Orth_2003.tab")

plots <- read_csv("/Users/marco/GitHub/graslandvielfalt/R_files/2023-joinedPlotSelection_v3.csv") %>%
  filter(!priority %in% c("MP5", "MP6", "MP7")) %>%
  mutate(sID = gsub("-", "", sub(".*?-+", "", ID))) %>%
  rename(PplotID = plotID) %>%
  left_join(ph, by = "PplotID") %>%
  select(sID, plotID, PplotID)

d <- read_csv("./1_original_data/Data_Markus_Hohl_versionFelix/Hohl_Data_edit.csv") %>%
  rename(plotID = PlotID,
         x_coord = Coordinate_X_LV95,
         y_coord = Coordinate_Y_LV95,
         elevation = Elevation,
         date = Date,
         slope = Slope,
         exposition = Exposition,
         taxon = Tiergruppe,
         LU2000 = Landuse_type) %>%
  mutate(Species = gsub("_", " ", Species)) %>%
  filter(plotID %in% unique(ph$plotID),
         taxon == "Orthopteren") %>%
  select(plotID, Species, Abundance) %>%
  left_join(plots %>% select(plotID, sID), by = "plotID") %>%
  select(-plotID)

# Pivot the data using tidyr's pivot_wider() function and summarizing duplicates with the sum function
pivoted_tibble <- d %>%
  pivot_wider(
    names_from = sID,
    values_from = Abundance,
    values_fn = list(Abundance = sum)
  ) %>% as.data.frame()

# Set Species as row names
rownames(pivoted_tibble) <- pivoted_tibble$Species

# Replace NA values with 0 for all columns
pivoted_tibble <- pivoted_tibble %>% 
  mutate_at(vars(-Species), ~replace_na(., 0))

write_csv(pivoted_tibble, "/Users/marco/GitHub/graslandvielfalt/R_files/hohl_grashopperAbundance.csv")

# Remove the Species column as it is now part of the row names
pivoted_tibble <- pivoted_tibble[-1]

# Print the resulting table
print(pivoted_tibble)



join_mefa <- function(mefa_obj_list) {
  
  result <- data.frame()
  
  for (i in 1:length(mefa_obj_list)) {
    
    obj <- mefa_obj_list[[1]]
    
    index_df <- obj$samp %>%
      as.data.frame() %>%
      mutate(samp = rownames(.),
             plotID = Bezeichnung.neu) %>%
      select(samp, plotID)
    
    res <- mefa::melt(obj) %>%
      left_join(index_df, by = "samp") %>%
      mutate(plotID = plotID)
    
    result <- result %>% bind_rows(res)
    
  }

  return(result %>% select(-samp, -segm))  

}

(p <- join_mefa(list(hohl_2002_griwa_orth, hohl_2003_griwa_orth, hohl_2002_tuj_orth, hohl_2003_tuj_orth)) %>%
  #filter(plotID %in% unique(ph$plotID)) %>%
  distinct(plotID))

