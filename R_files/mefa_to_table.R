library(tidyverse)

setwd("/Users/marco/GitHub/GitHub_G4B/2023_re-survey/")
source("./3_scripts/config_1_readVegedaz_marco.R")
hohl_2002_griwa_orth <- readVegedaz("./1_original_data/Data_Markus_Hohl_versionNFP48/Markus_extracted/Griwa_Orth_2002.tab")
hohl_2003_griwa_orth <- readVegedaz("./1_original_data/Data_Markus_Hohl_versionNFP48/Markus_extracted/Griwa_Orth_2003.tab")
hohl_2002_tuj_orth <- readVegedaz("./1_original_data/Data_Markus_Hohl_versionNFP48/Markus_extracted/Tuj_Orth_2002.tab")
hohl_2003_tuj_orth <- readVegedaz("./1_original_data/Data_Markus_Hohl_versionNFP48/Markus_extracted/Tuj_Orth_2003.tab")

plots <- read_csv("./2023-joinedPlotSelection_v3.csv") %>%
  filter(!priority %in% c("MP5", "MP6", "MP7"))

transform_mefa_to_table <- function(mefa_object_list, plot_df) {
  require(tibble)
  require(dplyr)
  require(purrr)
  
  # Initialize an empty list to store the results
  result_list <- list()
  
  for (i in 1:length(mefa_object_list)) {
  
    mefa_object <- mefa_object_list[[i]]
    
    # Get the species names and their corresponding abundance values
    species_abundance_df <- mefa_object$xtab %>%
      as.data.frame() %>%
      mutate(index = rownames(.))
  
    index_df <- mefa_object$samp %>%
      as.data.frame() %>%
      mutate(index = rownames(.),
             plotID = gsub(" ", "", Bezeichnung.neu)) %>%
      left_join(plot_df %>% select(ID, plotID), by = "plotID") %>%
      mutate(sID = gsub("-", "", sub(".*?-+", "", ID))) %>%
      #select(sID, everything(), -plotID, -ID) %>%
      select(index, sID)
    
    df <- inner_join(species_abundance_df, index_df, by = "index") %>%
      select(-index) %>%
      filter(!is.na(sID)) %>%
      group_by(sID) %>%
      summarise_at(vars(-group_cols()), sum) %>%
      as.data.frame()
    
    # Set the row names to plotID
    rownames(df) <- df$sID
    
    # Remove the plotID column
    df <- df %>%
      #inner_join(plot_df %>% select(ID, plotID), by = "plotID") %>%
      #mutate(sID = gsub("-", "", sub(".*?-+", "", ID))) %>%
      #select(sID, everything(), -plotID, -ID) %>%
      t() %>%
      as.data.frame() %>%
      rownames_to_column("Species") %>%
      .[-1,]
    
    # Add the current result to the result list
    result_list[[length(result_list) + 1]] <- df
    names(result_list)[length(result_list)] <- paste("Result", length(result_list), sep = "_")
    
  }
  
  # Combine the results from the list into a single data frame with bind_rows
  result <- bind_rows(result_list) %>%
    mutate(across(.cols = -Species, .fns = as.numeric)) %>%  # Convert all numeric columns to numeric
    group_by(Species) %>%
    summarise(across(.fns = sum)) %>%
    mutate(across(.cols = -Species, ~replace_na(.x, 0))) %>%
  
  return(result)
}

result_combined <- transform_mefa_to_table(list(hohl_2002_griwa_orth, hohl_2003_griwa_orth, hohl_2002_tuj_orth, hohl_2003_tuj_orth),
                                           plots)
