library(tidyverse)
library(stringr)
library(sf)
library(mapview)
library(sp)
library(rgdal)
library(data.table)

#setwd("/home/agsad.admin.ch/f80869996/mnt/Data-Work-CH/22_Plant_Production-CH/225.2_Naturfutterbau_Berg_protected/13_Wiesennutzung/G4B/marco/")
setwd("/Users/marco/GitHub/GitHub_G4B/2023_re-survey/")

source("./3_scripts/config_1_readVegedaz_marco.R")

### Dorotea; Read in  704  relevees with  703  plant species in the years 2003 to 2004
##########################################################################################################################################
dorothea_vege <- readVegedaz("./1_original_data/Data_Dorothea_Kampmann/NFP48_C_Vegetationsaufnahmen.tab")

plants <- dorothea_vege$xtab %>%
  as.data.frame()



# Identify the row labeled "SPOE10"
plants = as.data.frame(dorothea_vege$xtab)["PEWI10",]

# Remove values equal to 0
non_zero_values = plants[,plants != "0"]
colnames(non_zero_values)

# Store the column names in a vector format
column_vector = pd.Series(non_zero_columns).to_numpy()


orth <- dorothea_orth$samp
pl <- dorothea_vege$xtab

p <- read_csv("/Users/marco/GitHub/GitHub_G4B/2023_re-survey/2_generated_data/2023-joinedPlotSelection_v3.csv")

orths <- as.data.frame(dorothea_orth$xtab) %>%
  rownames_to_column(var = "rn") %>%
  filter(str_detect(rn, str_replace_all(str_c(p$plotID, collapse = "|"), "\\..1*", ""))) %>%
  select(-rn) %>%
  select(where(~any(. != 0))) %>%
  colnames()

write_


                     