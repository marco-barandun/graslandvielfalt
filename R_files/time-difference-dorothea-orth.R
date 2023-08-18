library(tidyverse)
library(stringr)
library(sf)
library(mapview)
library(sp)
library(rgdal)
library(data.table)
library(DT)

setwd("/Users/marco/GitHub/graslandvielfalt/R_files/")
source("/Users/marco/GitHub/GitHub_G4B/2023_re-survey/3_scripts/config_1_readVegedaz_marco.R")

plots <- read_csv("/Users/marco/GitHub/graslandvielfalt/R_files/2023-joinedPlotSelection_v3.csv") %>% 
  filter(!priority %in% c("MP5", "MP6", "MP7"))

p <- readVegedaz("/Users/marco/GitHub/GitHub_G4B/2023_re-survey/1_original_data/Data_Dorothea_Kampmann/NFP48_C_Heuschreckenaufnahmen.tab")$samp %>%
  rownames_to_column(var = "rn") %>%
  mutate(rn = gsub("\\.1", "", rn),
         Datum = as.Date(Datum, format = "%d.%m.%Y")) %>%   # Convert 'Datum' to Date type %>%
  filter(rn %in% plots$plotID)

# Group by 'rn' and calculate date difference
p_diff <- p %>%
  group_by(rn) %>%
  summarize(date_diff = as.numeric(max(Datum) - min(Datum))) %>%
  filter(!is.na(date_diff) & is.finite(date_diff) & date_diff > 2)  # Filter out missing or non-finite values

# Create a histogram
ggplot(p_diff, aes(x = date_diff)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  scale_x_continuous(labels = scales::comma_format()) +  # Format x-axis labels
  labs(x = "Date Difference (days)", y = "Frequency", title = "Distribution of Date Differences")
