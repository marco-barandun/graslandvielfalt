library(tidyverse)
setwd("/Users/marco/GitHub/graslandvielfalt/R_files")

gps <- read_tsv("./gps/archive/2023-05-23-1116-v4.txt") %>% as.data.frame()
write_csv(gps, "./gps/2023-05-23-1116-v4-to-clean.csv")
