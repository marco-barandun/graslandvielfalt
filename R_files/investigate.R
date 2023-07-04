plants <- dorothea_vege$xtab %>%
  as.data.frame()



# Identify the row labeled "SPOE10"
plants = as.data.frame(dorothea_vege$xtab)["SPOEX1",]

# Remove values equal to 0
non_zero_values = plants[,plants != "0"]


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


                     