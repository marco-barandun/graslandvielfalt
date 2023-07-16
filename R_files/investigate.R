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
  select(where(~any(. != 0))) %>% # select only the columns that contain at least one value that is not equal to zero.
  colnames()


dorothea_orth <- readVegedaz("./1_original_data/Data_Dorothea_Kampmann/NFP48_C_Heuschreckenaufnahmen.tab")
dorothea_plants <- readVegedaz("./1_original_data/Data_Dorothea_Kampmann/NFP48_C_Vegetationsaufnahmen.tab")
peter_grindelwald <- readVegedaz("/Users/marco/GitHub/GitHub_G4B/2023_re-survey/1_original_data/Data_Markus_Peter_ART-Datenbank/Peter_Grindelwald_ART-Datenbank.tab")
peter_tujetsch <- readVegedaz("/Users/marco/GitHub/GitHub_G4B/2023_re-survey/1_original_data/Data_Markus_Peter_ART-Datenbank/Peter_Tujetsch_ART-Datenbank.tab")

plants <- as.data.frame(peter_grindelwald$xtab) %>%
  rownames_to_column(var = "rn") %>%
  mutate(row_group = sub("\\..*$", "", rn)) %>%
  mutate(across(-c(rn, row_group), ~ifelse(. != 0 & !is.na(.), 1, 0))) %>%
  group_by(row_group) %>%
  summarize(across(-rn, sum)) %>%
  ungroup() %>%
  filter(row_group %in% p$plotID) %>%
  select(where(~any(. != 0))) %>%
  t() %>%
  as.data.frame() %>%
  setNames(.[1, ]) %>%
  slice(-1) %>%
  as.data.frame()

orths <- as.data.frame(dorothea_orth$xtab) %>%
  rownames_to_column(var = "rn") %>%
  mutate(row_group = sub("\\..*$", "", rn)) %>%
  mutate(across(-c(rn, row_group), ~ifelse(. != 0 & !is.na(.), 1, 0))) %>%
  group_by(row_group) %>%
  summarize(across(-rn, sum)) %>%
  ungroup() %>%
  filter(row_group %in% p$plotID) %>%
  select(where(~any(. != 0))) %>%
  t() %>%
  as.data.frame() %>%
  setNames(.[1, ]) %>%
  slice(-1) %>%
  as.data.frame()


# Create the species table
t <- datatable(
  orths,
  class = "display nowrap",
  escape = FALSE,
  rownames = TRUE,
  options = list(
    dom = "lfrtip",
    lengthMenu = list(c(10, 25, 50, -1), c("10", "25", "50", "All")),
    pageLength = 25,
    initComplete = JS(
      "function(settings, json) {",
      "  var table = settings.oInstance.api();",
      "  var input = $('<input type=\"text\" placeholder=\"Search column\">');",
      "  var filter = $('div.dataTables_filter');",
      "  var rowNames = table.column(0).data().toArray();",  # Get the row names from the first column
      "  $('thead tr', table.table().container()).append('<th></th>');",
      "  $('thead tr th:last-child', table.table().container()).addClass('no-search');",
      "  input.on('keyup', function () {",
      "    var searchValue = input.val().toLowerCase();",
      "    table.columns().every(function(i) {",
      "      if (i === 0 || table.column(i).header().textContent.toLowerCase().indexOf(searchValue) > -1 || table.column(i).header().textContent === 'Species') {",
      "        $(table.column(i).header()).show();",
      "        $(table.column(i).footer()).show();",
      "        this.nodes().each(function(cell, j) {",
      "          if (i === 0) {",
      "            $(cell).text(rowNames[j]);",  # Assign row names to the cells of the first column
      "            $(cell).show();",
      "          } else {",
      "            $(cell).show();",
      "          }",
      "        });",
      "      } else {",
      "        $(table.column(i).header()).hide();",
      "        $(table.column(i).footer()).hide();",
      "        this.nodes().each(function(cell, j) {",
      "          if (i === 0) {",
      "            $(cell).hide();",
      "          } else {",
      "            $(cell).hide();",
      "          }",
      "        });",
      "      }",
      "    });",
      "  });",
      "  filter.prepend(input);",
      "  filter.css('display', 'flex');",
      "  input.css('margin-right', '10px');",
      "}"
    ),
    columnDefs = list(
      list(className = "dt-head-center", targets = "_all"),
      list(className = "dt-body-center", targets = "_all")
    )
  )
)

htmltools::save_html(t, file="/Users/marco/GitHub/graslandvielfalt/R_files/2023-orth-species-table.html")























library(DT)

# Create the species table
datatable(
  orths,
  class = "display nowrap",
  escape = FALSE,
  rownames = TRUE,
  options = list(
    dom = "lfrtip",
    lengthMenu = list(c(10, 25, 50, -1), c("10", "25", "50", "All")),
    pageLength = 25,
    initComplete = JS(
      "function(settings, json) {",
      "  var table = settings.oInstance.api();",
      "  var input = $('<input type=\"text\" placeholder=\"Search column\">');",
      "  var filter = $('div.dataTables_filter');",
      "  var rowNames = table.column(0).data().toArray();",  # Get the row names from the first column
      "  $('thead tr', table.table().container()).append('<th></th>');",
      "  $('thead tr th:last-child', table.table().container()).addClass('no-search');",
      "  input.on('keyup', function () {",
      "    var searchValue = input.val().toLowerCase();",
      "    table.columns().every(function(i) {",
      "      if (i === 0 || table.column(i).header().textContent.toLowerCase().indexOf(searchValue) > -1 || table.column(i).header().textContent === 'Species') {",
      "        $(table.column(i).header()).show();",
      "        $(table.column(i).footer()).show();",
      "        this.nodes().each(function(cell, j) {",
      "          if (i === 0) {",
      "            $(cell).text(rowNames[j]);",  # Assign row names to the cells of the first column
      "            $(cell).show();",
      "          } else {",
      "            $(cell).show();",
      "          }",
      "        });",
      "      } else {",
      "        $(table.column(i).header()).hide();",
      "        $(table.column(i).footer()).hide();",
      "        this.nodes().each(function(cell, j) {",
      "          if (i === 0) {",
      "            $(cell).hide();",
      "          } else {",
      "            $(cell).hide();",
      "          }",
      "        });",
      "      }",
      "    });",
      "  });",
      "  filter.prepend(input);",
      "  filter.css('display', 'flex');",
      "  input.css('margin-right', '10px');",
      "}"
    ),
    columnDefs = list(
      list(className = "dt-head-center", targets = "_all"),
      list(className = "dt-body-center", targets = "_all")
    )
  )
)

