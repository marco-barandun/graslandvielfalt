library(tidyverse)
library(stringr)
library(sf)
library(mapview)
library(sp)
library(rgdal)
library(data.table)

setwd("/Users/marco/GitHub/graslandvielfalt/R_files/")
source("/Users/marco/GitHub/GitHub_G4B/2023_re-survey/3_scripts/config_1_readVegedaz_marco.R")

plots <- read_csv("./2023-joinedPlotSelection_v3.csv") %>% filter(!priority %in% c("MP5", "MP6", "MP7"))

dorothea_orth <- readVegedaz("/Users/marco/GitHub/GitHub_G4B/2023_re-survey/1_original_data/Data_Dorothea_Kampmann/NFP48_C_Heuschreckenaufnahmen.tab")

orths <- as.data.frame(dorothea_orth$xtab) %>%
  rownames_to_column(var = "rn") %>%
  mutate(row_group = sub("\\..*$", "", rn)) %>%
  mutate(across(-c(rn, row_group), ~ifelse(. != 0 & !is.na(.), 1, 0))) %>%
  group_by(row_group) %>%
  summarize(across(-rn, sum)) %>%
  ungroup() %>%
  filter(row_group %in% plots$plotID) %>%
  rename(plotID = row_group) %>%
  left_join(plots %>% select(ID, plotID), by = "plotID") %>%
  mutate(sID = gsub("-", "", sub(".*?-+", "", ID))) %>%
  select(-plotID, -ID) %>%
  select(where(~any(. != 0))) %>%
  select(sID, everything()) %>%
  t() %>%
  as.data.frame() %>%
  setNames(.[1, ]) %>%
  slice(-1) %>%
  as.data.frame()


# Create the species table
(t <- datatable(
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
))


htmltools::save_html(t, file="2023-orth-species-table.html")



