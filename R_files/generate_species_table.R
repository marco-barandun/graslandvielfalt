library(leaflet)
library(DT)
library(scales)
library(tidyverse)
library(sf)

setwd("/Users/marco/GitHub/wiesenbiodiversitaet/R_files")

plots <- read_csv("./2023-joinedPlotSelection_v3.csv") %>%
  rename(plot_id = ID,
         ID = plotID) %>%
  mutate(plot_id = toupper(str_remove(plot_id, "^[^-]*-") %>% str_replace_all("-", "")))

# Weird cathegories
categories <- dorothea_orth %>%
  select(-1) %>%
  .[-1,] %>%
  lapply(unique) %>%
  unlist() %>%
  unique()

# Peter plants
peter_plants <- read.table("/Users/marco/kDocuments_Marco/PhD/server/1_original_data/Data_Markus_Peter_ART-Datenbank/Peter_Grindelwald_ART-Datenbank.tab", 
           sep = ";", na.strings = c("NA", "")) %>%
  slice(c(-1, -(3:67))) %>%
  select(-V1, -V3) %>%
  t() %>%
  as.data.frame() %>%
  set_names(.[1, ]) %>%
  .[-1,] %>% # use first row as column names
  filter(Nummer_Autor %in% c(unique(plots$ID), "id")) %>%
  rename(ID = Nummer_Autor) %>%
  left_join(plots %>% select(plot_id, ID), by = "ID") %>%
  select(plot_id, everything(), -ID) %>%
  mutate_at(vars(-plot_id), ~ifelse(!is.na(.), 1, NA)) %>%
  group_by(plot_id) %>%
  summarize_all(sum) %>%
  ungroup() %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  set_names(.[1, ]) %>%
  .[c(-1),] %>%
  rename(Species = plot_id)

# Dorothea plants
dorothea_plants <- read.table("/Users/marco/kDocuments_Marco/PhD/server/1_original_data/Data_Dorothea_Kampmann/NFP48_C_Vegetationsaufnahmen.tab", 
                            sep = ";", na.strings = c("NA", ""))  %>%
  slice(-(2:8)) %>%
  select(-V1, -V3) %>%
  t() %>%
  as.data.frame() %>%
  filter(V1 %in% c(unique(plots$ID), "id")) %>%
  t() %>%
  as.data.frame() %>%
  set_names(.[1, ]) %>%
  .[-1,] %>% # use first row as column names
  mutate_at(vars(-id), ~ifelse(!is.na(.), 1, NA)) %>%
  group_by(id) %>%
  summarize_all(sum) %>%
  ungroup() %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  set_names(.[1, ]) %>%
  .[-1,] %>%
  rename(ID = id) %>%
  left_join(plots %>% select(plot_id, ID), by = "ID") %>%
  select(plot_id, ID, everything()) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  set_names(.[1, ]) %>%
  .[c(-1, -2),] %>%
  rename(Species = plot_id)

plants <- full_join(dorothea_plants, peter_plants, by = "Species")


# There is an issue. There are some Hohl plots which do not have plant data..... CHECK THIS


# Dorothea orth
dorothea_orth <- read.table("/Users/marco/kDocuments_Marco/PhD/server/1_original_data/Data_Dorothea_Kampmann/NFP48_C_Heuschreckenaufnahmen.tab", sep = ";", na.strings = c("NA", "")) %>%
  slice(-(2:15)) %>%
  select(-V1, -V3) %>%
  .[c(-40, -46),] %>%# remove first row and "Reptilien" and "Tetrix sp"
  t() %>%
  as.data.frame(.) %>%
  set_names(.[1, ]) %>%
  .[-1,] %>% # use first row as column names
  mutate_at(vars(-ID), ~ifelse(!is.na(.), 1, NA)) %>%
  group_by(ID) %>%
  summarise_all(sum) %>%
  ungroup(.) %>%
  filter(toupper(ID) %in% toupper(unique(plots$ID))) %>%
  mutate(ID = toupper(ID)) %>%
  left_join(plots %>% select(plot_id, ID), by = "ID") %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  set_names(.[51, ]) %>%
  .[c(-1, -51),] %>%# use first row as column names
  rename(Species = plot_id)




# Getting the species present in the dataframe
present <- dorothea_orth %>%
  rowwise() %>%
  summarize(has_nonzero = any(c_across(-Species) != 0)) %>%
  bind_cols(dorothea_orth, .) %>%
  filter(has_nonzero == TRUE)

dorothea_orth <- dorothea_orth %>%
  filter(Species %in% present$Species)

# Create the species table
datatable(dorothea_orth, 
          class = "display nowrap",
          escape = F,
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
              "  var colHeaders = [];",
              "  table.columns().every(function() {",
              "    colHeaders.push($(this.header()).text().trim());",
              "  });",
              "  $('thead tr', table.table().container()).append('<th></th>');",
              "  $('thead tr th:last-child', table.table().container()).addClass('no-search');",
              "  input.on('keyup', function () {",
              "    var searchValue = input.val().toLowerCase();",
              "    table.columns().every(function(i) {",
              "      if (colHeaders[i].toLowerCase().indexOf(searchValue) > -1 || colHeaders[i] === 'Species') {",
              "        $(table.column(i).header()).show();",
              "        $(table.column(i).footer()).show();",
              "        this.nodes().each(function(cell, j) {",
              "          $(cell).show();",
              "        });",
              "      } else {",
              "        $(table.column(i).header()).hide();",
              "        $(table.column(i).footer()).hide();",
              "        this.nodes().each(function(cell, j) {",
              "          $(cell).hide();",
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
          ))


(t <- DT::datatable(plots,
                    class = "display nowrap",
                    escape = F,
                    rownames = FALSE))

#htmltools::save_html(t, file="2023-orth-species-table.html")


