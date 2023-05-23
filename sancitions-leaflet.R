library(tidyverse)
library(readxl)
library(rnaturalearth)
library(leaflet)
library(sf)
library(leaflet.extras)

# loading in data 
df <- read.csv("/Users/dwaste/Desktop/London-Politica/Sanctions-Middle-East/Ongoing Sanctions in the Middle East-A New Dataset.csv")

df <- df %>%
  rename("name" = "target_state")

df[2,1] <- "Palestine"

# loading in world map
world <- ne_countries(scale = "medium",
                      returnclass = "sf")

# joining sf data to military data
df <- left_join(df, world, by = "name")

df <- st_as_sf(df)

# color scales
palInt <- colorNumeric("Reds", df$intensity, na.color = "white")
palEf <- colorNumeric("Blues", df$effectiveness, na.color = "white")
palNumInd <- colorNumeric("Reds", df$number_target_individuals, na.color = "white")
palNumEnt <- colorNumeric("Reds", df$number_target_entities, na.color = "white")
palTerror <- colorFactor(c("white", "#d51f1f"), df$is_terrorist, na.color = "white")
palIntEnf <- colorNumeric("Greens", df$intl_enforcement, na.color = "white")

df$name[2] <- "Gaza"

# Define the recode mapping
recode_mapping <- c("1" = "Energy",
                    "2" = "Defence",
                    "4" = "Agriculture",
                    "5" = "Banking",
                    "6" = "Other")

palFin <- colorFactor(c("white", "#28d64d"), df$financial, na.color = "white")

# Recode the column using the mapping
df$financial_sector <- recode(df$financial_sector, !!!recode_mapping)
df$financial_sector_other <- recode(df$financial_sector_other, !!!recode_mapping)

popup.form <- paste0("<center><b>Sanctions Info: ", df$name,"</b>", "\n",
                         "<center>Start Year: ", ifelse(is.na(df$start_year), "", df$start_year), "\n",
                         "<center>Duration: ", ifelse(is.na(df$start_year), "", paste0(2023 - df$start_year, " years")), "\n",
                         "<center>Additional Episodes: ", ifelse(is.na(df$additional_episode_1), "", df$additional_episode_1), ifelse(is.na(df$additional_episode_2), "", paste0(", ", df$additional_episode_2)), "\n",
                         "<center>Addit. Episodes Duration: ", ifelse(is.na(df$additional_episode_1), "", paste0(2023 - df$additional_episode_1, " years")), ifelse(is.na(df$additional_episode_2), "", paste0(", " , 2023 - df$additional_episode_2, " years")), "\n",
                         "<center>Senders: ", ifelse(is.na(df$sender_state), "", df$sender_state), ifelse(is.na(df$second_sender), "", paste0(", ", df$second_sender, ", ")), ifelse(is.na(df$third_sender), "", paste0(df$third_sender)), "\n",
                         "<center>GDP Change: ", "$", ifelse(is.na(df$gdp_current) | is.na(df$gdp_start), "", round(df$gdp_current - df$gdp_start, digits = 2)), " Billion(s) USD")

popup.form.fin <- paste0("<center><b>Sanctions Info: ", df$name,"</b>", "\n",
                     "<center>Start Year: ", ifelse(is.na(df$start_year), "", df$start_year), "\n",
                     "<center>Duration: ", ifelse(is.na(df$start_year), "", paste0(2023 - df$start_year, " years")), "\n",
                     "<center>Additional Episodes: ", ifelse(is.na(df$additional_episode_1), "", df$additional_episode_1), ifelse(is.na(df$additional_episode_2), "", paste0(", ", df$additional_episode_2)), "\n",
                     "<center>Addit. Episodes Duration: ", ifelse(is.na(df$additional_episode_1), "", paste0(2023 - df$additional_episode_1, " years")), ifelse(is.na(df$additional_episode_2), "", paste0(", " , 2023 - df$additional_episode_2, " years")), "\n",
                     "<center>Senders: ", ifelse(is.na(df$sender_state), "", df$sender_state), ifelse(is.na(df$second_sender), "", paste0(", ", df$second_sender, ", ")), ifelse(is.na(df$third_sender), "", paste0(df$third_sender)), "\n",
                     "<center>GDP Change: ", "$", ifelse(is.na(df$gdp_current) | is.na(df$gdp_start), "", round(df$gdp_current - df$gdp_start, digits = 2)), " Billion(s) USD", 
                     "<center>Financial Sector: ", ifelse(is.na(df$financial_sector), "", df$financial_sector),
                     "<center>Other Financial Sector: ", ifelse(is.na(df$financial_sector_other), "", df$financial_sector_other))

# creating leaflet
plot <- leaflet(df) %>%
  setView(40, 35, 4) %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels, options = leafletOptions(scrollWheelZoom = FALSE)) %>%
  addPolygons(group = "Intensity", fill = ~intensity, fillColor = ~palInt(intensity), weight = 4, 
              opacity = 0, label = ~name, fillOpacity = .75, popup = popup.form) %>%
  addLegend("bottomleft", title = paste0("<b>Intensity"),
            pal = palInt, values = ~intensity, opacity = .7, group = "Intensity", 
            na.label = "None") %>%
  addPolygons(group = "Effectiveness", fill = ~effectiveness, fillColor = ~palEf(effectiveness), weight = 4, 
            opacity = 0, label = ~name, fillOpacity = .75, popup = popup.form) %>%
  addLegend("bottomleft", title = paste0("<b>Effectiveness"), 
            pal = palEf, values = ~effectiveness, opacity = .7, group = "Effectiveness", 
            na.label = "None") %>%
  addPolygons(group = "Financial Sanctions", fill = ~financial, fillColor = ~palFin(financial), weight = 4, 
              opacity = 0, label = ~name, fillOpacity = .75, popup = popup.form.fin) %>%
  addPolygons(group = "Number of Target Individuals", fill = ~number_target_individuals, fillColor = ~palNumInd(number_target_individuals), weight = 4, 
              opacity = 0, label = ~name, fillOpacity = .75, popup = popup.form) %>%
  addLegend("bottomleft", title = paste0("<b>Number of Target Individuals"),
            pal = palNumInd, values = ~number_target_individuals, opacity = .7, group = "Number of Target Individuals", 
            na.label = "None") %>%
  addPolygons(group = "Number of Target Entities", fill = ~number_target_entities, fillColor = ~palNumEnt(number_target_entities), weight = 4, 
              opacity = 0, label = ~name, fillOpacity = .75, popup = popup.form) %>%
  addLegend("bottomleft", title = paste0("<b>Number of Target Entities"),
            pal = palNumEnt, values = ~number_target_entities, opacity = .7, group = "Number of Target Entities", 
            na.label = "None") %>%
  addPolygons(group = "Terrorist Organization", fill = ~is_terrorist , fillColor = ~palTerror(is_terrorist), weight = 4, 
              opacity = 0, label = ~name, fillOpacity = .75, popup = popup.form) %>%
  addLegend("bottomleft", title = paste0("<b>Terrorist Organization"),
            pal = palTerror, values = ~round(is_terrorist, digits = 0), opacity = .7, group = "Terrorist Organization", 
            na.label = "None") %>%
  addPolygons(group = "International Enforcement", fill = ~intl_enforcement , fillColor = ~palIntEnf(intl_enforcement), weight = 4, 
              opacity = 0, label = ~name, fillOpacity = .75, popup = popup.form) %>%
  addLegend("bottomleft", title = paste0("<b>International Enforcement"),
            pal = palIntEnf, values = ~intl_enforcement, opacity = .7, group = "International Enforcement", 
            na.label = "None") %>%
  addLayersControl(baseGroups = c("Intensity", "Effectiveness", "Financial Sanctions", "Number of Target Individuals", "Number of Target Entities", "Terrorist Organization", "International Enforcement"), 
                 options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(c("Effectiveness", "Financial Sanctions", "Number of Target Individuals", "Number of Target Entities", "Terrorist Organization", "International Enforcement")) %>%
  htmlwidgets::onRender("
    function(el, x) {
      var updateLegend = function () {
          var selectedGroup = document.querySelectorAll('input:checked')[0].nextSibling.innerText.substr(1);

          document.querySelectorAll('.legend').forEach(a => a.hidden=true);
          document.querySelectorAll('.legend').forEach(l => {
            if (l.children[0].children[0].innerText == selectedGroup) l.hidden=false;
          });
      };
      updateLegend();
      this.on('baselayerchange', e => updateLegend());
    }") %>%
  suspendScroll()

plot




