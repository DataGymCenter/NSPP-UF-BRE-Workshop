###################
library("readr")
library("tidytext")
library("ggplot2")
library(lubridate)
library(stringr)
library(exploratory)
library(tidyverse)
library(leaflet)
library(maps)
library(ggthemes)
library(htmlwidgets)
library(htmltools)
# library(mapview) # CTRL+SHIFT+C to comment or uncomment

####### Set your work directory
setwd("C:/RTuts/Projects/NemaTweets/newest/Nema_Twts/")

# Get vector of file names
nems <- read_csv("all_nema_twit_final.csv")
nems1 <- read_csv("all_ICN_twits.csv")

####
nems_all <- rbind(nems, nems1, fill=TRUE)

########### readr::read_csv("all_nema_twit_final.csv") 
nema1 <- nems_all %>% 
  readr::type_convert() %>%
  dplyr::mutate(place = str_remove_all(place, regex("\\{'fullName': ", ignore_case = TRUE)), 
                place = str_remove(place, regex("'", ignore_case = TRUE)), place = str_remove_all(place, regex("'", ignore_case = TRUE)), 
                place = str_remove_all(place, regex("'", ignore_case = TRUE)), place = str_remove_all(place, regex("\\}", ignore_case = TRUE)), 
                place = str_remove_all(place, regex("name:|type: city|type: admin", ignore_case = TRUE)), 
                place = str_remove_all(place, regex(", country: ", ignore_case = TRUE)), 
                place = str_remove_all(place, regex("countryCode: ", ignore_case = TRUE))) %>% 
  dplyr::mutate(place = str_remove_all(place, regex("\\{'fullName': '|'type': 'admin'|'countryCode': '|'name':|\\}", ignore_case = TRUE)), 
                place = str_remove_all(place, regex(", 'country': ", ignore_case = TRUE)), place = str_remove_all(place, regex("'type': 'city'", ignore_case = TRUE)), 
                place = str_remove_all(place, regex("'", ignore_case = TRUE))) %>%
  #separate(place, into = c("City", "State", "FullName", "Country", "CountryCode"), sep = "\\s*\\,\\s*", convert = TRUE) %>%
  dplyr::mutate(coordinates = str_remove_all(coordinates, regex("(\\{'longitude':)", ignore_case = TRUE)), coordinates = str_remove_all(coordinates, regex("'latitude':|\\}", ignore_case = TRUE))) %>%
  separate(coordinates, into = c("long", "lat"), sep = "\\s*\\,\\s*", remove = FALSE, convert = TRUE) %>%
  dplyr::mutate(source = str_remove_all(sourceLabel, regex("Twitter for ", ignore_case = TRUE))) %>%
  dplyr::mutate(Device = str_remove_all(source, regex("Twitter ", ignore_case = TRUE))) %>%
  mutate(Year = lubridate::year(date), Month = lubridate::month(date, label = TRUE, abbr=T)) 


################
total <- nema1 %>% 
  dplyr::select(date, username, followersCount, Longitude, Latitude) %>%
  #filter(!username == "EurekaMag") %>%
  mutate(lon=round(Longitude, digits = 2), lat=round(Latitude, digits = 2)) %>%
  group_by(date, lon, lat) %>% 
  count(lon, lat) %>%
  summarise(freq = sum(n)) %>%
  filter(!is.na(lon))

######### Plot Map
twits_map <- leaflet(
  total,
  options = leafletOptions(zoomControl = T)) %>%
  
  ## 
  addProviderTiles("CartoDB.DarkMatter", group = "CartoDB", 
                  options = providerTileOptions(minZoom = 1.5, maxZoom = 10)) %>%
  ## Markers
  # addTiles() %>% 
  # addMarkers(
  #   clusterOptions = markerClusterOptions()) %>%

  ## Circles
  addCircleMarkers(
    lng = total$lon,
    lat = total$lat,
    popup = total$freq,
    radius = ~ sqrt(freq)*2.3,
    stroke = F, color = "yellow", opacity = 0.95) %>%
  ## add a layers control
  addMiniMap(width = 220, height = 150, position = "bottomleft", 
             tiles = "CartoDB.DarkMatter"); twits_map




### R City Views
# https://github.com/koenderks/rcityviews
# install.packages("remotes") # Uncomment if you do not have the 'remotes' package installed
remotes::install_github("koenderks/rcityviews", dependencies = TRUE)

#After installation, you can load the package into the R session using the following command.
library(rcityviews)

list_cities(match = "Ams")

#If you cannot find your preferred city in the internal package database, you can use the new_city() function to manually #specify a location using its latitude and longitude coordinates.

city <- new_city(name = "Lagos", country = "Portugal", lat = 37.10, long = -8.68)
#> Discovered the city of Lagos, Portugal at 37.1° / -8.68°!

p <- cityview(name = "Amsterdam", zoom = 1) # or cityview(name = city) # zoom > 1, decreases computation time
# see ?cityview for more input parameters of this function

ggplot2::ggsave(filename = "Amsterdam.png", plot = p, height = 500, width = 500, units = "mm", dpi = 100)

# For example: black, beige and white theme, streets only
myTheme <- list(
  colors = list(
    background = "#232323",
    water = NA,
    landuse = NA,
    contours = NA,
    streets = "#d7b174",
    rails = c("#d7b174", "#232323"),
    buildings = NA,
    text = "#ffffff",
    waterlines = NA
  ),
  font = list(
    family = "serif",
    face = "bold",
    scale = 1,
    append = "\u2014"
  ),
  size = list(
    borders = list(
      contours = 0.15,
      water = 0.4,
      canal = 0.5,
      river = 0.6
    ),
    streets = list(
      path = 0.2,
      residential = 0.3,
      structure = 0.35,
      tertiary = 0.4,
      secondary = 0.5,
      primary = 0.6,
      motorway = 0.8,
      rails = 0.65,
      runway = 3
    )
  )
)

########
cityview(name = "Akure", zoom = 0.5, theme = myTheme, border = "square", filename = "AKR.png")
#halftone = "#ffffff", legend = TRUE, border = (square, hexagon, octagon, decagon, bbox), places = 10