# -----Load Packages ------------------------------------------------

library(tidyverse)
library(janitor)
library(skimr)
library(readr)
library(sf)
library(tigris)
library(plotly)
library(rvest)
library(here)
library(ggplot2)


# Read in file ------------------------------------------------------------

hike_data <- readr::read_rds(url('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-24/hike_data.rds'))


# Download file -----------------------------------------------------------

download.file(url = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-24/hike_data.rds",
                destfile = "data-raw-top/hikes.csv")

# Import Data -------------------------------------------------------------

#hikes <- read_csv("data-raw-top/hikes.csv")
#error: Error in make.names(x) : invalid multibyte string 1


# Clean Data --------------------------------------------------------------

clean_hikes <- hike_data %>% 
  clean_names() %>% 
  mutate(
    trip = case_when(
      grepl("roundtrip",length) ~ "roundtrip",
      grepl("one-way",length) ~ "one-way",
      grepl("of trails",length) ~ "trails"),
    length_total = as.numeric(gsub("(\\d+[.]\\d+).*","\\1", length)) * ((trip == "one-way") + 1),
    gain = as.numeric(gain),
    highpoint = as.numeric(highpoint),
    rating = as.numeric(rating),
    location_general = gsub("(.*)\\s[-][-].*","\\1",location)) %>% 
  select(-location, -length, -description, -trip)
  

hike_features <- clean_hikes %>% 
  unnest(features) %>%
  group_by(features) 

#% hikes according to feature
percent_feature <- hike_features %>% 
  summarise(number = n(),
            percent = n() / nrow(hike_data) * 100) %>%
  ungroup() 

#hikes good for kids
kid_hikes <- hike_features %>% 
  filter(features == "Good for kids")
