

# -----Load Packages----- ------------------------------------------------

library(tidyverse)
library(janitor)
library(readxl)
library(skimr)
library(readr)

# ----download file------ ------------------------------------------------

download.file(url = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv",
              destfile = "data-raw-gd/measles.csv")


# Import Data -------------------------------------------------------------

measles <- read_csv("data-raw-gd/measles.csv")
View(measles)


# Clean Data --------------------------------------------------------------

measles_clean_rates <- measles %>% 
  #Get rid of negative mmr and overall rates (just filtered, but should I replace with NA as commented attempt below? I think there are some other negative values that would be hard to pin down?)
  filter(mmr >= 0, overall >= 0) %>% 
  select("state", "type", "mmr", "overall", "enroll") 
  #mutate(mmr = replace(mmr, -1, values = NA))

measles_exemptions <- measles %>% 
  #Fix personal exemption rate range to be 0-100 (not 170)
  filter(xper<=100) %>% 
  select ("state", "xper", "xmed", "xrel") %>% 
  #Fix exemption columns to be the same type; what about the xrel TRUE response rather than %? TRUE seems to have disappeared)
   pivot_longer(cols = -state, 
               names_to = "exemption_type",
               values_to = "percent") %>% 
#rename exemptions in column more clearly
   mutate(exemption_type = case_when(
     exemption_type == "xper" ~ "personal",
     exemption_type == "xmed" ~ "medical"),
     exemption_type == "xrel" ~ "religious") %>%
  #Why isn't this working? Says input 2 must be a vector, not formula object. 
  group_by(state)
#I think I should leave the NA percents, as they were not reported (rather than make 0s)
#With this dataframe, can I add back in the school type column since I pivoted_longer and only kept state? When I tried to keep state and type, it said type was in the way and couldn't be combined. 

