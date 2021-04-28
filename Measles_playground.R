

# -----Load Packages ------------------------------------------------

library(tidyverse)
library(janitor)
library(readxl)
library(skimr)
library(readr)

# ----Download file ------------------------------------------------

#download.file(url = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv",
 #             destfile = "data-raw-gd/measles.csv")


# Import Data -------------------------------------------------------------

measles <- read_csv("data-raw-gd/measles.csv")
View(measles)


# Clean Data --------------------------------------------------------------

measles_clean_rates <- measles %>% 
  select(state, name, type, enroll, mmr, overall) %>% 
  mutate(overall = ifelse(overall < 0, NA, overall)) %>% 
  mutate(mmr = ifelse(mmr < 0, NA, mmr)) %>% 
  distinct()

#YEAR OBSERVATION: 484+1515(WI only) schools reported for 2018-19, 567 schools reported for 2017-18, 1567 null (NA). I'm just interested in total rates, and checked that schools didn't report for multiple years, so I'm not going to analyze by year. But if I were, I would need to change null to NA and run into a big bummer/weak analysis.
#INDEX OBSERVATION: The index ID assigned in the set is by year (and WI has its own), so there are repeats. If I were going to analyze by unique ID, I would need to create a new variable ID. 

measles_exemptions <- measles %>% 
  filter(xper<=100) %>% 
  select (state, name, type, enroll, xper, xmed, xrel) %>% 
   pivot_longer(cols = xper:xrel, 
               names_to = "exemption_type",
               values_to = "percent") %>% 
   mutate(exemption_type = case_when(
     exemption_type == "xper" ~ "personal",
     exemption_type == "xmed" ~ "medical",
     exemption_type == "xrel" ~ "religious",
     TRUE ~ exemption_type)) %>%
  drop_na(percent) %>% 
       distinct() 
  
#Question: I'm converting the % to # students, but before I add the below rounding code, do you think it's a problem that so many numbers are partial, not even close to a whole number? Is it fair to round those then sum and reconvert to %?
measles_exemptions_number <- measles_exemptions %>% 
  group_by(state, type, exemption_type) %>% #what is this line actually accomplishing? no changes when I pound it out.
  mutate(number_of_exempt_students = (percent*enroll)/100) 
#%>% 
 # mutate(number_of_exempt_students = round(number_of_exempt_students, digits = 0))


#Note: Bummer that so many schools didn't report enrollment, so I can only calculate 4692/7316 numbers. Still, more than half. I wonder if percents will be more meaningful, if I can use ranges maybe (leads to next question)...

#Question: Next step I'm thinking of creating an exemption_ranges variable for later visualization. Can you remind me what package to start with? Need to decide which ranges are meaningful, since most schools have very few exemptions.


# Export data to Rds ------------------------------------------------------

write_rds(measles_clean_rates,
          path = "data/measles-clea-rates.rds")


