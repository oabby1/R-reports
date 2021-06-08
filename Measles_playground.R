

# -----Load Packages ------------------------------------------------

library(tidyverse)
library(janitor)
library(readxl)
library(skimr)
library(readr)
library(sf)
library(tigris)
library(plotly)

#YAML for saving as PDF
# knit: pagedown::chrome_print
# output:
#   pagedown::html_paged: 
#   toc: TRUE
# number_sections: FALSE

#HTML
# output: 
#   html_document:
#   df_print: kable
# toc: TRUE

#Slides (use --- to break each slide)
# output:
#   xaringan::moon_reader

# ----Download file ------------------------------------------------

# download.file(url = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv",
#             destfile = "data-raw-top/measles.csv")

# Import Data -------------------------------------------------------------

measles <- read_csv("data-raw-top/measles.csv")
View(measles)

# Clean Data --------------------------------------------------------------

#Take out negative rates, replace with NA
measles_clean_rates <- measles %>% 
  select(state, name, type, enroll, mmr, overall, lng, lat) %>% 
  mutate(overall = ifelse(overall < 0, NA, overall)) %>% 
  mutate(mmr = ifelse(mmr < 0, NA, mmr)) %>% 
  distinct()

# #CHARLIE here is the new dataframe to use for rejoining lat/lng later (43,233 rows);
# #I took out negative mmr rates and replace w NA, took out lat/lng, removed duplicates
# measles_clean_rates_2 <- measles %>% 
#   select(state, name, type, enroll, mmr, overall) %>% 
#   mutate(overall = ifelse(overall < 0, NA, overall)) %>% 
#   mutate(mmr = ifelse(mmr < 0, NA, mmr)) %>% 
#   distinct()
# 
# #CHARLIE HELP here: how to add lat/long back in to just these 43,233 rows? below gives me 64426. Then I'll rename as measles_clean_rates so it updates all my other analyses. 
# measles_clean_rates_2 %>% 
#   left_join(clean_schools_mapping)



#Not going to analyze by year: 484+1515(WI only) schools reported for 2018-19, 567 schools reported for 2017-18, 1567 null (NA). 
#The index ID assigned is not unique. Ignoring. Not interested by school. 
 
#there are 109 religious exemptions, but as character (TRUE) in the dataset, not numeric. cannot be summarized with other exemptions!!
measles %>% 
  count(xrel == "TRUE")
#what can I found out about those 109 exemptions?
measles_religious <- measles %>% 
  select(state, name, type, enroll, mmr, overall, xrel) %>% 
  filter(xrel == "TRUE") %>% 
  mutate(overall = ifelse(overall < 0, NA, overall)) %>% 
  mutate(mmr = ifelse(mmr < 0, NA, mmr)) %>% 
  distinct()
#They don't seem to impact school vax rates - still very high. Except one school in WA. 
#how can there by TRUE exemptions with 100% overall rates? (NC). I feel good about leaving these out after looking briefly.

#Combining exemptions and renaming. Removed xrel (religious) because I have no idea what numeric value would be. Reported as TRUE.
measles_exemptions <- measles %>% 
  filter(xper<=100) %>% 
  select (state, name, type, enroll, xper, xmed) %>% 
   pivot_longer(cols = xper:xmed, 
               names_to = "exemption_type",
               values_to = "percent") %>% 
   mutate(exemption_type = case_when(
     exemption_type == "xper" ~ "personal",
     exemption_type == "xmed" ~ "medical",
     TRUE ~ exemption_type)) %>%
  mutate(exempt_ranges = case_when(
    percent < 1 ~ "Less than 1",
    percent < 5 ~ "2 to 5",
    percent < 10 ~ "6 to 10",
    percent < 25 ~ "11 to 25",
    percent < 50 ~ "26 to 50",
    percent > 50 ~ "More than 50")) %>% 
  drop_na(percent) %>% 
       distinct()

#Added number of students with exemptions, then rounded
#Is it a problem that so many numbers are partial, not even close to a whole number? Is it fair to round those to later use as sum or new %?
measles_exemptions_number <- measles_exemptions %>% 
  #group_by(state, type, exemption_type) %>%
  mutate(number_of_exempt_students = (percent*enroll)/100) %>% 
  mutate(number_of_exempt_students = round(number_of_exempt_students, digits = 0))

#Note: Bummer that so many schools didn't report enrollment, so I can only calculate 4692/7316 numbers. Still, more than half. 

# Export data to top-level data folder Rds ------------------------------------------------------

write_rds(measles_clean_rates,
          path = "data-top/measles-clean-rates.rds")

write_rds(measles_exemptions_number,
          path = "data-top/measles-exemptions.rds")


# About reporting schools (clean rates) - for RMD file ---------------------------------------------------

##Number of Schools Reporting Vaccination Data, by State
measles_clean_rates %>% 
  count(state) %>% 
  arrange(n)

#add total n
measles_clean_rates %>% 
  count()

#schools by type
measles_clean_rates %>% 
  count(type) %>% 
  drop_na(type) %>% 
  arrange(desc(n))

measles_clean_rates %>% 
  ggplot(aes(x = mmr)) +
  geom_histogram(bins = 100)

#something about enrollment distribution (small class sizes)
measles_clean_rates %>% 
  drop_na(type) %>% 
  ggplot(aes(x = enroll, fill = enroll)) +
  geom_bar(show.legend = FALSE) +
  scale_fill_brewer(palette = "Set3")

# Average Vaccination Rates Among Reporting Schools -----------------------

#average rates
mean_mmr_rate <- measles_clean_rates %>% 
  summarize(mean_mmr_rate = mean(mmr, na.rm = TRUE)) 
#round to 1 decimal?

mean_allvax_rate <- measles_clean_rates %>% 
  summarize(mean_allvax_rate = mean(overall, na.rm = TRUE)) 

#Create new dataframes - means by state
mean_mmr_state <- measles_clean_rates %>% 
  group_by(state) %>% 
  summarize(mean_mmr_rate = mean(mmr, na.rm = TRUE)) %>% 
  drop_na() %>% 
  arrange(desc(mean_mmr_rate))

mean_overall_state <- measles_clean_rates %>% 
  group_by(state) %>% 
  summarize(mean_allvax_rate = mean(overall, na.rm = TRUE)) %>% 
  drop_na() %>% 
  arrange(desc(mean_allvax_rate))

#mean rates by type
mean_mmr_by_type <- measles_clean_rates %>% 
  group_by(type) %>% 
  drop_na(type) %>% 
  summarize(mean_mmr_rate = mean(mmr, na.rm = TRUE)) %>% 
  arrange(desc(mean_mmr_rate))

mean_overall_by_type <- measles_clean_rates %>% 
  group_by(type) %>% 
  drop_na(type) %>% 
  summarize(mean_overall_rate = mean(overall, na.rm = TRUE)) %>% 
  arrange(desc(mean_overall_rate))

#join above tables
mean_mmr_by_type %>% 
  left_join(mean_overall_by_type)

#plot mean US rate vline and label: USE IN RMD
ggplot(data = mean_mmr_state,
       mapping = aes(x = mean_mmr_rate, y = state))+
  geom_bar(stat = "identity") +
  geom_vline(xintercept = mean(mean_mmr_state$mean_mmr_rate),
             linetype = "dashed") +
  annotate("text",
           x = mean(mean_mmr_state$mean_mmr_rate) + 5,
           y = nrow(mean_mmr_state) / 2,
           colour = "black",
           label = paste0("Mean Rate (", round(mean(mean_mmr_state$mean_mmr_rate)), ")"),
           angle = 90)


# Exemptions --------------------------------------------------------------


#plot exemption ranges: *USE IN RMD*
measles_exemptions %>%   
  count(state, exempt_ranges) %>% 
  drop_na(exempt_ranges) %>% 
  mutate(exempt_ranges = fct_relevel(exempt_ranges, c("Less than 1", "2 to 5", "6 to 10", "11 to 25", "26 to 50", "More than 50"))) %>% 
  ggplot(aes(x = n,
         y = exempt_ranges,
         fill = state)) +
  geom_col(show.legend = TRUE) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Percent of Personal or Medical Vaccine Exemptions, by State",
       x = "Number of Schools",
       y = "Ranges by Percent") +
  theme_ai() 
  

#REORDER Y scale

# All Schools with personal Exemptions = 6312, medical 1003 (rel 109, or 0.23%)

measles_exemptions %>% 
  count(exemption_type == "personal") 

measles_exemptions %>% 
  count(exemption_type == "medical") 

109/46243

#measles_exemptions %>% 
 # count(exemption_type == "religious")

#%>% 
 # mutate(n = 100 * n / sum(n))


#Percent of MMR Exemptions by State and School Type 

measles_exemptions %>% 
  group_by(state) %>% 
  count(state,type,exemption_type)


#Exemptions by State 

measles_exemptions %>% 
  group_by(state) %>% 
  count(state, exemption_type) 

#plot personal exemptions

ggplot(data = measles_exemptions, 
       mapping = aes(x = state, 
                     y = exemption_type,
                     fill = state)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~exemption_type) +
  coord_flip() +
  scale_fill_brewer(palette = "Set3")
#fix!!
  

# Low Rate State Analyses ----------------------------------------------------------

# Arrange state MMR rates from low to high (focus below 90%)
mean_mmr_state %>% 
  arrange(mean_mmr_rate)

# overall low to high
mean_overall_state %>% 
  arrange(mean_allvax_rate)

# Look at rates in WA (no enrollment data)

WA_clean_rates <- measles %>% 
  filter(state == "Washington") %>% 
  select(state, name, county, type, mmr, overall) %>% 
  mutate(mmr = ifelse(overall < 0, NA, overall)) %>% 
  mutate(mmr = ifelse(mmr < 0, NA, mmr)) %>% 
  distinct()

#mmr rate by county



WA_exempt <- measles_exemptions %>% 
  filter(state == "Washington")


# Rates in AR: Lowest MMR average. Arkansas isn't in the exemptions dataframe? No exemptions or overall rates. 
AR_clean_rates <- measles %>% 
  filter(state == "Arkansas") %>% 
  select(state, name, county, type, enroll, mmr) %>% 
  mutate(mmr = ifelse(mmr < 0, NA, mmr)) %>% 
  distinct()

#histogram mmr rates
AR_clean_rates %>%  
  ggplot(aes(x = mmr)) +
  geom_histogram(bins = 100)

AR_clean_rates %>%  
  ggplot(aes(x = enroll)) +
  geom_histogram(bins = 100)


# Visualizations extra ----------------------------------------------------

##Scatterplot of vax rate and enroll. Need to make new dataframe with exempt if wanted.

#mmr by enrollment
ggplot(data = measles_clean_rates,
       mapping = aes(x = mmr, y = enroll, color = overall)) +
  geom_point() +
  facet_wrap(~type)

#overall by enrollment
ggplot(data = measles_clean_rates,
       mapping = aes(x = enroll, y = overall, color = mmr)) +
  geom_point() +
  coord_flip() +
  facet_wrap(~type)

##Histograms (new variables)

measles %>% 
  filter(xper <= 100) %>% 
  ggplot(aes(x = xper)) +
  geom_histogram(bins = 100)


#Bar Chart v2 MMR by school type - USE

# mmr_by_type <- measles_clean_rates %>% 
#   drop_na(type) %>% 
#   group_by(type) %>% 
#   summarize(mean_mmr = mean(mmr, na.rm = TRUE))

mean_mmr_by_type_1 <- mean_mmr_by_type %>% 
   mutate(mmr_type_one_dig = format(round(mean_mmr_rate, 1), nsmall = 1))

ggplot(data = mean_mmr_by_type_1,
       mapping = aes(x = type, y= mean_mmr_rate, fill = type)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_brewer(palette = "Set3") +
  geom_text(aes(label = mmr_type_one_dig),
            vjust = 1.5,
            show.legend = FALSE,
            color = "black") +
  labs(title = "Average MMR Vaccination Rates by School Type",
       x = "School Type",
       y = "Average MMR Rate") +
  theme_ai()


# Mapping clean schools

counties_sf <- counties(state = 53)

clean_schools_mapping <- measles_clean_rates %>% 
  drop_na(lat,lng) %>% 
  mutate(lat_from_lng = lng,
         long_from_lat = lat) %>% 
  st_as_sf(coords = c("lat_from_lng", "long_from_lat"), crs = 4326) 


#The columns needs to be swapped around:

clean_schools_mapping %>% 
  rename(latitude = lng,
         longitude = lat) %>% 
  drop_na(latitude, longitude) %>% 
  st_as_sf(coords = c("latitude", "longitude"), crs = 4326) %>% 
  ggplot() +
  geom_sf()
  
# Mapping schools with MMR rates < 95%

clean_schools_mapping %>% 
  filter(mmr < 95) %>% 
  rename(latitude = lng,
         longitude = lat) %>% 
  drop_na(latitude, longitude) %>% 
  st_as_sf(coords = c("latitude", "longitude"), crs = 4326) %>% 
  ggplot() +
  geom_sf() 
  
# Map WA state schools < 95% MMR

washington_sf <- states() %>% 
  filter(NAME == "Washington")

wa_counties_sf <- counties_sf %>% 
  filter(STATEFP == 53)

# wa_schools <- clean_schools_mapping %>% 
#   filter(mmr < 95) %>% 
#   filter(state == "Washington") %>% 
#   rename(latitude = lng,
#          longitude = lat) %>% 
#   drop_na(latitude, longitude) %>% 
#   st_as_sf(coords = c("latitude", "longitude"), crs = 4326) 

#switch the geom_sf layers if removing alpha for color and fill preferences
ggplot() +
  geom_sf(data = wa_schools) +
  geom_sf(data = washington_sf, alpha = 0) +
  labs(title = "60% of WA schools had MMR vaccination rates below 95 percent",
       x = "long",
       y = "lat") +
  theme_minimal() +
  theme(axis.title = element_blank())


# wa_schools <- clean_schools_mapping %>% 
#   filter(mmr < 95) %>% 
#   filter(state == "Washington") %>% 
#   distinct() %>% 
#   rename(latitude = lng,
#          longitude = lat) %>% 
#   drop_na(latitude, longitude) %>% 
#   st_as_sf(coords = c("latitude", "longitude"), crs = 4326) 
# ggplot() +
#   geom_sf(data = wa_schools) +
#   geom_sf(data = wa_counties_sf, alpha = 0) +
#   labs(title = "60% of WA schools had MMR vaccination rates below 95 percent",
#        family = "Calibri",
#        x = "long",
#        y = "lat") +
#   theme_void(base_family = "Calibri") +
#   theme(plot.title = element_text(face = "bold"))

# Map of WA Counties for Rmd
# ggplot() +
#   geom_sf(data = wa_schools) +
#   geom_sf(data = wa_counties_sf, alpha = 0) +
#   labs(title = "60% of WA schools had MMR vaccination rates below 95 percent",
#        x = "long",
#        y = "lat") +
#   theme_void(base_family = "Calibri") +
#   theme(plot.title = element_text(face = "bold"))

  
measles_clean_rates %>% 
  filter(state == "Washington") %>% 
  filter(mmr < 95)

wa_schools <- clean_schools_mapping %>%
  filter(mmr < 95) %>%
  filter(state == "Washington") %>%
  distinct() %>%
  rename(latitude = lng,
         longitude = lat) %>%
  drop_na(latitude, longitude) %>%
  st_as_sf(coords = c("latitude", "longitude"), crs = 4326)

wa_schools_yay <- clean_schools_mapping %>%
  filter(mmr > 95) %>%
  filter(state == "Washington") %>%
  distinct() %>%
  rename(latitude = lng,
         longitude = lat) %>%
  drop_na(latitude, longitude) %>%
  st_as_sf(coords = c("latitude", "longitude"), crs = 4326)

ggplot() +
  geom_sf(data = wa_schools_yay, color = "lightblue") +
  geom_sf(data = wa_schools) +
  geom_sf(data = wa_counties_sf, alpha = 0) +
  labs(title = "WA schools with MMR vaccination rates <span style = 'color: lightblue'>above</span> and below 95 percent",
       family = "Calibri",
       x = "long",
       y = "lat") +
  theme_void(base_family = "Calibri") -> wa_plot
  #theme(plot.title = element_markdown(face = "bold")) 


library(leaflet)

# Start off a map like so!
wa_schools_yay %>% 
  leaflet()

leaflet() %>% 
  addPolygons(data = wa_counties_sf,
              weight = 1,
              color = "black",
              fillColor = "white") %>% 
  addMarkers(data = wa_schools_yay)

# If no tiles then to add a background color
# Use the leaflet.extras package

library(leaflet.extras)

leaflet() %>% 
  # addProviderTiles(providers$Esri.WorldShadedRelief) %>% 
  addPolygons(data = wa_counties_sf,
              weight = 1,
              fillOpacity = 1,
              color = "black",
              fillColor = "white") %>%
  addCircleMarkers(data = wa_schools_yay,
                   stroke = FALSE,
                   fillOpacity = 0.8,
                   radius = 2, 
                   fillColor = "blue",
                   label = ~name
                   ) %>% 
  addCircleMarkers(data = wa_schools,
                   stroke = FALSE,
                   fillColor = "red",
                   fillOpacity = 0.8,
                   radius = 2, 
                   label = ~name
  ) %>% 
  setMapWidgetStyle(style = list(background = "white"))



#CHARLIE how can I add hovering with school names rather than geolocation?
plot_ly(wa_schools_yay)

ggplotly(wa_schools)


1332/2203


#Map AR few schools (7%) reported > 95% MMR
ar_schools_yay <- clean_schools_mapping %>% 
  filter(mmr > 95) %>% 
  filter(state == "Arkansas") %>% 
  rename(latitude = lng,
         longitude = lat) %>% 
  drop_na(latitude, longitude) %>% 
  st_as_sf(coords = c("latitude", "longitude"), crs = 4326) 



ggplot() +
  geom_sf(data = ar_schools_yay) +
  geom_sf(data = arkansas_sf, alpha = 0) +
  labs(title = "Only 2 Arkansas schools had MMR vaccination rates ABOVE 95 percent",
       x = "long",
       y = "lat") +
  theme_ai()

#AR schools below 95
ar_schools_nay <- clean_schools_mapping %>% 
  filter(mmr < 95) %>% 
  filter(state == "Arkansas") %>% 
  rename(latitude = lng,
         longitude = lat) %>% 
  drop_na(latitude, longitude) %>% 
  st_as_sf(coords = c("latitude", "longitude"), crs = 4326) 

ggplot() +
  geom_sf(data = ar_schools_nay) +
  geom_sf(data = arkansas_sf, alpha = 0) +
  labs(title = "99.6% of Arkansas schools had MMR vaccination rates below 95 percent",
       x = "long",
       y = "lat") +
  theme_minimal() +
  theme(axis.title = element_blank())

arkansas_sf <- states() %>% 
  filter(NAME == "Arkansas")

measles_clean_rates %>% 
  filter(state == "Arkansas") %>% 
  filter(mmr < 95)

555/557
2/557
# Code for saving plots? --------------------------------------------------

#install.packages("devtools")
#library(devtools)

#ggsave(filename = "plots/avg-mmr-by-type.png",
 #      height = 5,
  #     width = 8,
   #    units = "in")





