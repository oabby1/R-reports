

# -----Load Packages ------------------------------------------------

library(tidyverse)
library(janitor)
library(readxl)
library(skimr)
library(readr)

# ----Download file ------------------------------------------------

# download.file(url = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv",
#             destfile = "data-raw-top/measles.csv")

# Import Data -------------------------------------------------------------

measles <- read_csv("data-raw-top/measles.csv")
View(measles)

# Clean Data --------------------------------------------------------------

#Take out negative rates, replace with NA
measles_clean_rates <- measles %>% 
  select(state, name, type, enroll, mmr, overall) %>% 
  mutate(overall = ifelse(overall < 0, NA, overall)) %>% 
  mutate(mmr = ifelse(mmr < 0, NA, mmr)) %>% 
  distinct()

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
    percent < 5 ~ "Less than 5",
    percent < 10 ~ "5 to 10",
    percent < 25 ~ "10 to 25",
    percent < 50 ~ "25 to 50",
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

# Exemptions --------------------------------------------------------------


#plot exemption ranges
measles_exemptions %>%   
  count(state, exempt_ranges) %>% 
  drop_na(exempt_ranges) %>% 
  ggplot(aes(x = n,
         y = exempt_ranges,
         fill = state)) +
  geom_col(show.legend = TRUE) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Percent of Personal or Medical Vaccine Exemptions, by State",
       x = "Number",
       y = "Ranges by Percent") +
  theme_classic()

#Percent of All Schools with personal Exemptions = 6312, medical 1003 (rel 109)

measles_exemptions %>% 
  count(exemption_type == "personal") 

measles_exemptions %>% 
  count(exemption_type == "medical") 

109/46243

#measles_exemptions %>% 
 # count(exemption_type == "religious")

#%>% 
 # mutate(n = 100 * n / sum(n))


#Percent of Exemptions by State and School Type 

measles_exemptions %>% 
  group_by(state) %>% 
  count(state,type,exemption_type)


#Exemptions by State 


  


#State analyses
# Arrange state MMR rates from low to high


# visualizations extra ----------------------------------------------------

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
  theme_classic()


# States with Vaccination Rates below 90%
#Arkansas isn't in the exemptions dataframe? Low rates and no exemptions?

# Washington
measles_clean_rates %>% 
  filter(state == "Washington") 
 
  

# Code for saving plots? --------------------------------------------------

#install.packages("devtools")
library(devtools)

ggsave(filename = "plots/avg-mmr-by-type.png",
       height = 5,
       width = 8,
       units = "in")





