#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from [...UPDATE ME!!!!!]
# Author: Rohan Alexander and Sam Caetano [CHANGE THIS TO YOUR NAME!!!!]
# Data: 22 October 2020
# Contact: rohan.alexander@utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.
setwd("C:/Users/teren/Documents/University of Toronto/Fifth Year/STA304/PS3")
raw_data_post <- read_dta("usa_00003.dta.gz")


# Add the labels
raw_data_post <- labelled::to_factor(raw_data_post)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_data_post <- 
  raw_data_post %>% 
  select(region,
         stateicp,
         sex, 
         age, 
         race, 
         hispan,
         marst, 
         bpl,
         citizen,
         educd,
         labforce,
         inctot)
         

#### What's next? ####

## Here I am only splitting cells by age, but you 
## can use other variables to split by changing
## count(age) to count(age, sex, ....)

reduced_data_post <- 
  reduced_data_post %>%
  #count(region, stateicp, sex, age, race, hispan, marst, bpl, citizen, educd,
        #labforce, inctot) %>%
  #group_by(region, stateicp, sex, age, race, hispan, marst, bpl, citizen, educd,
         #  labforce, inctot) 
  count(labforce, sex, race, hispan, stateicp, age) %>% 
  group_by(labforce, sex, race, hispan, stateicp, age)

reduced_data_post <- 
  reduced_data_post %>%
  rename(gender = sex) %>%
  rename(hispanic = hispan) %>% 
  rename(race_ethnicity = race) %>% 
  rename(employment = labforce) %>% 
  rename(state = stateicp)

reduced_data_post <- 
  reduced_data_post %>% 
  mutate(employment = case_when( 
    employment == "n/a" ~ "No",
    employment == "no, not in the labor force" ~ "No",
    employment == "yes, in the labor force" ~ "Yes"))

reduced_data_post <- 
  reduced_data_post %>% 
  mutate(gender = case_when(
    gender == "male" ~ "Male",
    gender == "female" ~ "Female"
  ))

reduced_data_post <-
  reduced_data_post %>% 
  mutate(hispanic = case_when(
    hispanic == "not hispanic" ~ "Not Hispanic",
    hispanic == "mexican" ~ "Mexican",
    hispanic == "puerto rican" ~ "Puerto Rican",
    hispanic == "cuban" ~ "Cuban",
    hispanic == "other" ~ "Other",
    hispanic == "not reported" ~ "Not Hispanic"
  ))

reduced_data_post <- 
  reduced_data_post %>% 
  mutate(race_ethnicity = case_when(
    race_ethnicity == "white" ~ "White",
    race_ethnicity == 
      "black/african american/negro" ~ "Black, or African American",
    race_ethnicity == "american indian or alaska native" ~ "Native American",
    race_ethnicity == "chinese" ~ "Asian",
    race_ethnicity == "japanese" ~ "Asian",
    race_ethnicity == "other asian or pacific islander" ~ "Pacific Islander",
    race_ethnicity == "other racem nec" ~ "Other",
    race_ethnicity == "two major races" ~ "Other",
    race_ethnicity == "three or more major races" ~ "Other"
  ))

#cleanse the state and age observations


#  filter(age != "less than 1 year old") %>%
#  filter(age != "90 (90+ in 1980 and 1990)") %>% 
#  filter(age != 2) %>% 
#  filter(age != 3) %>% 
#  filter(age != 4) %>% 
#  filter(age != 5) %>%
#  filter(age != 6) %>% 
#  filter(age != 7) %>% 
#  filter(age != 8) %>% 
#  filter(age != 9) %>% 
#  filter(age != 10) %>% 
#  filter(age != 11) %>% 
#  filter(age != 12) %>% 
#  filter(age != 13) %>% 
#  filter(age != 14) %>% 
#  filter(age != 15) %>% 
#  filter(age != 16) %>% 
#  filter(age != 17) %>% 
#  filter(age != 18) %>%
#  filter(age != 94) %>% 
#  filter(age != 95) %>% 
#  filter(age != 96) %>% 
#  filter(age != 97) %>% 
  
reduced_data_post$age <- as.integer(reduced_data_post$age)

# Saving the census data as a csv file in my
# working directory
write_csv(reduced_data_post, "census_data.csv")
























         