
#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.
setwd("C:/Users/teren/Documents/University of Toronto/Fifth Year/STA304/PS3")
raw_data_post <- read_dta("usa_00003.dta.gz")


# Add the labels
raw_data_post <- labelled::to_factor(raw_data_post)


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
         



reduced_data_post <- 
  reduced_data_post %>%

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


  
reduced_data_post$age <- as.integer(reduced_data_post$age)


write_csv(reduced_data_post, "census_data.csv")
























         
