############################
# Prepare sample of analysis 
# Information based on each restaurant in an education premise that has a takeaway around 1.5 km or less
# Running model 2 - Ratings and neighbouring ratings
# February 2018 
# @EduGonzalo
##################################


library(tidyverse)
library(rio)
library(sandwich)
library(MASS)
library(margins)
library(ordinal)
library(broom)
library(margins)
library(janitor)
library(forcats)
library(stringr)
library(ggplot2)
library(dummies)

install.packages("forcats")

###########
# 1.5 Km 
##############

schools = import("data/processed/schools_foods_outlets.csv")

schools_short = schools %>% filter(distance <= 1.5)

test_schools =  schools_short %>% 
  group_by(from_id) %>%
  filter(distance == min(distance))


test_schools= test_schools %>%
  group_by(from_id) %>% 
  mutate(n_nclose_neigh = n()) 


# lump categories 5,4, other

test_schools = test_schools %>%
  filter(!from_rating %in% c("AwaitingInspection", "Exempt")) %>% 
  filter(!to_rating %in% c("AwaitingInspection", "Exempt")) %>% 
  mutate_at(vars(from_rating, to_rating), funs(as.factor)) %>%
  mutate(rating_from_recoded= ifelse(!from_rating %in% c("4", "5"), "3", from_rating )) %>%
  mutate(rating_to_recoded= ifelse(!to_rating %in% c("4", "5"), "3", to_rating ))


test_schools = test_schools %>%
  dplyr::mutate(rating_from_recoded= recode(rating_from_recoded, `5`= "rating_5",
                                       `4`= "rating_4", 
                                       `3`= "rating_3"))

test_schools = test_schools %>%
  dplyr::mutate(rating_to_recoded= recode(rating_to_recoded, `5`= "rating_5",
                                            `4`= "rating_4", 
                                            `3`= "rating_3"))



test_schools = import( "data/processed/clean_schools_v2.csv")

# recode from rating
test_schools<- cbind(test_schools,
                      dummy(test_schools$rating_from_recoded, sep = "_"))



names(test_schools) = gsub("test_schools_rating", "rating_from", names(test_schools))

# recode to rating
test_schools<- cbind(test_schools,
                      dummy(test_schools$rating_to_recoded, sep = "_"))

names(test_schools) = gsub("test_schools_rating", "rating_to", names(test_schools))


# recode region

test_schools<- cbind(test_schools,
                      dummy(test_schools$region, sep = "_"))

names(test_schools) = gsub("test_schools", "region", names(test_schools))


population = import("data/processed/population.csv")

test_schools_beta = left_join(test_schools, population, by = "oslaua")

write.csv(test_schools_beta, "data/processed/clean_schools_v2.csv", row.names = FALSE )
