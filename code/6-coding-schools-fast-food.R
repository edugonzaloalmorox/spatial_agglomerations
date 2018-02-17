#####################
# Schools and fast food classification (ratings.food.gov.uk)
# November 2017
# @Edu Gonzalo
########################


library(rgdal)
library(foreign)
library(ggplot2)
library(choroplethr)
library(XML)
library(maptools)
library(dplyr)
library(tidyverse)
library(stringr)
library(rio)
library(grid)
library(gridExtra)
library(viridis)
library(readxl)
library (janitor)
library(forcats)
library(Hmisc)
library(labelled)

# Stata data 
df = import("/Users/Personas/Downloads/classifying schools and food.dta")

# csv data
schools =  import("data/processed/schools_distance_unique.csv")

#######################
# clean names: Schools
#######################


schools = schools  %>% mutate(from = tolower(from_id))

# select unique names of schools
      test = schools %>%  group_by(from) %>% select(from, from_postcode) %>% unique()
      
      test = test %>% mutate(from_est = ifelse(str_detect(from, 'nursery|childcare|school|primary|kindergarten|children|junior|prim|st|kids|kid|pupil|infant|inf|jun'), "primary", NA), 
                             from_est = ifelse(str_detect(from, 'secondary|high|academy|middle|seconda'), "secondary", from_est), 
                             from_est = ifelse(str_detect(from, 'college|university|campus'), "college", from_est))

# check missing in test
      check = test %>% filter(is.na(from_est))

# link unique names of schools to base dataset with all the information
      schools = left_join(schools, test, by = c("from", "from_postcode"))
      
      # recode some schools
      schools = schools %>% mutate(from_est = ifelse(from == "2go coffee shop", "college", from_est),
                                   from_est =  ifelse(str_detect(from, '6th form'), "secondary", from_est))


# check missing
check = schools %>% filter(is.na(from_est))

length(unique(check$from_id))/length(unique(schools$from_id))  # number of missing information (school whose category is missing)


###########################
# clean names: Restaurants 
###########################

schools = schools  %>% mutate(to = tolower(to_id))

# select unique names of schools
      restaurants = schools %>%  group_by(to) %>% select(to, to_postcode) %>% unique()
      
      restaurants = restaurants %>% mutate(to_est = ifelse(str_detect(to, 'fish|fryer|fried|chippy|plaice|chips|crispy|cod|fry|chip|crust|crusty'), "fish", NA), 
                             to_est = ifelse(str_detect(to, 'chicken|kfc|k f c|piri piri|peri peri|wings|chickie|roosters|burger|mcdonald*|flame|bbq|grill|ribs|pork'), "chicken_burger", to_est), 
                             to_est = ifelse(str_detect(to, 'pizza|pizzeria|papa john*|bella|italia|mamma|hut|domino|italy'), "pizza", to_est), 
                             to_est = ifelse(str_detect(to, 'kebab|istanbul|ali|turkish|shawarma|bodrum|anatolia'), "kebab", to_est), 
                             to_est = ifelse(str_detect(to, 'greggs|subway|pret a|starbucks|costa|nero'), "sandwich", to_est),
                             to_est = ifelse(str_detect(to, 'chinese|china|india|rice|noodle|tandoori|thai|spice|indian|canton|
                                                        cantonese|oriental|dragon|jade garden|shangai|pekin|wok|kong|curry|beijing|lucky house|chopsticks|raj|happy house|bamboo|lucky star|mandarin|bombay|orient'),
                                             "asian", to_est))
      
# link with base dataset
      schools = left_join(schools, restaurants, by = c("to", "to_postcode"))

# recode as "other" those restaurants that are difficult to classify
    schools = schools %>% mutate(to_est = ifelse(is.na(to_est), "other", to_est))

write.csv(schools, "data/processed/schools_distance_unique.csv", row.names = FALSE)

########################
# Create dummies
########################

library(dummies)

# new dataset: df_schools
      df_schools<- cbind(schools, dummy(schools$from_est, sep = "_"))
      df_schools<- cbind(df_schools, dummy(df_schools$to_est, sep = "_"))

# clean up names
      names(df_schools) = gsub("schools_", "",  names(df_schools))
      names(df_schools) = gsub("df_", "",  names(df_schools))


      df_schools = df_schools %>% 
        rename(undetermined = `NA`) %>%
        select(-from, -to)
     
      
 write.csv(df_schools, "data/processed/schools_distance_unique.csv", row.names = FALSE)

##############
# Labels 
###############
 
 schools =  import("data/processed/schools_distance_unique.csv")
 

 
  var.labels = c(from_id = "id food establishment origin (academic)",
  to_id = "id food establishment destiny (takeaway)",
  from_business =  "business type (origin)",
  to_business = "business type (destiny)",
  from_postcode = "postcode origin",
  to_postcode = "postcode destiny",
  distance  = "distance between origin and destiny (km)",
  region ="region",
  pcd2from  = "postal code establishment origin",
  pcd2to   = "postal code establishment destiny",      
 from_rating =  "rating establishment origin",
 scores = "scores rating establishment origin",
 to_rating =  "rating establishment destiny",
date = "date inspection",
local_authority = "local authority origin",
lat_from = "latitude establishment origin",
long_from = "longitude establishment origin",
lsoa_from = "lsoa establishment origin",
msoa_from = "msoa establishment origin", 
ttwa_from = "time to travel establishment origin",      
oslaua_from = "district code establishment origin", 
imd_from = "imd score establishment origin (lsoa level)",
from_est =  "category establishment origin" ,
to_est =  "category establishment destiny",
college = "establishment origin is a college",
primary = "establishment origin is a primary",
secondary = "establishment origin is a secondary",
undetermined = "no information about establishment origin",
asian = "destiny establishment is asian (chinese or indian)",
chicken_burger  = "destiny establishment burger or similar",
fish = "destiny establishment is fish and chips", 
kebab = "destiny establishment is kebab",
other = "destiny establishment is general takeaway", 
pizza = "destiny establishment is pizza", 
sandwich = "sandwich")
 
  export(schools, "data/processed/schools_food_outlets.dta")

 