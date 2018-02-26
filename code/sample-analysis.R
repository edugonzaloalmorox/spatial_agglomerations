############################
# Prepare sample of analysis 
# Information based on each restaurant in an education premise that has a takeaway around 1.5 km or less
# Running model 1 - Ratings and local characteristics
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
      summarise(mean_distance = mean(distance),
                n_takeaways = n())
    
    # number of places that don´t have a takeaway closeby  
      schools_total = unique(unique(schools$from_id))
    
      schools_shortsam = unique(unique(schools_short$from_id))
    
      not_inclu = setdiff(schools_total, schools_shortsam)

      check = schools %>% filter(from_id %in% not_inclu)
      

# proportion of with each rating
      
      test_asian = schools_short %>%
        group_by(from_id) %>% 
        filter(asian == 1) %>%
        summarise(n_asian = n())
      
      test_chicken_burger = schools_short %>%
        group_by(from_id) %>% 
        filter(chicken_burger == 1) %>%
        summarise(n_chicken = n())
      
      test_fish = schools_short %>%
        group_by(from_id) %>% 
        filter(fish == 1) %>%
        summarise(n_fish= n())
      
      test_kebab = schools_short %>%
        group_by(from_id) %>% 
        filter(kebab == 1) %>%
        summarise(n_kebab = n())
      
      test_other = schools_short %>%
        group_by(from_id) %>% 
        filter(other == 1) %>%
        summarise(n_other = n())
      
      test_pizza = schools_short %>%
        group_by(from_id) %>% 
        filter(pizza == 1) %>%
        summarise(n_pizza = n())
      
      test_sandwich = schools_short %>%
        group_by(from_id) %>% 
        filter(sandwich == 1) %>%
        summarise(n_sandwich = n())
      

test_schools_beta = left_join(test_schools, test_asian, by = "from_id") %>%
  left_join(test_chicken_burger, by = "from_id" ) %>%
  left_join(test_fish, by = "from_id" ) %>%
  left_join(test_kebab, by = "from_id" ) %>%
  left_join(test_other, by = "from_id" ) %>% 
  left_join(test_pizza, by = "from_id" ) %>% 
  left_join(test_sandwich, by = "from_id") 


school_vars = schools_short %>%
  dplyr::select(from_id, from_postcode, region, from_rating, date, local_authority.x, lsoa_from:imd_from, imd_decile, jsa_number, jsa_rate) %>%
  unique() %>%
  filter(from_id %in% unique(test_schools_beta$from_id))


test_schools_beta_v2 = left_join(test_schools_beta, school_vars, by = "from_id") 

# drop places that are the same premise but have different postcode
clean_schools = test_schools_beta_v2 %>%
  group_by(from_id) %>%
  distinct(fromd_id, distance, .keep_all = TRUE)


# lump categories 5,4, other

clean_schools = clean_schools %>%
  filter(!from_rating %in% c("AwaitingInspection", "Exempt")) %>% 
  mutate_at(vars(from_rating), funs(as_factor)) %>%
  mutate(rating_recoded= ifelse(!from_rating %in% c("4", "5"), "3", from_rating ))
        
         
     clean_schools = clean_schools %>%
          mutate(rating_recoded= fct_recode(rating_recoded, c(rating_5 = "5", 
                                     rating_4 = "4", 
                                     rating_3 = "3")))


write.csv(clean_schools, "data/processed/clean_schools.csv", row.names = FALSE)

# create dummies
 clean_schools<- cbind(clean_schools,
                           dummy(clean_schools$rating_recoded, sep = "_"))
 


 names(clean_schools) = gsub("clean_schools", "rating", names(clean_schools))


 clean_schools<- cbind(clean_schools,
                      dummy(clean_schools$region, sep = "_"))

 names(clean_schools) = gsub("clean_schools", "region", names(clean_schools))


 
# complete NAs
clean_schools = clean_schools %>% complete(from_id, fill = list(n_asian = 0, 
                                                n_chicken = 0, 
                                                n_fish = 0, 
                                                n_kebab = 0, 
                                                n_other = 0, 
                                                n_pizza = 0, 
                                                n_sandwich = 0))

clean_schools = clean_schools %>%
  mutate(prop_asian = n_asian/n_takeaways,
         prop_chicken = n_chicken/n_takeaways,
         prop_fish = n_fish/n_takeaways,
         prop_kebab = n_kebab/n_takeaways,
         prop_other = n_other/n_takeaways,
         prop_pizza = n_pizza/n_takeaways,
         prop_sandwich = n_sandwich/n_takeaways)


write.csv(clean_schools, "data/processed/clean_schools.csv", row.names = FALSE)

##########################
# 500m 
##########################

schools = import("data/processed/schools_foods_outlets.csv")

schools_short = schools %>% filter(distance <= 0.5)

test_schools =  schools_short %>% 
  group_by(from_id) %>%
  summarise(mean_distance = mean(distance),
            n_takeaways = n())

# number of places that don´t have a takeaway closeby  
schools_total = unique(unique(schools$from_id))

schools_shortsam = unique(unique(schools_short$from_id))

not_inclu = setdiff(schools_total, schools_shortsam)

check = schools %>% filter(from_id %in% not_inclu)


# proportion of with each rating

test_asian = schools_short %>%
  group_by(from_id) %>% 
  filter(asian == 1) %>%
  summarise(n_asian = n())

test_chicken_burger = schools_short %>%
  group_by(from_id) %>% 
  filter(chicken_burger == 1) %>%
  summarise(n_chicken = n())

test_fish = schools_short %>%
  group_by(from_id) %>% 
  filter(fish == 1) %>%
  summarise(n_fish= n())

test_kebab = schools_short %>%
  group_by(from_id) %>% 
  filter(kebab == 1) %>%
  summarise(n_kebab = n())

test_other = schools_short %>%
  group_by(from_id) %>% 
  filter(other == 1) %>%
  summarise(n_other = n())

test_pizza = schools_short %>%
  group_by(from_id) %>% 
  filter(pizza == 1) %>%
  summarise(n_pizza = n())

test_sandwich = schools_short %>%
  group_by(from_id) %>% 
  filter(sandwich == 1) %>%
  summarise(n_sandwich = n())


test_schools_beta = left_join(test_schools, test_asian, by = "from_id") %>%
  left_join(test_chicken_burger, by = "from_id" ) %>%
  left_join(test_fish, by = "from_id" ) %>%
  left_join(test_kebab, by = "from_id" ) %>%
  left_join(test_other, by = "from_id" ) %>% 
  left_join(test_pizza, by = "from_id" ) %>% 
  left_join(test_sandwich, by = "from_id") 


school_vars = schools_short %>%
  dplyr::select(from_id, from_postcode, region, from_rating, date, local_authority.x, lsoa_from:imd_from, imd_decile, jsa_number, jsa_rate) %>%
  unique() %>%
  filter(from_id %in% unique(test_schools_beta$from_id))


test_schools_beta_v2 = left_join(test_schools_beta, school_vars, by = "from_id") 

# drop places that are the same premise but have different postcode
clean_schools = test_schools_beta_v2 %>%
  group_by(from_id) %>%
  distinct(fromd_id, distance, .keep_all = TRUE)


# lump categories 5,4, other

clean_schools = clean_schools %>%
  filter(!from_rating %in% c("AwaitingInspection", "Exempt")) %>% 
  mutate_at(vars(from_rating), funs(as.factor)) %>%
  mutate(rating_recoded= ifelse(!from_rating %in% c("4", "5"), "3", from_rating ))


clean_schools = clean_schools %>%
  dplyr::mutate(rating_recoded= recode(rating_recoded, `5`= "rating_5",
                                                         `4`= "rating_4", 
                                                         `3`= "rating_3"))


write.csv(clean_schools, "data/processed/clean_schools_500.csv", row.names = FALSE)


clean_schools =  import("data/processed/clean_schools_500.csv")


# create dummies
library(dummies)
clean_schools<- cbind(clean_schools,
                      dummy(clean_schools$rating_recoded, sep = "_"))



names(clean_schools) = gsub("rating_rating", "rating", names(clean_schools))


clean_schools<- cbind(clean_schools,
                      dummy(clean_schools$region, sep = "_"))

names(clean_schools) = gsub("clean_schools", "region", names(clean_schools))



# complete NAs
clean_schools = clean_schools %>% complete(from_id, fill = list(n_asian = 0, 
                                                                n_chicken = 0, 
                                                                n_fish = 0, 
                                                                n_kebab = 0, 
                                                                n_other = 0, 
                                                                n_pizza = 0, 
                                                                n_sandwich = 0))

clean_schools = clean_schools %>%
  mutate(prop_asian = n_asian/n_takeaways,
         prop_chicken = n_chicken/n_takeaways,
         prop_fish = n_fish/n_takeaways,
         prop_kebab = n_kebab/n_takeaways,
         prop_other = n_other/n_takeaways,
         prop_pizza = n_pizza/n_takeaways,
         prop_sandwich = n_sandwich/n_takeaways)


write.csv(clean_schools, "data/processed/clean_schools_500.csv", row.names = FALSE)

########################
# 250 m
#########################

schools = import("data/processed/schools_foods_outlets.csv")

schools_short = schools %>% filter(distance <= 0.25)

test_schools =  schools_short %>% 
  group_by(from_id) %>%
  summarise(mean_distance = mean(distance),
            n_takeaways = n())

# number of places that don´t have a takeaway closeby  
schools_total = unique(unique(schools$from_id))

schools_shortsam = unique(unique(schools_short$from_id))

not_inclu = setdiff(schools_total, schools_shortsam)

check = schools %>% filter(from_id %in% not_inclu)


# proportion of with each rating

test_asian = schools_short %>%
  group_by(from_id) %>% 
  filter(asian == 1) %>%
  summarise(n_asian = n())

test_chicken_burger = schools_short %>%
  group_by(from_id) %>% 
  filter(chicken_burger == 1) %>%
  summarise(n_chicken = n())

test_fish = schools_short %>%
  group_by(from_id) %>% 
  filter(fish == 1) %>%
  summarise(n_fish= n())

test_kebab = schools_short %>%
  group_by(from_id) %>% 
  filter(kebab == 1) %>%
  summarise(n_kebab = n())

test_other = schools_short %>%
  group_by(from_id) %>% 
  filter(other == 1) %>%
  summarise(n_other = n())

test_pizza = schools_short %>%
  group_by(from_id) %>% 
  filter(pizza == 1) %>%
  summarise(n_pizza = n())

test_sandwich = schools_short %>%
  group_by(from_id) %>% 
  filter(sandwich == 1) %>%
  summarise(n_sandwich = n())


test_schools_beta = left_join(test_schools, test_asian, by = "from_id") %>%
  left_join(test_chicken_burger, by = "from_id" ) %>%
  left_join(test_fish, by = "from_id" ) %>%
  left_join(test_kebab, by = "from_id" ) %>%
  left_join(test_other, by = "from_id" ) %>% 
  left_join(test_pizza, by = "from_id" ) %>% 
  left_join(test_sandwich, by = "from_id") 


school_vars = schools_short %>%
  dplyr::select(from_id, from_postcode, region, from_rating, date, local_authority.x, lsoa_from:imd_from, imd_decile, jsa_number, jsa_rate) %>%
  unique() %>%
  filter(from_id %in% unique(test_schools_beta$from_id))


test_schools_beta_v2 = left_join(test_schools_beta, school_vars, by = "from_id") 

# drop places that are the same premise but have different postcode
clean_schools = test_schools_beta_v2 %>%
  group_by(from_id) %>%
  distinct(fromd_id, distance, .keep_all = TRUE)


# lump categories 5,4, other

clean_schools = clean_schools %>%
  filter(!from_rating %in% c("AwaitingInspection", "Exempt")) %>% 
  mutate_at(vars(from_rating), funs(as.factor)) %>%
  mutate(rating_recoded= ifelse(!from_rating %in% c("4", "5"), "3", from_rating ))


clean_schools = clean_schools %>%
  dplyr::mutate(rating_recoded= recode(rating_recoded, `5`= "rating_5",
                                       `4`= "rating_4", 
                                       `3`= "rating_3"))


write.csv(clean_schools, "data/processed/clean_schools_250.csv", row.names = FALSE)


clean_schools =  import("data/processed/clean_schools_250.csv")


# create dummies
library(dummies)
clean_schools<- cbind(clean_schools,
                      dummy(clean_schools$rating_recoded, sep = "_"))



names(clean_schools) = gsub("clean_schools_rating", "rating", names(clean_schools))


clean_schools<- cbind(clean_schools,
                      dummy(clean_schools$region, sep = "_"))

names(clean_schools) = gsub("clean_schools", "region", names(clean_schools))



# complete NAs
clean_schools = clean_schools %>% complete(from_id, fill = list(n_asian = 0, 
                                                                n_chicken = 0, 
                                                                n_fish = 0, 
                                                                n_kebab = 0, 
                                                                n_other = 0, 
                                                                n_pizza = 0, 
                                                                n_sandwich = 0))

clean_schools = clean_schools %>%
  mutate(prop_asian = n_asian/n_takeaways,
         prop_chicken = n_chicken/n_takeaways,
         prop_fish = n_fish/n_takeaways,
         prop_kebab = n_kebab/n_takeaways,
         prop_other = n_other/n_takeaways,
         prop_pizza = n_pizza/n_takeaways,
         prop_sandwich = n_sandwich/n_takeaways)


write.csv(clean_schools, "data/processed/clean_schools_250.csv", row.names = FALSE)


########################
# 1km
#########################

schools = import("data/processed/schools_foods_outlets.csv")

schools_short = schools %>% filter(distance <= 1)

test_schools =  schools_short %>% 
  group_by(from_id) %>%
  summarise(mean_distance = mean(distance),
            n_takeaways = n())

# number of places that don´t have a takeaway closeby  
schools_total = unique(unique(schools$from_id))

schools_shortsam = unique(unique(schools_short$from_id))

not_inclu = setdiff(schools_total, schools_shortsam)

check = schools %>% filter(from_id %in% not_inclu)


# proportion of with each rating

test_asian = schools_short %>%
  group_by(from_id) %>% 
  filter(asian == 1) %>%
  summarise(n_asian = n())

test_chicken_burger = schools_short %>%
  group_by(from_id) %>% 
  filter(chicken_burger == 1) %>%
  summarise(n_chicken = n())

test_fish = schools_short %>%
  group_by(from_id) %>% 
  filter(fish == 1) %>%
  summarise(n_fish= n())

test_kebab = schools_short %>%
  group_by(from_id) %>% 
  filter(kebab == 1) %>%
  summarise(n_kebab = n())

test_other = schools_short %>%
  group_by(from_id) %>% 
  filter(other == 1) %>%
  summarise(n_other = n())

test_pizza = schools_short %>%
  group_by(from_id) %>% 
  filter(pizza == 1) %>%
  summarise(n_pizza = n())

test_sandwich = schools_short %>%
  group_by(from_id) %>% 
  filter(sandwich == 1) %>%
  summarise(n_sandwich = n())


test_schools_beta = left_join(test_schools, test_asian, by = "from_id") %>%
  left_join(test_chicken_burger, by = "from_id" ) %>%
  left_join(test_fish, by = "from_id" ) %>%
  left_join(test_kebab, by = "from_id" ) %>%
  left_join(test_other, by = "from_id" ) %>% 
  left_join(test_pizza, by = "from_id" ) %>% 
  left_join(test_sandwich, by = "from_id") 


school_vars = schools_short %>%
  dplyr::select(from_id, from_postcode, region, from_rating, date, local_authority.x, lsoa_from:imd_from, imd_decile, jsa_number, jsa_rate) %>%
  unique() %>%
  filter(from_id %in% unique(test_schools_beta$from_id))


test_schools_beta_v2 = left_join(test_schools_beta, school_vars, by = "from_id") 

# drop places that are the same premise but have different postcode
clean_schools = test_schools_beta_v2 %>%
  group_by(from_id) %>%
  distinct(fromd_id, distance, .keep_all = TRUE)


# lump categories 5,4, other

    clean_schools = clean_schools %>%
      filter(!from_rating %in% c("AwaitingInspection", "Exempt")) %>% 
      mutate_at(vars(from_rating), funs(as.factor)) %>%
      mutate(rating_recoded= ifelse(!from_rating %in% c("4", "5"), "3", from_rating ))
    
    
    clean_schools = clean_schools %>%
      dplyr::mutate(rating_recoded= recode(rating_recoded, `5`= "rating_5",
                                           `4`= "rating_4", 
                                           `3`= "rating_3"))
    
    
    write.csv(clean_schools, "data/processed/clean_schools_1km.csv", row.names = FALSE)
    
    
    clean_schools =  import("data/processed/clean_schools_1km.csv")


# create dummies for categorical variable
library(dummies)
      clean_schools<- cbind(clean_schools,
                            dummy(clean_schools$rating_recoded, sep = "_"))
      
      
      
      names(clean_schools) = gsub("clean_schools_rating", "rating", names(clean_schools))
      
      
      clean_schools<- cbind(clean_schools,
                            dummy(clean_schools$region, sep = "_"))
      
      names(clean_schools) = gsub("clean_schools", "region", names(clean_schools))
      
      
      
      # complete NAs
      clean_schools = clean_schools %>% complete(from_id, fill = list(n_asian = 0, 
                                                                      n_chicken = 0, 
                                                                      n_fish = 0, 
                                                                      n_kebab = 0, 
                                                                      n_other = 0, 
                                                                      n_pizza = 0, 
                                                                      n_sandwich = 0))

clean_schools = clean_schools %>%
  mutate(prop_asian = n_asian/n_takeaways,
         prop_chicken = n_chicken/n_takeaways,
         prop_fish = n_fish/n_takeaways,
         prop_kebab = n_kebab/n_takeaways,
         prop_other = n_other/n_takeaways,
         prop_pizza = n_pizza/n_takeaways,
         prop_sandwich = n_sandwich/n_takeaways)


write.csv(clean_schools, "data/processed/clean_schools_1km.csv", row.names = FALSE)

#########################
# Link population data
# #####################

population = import("data/processed/population.csv")


      clean_schools_1km_test = left_join(clean_schools_1km, population, by = c("oslaua_from" = "oslaua"))
      
      clean_schools_1_5km_test = left_join(clean_schools_1_5km, population, by = c("oslaua_from" = "oslaua")) 
      
      clean_schools_5km_test = left_join(clean_schools_5km, population, by = c("oslaua_from" = "oslaua"))
      
      clean_schools_25km_test = left_join(clean_schools_25km, population, by = c("oslaua_from" = "oslaua")) 

write.csv(clean_schools_1km_beta, "data/processed/clean_schools_1km.csv", row.names = FALSE)
write.csv(clean_schools_1_5km_beta, "data/processed/clean_schools_1_5km.csv", row.names = FALSE)
write.csv(clean_schools_5km_beta, "data/processed/clean_schools_500m.csv", row.names = FALSE)





