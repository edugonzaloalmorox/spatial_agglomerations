#############
# Additional information
# Data referred to August 2017
# Source: NOMIS
# @EduGonzalo
########

library(tidyverse)
library(readxl)
library(janitor)


jsa = read_excel("data/jsa-august-2017.xlsx", sheet = "Data", range = "A9:D335") 
names(jsa) = c("local_authority", "oslaua", "jsa_number", "jsa_rate")

# ---------------------
# link to schools data
# ---------------------

schools = import("data/processed/schools_food_outlets.csv")

schools_test = left_join(t, jsa, by  = c("oslaua" = "oslaua"))

write.csv(schools_test, "data/processed/schools_foods_outlets.csv", row.names = FALSE)
  
# --------------
# Inmigrants
# --------------

    inmigrants = read_excel("data/non-uk-short-term-rseidents-pop-density.xlsx", sheet = "Data", range = "A8:D332")
    
    names(inmigrants) = c("local_authority", "oslaua", "residents", "density")
    
    write.csv(inmigrants, "data/processed/inmigrants.csv", row.names = FALSE)

# --------------
# Population
# --------------

    population = read_excel("data/population.xlsx", sheet = "Data", range = "A7:H334")

    population = clean_names(population)
    
    # rate
    pop_rate = population %>% select(local_authority = local_authority_district_unitary_as_of_april_2015, 
                                     starts_with("x_")) 
    
    pop_rate = pop_rate %>% rename(oslaua = x_1,
                                 prop_pop_aged_0_15 = x_2, 
                                 prop_pop_aged_16_64 = x_3,
                                 prop_pop_aged_65_and_over = x_4) %>%
    filter(!is.na(oslaua))
  
  

  # number
   pop_number = population %>% select(local_authority = local_authority_district_unitary_as_of_april_2015,
                                      oslaua = x_1, 
                                      starts_with("aged_")) %>%
     filter(!is.na(oslaua))  %>%
     arrange(local_authority, oslaua)
   
  names(pop_number) = gsub("aged_", "number_pop_aged_", names(pop_number))
 
     
   
   population = left_join(pop_rate, pop_number, by = c("local_authority", "oslaua"))
   
   write.csv(population, "data/processed/population.csv", row.names = FALSE)
  
  
