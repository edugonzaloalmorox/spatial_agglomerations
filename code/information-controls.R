#############
# Additional information
# Data referred to August 2017
# Source: NOMIS
# @EduGonzalo
########

library(tidyverse)
library(readxl)


jsa = read_excel("data/jsa-august-2017.xlsx", sheet = "Data", range = "A9:D335") 
names(jsa) = c("local_authority", "oslaua", "jsa_number", "jsa_rate")

# ---------------------
# link to schools data
# ---------------------

schools = import("data/processed/schools_food_outlets.csv")

schools_test = left_join(t, jsa, by  = c("oslaua" = "oslaua"))

write.csv(schools_test, "data/processed/schools_foods_outlets.csv", row.names = FALSE)
  



inmigrants = read_excel("data/non-uk-short-term-rseidents-pop-density.xlsx", sheet = "Data", range = "A9:D335")
