# ----------------------------------------------------
# Link additional information to the main dataset
# Add information about the schools and the takeaways
# November-2017
# @Edu Gonzalo Almorox
# --------------------------------------------------


library(rio)
library(tidyr)
library(ggplot2)
library(janitor)
library(viridis)
library(tidyverse)
library(readxl)
library(dplyr)
library(sp)
library(rgeos)
library(geosphere)
library(RANN)
library(skimr)

schools =  import("data/processed/schools_distance.csv")
ratings = import("data/processed/ratings_clean_geo.csv")



schools = schools %>% arrange(from.ID) %>% clean_names()

# remove duplicates
      
        schools_unique = schools %>%
        group_by(from_id, to_id, from_postcode, to_postcode) %>% unique()
      


#######################################
# Select information regarding schools
#######################################

# --------
# Schools       
# --------

# ratings
    rat = ratings %>% select(date, pcd2, business_name, business_type, rating, local_authority,lat:imd)

# remove duplicates
    rat_single = ratings %>% filter(business_type == "School/college/university") %>%
      group_by(business_name, pcd2) %>%
      arrange(date) %>%
      filter(row_number() == n())

# key variable - postcodes 
    schools = schools %>% mutate(pcd2from = gsub("[[:blank:]]", "", from_postcode),
                                 pcd2to = gsub("[[:blank:]]", "", to_postcode))

# link schools and ratings 
    sch_test = left_join(schools, rat_single, by = c("pcd2from" = "pcd2", "from_id" = "business_name")) 

    sch_test=  sch_test %>% select(-business_type, -post, -pcd, -streg) %>% rename(lat_from = lat, 
                                                                                   long_from = long,
                                                                                   lsoa_from = lsoa11, 
                                                                                   msoa_from = msoa11, 
                                                                                   ttwa_from = ttwa, 
                                                                                   oslaua_from = oslaua, 
                                                                                   imd_from = imd)
 
# ----------
# Take-aways
# ----------

# Add information of takeaways
    rat_single = ratings %>% filter(business_type == "Takeaway/sandwich shop") %>%
        group_by(business_name, pcd2) %>%
        arrange(date) %>%
        filter(row_number() == n())

      rat_single = rat_single %>% select(business_name, pcd2, rating)
      
      sch_test1 = left_join(sch_test, rat_single, by = c("pcd2to" = "pcd2", "to_id" = "business_name"))
      
      sch_test1 = sch_test1 %>% rename(from_rating = rating.x,
                                       to_rating = rating.y)
      
      sch_test1 = sch_test1 %>% select(from_id:scores,to_rating, date:imd_from )

write.csv(sch_test1, "data/processed/schools_distance_unique.csv", row.names = FALSE) 



