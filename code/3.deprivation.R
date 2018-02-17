
library(rio)
library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)
library(viridis)
library(tidyverse)
library(readxl)

# ------------------------------------
# Relationship betweend IMD and hygiene
# ------------------------------------

# load data 

# ratings
df_ratings = import("data/processed/ratings_clean_geo.csv")


# deprivation - district level
imd_districts = read_excel("data/processed/districts_imd.xlsx", sheet = 2)

imd_districts = clean_names(imd_districts)

##########



# Add and select information 

# select regions in england
df_ratings_clean = df_ratings %>% 
  filter(! streg %in% c(0,9, " ")) %>% 
  filter(!is.na(oslaua)) %>% # no missing districts codes
  rename(region = streg) 




# select imd average score (district level) - our variable for representing IMD
imd = imd_districts %>% select(local_authority_district_code_2013, 
                               local_authority_district_name_2013, 
                               imd_average_score)




####################
# Get frequency of businesses for each district (oslaua)
summary_business = df_ratings_clean %>% 
  group_by(oslaua, region, business_type, ) %>%
  crosstab(oslaua, business_type) %>% 
  adorn_crosstab(show_n = FALSE, show_totals = TRUE) 

# long format
sum_long  = summary_business %>%
  gather(business, percentage, `Distributors/Transporters`:`Takeaway/sandwich shop`) %>% 
  arrange(oslaua) %>% 
  mutate(percentage = gsub("%", "", percentage)) %>%
  mutate_at(vars(percentage), funs(as.numeric))

# link region 
regions = df_ratings_clean %>% 
  select(oslaua, region) %>% unique()

sum_long = left_join(sum_long, regions, by = "oslaua")
# link deprivation data
sum_long = left_join(sum_long, imd, by = c("oslaua" = "local_authority_district_code_2013"))


# plot number of inspected business (fig1) 
# ---------------------------------
business_freq =  df_ratings_clean %>% 
  tabyl(business_type) %>% arrange(desc(n))

ggplot(business_freq, aes(x = reorder(business_type, n), y = n)) + 
  geom_bar(stat = "identity") +
  coord_flip() + 
  labs(x = "Business Type", title = "Types of inspected businesses")



# select relevant businesses
imp_businesses = sum_long %>% filter(business %in%  c("Pub/bar/nightclub", "Restaurant/Cafe/Canteen", 
                                                      "Hospitals/Childcare/Caring Premises", "Takeaway/sandwich shop", 
                                                      "Retailers - supermarkets/hypermarkets", "Retailers - other", 
                                                      "School/college/university"))


# plot type  of business and deprivation (fig2)
# ---------------------------------------------

ggplot(imp_businesses, aes(imd_average_score, percentage)) +
  geom_point() +
  geom_smooth(method = lm) + 
  facet_wrap(  ~ business, ncol = 2, scales = "free") +
  labs(title = "Number of businesses and Deprivation", x = "IMD average score", y  = "Percentage of businesses")



#################

# Ratings of business and deprivation 

# Get frequency of businesses for each district (oslaua)



summary_rating = df_ratings_clean %>% 
  mutate_at(vars(rating), funs(as.factor)) %>% # rating categorical variable
  crosstab(oslaua, rating) %>% 
  adorn_crosstab(show_n = FALSE, show_totals = TRUE)


# long format
sum_long_rating  = summary_rating %>%
  gather(rating, percentage, `0`:`Pass`) %>% 
  arrange(oslaua) %>% 
  mutate(percentage = gsub("%", "", percentage)) %>%
  mutate_at(vars(percentage), funs(as.numeric))

# link deprivation data
sum_long_rating = left_join(sum_long_rating, imd, by = c("oslaua" = "local_authority_district_code_2013"))


categories = rep(0:5)


sum_rating_categories = sum_long_rating %>% filter(rating %in% categories)

# plot ratings and deprivation (fig3)

ggplot(sum_rating_categories, aes(imd_average_score, percentage)) +
  geom_point() +
  geom_smooth(method = lm) + 
  facet_wrap(  ~ rating, ncol = 2, scales = "free") +
  labs(title = "Food Hygiene ratings and Deprivation", x = "IMD average score", y  = "Percentage of businesses")


# Update: February 2018 - Link deciles 

geo = import("data/geography/ONSPD_FEB_2017_UK/Data/ONSPD_FEB_2017_UK.csv")

geo = clean_names(geo)

geo_england = geo %>%
  dplyr::select(lsoa11, oslaua, imd) %>%
  filter(str_detect(oslaua, "^E")) %>%
  group_by(lsoa11, imd) %>%
  unique()

geo_england %>% mutate(imd_decile = ntile(imd, 10)) ->  geo_england

schools = import("data/processed/schools_food_outlets.dta")

schools = left_join(schools, geo_england, by = c("lsoa_from" = "lsoa11"))


write.csv(schools, "data/processed/schools_food_outlets.csv", row.names = FALSE)



