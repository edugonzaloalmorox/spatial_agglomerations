############################
# Code for extracting information from ratings.food.gov.uk
# Accessed August 2017
# Edu Gonzalo Almorox
############################




library(RJSONIO)
library(xml2)
library(rvest)
library(stringr)
library(dplyr)
library(tidyverse)
library(rio)
library(janitor)
library(tibble)
library(caret)

#######################################
# SCRAP LINKS OF THE LOCAL AUTHORITIES
########################################


# The following gets all the links from the website ------------

        URL <- "http://ratings.food.gov.uk/open-data/"
        
        scraping_url <- read_html(URL)
        
        members = scraping_url %>% html_nodes("a") %>%
                  html_attr("href") %>%
                  as.data.frame()
        
        colnames(members) <-  "urls" # rename the first variable

# Select the info corresponding to the local authorities based in England (note: there are ratings also ratings from Wales and I drop them)
  
        members$urls = as.character(members$url)
  
        members_la = members %>% filter(grepl("http://ratings.food.gov.uk/OpenDataFiles/FHRS.*en-GB", urls))


# check that is alright and create a column with the position - helps to have an idea of the number of local authorities assessed
        members_la = members_la %>% mutate(rows = row_number())
        View(members_la)
  
        urls = as.character(members_la$urls)

################################
# PREPARE THE DATASET
################################
        
# Function that loops over the urls and does the following: 
        
#        - convert the set of urls into a xml object
#        - extract the variables of interest 
        
 df <- lapply(urls, function(u){
   
   data <- read_xml(u)
   business_name <- data %>% xml_find_all("//BusinessName") %>% xml_text() 
   business_type <- data %>% xml_find_all("//BusinessType") %>% xml_text()
   # observations that don´t have certain nodes - fill them with NA
   places <- data %>% xml_find_all("//EstablishmentDetail") 
   
   

   
   post = sapply(places, function(x) {
     tryCatch(xml_text(xml_find_first(x, "./PostCode")),
              error=function(err) NA)
   })
   
   
   rating <- data %>% xml_find_all("//RatingValue")  %>% xml_text()
   scores <- data %>% xml_find_all("//Scores")  %>% xml_text()
   date <-  data %>% xml_find_all("//RatingDate")  %>% xml_text()
   local_authority <- data %>% xml_find_all("//LocalAuthorityName")  %>% xml_text()
   
   df = data.frame(business_name, business_type, post, rating, scores, date, local_authority)
 
   })

# convert the list into a data.frame - it eases manipulation   
 
df <- do.call(rbind, df) 
 
# save 
write.csv(df, "~/Dropbox/side_projects/spatial_obesity/data/processed/ratings_raw.csv", row.names = FALSE)
 
#####################

# clean those observations that do not have a postcode

 df_clean = df %>% filter(!is.na(post))

codes = import("/Users/Personas/Downloads/ONSPD_FEB_2017_UK/Data/ONSPD_FEB_2017_UK.csv")
 
# select information for geolocation

 code_info  = codes %>% select(pcd, lat, long, lsoa11, msoa11, ttwa, oslaua, imd)
 
# select rated postcodes
 
  posts_rate = as.character(unique(df_clean$post))
  
  # create a variable of the postcode without clean of spaces and  trimmed 
  
  posts_rate = gsub("[[:blank:]]", "", posts_rate)
  
  df_clean = df_clean %>% mutate(pcd2 = gsub("[[:blank:]]", "", post))
  
  code_info = code_info %>% mutate(pcd2 = gsub("[[:blank:]]", "", pcd))
  

  codes_rated =  code_info %>% filter(pcd2 %in% posts_rate)

   c = setdiff(posts_rate, codes_rated$pcd2) 
   
  # link information of the code info
   
   df_geo = left_join(df_clean, code_info, by = ("pcd2"))
   
   write.csv(df_geo, "~/Dropbox/side_projects/spatial_obesity/data/processed/ratings_clean_geo.csv", row.names = FALSE)
   
   business = df_geo %>% tabyl(business_name) %>% arrange(desc(percent))
   



