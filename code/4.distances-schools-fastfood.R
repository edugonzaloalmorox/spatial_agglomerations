# Distances betweeen schools and fast food


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

# -----------------------------------------

# ratings
df_ratings = import("data/processed/ratings_clean_geo.csv")

# select informmation corresponding to england
df_ratings_clean = df_ratings %>% 
  filter(! streg %in% c(0,9, " ")) %>% 
  filter(!is.na(oslaua)) %>% # no missing districts codes
  rename(region = streg) 

# select schools and fast food and relevena

fast_schools = c("School/college/university", "Takeaway/sandwich shop")

schools = df_ratings_clean %>% filter(business_type %in% fast_schools)
 
schools_clean = schools %>% select(business_name, business_type, post, long, lat, region)  



#----------
# region 4 
#----------
schools_test = schools_clean %>% filter(region == 4)

# distance matrix 
dist_matrix <- select(schools_test, long, lat) %>% 
  distm %>% `/`(1000)%>%  as.dist


library(reshape2)

# df gives the distance between the roows and columns of the dataset. 

df <- melt(as.matrix(dist_matrix), varnames = c("row", "col"))

head(df)

s =  schools_test %>%
  mutate(row.1 = 1:nrow(schools_test)) %>%
  select(business_name:region, row.1)

# I link the rows according to their number in the first data frame.

    df = left_join(df, s, by = c("row"= "row.1"))

# As the matrix is symmetric, columns must follow the same order as rows. 
    df = left_join(df, s, by = c("col"= "row.1"))

# Note: output gives information regarding the distance between every pair of care home

# clean and rename variables in data frame

df = df %>% 
  select(from.ID = business_name.x, to.ID = business_name.y,
         from_business = business_type.x, to_business = business_type.y,
         from.postcode = post.x, to.postcode = post.y, distance = value) %>%
  filter(distance >0)

# those less than 5 km away
df_clean =  df %>% 
  filter(from_business == "School/college/university", to_business == "Takeaway/sandwich shop") %>%
  filter(distance < 5)

df_region4 = df_clean

write.csv(df_region4, "data/processed/distance_region4.csv", row.names = FALSE)

library(gdata)

keep(schools_clean, df_ratings_clean, sure = TRUE)

#-------------------------------------------------------

#----------
# region 1 
#----------
schools_test = schools_clean %>% filter(region == 1)

# distance matrix 
dist_matrix <- select(schools_test, long, lat) %>% 
  distm %>% `/`(1000)%>%  as.dist


library(reshape2)

# df gives the distance between the roows and columns of the dataset. 

df <- melt(as.matrix(dist_matrix), varnames = c("row", "col"))

head(df)

s =  schools_test %>%
  mutate(row.1 = 1:nrow(schools_test)) %>%
  select(business_name:region, row.1)

# I link the rows according to their number in the first data frame.

df = left_join(df, s, by = c("row"= "row.1"))

# As the matrix is symmetric, columns must follow the same order as rows. 
df = left_join(df, s, by = c("col"= "row.1"))

# Note: output gives information regarding the distance between every pair of care home

# clean and rename variables in data frame

df = df %>% 
  select(from.ID = business_name.x, to.ID = business_name.y,
         from_business = business_type.x, to_business = business_type.y,
         from.postcode = post.x, to.postcode = post.y, distance = value) %>%
  filter(distance >0)

# those less than 5 km away
df_clean =  df %>% 
  filter(from_business == "School/college/university", to_business == "Takeaway/sandwich shop") %>%
  filter(distance < 5)

df_region4 = df_clean

write.csv(df_region4, "data/processed/distance_region1.csv", row.names = FALSE)

keep(schools_clean, df_ratings_clean, sure = TRUE)

# ---------------------------------------------------------


#----------
# region 2 
#----------
schools_test = schools_clean %>% filter(region == 2)

# distance matrix 
dist_matrix <- select(schools_test, long, lat) %>% 
  distm %>% `/`(1000)%>%  as.dist


library(reshape2)

# df gives the distance between the roows and columns of the dataset. 

df <- melt(as.matrix(dist_matrix), varnames = c("row", "col"))

head(df)

s =  schools_test %>%
  mutate(row.1 = 1:nrow(schools_test)) %>%
  select(business_name:region, row.1)

# I link the rows according to their number in the first data frame.

df = left_join(df, s, by = c("row"= "row.1"))

# As the matrix is symmetric, columns must follow the same order as rows. 
df = left_join(df, s, by = c("col"= "row.1"))

# Note: output gives information regarding the distance between every pair of care home

# clean and rename variables in data frame

df = df %>% 
  select(from.ID = business_name.x, to.ID = business_name.y,
         from_business = business_type.x, to_business = business_type.y,
         from.postcode = post.x, to.postcode = post.y, distance = value) %>%
  filter(distance >0)

# those less than 5 km away
df_region2 =  df %>% 
  filter(from_business == "School/college/university", to_business == "Takeaway/sandwich shop") %>%
  filter(distance < 5)



write.csv(df_region2, "data/processed/distance_region2.csv", row.names = FALSE)

keep(schools_clean, df_ratings_clean, sure = TRUE)


# ---------------------------------------------------------


#----------
# region 3 
#----------
schools_test = schools_clean %>% filter(region == 3)

# distance matrix 
dist_matrix <- select(schools_test, long, lat) %>% 
  distm %>% `/`(1000)%>%  as.dist


library(reshape2)

# df gives the distance between the roows and columns of the dataset. 

df <- melt(as.matrix(dist_matrix), varnames = c("row", "col"))

head(df)

s =  schools_test %>%
  mutate(row.1 = 1:nrow(schools_test)) %>%
  select(business_name:region, row.1)

# I link the rows according to their number in the first data frame.

df = left_join(df, s, by = c("row"= "row.1"))

# As the matrix is symmetric, columns must follow the same order as rows. 
df = left_join(df, s, by = c("col"= "row.1"))

# Note: output gives information regarding the distance between every pair of care home

# clean and rename variables in data frame

df = df %>% 
  select(from.ID = business_name.x, to.ID = business_name.y,
         from_business = business_type.x, to_business = business_type.y,
         from.postcode = post.x, to.postcode = post.y, distance = value) %>%
  filter(distance >0)

# those less than 5 km away
df_region3 =  df %>% 
  filter(from_business == "School/college/university", to_business == "Takeaway/sandwich shop") %>%
  filter(distance < 5)



write.csv(df_region3, "data/processed/distance_region3.csv", row.names = FALSE)

keep(schools_clean, df_ratings_clean, sure = TRUE)


# ---------------------------------------------------------


#----------
# region 5 
#----------
schools_test = schools_clean %>% filter(region == 5)

# split the sample 

st1 = schools_test[1:7604,]
st2 = schools_test[7605:15000, ]
st3 = schools_test[15001:22810, ]

# distance matrix 
dist_matrix <- select(st1, long, lat) %>% 
  distm %>% `/`(1000)%>%  as.dist

dist_matrix <- select(st2, long, lat) %>% 
  distm %>% `/`(1000)%>%  as.dist

dist_matrix <- select(st3, long, lat) %>% 
  distm %>% `/`(1000)%>%  as.dist


library(reshape2)

# df gives the distance between the roows and columns of the dataset. 

df <- melt(as.matrix(dist_matrix), varnames = c("row", "col"))

head(df)

s1 =  st1 %>%
  mutate(row.1 = 1:nrow(st1)) %>%
  select(business_name:region, row.1)

s2 =  st2 %>%
  mutate(row.1 = 1:nrow(st2)) %>%
  select(business_name:region, row.1)

s3 =  st3 %>%
  mutate(row.1 = 1:nrow(st3)) %>%
  select(business_name:region, row.1)

# I link the rows according to their number in the first data frame.

df = left_join(df, s1, by = c("row"= "row.1"))

df = left_join(df, s2, by = c("row"= "row.1"))

df = left_join(df, s3, by = c("row"= "row.1"))

# As the matrix is symmetric, columns must follow the same order as rows. 
df = left_join(df, s1, by = c("col"= "row.1"))

df = left_join(df, s2, by = c("col"= "row.1"))

df = left_join(df, s3, by = c("col"= "row.1"))

# Note: output gives information regarding the distance between every pair of care home

# clean and rename variables in data frame

df = df %>% 
  select(from.ID = business_name.x, to.ID = business_name.y,
         from_business = business_type.x, to_business = business_type.y,
         from.postcode = post.x, to.postcode = post.y, distance = value) %>%
  filter(distance >0)

# those less than 5 km away
df_region5.1 =  df %>% 
  filter(from_business == "School/college/university", to_business == "Takeaway/sandwich shop") %>%
  filter(distance < 5)

df_region5.2 =  df %>% 
  filter(from_business == "School/college/university", to_business == "Takeaway/sandwich shop") %>%
  filter(distance < 5)

df_region5.3 =  df %>% 
  filter(from_business == "School/college/university", to_business == "Takeaway/sandwich shop") %>%
  filter(distance < 5)




write.csv(df_region5.1, "data/processed/distance_region5.1.csv", row.names = FALSE)

write.csv(df_region5.2, "data/processed/distance_region5.2.csv", row.names = FALSE)

write.csv(df_region5.3, "data/processed/distance_region5.3.csv", row.names = FALSE)


region5.1 = import("data/processed/distance_region5.1.csv")
region5.2 = import("data/processed/distance_region5.2.csv")
region5.3 = import("data/processed/distance_region5.3.csv")

region5 = bind_rows(region5.1, region5.2, region5.3)


write.csv(region5, "data/processed/distance_region5.csv", row.names = FALSE)

keep(schools_clean, df_ratings_clean, sure = TRUE)


#----------
# region 6 
#----------
schools_test = schools_clean %>% filter(region == 6)

# distance matrix 
dist_matrix <- select(schools_test, long, lat) %>% 
  distm %>% `/`(1000)%>%  as.dist


library(reshape2)

# df gives the distance between the roows and columns of the dataset. 

df <- melt(as.matrix(dist_matrix), varnames = c("row", "col"))

head(df)

s =  schools_test %>%
  mutate(row.1 = 1:nrow(schools_test)) %>%
  select(business_name:region, row.1)

# I link the rows according to their number in the first data frame.

df = left_join(df, s, by = c("row"= "row.1"))

# As the matrix is symmetric, columns must follow the same order as rows. 
df = left_join(df, s, by = c("col"= "row.1"))

# Note: output gives information regarding the distance between every pair of care home

# clean and rename variables in data frame

df = df %>% 
  select(from.ID = business_name.x, to.ID = business_name.y,
         from_business = business_type.x, to_business = business_type.y,
         from.postcode = post.x, to.postcode = post.y, distance = value) %>%
  filter(distance >0)

# those less than 5 km away
df_region6 =  df %>% 
  filter(from_business == "School/college/university", to_business == "Takeaway/sandwich shop") %>%
  filter(distance < 5)



write.csv(df_region6, "data/processed/distance_region6.csv", row.names = FALSE)

keep(schools_clean, df_ratings_clean, sure = TRUE)

#----------
# region 7 
#----------
schools_test = schools_clean %>% filter(region == 7)

# distance matrix 
dist_matrix <- select(schools_test, long, lat) %>% 
  distm %>% `/`(1000)%>%  as.dist


library(reshape2)

# df gives the distance between the roows and columns of the dataset. 

df <- melt(as.matrix(dist_matrix), varnames = c("row", "col"))

head(df)

s =  schools_test %>%
  mutate(row.1 = 1:nrow(schools_test)) %>%
  select(business_name:region, row.1)

# I link the rows according to their number in the first data frame.

df = left_join(df, s, by = c("row"= "row.1"))

# As the matrix is symmetric, columns must follow the same order as rows. 
df = left_join(df, s, by = c("col"= "row.1"))

# Note: output gives information regarding the distance between every pair of care home

# clean and rename variables in data frame

df = df %>% 
  select(from.ID = business_name.x, to.ID = business_name.y,
         from_business = business_type.x, to_business = business_type.y,
         from.postcode = post.x, to.postcode = post.y, distance = value) %>%
  filter(distance >0)

# those less than 5 km away
df_region7 =  df %>% 
  filter(from_business == "School/college/university", to_business == "Takeaway/sandwich shop") %>%
  filter(distance < 5)



write.csv(df_region7, "data/processed/distance_region7.csv", row.names = FALSE)

keep(schools_clean, df_ratings_clean, sure = TRUE)

#----------
# region 8 
#----------
schools_test = schools_clean %>% filter(region == 8)

# distance matrix 
dist_matrix <- select(schools_test, long, lat) %>% 
  distm %>% `/`(1000)%>%  as.dist


library(reshape2)

# df gives the distance between the roows and columns of the dataset. 

df <- melt(as.matrix(dist_matrix), varnames = c("row", "col"))

head(df)

s =  schools_test %>%
  mutate(row.1 = 1:nrow(schools_test)) %>%
  select(business_name:region, row.1)

# I link the rows according to their number in the first data frame.

df = left_join(df, s, by = c("row"= "row.1"))

# As the matrix is symmetric, columns must follow the same order as rows. 
df = left_join(df, s, by = c("col"= "row.1"))

# Note: output gives information regarding the distance between every pair of care home

# clean and rename variables in data frame

df = df %>% 
  select(from.ID = business_name.x, to.ID = business_name.y,
         from_business = business_type.x, to_business = business_type.y,
         from.postcode = post.x, to.postcode = post.y, distance = value) %>%
  filter(distance >0)

# those less than 5 km away
df_region8 =  df %>% 
  filter(from_business == "School/college/university", to_business == "Takeaway/sandwich shop") %>%
  filter(distance < 5)



write.csv(df_region8, "data/processed/distance_region8.csv", row.names = FALSE)

keep(schools_clean, df_ratings_clean, sure = TRUE)

##############################

# Link data sets

d1= import("distance_region1.csv") %>% mutate(region = 1)
d2= import("distance_region2.csv")  %>% mutate(region = 2)
d3= import("distance_region3.csv")  %>% mutate(region = 3)

d4= import("distance_region4.csv") %>% select(-radius10)  %>% mutate(region = 4)
d5= import("distance_region5.csv")  %>% mutate(region = 5)
d6= import("distance_region6.csv")  %>% mutate(region = 6)

d7= import("distance_region7.csv")  %>% mutate(region = 7)
d8= import("distance_region8.csv")  %>% mutate(region = 8)


schools_distance = bind_rows(d1, d2, d3, d4, d5, d6, d7, d8)


# Definite dataset
write.csv(schools_distance, "schools_distance.csv", row.names = FALSE)




