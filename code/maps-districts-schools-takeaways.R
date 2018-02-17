########################
# Descriptive analysis
# Maps:district analysis
#########################

library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(rio)
library(janitor)



# Consider all the ratings

rating = import("data/processed/ratings_clean_geo.csv")

# How many schools and takeaways we have

total_business = rating %>%
  group_by(oslaua) %>%
  summarise(total_number_business = n()) %>%
  filter(str_detect(oslaua, "^E")) %>%
  arrange(desc(total_number_business))


takes_schools = rating %>%
  filter(business_type %in% c("School/college/university", "Takeaway/sandwich shop")) %>%
  group_by(oslaua, business_type) %>%
  summarise(number_business = n()) %>%
  filter(str_detect(oslaua, "^E")) %>%
  ungroup() %>%
  group_by(oslaua) 

# wide 

takes_wide = takes_schools %>% 
  group_by(oslaua) %>%
  spread(business_type, number_business)

total_business = left_join(total_business, takes_wide, by= c("oslaua"))

total_business = total_business %>%
  group_by(oslaua) %>% 
  clean_names() %>%
  arrange(oslaua) %>%
  mutate(prop_schools = school_college_university/total_number_business,
         prop_takeaways = takeaway_sandwich_shop/total_number_business) %>%
  arrange(desc(prop_takeaways)) %>%
  mutate(quintile_take = ntile(prop_takeaways, 5),
         quintile_school = ntile(prop_schools, 5))

total_business = total_business %>%
  dplyr::mutate(quintile_take = ifelse(is.na(quintile_take), 1, quintile_take), 
         quintile_school = ifelse(is.na(quintile_school), 1, quintile_school))
  

summary(total_business)

write.csv(total_business, "data/processed/total_business.csv", row.names = FALSE)

total_business = import("data/processed/total_business.csv")

total_business %>%
  mutate_at(vars(quintile_take, quintile_school), funs(as.factor)) %>%
  group_by(quintile_take) %>% 
 dplyr::summarise(min_rest = min(prop_takeaways), 
                  max_rest = max(prop_takeaways))
  

total_business %>%
  mutate_at(vars(quintile_take, quintile_school), funs(as.factor)) %>%
  group_by(quintile_school) %>% 
  dplyr::summarise(min_sch = min(prop_schools), 
                   max_sch = max(prop_schools))


#################
# Geographical maps 
###############


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




sectors<- readOGR("/Users/Personas/Dropbox/side_projects/spatial_obesity/data/geography/Local_Authority_Districts_December_2015_Full_Clipped_Boundaries_in_Great_Britain",
                  "Local_Authority_Districts_December_2015_Full_Clipped_Boundaries_in_Great_Britain")

sectors@data$id = rownames(sectors@data)

sectors.points = fortify(sectors, region = "id")


sectors.df = inner_join(sectors.points, sectors@data, by = "id")  #data frame that we use for drawing the map 

sectors.df.england = sectors.df %>% 
  filter(str_detect(lad15cd, '^E0'))




# link information 
# -----------------

map_sectors = left_join(sectors.df.england,  total_business, by = c("lad15cd" = "oslaua"))



# plot 

map_rest = ggplot(map_sectors, aes(long, lat, group=group)) +
  geom_polygon(aes(fill = quintile_take), colour = alpha("grey", 1 /6), size = 0.1) + 
  labs(fill = "Takeaways (proportion)") +
  #labs(title = "Takeaways") +
  scale_fill_viridis(labels = c("(0 - 0.07]", "(0.07 - 0.094]", "(0.094 - 0.114]", "(0.114 - 0.143]", "( 0.143 - 0.418]")) + 
  theme(axis.text = element_blank()
        ,axis.title = element_blank()
        ,axis.ticks = element_blank()
        ,axis.line=element_blank()
        ,panel.grid = element_blank()
        ,legend.title = element_text(size = 4)
        ,legend.text = element_text(size = 6)
        ,legend.position = "right"
        ,legend.background = element_blank()
        ,panel.background = element_blank())

ggsave("output/figures/test_takeaways.png", map_rest, scale = 0.5)

map_sch = ggplot(map_sectors, aes(long, lat, group=group)) +
  geom_polygon(aes(fill = quintile_school), colour = alpha("grey", 1 /6), size = 0.1) + 
  labs(fill = "Restaurants in schools (proportion)") +
  scale_fill_viridis(labels = c("(0 - 0.056]", "(0.056 - 0.067]", "(0.067 - 0.076]", "(0.076 - 0.088]", "(0.088 - 0.165]")) + 
  theme(axis.text = element_blank()
        ,axis.title = element_blank()
        ,axis.ticks = element_blank()
        ,axis.line=element_blank()
        ,panel.grid = element_blank()
        ,legend.title = element_text(size = 4)
        ,legend.text = element_text(size = 6)
        ,legend.position = "right"
        ,legend.background = element_blank()
        ,panel.background = element_blank())

ggsave("output/figures/test_sch.png", map_sch, scale = 0.5)


