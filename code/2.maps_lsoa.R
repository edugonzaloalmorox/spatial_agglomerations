##############
# Map of crap
# LSOA level
#############


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





sectors<- readOGR("/Users/Personas/Dropbox/PhD/data/Lower_layer_super_output_areas_(E+W)_2011_Boundaries_(Full_Extent)_V2", "LSOA_2011_EW_BFE_V2")

sectors@data$id = rownames(sectors@data)

sectors.points = fortify(sectors, region = "id")


sectors.df = inner_join(sectors.points, sectors@data, by = "id")  #data frame that we use for drawing the map 

# clean England data 

sectors.df.england = sectors.df %>% 
  filter(str_detect(LSOA11CD, 'E0')) %>% 
  arrange(LSOA11CD) 


# ---------------------------

# ________________
# Link deprivation 
# ________________

library(readxl)

deprivation = read_excel("/Users/Personas/Downloads/File_1_ID_2015_Index_of_Multiple_Deprivation.xlsx", sheet = "IMD 2015")
names(deprivation) <- c("code", "lsoa", "oslaua", "district", "rank", "decile")

data_mapping = left_join(sectors.df.england, deprivation, by = c("LSOA11CD" = "code"))


# Plot

map_dep = 



map_deprivation = ggplot(data_mapping, aes(long, lat, group=group)) +
  geom_polygon(aes(fill = decile), colour = alpha("grey60", 1 / 20), size = 0.1) + 
  labs(fill = "Deprivation") +
  scale_fill_viridis() +   theme(axis.text = element_blank()
                                                 ,axis.title = element_blank()
                                                 ,axis.ticks = element_blank()
                                                 ,panel.grid = element_blank()
                                                 ,legend.title = element_text(size =6)
                                                 ,legend.text = element_text(size = 6)
                                                 ,legend.background = element_blank()
                                                 ,panel.background = element_blank()) 

ggsave("/Users/Personas/Dropbox/side_projects/spatial_obesity/output/test.png", map_deprivation, scale = 0.5)



