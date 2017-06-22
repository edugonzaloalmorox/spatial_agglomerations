############################################################
# Data manipulations 
# 1. Load information for each county (provided in XML format) - ratings.food.gov.uk
# 2. Link information by rows
# Data on ratings accessed: 31 -May 2017
##########################################################


library(xml2)
library(dplyr)
library(rio)


##############
# Darlington
###############

data <- read_xml("http://ratings.food.gov.uk/OpenDataFiles/FHRS874en-GB.xml")


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

darlington =  data.frame(business_name, business_type, post, rating, scores, date, local_authority)


############
# Durham
##########

data <- read_xml("http://ratings.food.gov.uk/OpenDataFiles/FHRS706en-GB.xml")


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

durham =  data.frame(business_name, business_type, post, rating, scores, date, local_authority)

write.csv(durham, "/Users/Personas/My Cloud/side_projects/spatial_obesity/data/durham_rat.csv")

############
# Gateshead
############

data <- read_xml("http://ratings.food.gov.uk/OpenDataFiles/FHRS410en-GB.xml")


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

gateshead =  data.frame(business_name, business_type, post, rating, scores, date, local_authority)

#############
# Hartlepool
############


rm(business_name, business_type, data, date, local_authority, places, post, rating, scores, address, id, lat, long)

data <- read_xml("http://ratings.food.gov.uk/OpenDataFiles/FHRS859en-GB.xml")


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

hartlepool =  data.frame(business_name, business_type, post, rating, scores, date, local_authority)

#################
# Middlesbrough
################


rm(business_name, business_type, data, date, local_authority, places, post, rating, scores)

data <- read_xml("http://ratings.food.gov.uk/OpenDataFiles/FHRS861en-GB.xml")


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

middlesbrough =  data.frame(business_name, business_type, post, rating, scores, date, local_authority)

################
# Newcastle
################

rm(business_name, business_type, data, date, local_authority, places, post, rating, scores)

data <- read_xml("http://ratings.food.gov.uk/OpenDataFiles/FHRS416en-GB.xml")


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

newcastle =  data.frame(business_name, business_type, post, rating, scores, date, local_authority)


#################
# North Tyneside
#################

rm(business_name, business_type, data, date, local_authority, places, post, rating, scores)

data <- read_xml("http://ratings.food.gov.uk/OpenDataFiles/FHRS417en-GB.xml")


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

northtyneside =  data.frame(business_name, business_type, post, rating, scores, date, local_authority)

#################
# Northumberland
##################


rm(business_name, business_type, data, date, local_authority, places, post, rating, scores)

data <- read_xml("http://ratings.food.gov.uk/OpenDataFiles/FHRS707en-GB.xml")


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

northumberland =  data.frame(business_name, business_type, post, rating, scores, date, local_authority)


########################
# Redcard and Cleveland
########################


rm(business_name, business_type, data, date, local_authority, places, post, rating, scores)

data <- read_xml("http://ratings.food.gov.uk/OpenDataFiles/FHRS860en-GB.xml")


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

redcard =  data.frame(business_name, business_type, post, rating, scores, date, local_authority)

####################
# South Tyneside
####################

rm(business_name, business_type, data, date, local_authority, places, post, rating, scores)

data <- read_xml("http://ratings.food.gov.uk/OpenDataFiles/FHRS427en-GB.xml")


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

south_tyneside =  data.frame(business_name, business_type, post, rating, scores, date, local_authority)


#############################
# Stockton on Tees
#######################


rm(business_name, business_type, data, date, local_authority, places, post, rating, scores)

data <- read_xml("http://ratings.food.gov.uk/OpenDataFiles/FHRS862en-GB.xml")


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

stockton =  data.frame(business_name, business_type, post, rating, scores, date, local_authority)

##############
# Sunderland
###############

rm(business_name, business_type, data, date, local_authority, places, post, rating, scores)

data <- read_xml("http://ratings.food.gov.uk/OpenDataFiles/FHRS862en-GB.xml")


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

sunderland =  data.frame(business_name, business_type, post, rating, scores, date, local_authority)

#####################

# Link all the datasets -  by row 

test = rbind(darlington, durham, gateshead, hartlepool, middlebrough, newcastle,
             northtyneside, northumberland, redcard, south_tyneside, stockton, sunderland)


write.csv(test, "/Users/Personas/My Cloud/side_projects/spatial_obesity/data/north_east_ratings.csv", row.names = FALSE)
