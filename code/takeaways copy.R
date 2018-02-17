# Scrap website

library(knitr)
library(sp)
library(raster)
library(rgdal)
library(rgeos)
library(cleangeo)
library(geosphere)
library(plyr)
library(dplyr)
library(rvest)
library(stringr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggmap)
library(ggrepel)
library(ggalt) #devtools::install_github("hrbrmstr/ggalt")
library(viridis)
library(readr)
install.packages("xml2")
library(xml2)

# Newcastle 
# get the urls 
n_ne = as.numeric(1:30)

urls = sprintf("https://www.localpeek.co.uk/town/newcastle-upon-tyne-ne/%s/", n_ne)

url =  lapply(urls, html)

# function to process the information in the url 
prueba = function(links){
  selector_name<-".listing-element-content"
  fnames<-html_nodes(x = links, css = selector_name) %>% html_text()
  return(fnames)
  }

# create a list  with all the elements of the list 
data = lapply(url, prueba)
head(data)

# transform the list into a dataframe 
df <- ldply(data, data.frame) %>% select(var = X..i..)

df = str_split_fixed(df$var, ";", 4) %>% as.data.frame()

df = df %>% select(companies = V1, restaurants = V2, bars = V3, takeaways = V4)

library(readr)

df1 = df %>% mutate(restaurants = parse_number(restaurants),
                   bars = parse_number(bars),
                   takeaways =parse_number(takeaways)) 


# break the postcode
postcode = str_split_fixed(df1$companies, " ", 3) %>% as.data.frame()

postcode = postcode %>% mutate(post = paste(V1, V2, " "), 
                               firms = parse_number(V3)) %>% select(post, firms)

data_final = cbind(postcode, df1) %>% select(-companies)

rm(data, df, df1, n_ne, postcode, url, urls)

# ----------------------------------

# bath 

n = as.numeric(1:15)

urls = sprintf("https://www.localpeek.co.uk/town/bath-ba/%s/", n)


url =  lapply(urls, read_html)


data = lapply(url, prueba)
head(data)


# transform the list into a dataframe 
df <- ldply(data, data.frame) %>% select(var = X..i..)

df = str_split_fixed(df$var, ";", 4) %>% as.data.frame()

df = df %>% select(companies = V1, restaurants = V2, bars = V3, takeaways = V4)

df1 = df %>% mutate(restaurants = parse_number(restaurants),
                    bars = parse_number(bars),
                    takeaways =parse_number(takeaways)) 


# break the postcode
postcode = str_split_fixed(df1$companies, " ", 3) %>% as.data.frame()

postcode = postcode %>% mutate(post = paste(V1, V2, " "), 
                               firms = parse_number(V3)) %>% select(post, firms)

data_bath = cbind(postcode, df1) %>% select(-companies)

data_final = rbind(data_bath, data_final)

rm(data, df, df1, n, postcode, url, urls)

# bristol 

n = as.numeric(1:81)
urls = sprintf("https://www.localpeek.co.uk/town/bristol-bs/%s/", n)


url =  lapply(urls, read_html)

data = lapply(url, prueba)
head(data)


# transform the list into a dataframe 
df <- ldply(d, data.frame) %>% select(var = X..i..)

df = str_split_fixed(df$var, ";", 4) %>% as.data.frame()

df = df %>% select(companies = V1, restaurants = V2, bars = V3, takeaways = V4)

df1 = df %>% mutate(restaurants = parse_number(restaurants),
                    bars = parse_number(bars),
                    takeaways =parse_number(takeaways)) 


# break the postcode
postcode = str_split_fixed(df1$companies, " ", 3) %>% as.data.frame()

postcode = postcode %>% mutate(post = paste(V1, V2, " "), 
                               firms = parse_number(V3)) %>% select(post, firms)

data_bristol = cbind(postcode, df1) %>% select(-companies)

data_final = rbind(data_bath, data_final)

rm(data, df, df1, n, postcode, url, urls)

write.csv(data_final, "/Users/Personas/My Cloud/side_projects/scrapping/data_final.csv", row.names = FALSE)




links = urls

d <- vector("list", length(links))



url for (i in seq_along(links)) {
  if (!(links[i] %in% names(d))) {
    cat(paste("Doing", links[i], "..."))
    ok <- FALSE
    counter <- 0
    while (ok == FALSE & counter <= 5) {
      counter <- counter + 1
      out <- tryCatch({                  
        scrape_test(links[i])
      },
      error = function(e) {
        Sys.sleep(2)
        e
      }
      )
      if ("error" %in% class(out)) {
        cat(".")
      } else {
        ok <- TRUE
        cat(" Done.")
      }
    }
    cat("\n")
    d[[i]] <- out
    names(d)[i] <- links[i]
  }
} 


