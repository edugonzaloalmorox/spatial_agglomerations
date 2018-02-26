# Summary Statistics 
# Sample 1 and Sample 2
#########################


clean_schools = import("data/processed/clean_schools_1_5km.csv")
 
 var_sample = clean_schools %>%
   select(rating_3, rating_4, rating_5, imd_decile, primary, secondary, college, prop_pop_aged_0_15, jsa_rate, mean_distance, n_asian, n_chicken, n_fish, n_kebab, n_other, n_pizza, n_sandwich)
 
 sum_stats = function(vars, df){
   require(psych)
   
   # vars for getting main stats 
   # df for getting furhter info
   
 x = as.data.frame(describe(vars)) 
 
  lab_vars = c("Rating inspection: 3", 
    "Rating inspection: 4", 
    "Rating inspection: 5", 
    "IMD (deciles)",
    "Primary school", 
    "Secondary school",
      "College",
 "Population aged 0-15 (%)",
 "Job Seekers (%)",
 "Distance takeaways (avg)",
 "Number of Asian takeaways",
 "Number of burger and  fried chicken takeaways",
 "Number of fish and chips takeaways",
 "Number of kebab takeaways",
 "Number of other takeaways",
 "Number pizza takeaways",
 "Number of sandwich takeaways")
 
  row.names(x) = lab_vars
  
  x$variable  =  row.names(x)
  
  
  y = x %>% select(variable, mean, sd, min, max) %>% as.data.frame()

  # round variables
  g = y %>% mutate_at(vars(mean, sd, min, max), funs(as.numeric)) %>%
    mutate_if(is.numeric, round, digits = 2)
  
 
  add_vars = data.frame(variable = c("Observations", "Local Authorities (district)"), 
                        mean= c(length(unique(df$from_id)), length(unique(df$oslaua_from))), 
                        sd = c(NA, NA), 
                        min = c(NA, NA), 
                        max = c(NA, NA))
  
  g = bind_rows(g, add_vars)
  return(g)
  
  }
 
 stats_s1 = sum_stats(var_sample, clean_schools)
 stats_s1 
 
#############
# Sample 2
 #############
 clean_schools_v2 = import("data/processed/clean_schools_v2.csv")

 
 
 
 vars_s2 = clean_schools_v2 %>% select(rating_from_3:rating_from_5, primary, secondary,  college, imd_decile, rating_to_3:rating_to_5,  prop_pop_aged_0_15, jsa_rate, distance, asian,
                                       chicken_burger,  fish,  kebab, other,  pizza,  sandwich)
 
 
 sum_stats_sample2 = function(vars, df){
   require(psych)
   
   # vars for getting main stats 
   # df for getting furhter info
   
   x = as.data.frame(describe(vars_s2)) 
   
   lab_vars = c("Rating inspection: 3", 
                "Rating inspection: 4", 
                "Rating inspection: 5",
                "Primary school", 
                "Secondary school",
                "College",
                "IMD (deciles)",
                "Rating neighbour: 3", 
                "Rating neighbour: 4", 
                "Rating neighbour: 5",
                "Population aged 0-15 (%)",
                "Job Seekers (%)",
                "Min distance takeaways (km)",
                "Closest takeaway is Asian",
                "Closest takeaway is burger and  fried chicken",
                "Closest takeaway is fish and chips",
                "Closest takeaway is kebab",
                "Closest takeaway of other",
                "Closest takeaway pizza",
                "Closest takeaway of sandwich")
   
   
   
   row.names(x) = lab_vars
   
   x$variable  =  row.names(x)
   
   
   y = x %>% select(variable, mean, sd, min, max) %>% as.data.frame()
   
   # round variables
   g = y %>% mutate_at(vars(mean, sd, min, max), funs(as.numeric)) %>%
     mutate_if(is.numeric, round, digits = 2)
   
   
   add_vars = data.frame(variable = c("Observations", "Local Authorities (district)"), 
                         mean= c(length(unique(df$from_id)), length(unique(df$oslaua_from))), 
                         sd = c(NA, NA), 
                         min = c(NA, NA), 
                         max = c(NA, NA))
   
   g = bind_rows(g, add_vars)
   return(g)
   
 }
 
 

 stats_s2 = sum_stats_sample2(vars_s2, clean_schools_v2)
 stats_s2 

 
 sum_stats_total = bind_rows(stats_s1, stats_s2)
 
 export(sum_stats_total, "output/tables/summary_stats.xlsx")
 