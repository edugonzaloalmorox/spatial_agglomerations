library(ggplot2)
library(tibble)
library(rio)
library(broom)
library(margins)
library(Ecdat)
library(splines)
library(bife)
library(lmerTest)
library(lme4)
library(tidyverse)

clean_schools = import("data/processed/clean_schools_500.csv")


clean_schools = clean_schools %>%
  mutate_at(vars(starts_with("rating_"), from_id), funs(as.factor)) %>%
  mutate_at(vars(jsa_rate), funs(as.numeric)) %>%
  mutate(imd_recoded = ifelse(imd_decile == 1, 1, 0))





logit_3 = glm(rating_3 ~ mean_distance + n_asian + n_chicken + n_fish + n_kebab + n_other + n_pizza + n_sandwich + factor(imd_recoded) + jsa_rate, data = clean_schools, family = "binomial")

logit_4 = glm(rating_4 ~  mean_distance + n_asian + n_chicken + n_fish + n_kebab + n_other + n_pizza + n_sandwich + factor(imd_recoded) + jsa_rate, data = clean_schools, family = "binomial")

logit_5 = glm(rating_5 ~  mean_distance + n_asian + n_chicken + n_fish + n_kebab + n_other + n_pizza + n_sandwich + factor(imd_recoded) + jsa_rate, data = clean_schools, family = "binomial")


# Print models
# -----------

summary(logit_3)
summary(logit_4)
summary(logit_5)

mod3 = tidy(logit_3) %>% mutate(model = 3)
mod4 = tidy(logit_4) %>% mutate(model = 4)
mod5 = tidy(logit_5) %>% mutate(model = 5)

all_models <- bind_rows(mod3, mod4, mod5)

all_models


logit_table <- all_models %>%
  mutate_at(vars(estimate, std.error), funs(round(., 3))) %>%
  mutate_at(vars(p.value), funs(round(., 2))) %>%
  mutate(beta = ifelse(p.value <= 0.05, paste0(estimate,"**"), 
                       ifelse(p.value <= 0.01, paste0(estimate, "***"),
                              ifelse(p.value<= 0.1, paste0(estimate, "*"), estimate )))) %>% 
  select(term, model, beta, std.error) %>%
  gather(key, value, beta:std.error) %>%
  spread(model, value) %>%
  mutate(model_3 = ifelse(key == "std.error", paste0("(",`3`, ")"), `3`),
         model_4 = ifelse(key == "std.error", paste0("(",`4`, ")"), `4`),
         model_5 = ifelse(key == "std.error", paste0("(",`5`, ")"), `5`)) %>%
  group_by(term) %>%
  mutate(variable = ifelse(term == lead(term), term, NA)) %>%
  ungroup() %>%
  select(variable, model_3:model_5)
 

export(logit_table, "output/tables/table-logit-0.5km.xlsx")
  
# Marginal effects

effects_logit_3 = margins(logit_3) 
effects_logit_4 = margins(logit_4) 
effects_logit_5 = margins(logit_5) 

sum_effects3 = summary(effects_logit_3) %>% mutate(rating = "rating5")
sum_effects4 = summary(effects_logit_4) %>% mutate(rating = "rating4")
sum_effects5 = summary(effects_logit_5) %>% mutate(rating = "rating3")

sum_effects = bind_rows(sum_effects3, sum_effects4, sum_effects5) %>%
  mutate(var = -p) %>%
  rename(variable = factor) %>%
  group_by(variable) %>% 
  arrange(variable)


sum_effects 



# plot marginal effect
ggplot(data = sum_effects) +
  geom_point(aes(variable, AME, color =rating, size = var)) +
  scale_size("p-value",breaks=c(-1, -0.1, -0.05, 0),labels=c(1,0.1, 0.05, 0)) +
  #geom_errorbar(aes(x = factor, ymin = lower, ymax = upper)) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))



me <- sum_effects %>%
  mutate_at(vars(AME, SE), funs(round(., 3))) %>%
  mutate_at(vars(p), funs(round(., 2))) %>%
  mutate_at(vars(AME, SE), funs(as.character))%>%
  mutate(beta = ifelse(p <= 0.05, paste0(AME,"**"),
                     ifelse(p <= 0.01, paste0(AME, "***"),
                             ifelse(p<= 0.1, paste0(AME, "*"), AME)))) %>% 
  select(variable, rating, beta, SE) %>%
  arrange(rating, variable) %>%
  gather(key, value, beta:SE) %>%
  arrange(rating,variable) %>%
  spread(rating, value) %>%
  mutate(model_3 = ifelse(key == "SE", paste0("(",`rating3`, ")"), `rating3`),
         model_4 = ifelse(key == "SE", paste0("(",`rating4`, ")"), `rating4`),
         model_5 = ifelse(key == "SE", paste0("(",`rating5`, ")"), `rating5`)) %>%
  group_by(variable) %>%
  mutate(term = ifelse(variable == lead(variable), variable, NA)) %>%
  ungroup() %>%
  select(term, model_3:model_5, -variable)

me

export(me, "output/tables/table-logit-me-1km.xlsx")

