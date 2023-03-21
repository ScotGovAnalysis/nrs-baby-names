library(dplyr)
library(readr)
library(tidyr)
library(feather)

babynames <- read_csv(
  file = "www/Babies First Names - all names - all years.csv") %>%
  select("year" = yr, sex, "firstname" = FirstForename, number) %>%
  mutate(firstname = as.character(firstname),
         sex = as.factor(case_when(sex == "B" ~ "Male",
                         sex == "G" ~ "Female"))) %>%
  distinct(.keep_all = TRUE) %>% 
  pivot_wider(names_from = year, 
              values_from = number, 
             values_fill = 0)



write_feather(babynames, "www/data_all.feather")
