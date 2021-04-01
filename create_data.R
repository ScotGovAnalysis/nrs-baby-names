library(dplyr)
library(readr)
library(tidyr)
library(feather)

babynames <- read_csv(
  file = "www/Babies First Names - all names - all years.csv") %>%
  select(yr, sex, FirstForename, number) %>%
  transmute(firstname = as.character(FirstForename),
            sex = case_when(sex == "B" ~ "Male",
                            sex == "G" ~ "Female") %>%
              as.factor(),
            year = yr,
            number = number) %>%
  spread(key = year, value = number, fill = 0)

write_feather(babynames, "www/data_all.feather")
