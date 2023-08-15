library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(feather)


babynames <- read_csv(
  file = "www/Babies First Names - all names - all years.csv") %>%
  rbind(readxl::read_xlsx("www/FINAL_top_100_1935_1973.xlsx")) %>% 
  select(yr, sex, FirstForename, number, rank) %>%
  transmute(firstname = as.character(FirstForename),
            sex = case_when(sex %in% c("B", "Boy") ~ "Male",
                            sex %in% c("G", "Girl") ~ "Female",
                            TRUE ~ sex) %>%
              as.factor(),
            year = yr,
            number = number,
            rank = rank) %>%
  arrange(year, rank) %>% 
  pivot_wider(names_from = year, values_from = c(number, rank), values_fill = 0) %>% 
  rename_with(~str_remove(., 'number_'))

write_feather(babynames, "www/data_all.feather")

