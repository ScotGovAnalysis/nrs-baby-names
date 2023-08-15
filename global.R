library(shiny)
library(tidyverse)
library(stringdist)
library(shinyjs)
library(htmlwidgets)
library(feather)
library(shinyWidgets)
library(highcharter)

### Load data #################################################################
babynames <- read_feather("www/data_all.feather")

first_names <- unique(babynames[["firstname"]])

# URL for the twitter link
url <- "https://twitter.com/intent/tweet?text=Check%20out%20how%20popular%20your%20name%20is%20here!&hashtags=NRSstats&url=https://scotland.shinyapps.io/nrs-baby-names/"

# Globally format thousands separator for highcharts
hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
options(highcharter.lang = hcoptslang)

# Theme for highcharts
hc_theme <- hc_theme(
  chart = list(style = list(fontFamily = "Roboto")),
  xAxis = list(labels = list(style = list(fontSize = "18px")),
               title = list(enabled = F),
               tickLength = 0,
               lineColor = "transparent"),
  yAxis = list(labels = list(style = list(fontSize = "18px"),
                             format = "{value:,.0f}"),
               title = list(style = list(fontSize = "18px"),
                           fontFamily = "Roboto",
                           enabled = F),
               gridLineWidth = 0),
  tooltip = list(style = list(fontSize = "18px")),
  legend = list(symbolWidth = 40,
                itemStyle = list(fontSize = "18px",
                                 fontStyle = "normal",
                                 fontWeight = "light")),
  plotOptions = list(line = list(lineWidth = 4,
                                 clip = F)))



# create function for alternative name suggestions ------------------------

create_suggestions <- function(first_name, suggestions_df) {
  
  similar_names <- suggestions_df %>%
    filter(`Your name(s)` == first_name) %>%
    pull(`Similar names`)
  
  list(strong(paste0(first_name, ": ")),
       lapply(similar_names, function(x)
         actionLink(
           inputId = paste0("action_", x), label = x
         )),
       br())
}


# Function to create the total numbers plot  ------------------------------

create_plot <- function(babynames = babynames,
                        tidy_valid_names = tidy_valid_names,
                        selected_sex = selected_sex) {
  
  df_baby_names <- babynames %>%
    select(-c(starts_with("rank_"))) %>%
    filter(firstname %in% tidy_valid_names,
           sex %in% selected_sex) %>%
    gather(key = "year", value = "count", -firstname, -sex) %>%
    filter(year >= 1974) %>% 
    mutate(year = as.numeric(year),
           label = paste(firstname, "<br>", sex))
  
  
  
  main_plot <- hchart(
    df_baby_names %>% filter(sex == "Female"),
    type = "line",
    hcaes(
      x = year,
      y = count,
      group = label)
  ) %>%
    hc_add_series(
      df_baby_names %>% filter(sex == "Male"),
      type = "line",
      hcaes(
        x = year,
        y = count,
        group = label)
    ) %>%
    hc_yAxis(title = list(enabled = F)) %>% 
    hc_legend(labelFormatter = JS("function () {
            return this.name + '<br>(click to hide)';
        }")) %>%
    hc_add_theme(hc_theme)
  
}

# Function to create the RANK plot  ------------------------------

create_plot_rank <- function(babynames = babynames,
                             tidy_valid_names = tidy_valid_names,
                             selected_sex = selected_sex) {
  
  categories_list <- list("1" = "1st", "50" = "50th", "100" = "100th")
  categories <- seq(0, 100)
  
  for (v in names(categories_list)) {
    categories[[as.integer(v) + 1]] <- categories_list[[v]]
  }

  
  df_baby_names <- babynames %>%
    select(c(firstname, sex, starts_with("rank_")))  %>% 
    rename_with(~str_remove(., 'rank_')) %>% 
    filter(firstname %in% tidy_valid_names,
           sex %in% selected_sex) %>%
    gather(key = "year", value = "rank", -firstname, -sex) %>%
    mutate(year = as.numeric(year),
           rank = as.numeric(ifelse(!(rank %in% 1:100), NA_character_, rank)),
           rank = replace(rank, rank == 0, NA)) %>% 
    group_by(firstname, sex) %>% 
    complete(year = min(year):max(year)) %>% 
    ungroup() %>% 
    mutate(label = paste(firstname, "<br>", sex),
           rank = case_when(!(rank %in% 1:100) ~ NA,
                            TRUE ~ rank))
  
  main_plot <- hchart(
    df_baby_names %>% filter(sex == "Female"),
    type = "line",
    hcaes(
      x = year,
      y = rank,
      group = label),
    tooltip = list(pointFormat = "{point.label} <br> Rank: <b>{point.rank}</b>")) %>% 
    hc_add_series(
      df_baby_names %>% filter(sex == "Male"),
      type = "line",
      hcaes(
        x = year,
        y = rank,
        group = label),
      tooltip = list(pointFormat = "{point.label} <br> Rank: <b>{point.rank}</b>")
    ) %>% 
    hc_yAxis(reversed = T,
             categories = categories,
             tickPositions = c(1, 50, 100)) %>% 
    hc_xAxis(min = 1935,
             max = 2022) %>% 
    hc_legend(labelFormatter = JS("function () {
            return this.name + '<br>(click to hide)';
        }")) %>% 
    hc_add_theme(hc_theme)
  
}


# Default plot to reactively modify ---------------------------------------

default_plot <- create_plot(babynames = babynames,
                            tidy_valid_names = c("Olivia", "Noah"),
                            selected_sex = c("Female", "Male"))