library(shiny)
library(ggplot2)
library(tidyverse)
library(plotly)
library(dplyr)
library(stringdist)
library(shinyjs)
library(readr)
library(htmlwidgets)
library(feather)
library(shinyWidgets)

url <-
  "https://twitter.com/intent/tweet?text=Check%20out%20how%20popular%20your%20name%20is%20here!&hashtags=NRSstats&url=https://shiny.rstudio.com/gallery/widget-gallery.html/"

shinyUI(fluidPage(
  
  tags$head(includeCSS("www/style.css")),
  tags$html(lang = "en"),
  windowTitle = "NRS Baby Names",
  
  useShinyjs(),
  tags$head(HTML("<script src='https://cc.cdn.civiccomputing.com/8/cookieControl-8.x.min.js'></script>"),
            HTML("<script async src='https://www.googletagmanager.com/gtag/js?id=UA-91956629-1'></script>"),
            tags$style(type = "text/css",
                       "overflow-x: hidden;"),
            tags$script(src = "js.js"),
            tags$script(src = "cookie_control_config.js")),
  
  fluidRow(
    column(
      width = 12,
      align = "center",
      br(),
      a(
        href = "https://www.nrscotland.gov.uk/",
        img(
          src = "nrs_logo.svg",
          height = 33.5,
          width = 168.909653,
          alt = "National Records of Scotland logo. Directs to https://www.nrscotland.gov.uk/")
      ),
      a(
        href = "https://www.scotlandspeople.gov.uk/",
        img(
          src = "scotlandspeople_logo.svg",
          height = 33.5,
          width = 168.909653,
          alt = "Scotlands People logo. Directs to https://www.scotlandspeople.gov.uk/")
      )
    )),
  
  
  fluidRow(
    column(
      width = 12,
      align = "center",
      h1("Baby Names"),
      "How popular is your name in Scotland?",
      textInput(
        inputId = "name",
        label = NULL,
        value = "Isla, Jack"
      ),
      checkboxGroupButtons(
        inputId = "select_sex", 
        label = NULL, 
        choices = c("Female", "Male"), 
        selected = "Female", 
        width = "250px",
        justified = TRUE,
        status = "primary",
        checkIcon = list(yes = icon("check"), no = icon("times"))
        ),
      actionButton(inputId = "goButton", 
                   label = "Go!",
                  # icon = icon("check"),
                   style = "color: #fff; background-color: #84329B; border-color: #731F71; font-size:20px")
    )
  ),
  fluidRow(
    column(
      width = 12,
      align = "center",
      textOutput(outputId = "text"),
      conditionalPanel(condition = "output.valid_names",
                       plotlyOutput(outputId = "plot"))
    )
  ),
  fluidRow(column(
    12,
    align = "center",
    conditionalPanel(condition = "'' != input.name",
                     uiOutput(outputId = "suggestions"))
  )),
  fluidRow(column(
    width = 12,
    align = "center",
    br(),
    br(),
    p(
      "This chart is maintained by",
      strong(a("National Records of Scotland.",
        href = "https://www.nrscotland.gov.uk/")),
    "We also produce",
    strong(a("official statistics",
        href = "https://www.nrscotland.gov.uk/statistics-and-data")),
      "about Scotland's population and can help you",
    strong(a("find your Scottish ancestors.",
        href = "https://www.scotlandspeople.gov.uk/"))
    )
  )), 
  fluidRow(
    column(
      width = 12,
      align = "center",
      br(),
      actionButton(
        "twitter_share",
        label = "Share",
        icon = icon("twitter"),
        onclick = sprintf("window.open('%s')", url)
      ),
  fluidRow(
    column(
      width = 12,
      align = "center",
      br(),
      p(
        "Download ",
        strong(a(
          "the data",
          href = "https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/names/babies-first-names/babies-first-names-summary-records-comma-separated-value-csv-format")),
        br(),
        "Read the ",
        strong(a(
          "official statistics",
          href = "https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/names/babies-first-names/babies-first-names-2019"
        )),
        br(),
        "See more ",
        strong(a(
          "infographics & visualisations",
          href = "http://www.nrscotland.gov.uk/statistics-and-data/statistics/stats-at-a-glance/infographics-and-visualisations"
        ))
      ),
      p(
        a("Email us",
          href = "mailto:StatisticsCustomerServices@nrscotland.gov.uk"),
        " or follow us on Twitter: ",
        a("@NatRecordsScot",
          href = "https://twitter.com/NatRecordsScot")
      ),
      p(
        a("© Crown Copyright 2020",
          href = "http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/")
      )
    )
  ),
      br(),
      br()
    )
  ),
  tags$script(src = "mouseflow.js")
))