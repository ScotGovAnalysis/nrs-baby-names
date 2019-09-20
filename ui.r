library(shiny)
library(dygraphs)
library(colourpicker)

# ----------------------------------------------------------------------------

shinyUI(fluidPage(
  # tags$head(includeScript("google-analytics.js"),
  #           tags$script(src="js/main.js")
  # ),
  
  HTML("<script src='https://cc.cdn.civiccomputing.com/8/cookieControl-8.x.min.js'></script>"),
  HTML("<script async src='https://www.googletagmanager.com/gtag/js?id=UA-91956629-1'></script>"),
  tags$script(src = "cookie_control_config.js"),

  fluidRow(#1
    column(3, img(src = "nrs_logo.png", height = 67, width = 235)),
    column(9, titlePanel("Baby names trends in Scotland since 1974"))
  ), # End of fluidRow #1
  
  br(),
  h4("Enter a", strong("name,"), "select ", strong("sex"), "and click on", strong("'Apply'"), "to see how a name's popularity has changed over the years. You can enter up to two names and choose the colour for each one."),
  h4("App might be slow at busy times. Please be patient."),
  br(),
  #h5(strong("Note:"), "Data for 2017 are provisional figures up to 2 December and only available for top 100 boys' and girls' names. This will be updated in March 2018."),
  #br(),
  
  fluidRow(#2
    column(2, offset=1, textInput("Name1", "Name", placeholder="Enter name", width='150px')),
    column(2, selectInput("Sex1", "Sex", choices = list("Male", "Female"), selected = "Female", width='150px')),
    column(1, colourpicker::colourInput("col1", "Colour", "#703989", showColour = "background"))), # End of fluidRow #2

  fluidRow(#3
    column(2, offset=1, textInput("Name2", "", placeholder="Enter name", width='150px')),
    column(2, selectInput("Sex2", "", choices = list("Male", "Female"), selected = "Female", width='150px')),
    column(1, colourpicker::colourInput("col2", "", "#FF780E", showColour = "background"))), # End of fluidRow #3
  
  fluidRow(#4
    column(2, offset=1, submitButton(text="Apply")),  # End of fluidRow #4
    
  br(),
  br(),
  dygraphOutput("dygraph_plot"),
  
  br(),
  
  fluidRow(
    column(12, offset=1,
           h5(strong("Note:"), "If there is no line for the name you selected, the name is not in our records."))),
  
  fluidRow(#5
    column(3, offset=1,
           h6(strong("How to use this app")),
           h6("Hover over years to highlight individual values"),
           h6("Click and drag to zoom"),
           h6("Double-click to zoom out"),
           br()),
    column(4,
           h6("Data: ", a("Baby names, Scotland, 1974-2018 (csv)", 
                href="http://www.nrscotland.gov.uk/files//statistics/nrs-visual/baby-18/babies-first-names-18-vis-final.csv",
                target="_blank")),
           h6("Publications: ", a("Baby names, Scotland, 1974-2018", 
                href="http://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/names/babies-first-names", 
                target="_blank")),
           br()),
    column(4,
           h6(a("National Records of Scotland", href="http://www.nrscotland.gov.uk", target="_blank")),
           h6("© Crown Copyright 2019 - ",
              a("Copyright conditions", href="http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/", target="_blank")),
           h6("Follow us on Twitter - ", 
              a("@NatRecordsScot", href="https://twitter.com/NatRecordsScot", target="_blank")),
           h6("See more ", a("Infographics & Visualisations", href="http://www.nrscotland.gov.uk/statistics-and-data/statistics/stats-at-a-glance/infographics-and-visualisations", 
                             target="_blank")),
           br())
    ), # End of fluidRow #5
    br(),
    h6("Any feedback about this visualisation?", 
        a("Get in touch!", href="mailto:victoria.avila@nrscotland.gov.uk?cc=statisticscustomerservices@nrscotland.gov.uk&subject=Baby%20names%202015%20visualisation"))
  ) # End of fluidPage
) # End of shinyUI
)