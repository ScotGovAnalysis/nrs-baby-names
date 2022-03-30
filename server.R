### Load data #################################################################
babynames <- read_feather("www/data_all.feather")

first_names <- unique(babynames[["firstname"]])

### NRS style #################################################################
  nrs_font <- "Segoe UI"
  info_text_size <- 3.5
  
  col_neut_black <- "#000000"
  col_neut_tundora <- "#4B4B4B"
  col_neut_grey <- "#808080"
  col_neut_silver <- "#b9b9b9"
  col_neut_white <- "#FFFFFF"
  
  col_mig <- "#90278E"
  col_mig_dark <- "#7317F1"
  col_mig_light <- "#C793C6"
    
  col_adopt <- "#EE6214"
  col_adopt_dark <- "#BE4E10"
  col_adopt_light <- "#F6B089"
  
  col_births <- "#2E8ACA"
  col_deaths <- "#284F99"
  col_house <- "#5C7B1E"
  col_lifexp <- "#6566AE"
  col_elec <- "#C9347C"
  col_pop <- "#2DA197"
  
  col_all <- c(
    col_mig,
    col_adopt,
    col_house,
    col_births,
    col_elec,
    col_lifexp,
    col_deaths,
    col_pop
  )
  
  col_select <- c(
    col_mig,
    col_adopt,
    col_house,
    col_pop
  )
  
  
  info_point_shape <- 21
  info_point_size <- 2
  info_point_stroke <- 0.5
  
  info_line_size <- 1.4
  
  info_axis_text_size_large <- 13
  info_axis_text_size_small <- 10
  
### Make theme function #######################################################
  theme_info <- function() {
    ggplot2::theme(
      # Declutter
      strip.background =  ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks = element_blank(),

      #Text
      text = ggplot2::element_text(family = nrs_font),
      plot.title = element_text(hjust = 0, size = info_axis_text_size_large),
      axis.text = element_text(size = info_axis_text_size_small),
      strip.text.x = element_blank(),

      # Margin
      plot.margin = unit(c(0, 0, 0, 0), "npc"),
      panel.spacing = unit(0.1, "npc")
    )
  }
  
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

create_plot <- function(babynames = babynames,
                        tidy_valid_names = tidy_valid_names,
                        selected_sex = selected_sex) {

  df_baby_names <- babynames %>%
   filter(firstname %in% tidy_valid_names,
       sex %in% selected_sex) %>%
    gather(key = "year", value = "count", -firstname, -sex) %>%
    mutate(year = as.numeric(year))

  main_plot <- plot_ly(
    data = df_baby_names,
    x = ~ year,
    y = ~ count,
    color = ~ firstname,
    marker = list(size = 7)
  ) %>%
    group_by(sex) %>%
    add_trace(
      type = "scatter",
      mode = "markers+lines",
      color = ~ firstname,
      colors = col_select,
      linetype = ~ sex,
      linetypes = c("solid", "dot"),
      hovertemplate = paste("<b>%{x}</b>: %{y}")
    ) %>%
    config(displayModeBar = FALSE,
           showAxisDragHandles = FALSE) %>%
    layout(
      xaxis = list(
        linecolor = rgb(255, 255, 255, maxColorValue = 255),
        width = 0,
        fill = NA,
        fixedrange = TRUE,
        bty = "n",
        showline = FALSE,
        title = "",
        showgrid = FALSE,
        tickvals = c(1974, 1981, 1991, 2001, 2011, 2021),
        zeroline = FALSE,
        tickfont = list(size = 18)
      ),
      yaxis = list(
        fixedrange = TRUE,
        showline = FALSE,
        title = "",
        showgrid = FALSE,
        tickformat = "f.0",
        tickfont = list(size = 18),
        tick0 = 0,
        zeroline = FALSE
      ),
      legend = list(orientation = "h",
                    font = list(size = 18)),
      paper_bgcolor = "rgba(0, 0, 0, 0)",
      plot_bgcolor = "rgba(0, 0, 0, 0)",
      margin = list(l = 0,
                    r = 0),
      showlegend = T) %>%
    onRender(
      "function(el, x) {
      Plotly.d3.select('.cursor-pointer').style('cursor', 'crosshair')}"
    )

  if (max(df_baby_names[["count"]]) < 15) {
    main_plot %>%
      layout(yaxis = list(dtick = 1))
  } else {
    main_plot
  }
}

default_plot <- create_plot(babynames = babynames,
                            tidy_valid_names = c("Isla", "Jack"),
                            selected_sex = c("Female", "Male"))

### Server function ###########################################################
shinyServer(function(input, output, session) {

  observe({
    if (length(input$select_sex) < 1) {
      updateCheckboxGroupButtons(session,
                                 "select_sex",
                                 selected = c("Female",
                                              "Male"))
    }
  })

  output$progress <- reactive({
    withProgress(message = "Loading names...", value = 0, {
      n <- 40
      for (i in 1:n) {
        incProgress(1 / n)
        Sys.sleep(0.1)
      }
    })
  })

  selected_sex <- reactive({
    selected_sex <- input$select_sex
  })

  tidy_names <- eventReactive(input$goButton, {
    tidy_names <- input$name %>%
      stringr::str_to_title() %>%
      strsplit(split = "[^A-Za-z'-]") %>%
      `[[`(1) %>%
      stringi::stri_remove_empty() %>%
      stringr::str_replace_all("T.j.", "T.J.") %>%
      unique()
    tidy_names
    })

  tidy_valid_names <- reactive({
    intersect(tidy_names(), first_names)
  })

  tidy_invalid_names <- reactive({
    setdiff(tidy_names(), tidy_valid_names())
  })

  output$valid_names <- reactive({
    if (length(tidy_valid_names()) > 0)
      return(TRUE)
    else
      return(FALSE)
  })

  suggestions_df <- reactive({
    stringdistmatrix(first_names,
                     tidy_names(), method = "lv") %>%
      as.data.frame() %>%
      `names<-`(tidy_names()) %>%
      mutate(`Similar names` = first_names) %>%
      gather(key = "Your name(s)", value = "Distance", -`Similar names`) %>%
      filter(Distance != 0) %>%
      group_by(`Your name(s)`) %>%
      top_n(n = 10, wt = -Distance) %>%
      ungroup()
  })

  observeEvent(input$js.link_clicked, {
    n <- unlist(str_split(input$js.link_clicked, "_"))[[2]]
    updateTextInput(session = session,
                       inputId = "name",
                       value = n)
    click("goButton")
  })

  output$suggestions <- renderUI({
    list(
      h2("Similar names:"),
      lapply(
        X = tidy_names(),
        FUN = create_suggestions,
        suggestions_df = suggestions_df()
      )
    )
  })

  output$text <- renderText({
    if (length(tidy_invalid_names()) > 0)
      paste("Oops! No babies have been recorded with the name(s):",
            paste(tidy_invalid_names(), collapse = ", "))
  })

  output$plot <- renderPlotly({

    if (length(tidy_valid_names() > 0)) {

        create_plot(babynames = babynames,
                    tidy_valid_names = tidy_valid_names(),
                    selected_sex = selected_sex())
    }
  })

  outputOptions(output, "valid_names", suspendWhenHidden = FALSE)
  outputOptions(output, "progress", suspendWhenHidden = FALSE)

 })