
### Server function ###########################################################
shinyServer(function(input, output, session) {
  
  # Forces the selext sex button to select female if nothing selected
  observe({
    if (length(input$select_sex) < 1) {
      updateCheckboxGroupButtons(session,
                                 "select_sex",
                                 selected = c("Female"))
    }
  })

  # Show loading names message when app is loading
  output$progress <- reactive({
    withProgress(message = "Loading names...", value = 0, {
      n <- 40
      for (i in 1:n) {
        incProgress(1 / n)
        Sys.sleep(0.1)
      }
    })
  })

  # Reactively update selected sex
  selected_sex <- reactive({
    selected_sex <- input$select_sex
  })

  # reactively create tidy names data set with user inputed names
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

  # Create df with only valid names from the user input
  tidy_valid_names <- reactive({
    intersect(tidy_names(), first_names)
  })
  
  tidy_valid_names_rank <- reactive({
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

  # calculate suggested names
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
  


  # Collection methods note -------------------------------------------------
  output$text_rank <- renderUI({
    if (input$radio == 2)
         
    tagList("Note: Collection methods changed from 1974. See ",
            a("the publication that explains this.",
              href = "https://www.nrscotland.gov.uk/files//statistics/babies-names/historic/baby-names-1935-2022-key-findings.pdf")) 
    })

  # Create reactive df for ranked baby names
  df_babynames_rank <- reactive({
    babynames %>%
      select(c(firstname, sex, starts_with("rank_")))  %>% 
      rename_with(~str_remove(., 'rank_')) %>% 
      filter(firstname %in% tidy_valid_names,
             sex %in% selected_sex) %>%
      gather(key = "year", value = "rank", -firstname, -sex) %>%
      mutate(year = as.numeric(year),
             rank = as.numeric(ifelse(!(rank %in% 1:100), NA_character_, rank))) %>% 
      group_by(firstname, sex) %>% 
      complete(year = min(year):max(year)) %>% 
      ungroup() %>% 
      mutate(label = paste(firstname, "<br>", sex)) %>% 
      filter(rank %in% c(1:100)) 
  })
  
  # render highchart 
  output$plot <- renderHighchart({
    if (length(tidy_valid_names() > 0)) {
      if (input$radio == 1) {
        create_plot(
          babynames = babynames,
          tidy_valid_names = tidy_valid_names(),
          selected_sex = selected_sex()
        )
      } else{
          create_plot_rank(
            babynames = babynames,
            tidy_valid_names = tidy_valid_names(),
            selected_sex = selected_sex()
          )
      }
    }
  })

  outputOptions(output, "valid_names", suspendWhenHidden = FALSE)
  outputOptions(output, "progress", suspendWhenHidden = FALSE)

 })