library(shiny)
library(dygraphs) # renderDygraph
library(stringr) # str_trim, str_to_title

load("babynames1974_2018final.rdata")

shinyServer(function(input, output) {
  # ------------------------------------------------------------------------------
  observe({  
    if ( input$Name1!="" & input$Name2!="" ) {
      output$dygraph_plot <- renderDygraph({
        req(input$Name1, input$Name2)
        dygraph_data<-as.data.frame(cbind(1974:2018, 
                                          as.numeric(babynames[babynames$firstname==str_trim(str_to_title(input$Name1)) & babynames$sex==input$Sex1, -(1:2)]),
                                          as.numeric(babynames[babynames$firstname==str_trim(str_to_title(input$Name2)) & babynames$sex==input$Sex2, -(1:2)])))
        
        main_lb<-""
        if (input$Sex1 == input$Sex2) {
          main_lb<-ifelse(input$Sex1 == "Male",
                          paste("Boys named", str_trim(str_to_title(input$Name1)), "and", str_trim(str_to_title(input$Name2))),
                          paste("Girls named", str_trim(str_to_title(input$Name1)), "and", str_trim(str_to_title(input$Name2))))
        }
        else {
          main_lb<-paste(ifelse(input$Sex1 == "Male",
                          paste("Boys named", str_trim(str_to_title(input$Name1))),
                          paste("Girls named", str_trim(str_to_title(input$Name1)))),
                         "and",
                         ifelse(input$Sex2 == "Male",
                                paste("boys named", str_trim(str_to_title(input$Name2))),
                                paste("girls named", str_trim(str_to_title(input$Name2)))))
        }

        dygraph(dygraph_data, main = main_lb) %>%
          
          dyAxis("x", label = "Year", rangePad = 5)  %>%
          
          dySeries(names(dygraph_data)[2], label = ifelse(input$Sex1 == "Male",
                                                          paste(str_trim(str_to_title(input$Name1)), "- Boys"),
                                                          paste(str_trim(str_to_title(input$Name1)), "- Girls")),
                   strokeWidth = 3, color = input$col1) %>%
          dySeries(names(dygraph_data)[3], label = ifelse(input$Sex2 == "Male",
                                                          paste(str_trim(str_to_title(input$Name2)), "- Boys"),
                                                          paste(str_trim(str_to_title(input$Name2)), "- Girls")),
                   strokeWidth = 3, color = input$col2) %>%

          dyOptions(fillGraph = T, fillAlpha = 0.4, drawGrid = F, includeZero = T,
                    drawPoints = T, pointSize = 4, axisLabelWidth = 40)  %>%
          
          dyHighlight(highlightCircleSize = 8, highlightSeriesBackgroundAlpha = 1)
        
      })
      
      # ------------------------------------------------------------------------------
      
    }
    else if( input$Name1=="" & input$Name2!="" ) {
      output$dygraph_plot <- renderDygraph({
        req(input$Name2)
        dygraph_data<-as.data.frame(cbind(1974:2018, 
                                          as.numeric(babynames[babynames$firstname==str_trim(str_to_title(input$Name2)) & babynames$sex==input$Sex2, -(1:2)])))
        
        main_lb<-""
        main_lb<-ifelse(input$Sex2 == "Male",
                          paste("Boys named", str_trim(str_to_title(input$Name2))),
                          paste("Girls named", str_trim(str_to_title(input$Name2))))
        

        dygraph(dygraph_data, main = main_lb) %>%
          
          dyAxis("x", label = "Year", rangePad = 5)  %>%
          
          dySeries(names(dygraph_data)[2], label = str_trim(str_to_title(input$Name2)), strokeWidth = 3, color = input$col2) %>%
          
          dyOptions(fillGraph = T, fillAlpha = 0.4, drawGrid = F, includeZero = T,
                    drawPoints = T, pointSize = 4, axisLabelWidth = 40)  %>%
          
          dyHighlight(highlightCircleSize = 8, highlightSeriesBackgroundAlpha = 1)
        
      })
      
      # ------------------------------------------------------------------------------
      
    }
    else if( input$Name1!="" & input$Name2=="" ) {
      output$dygraph_plot <- renderDygraph({
        req(input$Name1)
        dygraph_data<-as.data.frame(cbind(1974:2018, 
                                          as.numeric(babynames[babynames$firstname==str_trim(str_to_title(input$Name1)) & babynames$sex==input$Sex1, -(1:2)])))
        
        main_lb<-""
        main_lb<-ifelse(input$Sex1 == "Male",
                        paste("Boys named", str_trim(str_to_title(input$Name1))),
                        paste("Girls named", str_trim(str_to_title(input$Name1))))

        dygraph(dygraph_data, main = main_lb) %>%
          
          dyAxis("x", label = "Year", rangePad = 5)  %>%
          
          dySeries(names(dygraph_data)[2], label = str_trim(str_to_title(input$Name1)), strokeWidth = 3, color = input$col1) %>%
          
          dyOptions(fillGraph = T, fillAlpha = 0.4, drawGrid = F, includeZero = T,
                    drawPoints = T, pointSize = 4, axisLabelWidth = 40)  %>%
          
          dyHighlight(highlightCircleSize = 8, highlightSeriesBackgroundAlpha = 1)
        
      })
    }
  })
  
  
  
  
  
  
  
})