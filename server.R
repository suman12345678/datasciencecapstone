# server.R

library(shiny)
source("predictword.R")

shinyServer(
  function(input, output) {
    output$text1 <- renderText({
      paste(predictnextword(input$xx,input$noofpred))
    })
    
  }
)