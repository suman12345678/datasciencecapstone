# ui.R
library(shiny)
shinyUI(fluidPage(
  titlePanel("Predicting the Next Word after user input developed by : Suman"),
  fluidRow(
    column(12,
           br(),
           h4("This application predict the next word when you type it similar to google search"),
           h4("This data is taken from twitter,news and blogs to construct phrases"),
           br(),
           h4("Type a phrase below. Artical/prepositions/auxluary verbs are not predicted"),
           br(),
           h4("Select number of prediction from 1 to 10 to suggest number of words"),
           br(),
           h4("Below that, you will see the predicted number of words"),
           br()
    )
  ),
  fluidRow(
    column(6,
           textInput("xx", 
                     label = "Enter some text here:", 
                     value = " "
           )             
    )    
  ),
  fluidRow(
    column(10,
           sliderInput("noofpred", "No. of Predictions:",
                                              value = 1.0, min = 1.0, max = 10.0, step = 1.0)             
    )    
  ),
 fluidRow(
    column(12,
           br(),
           h4("Predicted next word is :", style = "color:blue"), 
           verbatimTextOutput("text1"),
           br(),
           br(),
           br(),
           br(),
           h4("coursera assigment capstone project"),
           br()
    )
  )
))