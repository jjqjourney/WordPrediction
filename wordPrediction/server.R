#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(qdap)
source('predictInputString.R')


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  #output$value <- renderText({ predictTop(as.character(input$inputString)) })
  
  output$value <- renderText({
    trimmed <- toString(input$inputString)
    text <- strsplit(trimmed, split= " ")[[1]]
    if (length(text) > 0) {
      sentence <- tolower(trimws(clean(paste(text[(length(text) - 2): (length(text))], collapse=" "))))
      result <- predictTop(sentence)
    }
  })
})
