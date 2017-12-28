#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(
  pageWithSidebar(
    headerPanel('Word Prediction App'),
    
    sidebarPanel(
      textInput("inputString", "Please type Your sentence:", "I go to the"),
      
      submitButton("Predict")
      
    ),
    mainPanel(
      HTML(
        paste(
          h4("Quick Guide"),
          h5("The application uses social media data from SwiftKey, back off model and Good-touring interpolation to predict the next word."),
          h5("Please type your intended sentence in the input box and wait a few seconds."),
          h5("The top possible words will be shown under it.")
        )
      ),
      
      verbatimTextOutput("value")
    )
  )
)

