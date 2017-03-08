#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
members <- data.frame(name=c("Name 1", "Name 2"), nr=c('BCRA1','FITM2'))
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  textInput("caption", "Enter the words and See predictions Below:", "love")
  ,verbatimTextOutput("glove")
  ,mainPanel(p("Prediction is from GloVe algorithm."))
  
  
  )
)
