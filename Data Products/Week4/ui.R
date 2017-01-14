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
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Predict the Miles per Gallon for Car with Selected Features Below"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h3("Please select the features and check the predicted values of MPG"),
      #Transmission
      selectInput("tr", "Transmission Type",choices = c("Automatic", "Manual")),
      
      # Quarter Mile Time in Seconds
      sliderInput("qsec", "Quarter Mile Time in Seconds:",min=14, max=22,value = 0.5, step= 0.1)
      
      #Weight
      ,sliderInput("wt", "Weight in 1000 lbs:",min=1, max=6,value = 0.5, step= 0.1)
      
      #Gross Horse Power
      ,sliderInput("hp", "Gross Horse Power:",min=50, max=400,value = 0.5, step= 0.1)
      
    ),
    

    mainPanel(
       h3("Your Predicted Miles Per Gallon is as below."),
      textOutput("mpg")
    )
  )
))
