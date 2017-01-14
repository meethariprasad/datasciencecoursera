#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  
  output$mpg <- renderText({
    
    if(input$tr=="Automatic"){tr<-0}else{tr<-1}
    qsec<-input$qsec
    wt<-input$wt
    hp<-input$hp
    mpg<-17.44019+wt*-3.23810+tr*-2.92550+qsec*0.81060+hp*-0.01765
    
  })
  
})
