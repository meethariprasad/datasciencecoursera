
library(shiny)


shinyServer(function(input, output) {
   
  
  output$mpg <- renderText({
    
    if(input$tr=="Automatic"){tr<-0}else{tr<-1}
    qsec<-input$qsec
    wt<-input$wt
    hp<-input$hp
    mpg<-17.44019+wt*-3.23810+tr*-2.92550+qsec*0.81060+hp*-0.01765
    
  })
  
})
