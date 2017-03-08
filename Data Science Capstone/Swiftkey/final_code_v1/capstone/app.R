rm(list = ls())
library(shiny)
members <- data.frame(name=c("Name 1", "Name 2"), nr=c('glove','count'))

ui <- fluidPage(titlePanel("Predict Next Word using GloVe or Ngram Backoff algorithms"), 
                sidebarLayout(
                  sidebarPanel(
                    fluidRow(
                      column(6, selectInput("Member", label=h5("Choose an Algorithm and Enter the words to predict next word:"),choices=c('glove','count'))
                      ))),
                  mainPanel(fluidRow(
                    htmlOutput("frame")
                  )
                  )
                ))

server <- function(input, output) {
  observe({ 
    
    if(input$Member=="glove"){
      query<-"glove"
    }else{
      query<-"Presentation"
    }
    
    test <<- paste0("https://meethariprasad.shinyapps.io/",query)
  })
  output$frame <- renderUI({
    input$Member
    my_test <- tags$iframe(src=test, height=600, width=900)
  })
}

shinyApp(ui, server)