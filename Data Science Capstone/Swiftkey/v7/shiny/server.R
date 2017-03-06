#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/

# # source("library.R",local = TRUE)
# # library(shiny)
# load("words_vector_data.RData")
# #isolate({source("model_gloveV2.R")})
# source("model_gloveV2.R",local = TRUE)
# # #Clean the input query.
# # source("cleansample.R",local = TRUE)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$value<-renderText({ 
    clean_words<-cleansample(input$caption)$sample
    clean_words<-predicted_word(clean_words,word_vectors)
    
    
   
    
  })
  
} )
