#Text to Vec Predict next Word.
# https://stevenfupc.shinyapps.io/shiny/
library(text2vec)
#Prediction
predicted_word<-function(test_sentence){
      #Test iterator
      test_text<-test_sentence
      it_test = itoken(test_text, progressbar = FALSE)
      #Create Term Matrices of Test and Train on
      #Projected Vector and then compare similiarities.
      dtm1 = create_dtm(it_train, vectorizer)
      dtm2 = create_dtm(it_test, vectorizer)
      jd<-sim2(dtm1, dtm2, method = "jaccard", norm = "none")
      #Create dataframe and order it
      distance<-data.frame(jd[,1])
      distance_order<-order(-distance)
      #Pick Top Prediction
      #as.character(tri.df$Gram_3[distance_order][1:5])
      top_predict<-as.character(tri.df$Gram_3[distance_order][1:5])
      #Print top predictions
      return(top_predict)
}