#Text to Vec Predict next Word.
# Inpired by but enhanced from https://stevenfupc.shinyapps.io/shiny/
library(text2vec)
library(hunspell)
# load("ngrams.RData")
# rm(bi,tri,fr,fvr,sgm,uni,uni_levels,corpus,uni.df)
# gc()
load("vectorizer_dtm.RData")
#Prediction
#test_sentence<-cleaned_sample
predicted_word<-function(test_sentence){
      #Test iterator

      test_text<-test_sentence
      for(i in 1:length(test_text)){
        test <- unlist(strsplit(test_text, " "))
        t<-hunspell(test)
        t<-as.character(t)
        misspelt<-t["character(0)"!=t]
        if(length(misspelt)!=0){
          test_text<-removeWords(test_text,misspelt)}
      }
      clean_words <- unlist(strsplit(test_text, " "))
      it_test = itoken(test_text, progressbar = FALSE)
      
      #source('get_global_dtm.R')
      if(length(clean_words)>=5){
        #vectorizer_dtm<-get_global_dtm(sgm)
        #Create Term Matrices of Test and Train on
        #Projected Vector and then compare similiarities.
        dtm2 = create_dtm(it_test, vectorizer_dtm[[9]])
        jd<-sim2(vectorizer_dtm[[10]], dtm2, method = "jaccard", norm = "none")
        #Create dataframe and order it
        distance<-data.frame(jd[,1])
        distance_order<-order(-distance)
        #Pick Top Prediction
        #as.character(tri.df$Gram_3[distance_order][1:5])
        load("sgm.RData")
        top_predict<-as.character(sgm.df$Gram_6[distance_order][1:20])
        top_predict<-na.omit(top_predict)
        top_predict<-top_predict[!top_predict %in% clean_words]
        return(top_predict)
        
      }else if(length(clean_words)>=4){
        #vectorizer_dtm<-get_global_dtm(fvr)
        #Create Term Matrices of Test and Train on
        #Projected Vector and then compare similiarities.
        dtm2 = create_dtm(it_test, vectorizer_dtm[[7]])
        jd<-sim2(vectorizer_dtm[[8]], dtm2, method = "jaccard", norm = "none")
        #Create dataframe and order it
        distance<-data.frame(jd[,1])
        distance_order<-order(-distance)
        #Pick Top Prediction
        #as.character(tri.df$Gram_3[distance_order][1:5])
        load("fgrm.RData")
        top_predict<-as.character(fvr.df$Gram_5[distance_order][1:20])
        top_predict<-na.omit(top_predict)
        top_predict<-top_predict[!top_predict %in% clean_words]
        return(top_predict)
      
      }else if(length(clean_words)>=3){
        #vectorizer_dtm<-get_global_dtm(fr)
        #Create Term Matrices of Test and Train on
        #Projected Vector and then compare similiarities.
        dtm2 = create_dtm(it_test, vectorizer_dtm[[5]])
        jd<-sim2(vectorizer_dtm[[6]], dtm2, method = "jaccard", norm = "none")
        #Create dataframe and order it
        distance<-data.frame(jd[,1])
        distance_order<-order(-distance)
        #Pick Top Prediction
        #as.character(tri.df$Gram_3[distance_order][1:5])
        load("fr.RData")
        top_predict<-as.character(fr.df$Gram_4[distance_order][1:20])
        top_predict<-na.omit(top_predict)
        top_predict<-top_predict[!top_predict %in% clean_words]
        return(top_predict)
      
      }else if(length(clean_words)>=2){
        #vectorizer_dtm<-get_global_dtm(tri)
        #Create Term Matrices of Test and Train on
        #Projected Vector and then compare similiarities.
        dtm2 = create_dtm(it_test, vectorizer_dtm[[3]])
        jd<-sim2(vectorizer_dtm[[4]], dtm2, method = "jaccard", norm = "none")
        #Create dataframe and order it
        distance<-data.frame(jd[,1])
        distance_order<-order(-distance)
        #Pick Top Prediction
        #as.character(tri.df$Gram_3[distance_order][1:5])
        load("tri.RData")
        top_predict<-as.character(tri.df$Gram_3[distance_order][1:20])
        top_predict<-na.omit(top_predict)
        top_predict<-top_predict[!top_predict %in% clean_words]
        return(top_predict)
      }else if(length(clean_words)>=1){
        #vectorizer_dtm<-get_global_dtm(bi)
        #Create Term Matrices of Test and Train on
        #Projected Vector and then compare similiarities.
        dtm2 = create_dtm(it_test, vectorizer_dtm[[1]])
        jd<-sim2(vectorizer_dtm[[2]], dtm2, method = "jaccard", norm = "none")
        #Create dataframe and order it
        distance<-data.frame(jd[,1])
        distance_order<-order(-distance)
        #Pick Top Prediction
        #as.character(tri.df$Gram_3[distance_order][1:5])
        load("bi.RData")
        top_predict<-as.character(bi.df$Gram_2[distance_order][1:20])
        top_predict<-na.omit(top_predict)
        top_predict<-top_predict[!top_predict %in% clean_words]
        return(top_predict)
      }else {
      top_predict<-"Nothing to predict"
      return(top_predict)
      }
      #Print top predictions
      
}
# test_sentence<-"space"
# p<-predicted_word(test_sentence)
# na.omit(p)
