library(stringr)
library(text2vec)
library(hunspell)
library(tm)

predicted_word<-function(clean_words,word_vectors){    
    
  #clean_words<-qry
  #Get word vectors for each word.
  #Get words from supplied sentence
  #Get word vectors for all the tokens and store it, if it exist in word vector list
  for(i in 1:length(clean_words)){
    test <- unlist(strsplit(clean_words, " "))
    t<-hunspell(test)
    t<-as.character(t)
    misspelt<-t["character(0)"!=t]
    if(length(misspelt)!=0){
      clean_words<-removeWords(clean_words,misspelt)}
  }
    clean_words <- unlist(strsplit(clean_words, " "))
    clean_words<-as.character(na.omit(replace(clean_words, clean_words == "", NA)))
    
    test<-list()
    word_string<-row.names(word_vectors)
    #clean_words<-c("i","love")
    for(i in 1:length(clean_words)){
      #if word is in word vectors
      iter<-clean_words[i]
      exact_word<-paste("\\b",iter,"\\b",sep = "")
      word_number<-paste("w",i,sep="")
      length(grep(exact_word, word_string, value=TRUE))
      if(length(grep(exact_word, word_string, value=TRUE))){
        
        test[[word_number]]<-word_vectors[iter, , drop = FALSE]
        
      }
      
    }
        if(length(test)!=0){
        #Considering the last 3 words
          #As maximum words have 3 word relationships
        # if(length(test)>3){
        # k<-length(test)
        # l<-k-3
        # test<-test[l:k]
        # }
        
        test<-Reduce('+', test)
        #Cosine Distance
        cos_sim = sim2(x = word_vectors, y = test, method = "cosine", norm = "l2")
        predicted<-data.frame(sort(cos_sim[,1], decreasing = TRUE))
        predicted<-row.names(predicted)
        length(predicted)
        #Get all predicted except the exact match of clean words
        predicted<-predicted[!predicted %in% clean_words]
        predicted<-as.character(na.omit(replace(predicted, predicted == "", NA)))
        #Top 10 Predictions
        predicted<-predicted[1:20]
        #predicted
        return(predicted)
        }else{
          predicted<-"Not found to predict"
          return(predicted)
          }
}

