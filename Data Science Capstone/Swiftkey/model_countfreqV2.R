
library(stringr)
library(qdap)
load("ngrams.RData")

#Start with checking 6 gram predictions, then go all the way up to bigram.

predicted<- function(qry){
  
  if(word_count(qry) >= 5 && any(with(sgm.df, grepl(paste(word(qry,-5:-1),collapse = " "), sgm.df$Gram_12345)))){ #Go for Six Gram with last 5 words
  
                    index <- with(sgm.df, grepl(paste(word(qry,-5:-1),collapse = " "), sgm.df$Gram_12345))
                    filtered<-sgm.df[index, ]
                    #Find frequency of each unique group
                    freq<-data.frame(table(filtered$Gram_6))
                    #Order by Frequency of Gram_3 & return top 10
                    freq<-head(freq[order(-freq$Freq),],10)
                    predict<-as.character(freq[(freq$Freq>0),]$Var1)
                    return(predict)

  }else if(word_count(qry) >= 4 && any(with(fvr.df, grepl(paste(word(qry,-4:-1),collapse = " "), fvr.df$Gram_1234)))){ #Go for five Gram with last 4 words
    
    index <- with(fvr.df, grepl(paste(word(qry,-4:-1),collapse = " "), fvr.df$Gram_1234))
    filtered<-fvr.df[index, ]
    #Find frequency of each unique group
    freq<-data.frame(table(filtered$Gram_5))
    #Order by Frequency of Gram_3 & return top 10
    freq<-head(freq[order(-freq$Freq),],10)
    predict<-as.character(freq[(freq$Freq>0),]$Var1)
    return(predict)
    
    
  }else if(word_count(qry) >= 3 && any(with(fr.df, grepl(paste(word(qry,-3:-1),collapse = " "), fr.df$Gram_123)))){ #Go for 4Gram Gram with last 3 words
    
    index <- with(fr.df, grepl(paste(word(qry,-3:-1),collapse = " "), fr.df$Gram_123))
    filtered<-fr.df[index, ]
    #Find frequency of each unique group
    freq<-data.frame(table(filtered$Gram_4))
    #Order by Frequency of Gram_3 & return top 10
    freq<-head(freq[order(-freq$Freq),],10)
    predict<-as.character(freq[(freq$Freq>0),]$Var1)
    return(predict)
    
  }else if(word_count(qry) >= 2 && any(with(tri.df, grepl(paste(word(qry,-2:-1),collapse = " "), tri.df$Gram_12)))){ #Go for 3 Gram with last 2 words
    
    index <- with(tri.df, grepl(paste(word(qry,-2:-1),collapse = " "), tri.df$Gram_12))
    filtered<-tri.df[index, ]
    #Find triequency of each unique group
    freq<-data.frame(table(filtered$Gram_3))
    #Order by Frequency of Gram_3 & return top 10
    freq<-head(freq[order(-freq$Freq),],10)
    predict<-as.character(freq[(freq$Freq>0),]$Var1)
    return(predict)
    
  }else if(word_count(qry) >= 1 && any(with(bi.df, grepl(paste(word(qry,-1),collapse = " "), bi.df$Gram_1)))){ #Go for 2 Gram with last 1 word
    
    index <- with(bi.df, grepl(paste(word(qry,-1),collapse = " "), bi.df$Gram_1))
    filtered<-bi.df[index, ]
    #Find biequency of each unique group
    freq<-data.frame(table(filtered$Gram_2))
    #Order by Frequency of Gram_3 & return top 10
    freq<-head(freq[order(-freq$Freq),],10)
    predict<-as.character(freq[(freq$Freq>0),]$Var1)
    return(predict)
    
  }else {
    predict<-"No Predictions from Count Method or you have not entered anything"
    return(predict)
    }

}

# qry<-"adam sandler"
# setwd("D:/DataScience/1Data Science Specialization Capstone/10Data Science Capstone/project/Coursera-SwiftKey/final/en_US")
# source("clean_query.R")
# qry<-cleansample(qry)
# predicted(qry)
  #
  #
  #
  
#         if (word_count(qry) >= 2){
#               
#               #Trigram Check
#               #Check if you find matching last two words in trigram Gram_12
#               index1 <- with(tri.df, grepl(test, tri.df$Gram_12))
#               #If found
#               if(any(index1)){
#                 #Subset the trigram and group by Gram_3
#                 index1 <- with(tri.df, grepl(test, tri.df$Gram_12))
#                 filtered<-tri.df[index1, ]
#                 #Find frequency of each unique group
#                 freq<-data.frame(table(filtered$Gram_3))
#                 #Order by Frequency of Gram_3 & return top 10
#                 freq<-head(freq[order(-freq$Freq),],10)
#                 predict<-as.character(freq[(freq$Freq>0),]$Var1)
#                 return(predict)
#               } else { #If notfound
#                 #Get only last word
#                 lastwd<-word(qry,-1)
#                 #Search in bi gram Gram_1 and Group by Gram_2
#                 index2 <- with(bi.df, grepl(lastwd, bi.df$Gram_1))
#                 if(any(index2)){
#                   filtered<-bi.df[index2, ]
#                   #Find frequency of each unique group
#                   freq<-data.frame(table(filtered$Gram_2))
#                   #Order by Frequency of Gram 2
#                   freq<-head(freq[order(-freq$Freq),],10)
#                   predict<-as.character(freq[(freq$Freq>0),]$Var1)
#                   return(predict)
#                 }else{(predict<-"Need more training to predict")
#                   return(predict)}
#               }
#         }else {#else if length words==1 & Applied
#           #Bigram Check
#           library(stringr)
#           lastwd<-word(qry,-1)
#           index3 <- with(bi.df, grepl(lastwd, bi.df$Gram_1))
#           if(any(index3)){
#             filtered<-bi.df[index3, ]
#             #Find frequency of each unique group
#             freq<-data.frame(table(filtered$Gram_2))
#             #Order by Frequency of Gram 2
#             freq<-head(freq[order(-freq$Freq),],10)
#             predict<-as.character(freq[(freq$Freq>0),]$Var1)
#             return(predict)
#           }else{(predict<-"Need more training to predict")
#             return(predict)}
#           
#         }
# }