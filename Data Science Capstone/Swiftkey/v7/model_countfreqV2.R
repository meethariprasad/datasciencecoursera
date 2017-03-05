
library(stringr)
library(qdap)
setwd("D:/DataScience/1Data Science Specialization Capstone/10Data Science Capstone/project/Coursera-SwiftKey/final/en_US")
load("ngrams.RData")

#Start with checking 6 gram predictions, then go all the way up to bigram.

predicted<- function(qry){
  
  if(word_count(qry) >= 5 && any(with(sgm.df, grepl(paste(word(qry,-5:-1),collapse = " "), sgm.df$Gram_12345)))){ #Go for Six Gram with last 5 words
                    #load("sgm.RData")
                    index <- with(sgm.df, grepl(paste(word(qry,-5:-1),collapse = " "), sgm.df$Gram_12345))
                    filtered<-sgm.df[index, ]
                    #Find frequency of each unique group
                    freq<-data.frame(table(filtered$Gram_6))
                    #Order by Frequency of Gram_3 & return top 10
                    freq<-head(freq[order(-freq$Freq),],20)
                    predict<-as.character(freq[(freq$Freq>0),]$Var1)
                    
                    return(predict)

  }else if(word_count(qry) >= 4 && any(with(fvr.df, grepl(paste(word(qry,-4:-1),collapse = " "), fvr.df$Gram_1234)))){ #Go for five Gram with last 4 words
    #load("fgrm.RData")
    index <- with(fvr.df, grepl(paste(word(qry,-4:-1),collapse = " "), fvr.df$Gram_1234))
    filtered<-fvr.df[index, ]
    #Find frequency of each unique group
    freq<-data.frame(table(filtered$Gram_5))
    #Order by Frequency of Gram_3 & return top 10
    freq<-head(freq[order(-freq$Freq),],20)
    predict<-as.character(freq[(freq$Freq>0),]$Var1)
  
    return(predict)
    
    
  }else if(word_count(qry) >= 3 && any(with(fr.df, grepl(paste(word(qry,-3:-1),collapse = " "), fr.df$Gram_123)))){ #Go for 4Gram Gram with last 3 words
    #load("fr.RData")
    index <- with(fr.df, grepl(paste(word(qry,-3:-1),collapse = " "), fr.df$Gram_123))
    filtered<-fr.df[index, ]
    #Find frequency of each unique group
    freq<-data.frame(table(filtered$Gram_4))
    #Order by Frequency of Gram_3 & return top 10
    freq<-head(freq[order(-freq$Freq),],20)
    predict<-as.character(freq[(freq$Freq>0),]$Var1)
  
    return(predict)
    
  }else if(word_count(qry) >= 2 && any(with(tri.df, grepl(paste(word(qry,-2:-1),collapse = " "), tri.df$Gram_12)))){ #Go for 3 Gram with last 2 words
    #load("tri.RData")
    index <- with(tri.df, grepl(paste(word(qry,-2:-1),collapse = " "), tri.df$Gram_12))
    filtered<-tri.df[index, ]
    #Find triequency of each unique group
    freq<-data.frame(table(filtered$Gram_3))
    #Order by Frequency of Gram_3 & return top 10
    freq<-head(freq[order(-freq$Freq),],20)
    predict<-as.character(freq[(freq$Freq>0),]$Var1)
  
    return(predict)
    
  }else if(word_count(qry) >= 1 && any(with(bi.df, grepl(paste(word(qry,-1),collapse = " "), bi.df$Gram_1)))){ #Go for 2 Gram with last 1 word
    #load("bi.RData")
    index <- with(bi.df, grepl(paste(word(qry,-1),collapse = " "), bi.df$Gram_1))
    filtered<-bi.df[index, ]
    #Find biequency of each unique group
    freq<-data.frame(table(filtered$Gram_2))
    #Order by Frequency of Gram_3 & return top 10
    freq<-head(freq[order(-freq$Freq),],20)
    predict<-as.character(freq[(freq$Freq>0),]$Var1)
  
    return(predict)
    
  }else {
    predict<-"No Predictions from Count Method or you have not entered anything"
    return(predict)
    }

}
