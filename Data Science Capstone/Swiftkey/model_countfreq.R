
library(stringr)
library(qdap)

predicted<- function(qry){
        if (word_count(qry) >= 2){
          lastwd<-word(qry,-2:-1)
          test<-paste(lastwd[1],lastwd[2])
          #Check if you find matching last two words in trigram Gram_12
          index1 <- with(tri.df, grepl(test, tri.df$Gram_12))
          #If found
          if(any(index1)){
            #Subset the trigram and group by Gram_3
            index1 <- with(tri.df, grepl(test, tri.df$Gram_12))
            filtered<-tri.df[index1, ]
            #Find frequency of each unique group
            freq<-data.frame(table(filtered$Gram_3))
            #Order by Frequency of Gram_3 & return top 5
            freq<-head(freq[order(-freq$Freq),],5)
            predict<-as.character(freq[(freq$Freq>0),]$Var1)
            return(predict)
          } else { #If notfound
            #Get only last word
            lastwd<-word(qry,-1)
            #Search in bi gram Gram_1 and Group by Gram_2
            index2 <- with(bi.df, grepl(lastwd, bi.df$Gram_1))
            if(any(index2)){
              filtered<-bi.df[index2, ]
              #Find frequency of each unique group
              freq<-data.frame(table(filtered$Gram_2))
              #Order by Frequency of Gram 2
              freq<-head(freq[order(-freq$Freq),],5)
              predict<-as.character(freq[(freq$Freq>0),]$Var1)
              return(predict)
            }else{(predict<-"Need more training to predict")
              return(predict)}
          }
        }else {#else if length words==1 & Applied
          library(stringr)
          lastwd<-word(qry,-1)
          index3 <- with(bi.df, grepl(lastwd, bi.df$Gram_1))
          if(any(index3)){
            filtered<-bi.df[index3, ]
            #Find frequency of each unique group
            freq<-data.frame(table(filtered$Gram_2))
            #Order by Frequency of Gram 2
            freq<-head(freq[order(-freq$Freq),],5)
            predict<-as.character(freq[(freq$Freq>0),]$Var1)
            return(predict)
          }else{(predict<-"Need more training to predict")
            return(predict)}
          
        }
}