library(stringi)
library(tm)
library(dplyr)
library(qdapRegex)
library(qdapTools)
library(stringr)
library(qdap)
library(hunspell)
cleansample<-function(sample){
  #Solve Encoding Issues
  sample <- iconv(sample, 'UTF-8', 'ASCII')
  #Remove NA resulting out of conversion
  sample<-(sample[!is.na(sample)])
  #Remove URL
  sample<-rm_url(sample)
  #Remove Email Address.
  sample<-rm_email(sample)
  #Remove Apostrophe
  sample<-str_replace_all(sample, "'", "")
  #Remove Alphanumerics
  sample<-trimws(stripWhitespace(str_replace_all(sample, "[^[:alnum:]]", " ")))
  #rm_non_words - Remove/replace/extract non-words 
  #(Anything that's not a letter or apostrophe; 
  #also removes multiple white spaces) from a string.
  sample<-rm_non_words(sample)
  #Remove numbers
  # rm_repeated_phrases(x)
  sample<-rm_repeated_phrases(sample)
  #To remove emotional like aaahhaaaaa wowwow
  # x <- "aaaahahahahaha that was a good joke peep and pepper and pepe"
  # rm_repeated_characters(x)
  sample<-rm_repeated_characters(sample)
  #Remove Stop Words. Not convinced as it might improve contextual accuracy
  #but might not fit well with grammer. Still, going ahead as per practice!
  mystopword<-stopwords('english')[!stopwords('english') %in% "i"]
  sample<-removeWords(as.character(sample),mystopword)
  #Remove additional white spaces between words.
  sample<-stripWhitespace(sample)
  #Trim the begining or end of sentence
  sample<-trimws(sample)
  
  #Before making lower apply hunspell check.
  #We will get list of spell mistakes and remove them from sample
  #As it will be huge list of stopwords best approach is to remove
  #line by line
  
  for(i in 1:length(sample)){
    test <- unlist(strsplit(sample[i], " "))
    t<-hunspell(test)
    t<-as.character(t)
    misspelt<-t["character(0)"!=t]
    if(length(misspelt)!=0){
      sample[i]<-removeWords(sample[i],misspelt)}
    }
  
  #Lower Case all characters.
  sample<-L(sample)
  
  # rm_repeated_phrases(x)
  sample<-rm_repeated_phrases(sample)
  
  #Remove NA
  sample<-(sample[!is.na(sample)])
  
  #Remove words with 1 characters other than a or i
  #List of letters except a and i
      l<-letters
      rm<-c("a","i")
      r<-removeWords(l,rm)
      r<-r[r != ""]
  #Removing the single words other than a and i.
  sample<-trimws(removeWords(sample,r))
  
  #Data Frame
  sample<-as.data.frame(sample, stringsAsFactors = FALSE)
  sample<-(na.omit(sample)) 
  sample <- sample[!apply(is.na(sample) | sample == "", 1, all),]
  sample<-as.data.frame(sample, stringsAsFactors = FALSE)
  sample<-as.data.frame(sample, stringsAsFactors = FALSE)
  return(sample)
}