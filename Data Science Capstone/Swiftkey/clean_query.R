library(stringi)
library(tm)
library(dplyr)
library(qdapRegex)
library(stringr)
library(qdap)
cleansample<-function(x){
  
  sample<-x
  #Solve Encoding Issues
  sample <- iconv(sample, 'UTF-8', 'ASCII')
  #Remove URL
  sample<-rm_url(sample)
  #Remove Email Address.
  sample<-rm_email(sample)
  #Lower Case all characters.
  sample<-L(sample)
  #Remove Puctuations
  sample<-gsub("'", '',sample)
  #Remove Alphanumerics
  sample<-trimws(stripWhitespace(str_replace_all(sample, "[^[:alnum:]]", " ")))
  #rm_non_words - Remove/replace/extract non-words 
  #(Anything that's not a letter or apostrophe; 
  #also removes multiple white spaces) from a string.
  sample<-rm_non_words(sample)
  #Remove numbers
  sample<-removeNumbers(sample)
  # rm_repeated_phrases(x)
  sample<-rm_repeated_phrases(sample)
  #To remove emotional like aaahhaaaaa wowwow
  # x <- "aaaahahahahaha that was a good joke peep and pepper and pepe"
  # rm_repeated_characters(x)
  sample<-rm_repeated_characters(sample)
  #Remove Stop Words. Not convinced as it might improve contextual accuracy
  #but might not fit well with grammer. Still, going ahead as per practice!
  sample<-removeWords(as.character(sample),stopwords('english'))
  #Remove additional white spaces between words.
  sample<-stripWhitespace(sample)
  #Trim the begining or end of sentence
  sample<-trimws(sample)
  #Remove non words
  sample<-rm_non_words(sample)
  #Remove Whitespace
  sample<-stripWhitespace(sample)
  #Trim the begining or end of sentence
  sample<-trimws(sample)
  #Remove NA
  sample<-(sample[!is.na(sample)])
  sample<-stripWhitespace(sample)
  #Data Frame
  sample<-as.data.frame(sample, stringsAsFactors = FALSE)
  sample<-(na.omit(sample)) 
  sample <- sample[!apply(is.na(sample) | sample == "", 1, all),]
  sample<-as.data.frame(sample, stringsAsFactors = FALSE)
  sample<-trimws(stripWhitespace(str_replace_all(sample, "[^[:alnum:]]", " ")))
  sample<-rm_non_words(sample)
  return(sample)
}