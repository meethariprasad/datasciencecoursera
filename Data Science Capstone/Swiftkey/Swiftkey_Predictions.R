##Load Libraries=====
source("library.R")

##Read Data=====
#source("library.R")
#:Saved the files in all.data.
# blogs <- readLines(con <- file("en_US.blogs.txt"), encoding = "UTF-8", skipNul = TRUE)
# close(con)
#
# con <- file("en_US.news.txt", open = "rb")
# news <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
# close(con)
#
# twitter <- readLines(con <- file("en_US.twitter.txt"), encoding = "UTF-8", skipNul = TRUE)
# twitter <- iconv(twitter, from = "latin1", to = "UTF-8", sub="")
# twitter <- stri_replace_all_regex(twitter, "\u2019|`","'")
# twitter <- stri_replace_all_regex(twitter, "\u201c|\u201d|u201f|``",'"')
# close(con)
# rm.obj=ls()
# save(blogs, news, twitter, file= "alldata.RData")
# rm(list=c(ls(),rm.obj))
# gc()

##Sampling====
source("library.R")
load("alldata.RData")

#Size of documents
length(blogs)
#0.9 Million
length(news)
#1Million
length(twitter)
##2.3 Million
head(twitter)
#Sampling
#1% sample
#Remember each document can be huge in terms of words.
#Better stratergy will be lot of twitter as 140 Character limit.
size<-0.001
size*length(blogs)
size*length(news)
size*length(twitter)


blogs_red<- sample(blogs,size*length(blogs) )
news_red <- sample(news, size*length(news))
twitter_red <- sample(twitter, size*length(twitter))

sample <- c(blogs_red, news_red, twitter_red)
save(sample, file= "sampledata.RData")
rm.obj=ls()
rm(list=rm.obj)
gc()

#Sample Cleaning=======
source("library.R")
load("sampledata.RData")
#Get Sentences
source("getsentece.R")
sample<-convert_text_to_sentences(sample)
save(sample, file= "sample_sentences_data.RData")
rm.obj=ls()
rm(list=rm.obj)
gc()

##Clean Sentences===========
source("library.R")
load("sample_sentences_data.RData")
#Clean Data Function
source("cleansample.R")
sample<-cleansample(sample)
save(sample, file= "sample_sent_clean_data.RData")
rm.obj=ls()
rm(list=rm.obj)
gc()
#===========##

##Build Corpus & Ngrams========
source("library.R")
load("sample_sent_clean_data.RData")
#Build Corpus
corpus <- Corpus(VectorSource(sample))
# #Conveying TM that document is plaintext.
# #Why it is needed? Still to explore more than just saying storing metadata.
# corpus <- tm_map(corpus, PlainTextDocument)
#Somehow in corpus we are getting numbers and puctuations
#Let us clean them.

corpus <- corpus %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  tm_map(PlainTextDocument) 

#unigram
uni <- NGramTokenizer(corpus, Weka_control(min = 1, max = 1))

uni.df<-as.data.frame(uni) %>% 
  separate(uni, into = paste("Gram", 1, sep = "_"))

#Get all rows with greater than 1 characters
uni.df<-uni.df[character_count(uni.df$Gram_1) > 1, ]
uni.df<-(uni.df[!is.na(uni.df)])
uni_levels<-unique(uni.df)
uni_levels[order(uni_levels)]

#Bigram
bi <- NGramTokenizer(corpus, Weka_control(min = 2, max = 2))

#Bi gram Data frame

bi.df<-as.data.frame(bi) %>% 
  separate(bi, into = paste("Gram", 1:2, sep = "_"))

#Get all rows with greater than 1 characters
bi.df<-bi.df[character_count(bi.df$Gram_1) > 1, ]
bi.df<-bi.df[character_count(bi.df$Gram_2) > 1, ]
bi.df<-bi.df[complete.cases(bi.df),]
bi.df$Gram_1<-factor(bi.df$Gram_1,levels=uni_levels)
bi.df$Gram_2<-factor(bi.df$Gram_2,levels=uni_levels)
#TriGram
tri <- NGramTokenizer(corpus, Weka_control(min = 3, max = 3))

#Tri gram Data frame
tri.df<-as.data.frame(tri) %>% 
  separate(tri, into = paste("Gram", 1:3, sep = "_"))

#Replacing Null values and removing those cases
tri.df<-tri.df[character_count(tri.df$Gram_1) > 1, ]
tri.df<-tri.df[character_count(tri.df$Gram_2) > 1, ]
tri.df<-tri.df[character_count(tri.df$Gram_3) > 1, ]
tri.df<-tri.df[complete.cases(tri.df),]
tri.df$Gram_1<-factor(tri.df$Gram_1,levels=uni_levels)
tri.df$Gram_2<-factor(tri.df$Gram_2,levels=uni_levels)
tri.df$Gram_3<-factor(tri.df$Gram_3,levels=uni_levels)
tri.df$Gram_12<-trimws(paste(tri.df$Gram_1,tri.df$Gram_2, sep=" "))
#Save bigram trigram
save(uni,bi,tri,uni_levels,uni.df,bi.df,tri.df,corpus,file= "ngrams.RData")
rm.obj=ls()
rm(list=rm.obj)
gc()

##Model: Count Algorithm Test====
source("library.R")
#Load Test Sentences
source("test_load.R")
#Load allNGram Data
load("ngrams.RData")

#Test
qry<- as.character(test.list[17])
#Clean the input query.
source("clean_query.R")
qry<-cleansample(qry)$sample
source("model_countfreq.R")
predicted(qry=qry)

##Model: Word2Vec Test=======
#Is predicting next word possible with text2vec?
#Idea is convert all words to vectors.
#substitute these vectors in place of words.
#We can get cosine distance between words. Can it be a good
#measure of nearest possible words?
#or Jaccard
#Go for predicting using RNN and so on see what happens


##Model: Recurrence Neural Network Test========
#Is predicting next word possible by word vectors?
#Is tensorflow seems interesting option?

##
