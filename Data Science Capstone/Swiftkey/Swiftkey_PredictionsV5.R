##Read Data=====
setwd("D:/DataScience/1Data Science Specialization Capstone/10Data Science Capstone/project/Coursera-SwiftKey/final/en_US")
source("library.R")
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
setwd("D:/DataScience/1Data Science Specialization Capstone/10Data Science Capstone/project/Coursera-SwiftKey/final/en_US")
source("library.R")
load("alldata.RData")

# #Size of documents
# length(blogs)
# #0.9 Million
# length(news)
# #1Million
# length(twitter)
# ##2.3 Million
# head(twitter)
#Sampling
#1% sample
#Remember each document can be huge in terms of words.
#Better stratergy will be lot of twitter as 140 Character limit.
size<-30
size<-30/100
size*length(blogs)
size*length(news)
size*length(twitter)


blogs_red<- sample(blogs,10000)
news_red <- sample(news, 10000)
twitter_red <- sample(twitter, 20000)
sample <- c(blogs_red, news_red, twitter_red)

save(sample, file= "sampledata.RData")
rm.obj=ls()
rm(list=rm.obj)
gc()

#Sample Cleaning=======
setwd("D:/DataScience/1Data Science Specialization Capstone/10Data Science Capstone/project/Coursera-SwiftKey/final/en_US")
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
setwd("D:/DataScience/1Data Science Specialization Capstone/10Data Science Capstone/project/Coursera-SwiftKey/final/en_US")
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
setwd("D:/DataScience/1Data Science Specialization Capstone/10Data Science Capstone/project/Coursera-SwiftKey/final/en_US")
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
#uni_levels[order(uni_levels)]

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
#Does it makes sense to extend till 4 grams?

#FourGram
fr <- NGramTokenizer(corpus, Weka_control(min = 4, max = 4))
#Tri gram Data frame
fr.df<-as.data.frame(fr) %>% 
  separate(fr, into = paste("Gram", 1:4, sep = "_"))
#Replacing Null values and removing those cases
fr.df<-fr.df[character_count(fr.df$Gram_1) > 1, ]
fr.df<-fr.df[character_count(fr.df$Gram_2) > 1, ]
fr.df<-fr.df[character_count(fr.df$Gram_3) > 1, ]
fr.df<-fr.df[character_count(fr.df$Gram_4) > 1, ]
fr.df<-fr.df[complete.cases(fr.df),]
fr.df$Gram_1<-factor(fr.df$Gram_1,levels=uni_levels)
fr.df$Gram_2<-factor(fr.df$Gram_2,levels=uni_levels)
fr.df$Gram_3<-factor(fr.df$Gram_3,levels=uni_levels)
fr.df$Gram_4<-factor(fr.df$Gram_4,levels=uni_levels)
fr.df$Gram_123<-trimws(paste(fr.df$Gram_1,fr.df$Gram_2,fr.df$Gram_3, sep=" "))

#FiveGram
fvr <- NGramTokenizer(corpus, Weka_control(min = 5, max = 5))
#Tri gram Data fvrame
fvr.df<-as.data.frame(fvr) %>% 
  separate(fvr, into = paste("Gram", 1:5, sep = "_"))
#Replacing Null values and removing those cases
fvr.df<-fvr.df[character_count(fvr.df$Gram_1) > 1, ]
fvr.df<-fvr.df[character_count(fvr.df$Gram_2) > 1, ]
fvr.df<-fvr.df[character_count(fvr.df$Gram_3) > 1, ]
fvr.df<-fvr.df[character_count(fvr.df$Gram_4) > 1, ]
fvr.df<-fvr.df[character_count(fvr.df$Gram_5) > 1, ]
fvr.df<-fvr.df[complete.cases(fvr.df),]
fvr.df$Gram_1<-factor(fvr.df$Gram_1,levels=uni_levels)
fvr.df$Gram_2<-factor(fvr.df$Gram_2,levels=uni_levels)
fvr.df$Gram_3<-factor(fvr.df$Gram_3,levels=uni_levels)
fvr.df$Gram_4<-factor(fvr.df$Gram_4,levels=uni_levels)
fvr.df$Gram_5<-factor(fvr.df$Gram_5,levels=uni_levels)
fvr.df$Gram_1234<-trimws(paste(fvr.df$Gram_1,fvr.df$Gram_2,fvr.df$Gram_3,fvr.df$Gram_4,sep=" "))
save(fvr.df,file= "fgrm.RData")

#6 Gram.

sgm <- NGramTokenizer(corpus, Weka_control(min = 6, max = 6))
#Tri gram Data sgmame
sgm.df<-as.data.frame(sgm) %>% 
  separate(sgm, into = paste("Gram", 1:6, sep = "_"))
#Replacing Null values and removing those cases
sgm.df<-sgm.df[character_count(sgm.df$Gram_1) > 1, ]
sgm.df<-sgm.df[character_count(sgm.df$Gram_2) > 1, ]
sgm.df<-sgm.df[character_count(sgm.df$Gram_3) > 1, ]
sgm.df<-sgm.df[character_count(sgm.df$Gram_4) > 1, ]
sgm.df<-sgm.df[character_count(sgm.df$Gram_5) > 1, ]
sgm.df<-sgm.df[character_count(sgm.df$Gram_6) > 1, ]
sgm.df<-sgm.df[complete.cases(sgm.df),]
sgm.df$Gram_1<-factor(sgm.df$Gram_1,levels=uni_levels)
sgm.df$Gram_2<-factor(sgm.df$Gram_2,levels=uni_levels)
sgm.df$Gram_3<-factor(sgm.df$Gram_3,levels=uni_levels)
sgm.df$Gram_4<-factor(sgm.df$Gram_4,levels=uni_levels)
sgm.df$Gram_5<-factor(sgm.df$Gram_5,levels=uni_levels)
sgm.df$Gram_6<-factor(sgm.df$Gram_6,levels=uni_levels)
sgm.df$Gram_12345<-trimws(paste(sgm.df$Gram_1,sgm.df$Gram_2,sgm.df$Gram_3,sgm.df$Gram_4,sgm.df$Gram_5,sep=" "))
save(sgm.df,file= "sgm.RData")



#Literature says anything greater than 4 is overfit
#Save bigram trigram
save(uni,bi,tri,fr,fvr,sgm,uni_levels,uni.df,bi.df,tri.df,fr.df,fvr.df,sgm.df,corpus,file= "ngrams.RData")
rm.obj=ls()
rm(list=rm.obj)
gc()

##Model: Count Algorithm====
setwd("D:/DataScience/1Data Science Specialization Capstone/10Data Science Capstone/project/Coursera-SwiftKey/final/en_US")
source("library.R")
#Load Test Sentences
source("test_load.R")
#Load allNGram Data
#load("ngrams.RData")

#Test
qry<- as.character(test.list[17])
#Clean the input query.
source("clean_query.R")
qry<-cleansample(qry)
source("model_countfreqV2.R")
predicted(qry=qry)

rm.obj=ls()
rm(list=rm.obj)
gc()


##Model: text2Vec-Jackard=======
#Is predicting next word possible with text2vec or
#Can it help by converting word to vectors?
#Idea is convert all words to unique vectors.
#substitute these vectors in place of words.
#We can get cosine distance between words. Can it be a good
#measure of nearest possible words?
#But it will give you with respect to all document context,
#Not with respect to the context of your sentence
#Is a graph algorithm is suited?
# A graph which tries to see any existing path between
#Expected words and Previous Words used based on trained
#Model? Does it exist or do we need to create such an 
#algorith?
#Go for predicting using RNN and so on see what happens

#http://text2vec.org/vectorization.html
#http://text2vec.org/similarity.html#cosine_similarity_with_tf-idf

setwd("D:/DataScience/1Data Science Specialization Capstone/10Data Science Capstone/project/Coursera-SwiftKey/final/en_US")
source("library.R")
#Load Test Sentences
source("test_load.R")
#Load allNGram Data
load("ngrams.RData")

#Checking text to vec
#Test Trigram Space
#sample_space<-c("i love you","i hate you","hate admit to")
load("sampledata.RData")
sample_space<-sample

#Objective, find the document nearest to test bigram
#Get the maximum matching trigrams by Jackard distance
#Get the the Third Gram of the top similiar one
#That will be our next word to predict.

#Prepare Vector Space
it = itoken(sample_space, progressbar = FALSE)
v = create_vocabulary(it) %>% prune_vocabulary(doc_proportion_max = 0.1, term_count_min = 5)
vectorizer = vocab_vectorizer(v)
#Train Iterator. Just creating iterator again to make it more clear
#Same as iterator it.
it_train = itoken(sample_space, progressbar = FALSE)

test_sentence<-as.character(test.list[5])#expect beach
test_sentence<-"i love"
# source("clean_query.R")
# test_sentence<-cleansample(test_sentence)$sample

#predict
source("model_vector_distance.R")
predicted_word(test_sentence)

rm.obj=ls()
rm(list=rm.obj)
gc()


##Model: text2Vec-CosineGloVe====
#Let us try with GloVe using word vectors.
setwd("D:/DataScience/1Data Science Specialization Capstone/10Data Science Capstone/project/Coursera-SwiftKey/final/en_US")

source("library.R")
#Load allNGram Data
load("ngrams.RData")

vector_data<-"words_vector_data.RData"
#Do you want to recreate RData of words_vector_data.RData
recreate_word_vector<-T
if (!file.exists(vector_data)||recreate_word_vector==T) {
    #Load Sample Data
    load("sample_sentences_data.RData")
    #Should we clean the text
    source("clean_query.R")
    sample<-cleansample(sample)
    library('stringr')
    sample<-str_c(sample,collapse=' ')
    source("model_get_glove_word_vector.R")
    model_get_glove_word_vector(sample)
}

#Load Test Sentences
source("test_load.R")
#Test it
test_s<-as.character(test.list[5])
#test_s<-"my friend"

source("clean_query.R")
clean_words<-as.character(cleansample(test_s))

#clean_words<-test_s
load("words_vector_data.RData")
source("model_glove.R")
test_s
clean_words
predicted_word(clean_words,word_vectors)


rm.obj=ls()
rm(list=rm.obj)
gc()

##Model: Recurrence Neural Network Test========
#Is predicting next word possible by word vectors?
#Is tensorflow seems interesting option?