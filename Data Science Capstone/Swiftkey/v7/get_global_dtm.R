#This script returns dtm1 to pass it to Jacquared Distance
library(text2vec)
library(hunspell)

#get_global_dtm<-function(ngram_sentence){

load("ngrams.RData")
grams<-list()
grams[[1]]<-bi
grams[[2]]<-tri
grams[[3]]<-fr
grams[[4]]<-fvr
grams[[5]]<-sgm

rm(bi,tri,fr,fvr,sgm,bi.df,tri.df,fr.df,fvr.df,sgm.df,corpus,uni.df,uni_levels,uni)


vectorizer_dtm<-list()

for (i in 1:5){
  sample_space<-grams[[3]]
  #Prepare Global Vector Space
  it = itoken(sample_space, progressbar = FALSE)
  v = create_vocabulary(it) #%>% prune_vocabulary(doc_proportion_max = 0.1, term_count_min = 5)
  vectorizer = vocab_vectorizer(v)
  #Train Iterator. Just creating iterator it again to make it 
  #more clear
  #Same as iterator it.
  it_train = itoken(sample_space, progressbar = FALSE)
  dtm1 = create_dtm(it_train, vectorizer)
  
  if(i==1){
    k<-1
    }
  if(i==2){
      k<-3
      }
  if(i==3){
        k<-5
        }
  if(i==4){
          k<-7
  }
  if(i==5){
    k<-9
  }
           
  
  vectorizer_dtm[[k]]<-vectorizer
  vectorizer_dtm[[k+1]]<-dtm1
}
save(vectorizer_dtm,file = "vectorizer_dtm.RData")

rm.obj=ls()
rm(list=rm.obj)
gc()
# return(vectorizer_dtm)
# 
# }