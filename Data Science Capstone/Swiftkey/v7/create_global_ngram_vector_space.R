#This script returns dtm1 to pass it to Jacquared Distance


#Prepare Global Vector Space
it = itoken(sample_space, progressbar = FALSE)
v = create_vocabulary(it) %>% prune_vocabulary(doc_proportion_max = 0.1, term_count_min = 5)
vectorizer = vocab_vectorizer(v)
#Train Iterator. Just creating iterator it again to make it 
#more clear
#Same as iterator it.
it_train = itoken(sample_space, progressbar = FALSE)
dtm1 = create_dtm(it_train, vectorizer)