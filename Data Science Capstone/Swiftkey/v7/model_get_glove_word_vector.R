library(stringr)
library(text2vec)

model_get_glove_word_vector<-function(sample){

    sample<-str_c(sample,collapse=' ')
    # Create iterator over tokens
    tokens <- space_tokenizer(sample)
    # Create vocabulary. Terms will be unigrams (simple words).
    it = itoken(tokens, progressbar = FALSE)
    vocab <- create_vocabulary(it)
    #vocab <- prune_vocabulary(vocab, term_count_min = 5L)
    vectorizer <- vocab_vectorizer(vocab, 
                                   # don't vectorize input
                                   grow_dtm = FALSE, 
                                   # use window of 5 for context words
                                   #Means don't worry about context of words 
                                   #distance  greater than 5
                                   skip_grams_window = 5L
                                   )
    tcm <- create_tcm(it, vectorizer)
    glove = GlobalVectors$new(word_vectors_size = 50, vocabulary = vocab, x_max = 10)
    glove_fit<-glove$fit(tcm, n_iter = 40)
    word_vectors <- glove$get_word_vectors()
    word_vectors<-subset(word_vectors,row.names(word_vectors)!="")
    save(word_vectors,file = "words_vector_data.RData")
    return(word_vectors)
}