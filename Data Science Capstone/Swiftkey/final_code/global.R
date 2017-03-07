#source("library.R")
# library(shiny)
# #Clean the input query.
library(stringr)
library(stringi)
library(dplyr)
library(qdapRegex)
library(qdapTools)
library(qdap)
library(text2vec)
library(hunspell)
library(tm)
source("cleansample.R")

#Count based Backoff Predictions
# load("../predictword/sgm.RData", envir=.GlobalEnv)
# load("../predictword/fgrm.RData", envir=.GlobalEnv)
# load("../predictword/fr.RData", envir=.GlobalEnv)
# load("../predictword/tri.RData", envir=.GlobalEnv)
# load("../predictword/bi.RData", envir=.GlobalEnv)
load("../predictword/ngramsdf.RData", envir=.GlobalEnv)
source("model_countfreqV2.R")

# #Glove Predictions
# load('../predictword/words_vector_data.RData', envir=.GlobalEnv)
# source("model_gloveV2.R")