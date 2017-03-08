#source("library.R")
# library(shiny)
# #Clean the input query.
library(stringr)
library(stringi)
library(dplyr)
library(qdapRegex)
library(qdapTools)
library(qdap)
library(shiny)
library(tm)
library(hunspell)
library(text2vec)


#Count based Backoff Predictions
# load("../predictword/sgm.RData", envir=.GlobalEnv)
# load("../predictword/fgrm.RData", envir=.GlobalEnv)
# load("../predictword/fr.RData", envir=.GlobalEnv)
# load("../predictword/tri.RData", envir=.GlobalEnv)
# load("../predictword/bi.RData", envir=.GlobalEnv)
source("cleansample.R")
source("model_countfreqV2.R")
load("../countfreq/ngramsdf.RData", envir=.GlobalEnv)

# # #Glove Predictions
# source("cleansample.R")
# source("model_gloveV2.R")
# load("../glove/words_vector_data.RData", envir=.GlobalEnv)
