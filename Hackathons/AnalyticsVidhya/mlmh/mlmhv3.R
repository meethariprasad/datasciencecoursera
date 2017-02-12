# Data
# Variable
# Definition
# Trip_ID
# ID for TRIP (Can not be used for purposes of modelling)
# Trip_Distance
# The distance for the trip requested by the customer
# Type_of_Cab
# Category of the cab requested by the customer
# Customer_Since_Months
# Customer using cab services since n months; 0 month means current month
# Life_Style_Index
# Proprietary index created by Sigma Cabs showing lifestyle of the customer based on their behaviour
# Confidence_Life_Style_Index
# Category showing confidence on the index mentioned above
# Destination_Type
# Sigma Cabs divides any destination in one of the 14 categories.
# Customer_Rating
# Average of life time ratings of the customer till date
# Cancellation_Last_1Month
# Number of trips cancelled by the customer in last 1 month
# Var1, Var2 and Var3
# Continuous variables masked by the company. Can be used for modelling purposes
# Gender
# Gender of the customer
# Surge_Pricing_Type
# Predictor variable can be of 3 types
#The Public and Private Test split is 25:75



set.seed(1234)
setwd("D:/DataScience/Analytics Vidhya/mlmh")
training<-read.csv("train.csv",stringsAsFactors = TRUE,na.strings = c("", " "))
train.row.length<-nrow(training)
testing<-read.csv("test.csv",stringsAsFactors = TRUE,na.strings = c("", " "))
TripID<-as.character(testing$Trip_ID)
testing$Surge_Pricing_Type<-NA

testing$Trip_ID<-NULL
training$Trip_ID<-NULL

combi<-rbind(training,testing)
#Remove ID
combi$Trip_ID<-NULL

#Encoding & Designating appropriate Variables
str(combi)
levels(combi$Type_of_Cab)<-c(1,2,3,4,5)
levels(combi$Confidence_Life_Style_Index)<-c(1,2,3)
levels(combi$Destination_Type)<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)
combi$Surge_Pricing_Type<-as.factor(combi$Surge_Pricing_Type)
levels(combi$Gender)<-c(1,2)
str(combi)
library(dmm)
combi$Confidence_Life_Style_Index<-unfactor(combi$Confidence_Life_Style_Index)
combi$Confidence_Life_Style_Index[is.na(combi$Confidence_Life_Style_Index)]<-0.5
combi$Confidence_Life_Style_Index<-as.factor(combi$Confidence_Life_Style_Index)
summary(combi)

training<-combi[(1:train.row.length),]
testing<-combi[-(1:train.row.length),]

#======Variable Importance
#========================Varimp========
#Looks like we need to use H20 Package to work on data of this size.
library(h2o)
localH2O <- h2o.init(nthreads = -1,max_mem_size="3G")
base.training.h2o <- as.h2o(training)
#dependent variable (Surge_Pricing_Type)
names(base.training.h2o)
y.dep <- 13
#independent variables
x.indep <- c(1:12)
#Random Forest for Variable Iportance
#Question to ponder: Why not deeplearning itself for variable importance?
base.training.h2o[,13]<-as.factor(base.training.h2o[,13])

baseline.rf.model <- h2o.randomForest(x=x.indep, y=y.dep, training_frame=base.training.h2o,seed=1234)
vip<-h2o.varimp(baseline.rf.model)
vip
# Variable Importances: 
#                       variable relative_importance scaled_importance percentage
# 1                  Type_of_Cab       898096.687500          1.000000   0.414788
# 2                Trip_Distance       181033.593750          0.201575   0.083611
# 3              Customer_Rating       164118.343750          0.182740   0.075798
# 4                         Var3       157634.281250          0.175520   0.072804
# 5                         Var2       148495.906250          0.165345   0.068583
# 6             Life_Style_Index       134119.296875          0.149337   0.061943
# 7        Customer_Since_Months       132546.953125          0.147587   0.061217
# 8                         Var1        96476.335938          0.107423   0.044558
# 9             Destination_Type        93144.343750          0.103713   0.043019
# 10    Cancellation_Last_1Month        70785.796875          0.078818   0.032693
# 11 Confidence_Life_Style_Index        63801.960938          0.071041   0.029467
# 12                      Gender        24940.695312          0.027771   0.011519
#================
#Will use deep learning for base model, without feature engineering,hyper parameter searching
baseline.dl.model <- h2o.deeplearning(x=x.indep, y=y.dep, training_frame=base.training.h2o,seed=1234,variable_importances = TRUE)
per1<-h2o.performance (baseline.dl.model)
per1
#Before Variable Reduction&imputation:Hit Ratio for 1,2,3 is 70,93,100 Percent 
#with 29.3% Overall error.
#Taking Top 8 values as important variables from rando forest.
#Again why not to select from DL? 
#Why deliberately reducing the feature engineering?
top.var.a<-vip$variable[1:9]
top.var.a
h2o.shutdown(prompt = F)
#####=========================================####===

#top.var.a corresponding index in combi: 2,1,7,11,10,4,3,9,6,13

#Let us go for feature imputation.


#=========Imputation: Divide, Impute, Merge
##LSI,Customer Since Month,Type of Cab, Imputation

#Train Imputations==========
#Taking from Combi set containing all variables to impute.
#Take all other except surging price

#Life_Style_Index Imputation
library(h2o)
localH2O <- h2o.init(nthreads = -1,max_mem_size="3G")
combi.LSI.Test<-training[which(is.na(training$Life_Style_Index)),]
summary(combi.LSI.Test)
combi.LSI.Train<-training[which(!is.na(training$Life_Style_Index)),]
summary(combi.LSI.Train$Life_Style_Index)
summary(combi.LSI.Test$Life_Style_Index)
#Looks like we need to use H20 Package to work on data of this size.
training.h2o <- as.h2o(combi.LSI.Train)
testing.h2o <- as.h2o(combi.LSI.Test)
names(training.h2o)
#dependent variable (LSI)
y.dep <- 4
#independent variables
x.indep <- c(1:3,5:12)
#Random Forest Modeling: Known to be doing fair predictions
dlmodelbase <- h2o.randomForest(x=x.indep, y=y.dep, training_frame=training.h2o)
h2o.performance (dlmodelbase)
submit.predict.LIS <- as.data.frame(h2o.predict(dlmodelbase, testing.h2o[,c(1:3,5:12)]))
class(submit.predict.LIS$predict)
#Combine Train & Test LIS
combi.LSI.Test$Life_Style_Index<-submit.predict.LIS$predict
names(combi.LSI.Train)
names(combi.LSI.Test)
training.imp<-rbind(combi.LSI.Train,combi.LSI.Test)
summary(training.imp)

#Customer_Since_Months Iputation
combi.LSI.Test<-training.imp[which(is.na(training.imp$Customer_Since_Months)),]
combi.LSI.Train<-training.imp[which(!is.na(training.imp$Customer_Since_Months)),]
summary(combi.LSI.Train$Customer_Since_Months)
summary(combi.LSI.Test$Customer_Since_Months)
training.h2o <- as.h2o(combi.LSI.Train)
testing.h2o <- as.h2o(combi.LSI.Test)
names(training.h2o)
#dependent variable (Customer_Since_Months
y.dep <- 3
#independent variables
x.indep <- c(1:2,4:12)
training.h2o$Customer_Since_Months<-as.factor(training.h2o$Customer_Since_Months)
dlmodelbase <- h2o.randomForest(x=x.indep, y=y.dep, training_frame=training.h2o)
h2o.performance (dlmodelbase)
h2o.varimp(dlmodelbase)
submit.predict.LIS <- as.data.frame(h2o.predict(dlmodelbase, testing.h2o[,c(1:2,4:12)]))
class(submit.predict.LIS$predict)
combi.LSI.Test$Customer_Since_Months<-submit.predict.LIS$predict
names(combi.LSI.Train)
names(combi.LSI.Test)
training.imp<-rbind(combi.LSI.Train,combi.LSI.Test)
summary(training.imp)

#Type_of_Cab
combi.LSI.Test<-training.imp[which(is.na(training.imp$Type_of_Cab)),]
combi.LSI.Train<-training.imp[which(!is.na(training.imp$Type_of_Cab)),]
summary(combi.LSI.Train$Type_of_Cab)
summary(combi.LSI.Test$Type_of_Cab)
training.h2o <- as.h2o(combi.LSI.Train)
testing.h2o <- as.h2o(combi.LSI.Test)
names(training.h2o)
#dependent variable (Type_of_Cab)
y.dep <- 2
#independent variables
x.indep <- c(1,3:12)
training.h2o$Type_of_Cab<-as.factor(training.h2o$Type_of_Cab)
summary(training.h2o$Type_of_Cab)
dlmodelbase <- h2o.deeplearning(x=x.indep, y=y.dep, training_frame=training.h2o)
h2o.performance (dlmodelbase)
submit.predict.LIS <- as.data.frame(h2o.predict(dlmodelbase, testing.h2o[,c(1,3:12)]))
class(submit.predict.LIS$predict)
#Combine Train & Test LIS
combi.LSI.Test$Type_of_Cab<-submit.predict.LIS$predict
names(combi.LSI.Train)
names(combi.LSI.Test)
training.imp<-rbind(combi.LSI.Train,combi.LSI.Test)
summary(training.imp)
write.csv(training.imp,"train_imputed.csv",row.names = F)
h2o.shutdown(prompt = F)

#Test Imputations==========
#Same as above but for testing
library(h2o)
localH2O <- h2o.init(nthreads = -1,max_mem_size="3G")
combi.LSI.Test<-testing[which(is.na(testing$Life_Style_Index)),]
summary(combi.LSI.Test)
combi.LSI.Train<-testing[which(!is.na(testing$Life_Style_Index)),]
training.h2o <- as.h2o(combi.LSI.Train)
testing.h2o <- as.h2o(combi.LSI.Test)
names(training.h2o)
#dependent variable (LSI)
y.dep <- 4
#independent variables
x.indep <- c(1:3,5:12)
#RF
dlmodelbase <- h2o.randomForest(x=x.indep, y=y.dep, training_frame=training.h2o)
h2o.performance (dlmodelbase)
h2o.varimp(dlmodelbase)
submit.predict.LIS <- as.data.frame(h2o.predict(dlmodelbase, testing.h2o[,c(1:3,5:12)]))
class(submit.predict.LIS$predict)
#Combine Train & Test LIS
combi.LSI.Test$Life_Style_Index<-submit.predict.LIS$predict
names(combi.LSI.Train)
names(combi.LSI.Test)
testing.imp<-rbind(combi.LSI.Train,combi.LSI.Test)
summary(testing.imp)

#Customer_Since_Months Iputation
combi.LSI.Test<-testing.imp[which(is.na(testing.imp$Customer_Since_Months)),]
summary(combi.LSI.Test)
combi.LSI.Train<-testing.imp[which(!is.na(testing.imp$Customer_Since_Months)),]
training.h2o <- as.h2o(combi.LSI.Train)
testing.h2o <- as.h2o(combi.LSI.Test)
names(training.h2o)
#dependent variable (Customer_Since_Months)
y.dep <- 3
#independent variables
x.indep <- c(1:2,4:12)
training.h2o$Customer_Since_Months<-as.factor(training.h2o$Customer_Since_Months)
dlmodelbase <- h2o.randomForest(x=x.indep, y=y.dep, training_frame=training.h2o)
h2o.performance (dlmodelbase)
h2o.varimp(dlmodelbase)
submit.predict.LIS <- as.data.frame(h2o.predict(dlmodelbase, testing.h2o[,c(1:2,4:12)]))
class(submit.predict.LIS$predict)
combi.LSI.Test$Customer_Since_Months<-submit.predict.LIS$predict
names(combi.LSI.Train)
names(combi.LSI.Test)
testing.imp<-rbind(combi.LSI.Train,combi.LSI.Test)
summary(testing.imp)

#Type_of_Cab
combi.LSI.Test<-testing.imp[which(is.na(testing.imp$Type_of_Cab)),]
summary(combi.LSI.Test)
combi.LSI.Train<-testing.imp[which(!is.na(testing.imp$Type_of_Cab)),]
training.h2o <- as.h2o(combi.LSI.Train)
testing.h2o <- as.h2o(combi.LSI.Test)
names(training.h2o)
#dependent variable (Type_of_Cab)
y.dep <- 2
#independent variables
x.indep <- c(1,3:12)
training.h2o$Type_of_Cab<-as.factor(training.h2o$Type_of_Cab)
summary(training.h2o$Type_of_Cab)
dlmodelbase <- h2o.deeplearning(x=x.indep, y=y.dep, training_frame=training.h2o)
h2o.performance (dlmodelbase)
submit.predict.LIS <- as.data.frame(h2o.predict(dlmodelbase, testing.h2o[,c(1,3:12)]))
class(submit.predict.LIS$predict)
#Combine Train & Test LIS
combi.LSI.Test$Type_of_Cab<-submit.predict.LIS$predict
names(combi.LSI.Train)
names(combi.LSI.Test)
testing.imp<-rbind(combi.LSI.Train,combi.LSI.Test)
summary(testing.imp)
write.csv(testing.imp,"test_imputed.csv",row.names = F)
h2o.shutdown(prompt = F)
#Imputation END ===================================================

##=======Appendix============
##Hyper Parameter Search without Feature Engineering Data Set
#Hyper Parameter Tuning of Deep Learning Model using Random Search Criteria.
#http://ethen8181.github.io/machine-learning/h2o/h2o_deep_learning/h2o_deep_learning.html
#https://www.r-bloggers.com/hyperparameter-optimization-in-h2o-grid-search-random-search-and-the-future/
#https://github.com/h2oai/h2o-tutorials/blob/master/h2o-open-tour-2016/chicago/grid-search-model-selection.R
#https://github.com/h2oai/h2o-tutorials/tree/master/tutorials/deeplearning
#
library(h2o)
localH2O <- h2o.init(nthreads = -1,max_mem_size="3G")
training.h2o <- as.h2o(training.imp)

# summary(training.h2o)
# 
split <- h2o.splitFrame( training.h2o, c(0.6, 0.2))
train.hp <- h2o.assign( split[[1]], "train" ) # 60%
valid.hp <- h2o.assign( split[[2]], "valid" ) # 20%
test.hp  <- h2o.assign( split[[3]], "test" )  # 20%

hyper_params <- list(
  activation=c("Rectifier","Tanh","Maxout","RectifierWithDropout","TanhWithDropout","MaxoutWithDropout"),
  hidden=list(c(20,20),c(50,50),c(30,30,30),c(25,25,25,25)),
  input_dropout_ratio=c(0,0.05),
  l1=seq(0,1e-4,1e-6),
  l2=seq(0,1e-4,1e-6)
)
hyper_params

# ## Stop once the top 5 models are within 1% of each other (i.e., the windowed average varies less than 1%)
search_criteria = list(strategy = "RandomDiscrete", max_runtime_secs = 360, max_models = 100, seed=1234567, stopping_metric="logloss",stopping_rounds=5, stopping_tolerance=1e-2)  
y <- "Surge_Pricing_Type"
x <- setdiff(names(training.h2o), y)
dl_random_grid <- h2o.grid(
  algorithm="deeplearning",
  grid_id = "dl_grid_random",
  training_frame=train.hp,
  validation_frame=valid.hp,
  x=x,
  y=y,
  epochs=1,

  stopping_tolerance=1e-2,        ## stop when logloss does not improve by >=1% for 2 scoring events
  stopping_rounds=2,
  score_validation_samples=10000, ## downsample validation set for faster scoring
  score_duty_cycle=0.025,         ## don't score more than 2.5% of the wall time
  max_w2=10,                      ## can help improve stability for Rectifier
  hyper_params = hyper_params,
  search_criteria = search_criteria
)
grid <- h2o.getGrid("dl_grid_random",sort_by="logloss",decreasing=FALSE)
grid
# 
grid@summary_table[1,]
best_model <- h2o.getModel(grid@model_ids[[1]]) ## model with lowest logloss
best_model
#Validation Miss Classification Error in best Model.
h2o.confusionMatrix(best_model,valid=T)
#31%,
best_params <- best_model@allparameters
# best_params$activation
# best_params$hidden
# best_params$input_dropout_ratio
# best_params$l1
# best_params$l2

#Applying it to model along with Variable Importance.
# Deep Learning
final_dl_model<- h2o.deeplearning(
  x=best_params$x, y=best_params$y, 
  training_frame=training.h2o, 
  model_id = NULL, 
  validation_frame = NULL, 
  nfolds = best_params$nfolds, 
  keep_cross_validation_predictions = best_params$keep_cross_validation_predictions, 
   keep_cross_validation_fold_assignment = best_params$keep_cross_validation_fold_assignment, 
   fold_assignment = best_params$fold_assignment, 
   fold_column = NULL, 
   ignore_const_cols = best_params$ignore_const_cols, 
   score_each_iteration = best_params$score_each_iteration, 
   weights_column = NULL, 
   offset_column = NULL, 
   balance_classes = best_params$balance_classes, 
   class_sampling_factors = NULL, 
   max_after_balance_size = best_params$max_after_balance_size, 
   max_hit_ratio_k = best_params$max_hit_ratio_k, 
   checkpoint = NULL, 
   pretrained_autoencoder = NULL, 
   overwrite_with_best_model = best_params$overwrite_with_best_model, 
   use_all_factor_levels = best_params$use_all_factor_levels, 
   standardize = best_params$standardize, 
   activation = best_params$activation, 
   hidden = best_params$hidden, 
   epochs = best_params$epochs, 
   train_samples_per_iteration = best_params$train_samples_per_iteration, 
   target_ratio_comm_to_comp = best_params$target_ratio_comm_to_comp, 
   seed = best_params$seed, adaptive_rate = best_params$adaptive_rate, 
   rho = best_params$rho, epsilon = best_params$epsilon, 
   rate = best_params$rate, 
   rate_annealing = best_params$rate_annealing, 
   rate_decay = best_params$rate_decay, 
   momentum_start = best_params$momentum_start, 
   momentum_ramp = best_params$momentum_ramp, 
   momentum_stable = best_params$momentum_stable, 
   nesterov_accelerated_gradient = best_params$nesterov_accelerated_gradient, 
   input_dropout_ratio = best_params$input_dropout_ratio, 
   hidden_dropout_ratios = NULL, l1 = best_params$l1, 
   l2 = best_params$l2, max_w2 = best_params$max_w2, 
   initial_weight_distribution = best_params$initial_weight_distribution, 
   initial_weight_scale = best_params$initial_weight_scale, 
   initial_weights = NULL, 
   initial_biases = NULL, 
   loss = best_params$loss, 
   distribution =  best_params$distribution     , 
   quantile_alpha = best_params$quantile_alpha, 
   tweedie_power = best_params$tweedie_power, 
   huber_alpha = best_params$huber_alpha, 
   score_interval = best_params$score_interval, 
   score_training_samples = best_params$score_training_samples, 
   score_validation_samples = best_params$score_validation_samples, 
   score_duty_cycle = best_params$score_duty_cycle, 
   classification_stop = best_params$classification_stop, 
   regression_stop = best_params$regression_stop, 
   stopping_rounds = best_params$stopping_rounds, 
   stopping_metric = best_params$stopping_metric,
   stopping_tolerance = best_params$stopping_tolerance, 
   max_runtime_secs = best_params$max_runtime_secs, 
   score_validation_sampling = best_params$score_validation_sampling, 
   diagnostics = TRUE, fast_mode = TRUE, 
   force_load_balance = TRUE, 
   variable_importances = FALSE, 
   replicate_training_data = TRUE, single_node_mode = FALSE, 
   shuffle_training_data = FALSE, 
   missing_values_handling = best_params$missing_values_handling, 
   quiet_mode = best_params$quiet_mode, autoencoder = best_params$autoencoder, 
   sparse = best_params$sparse, 
   col_major = best_params$col_major, 
   average_activation = best_params$average_activation, 
   sparsity_beta = best_params$sparsity_beta, 
   max_categorical_features = best_params$max_categorical_features, 
   reproducible = best_params$reproducible, 
   export_weights_and_biases = best_params$export_weights_and_biases, 
   mini_batch_size = best_params$mini_batch_size, 
   categorical_encoding = best_params$categorical_encoding, 
   elastic_averaging = best_params$elastic_averaging, 
   elastic_averaging_moving_rate = best_params$elastic_averaging_moving_rate, 
   elastic_averaging_regularization = best_params$elastic_averaging_regularization
   ) 
h2o.performance (final_dl_model)
#Final train model error is 31% with Hit Ratio 68.7, 92,100
testing.h2o <- as.h2o(testing.imp)
submit.predict.SPT <- as.data.frame(h2o.predict(final_dl_model, testing.h2o))
summary(submit.predict.SPT$predict)
submit<-data.frame(TripID,submit.predict.SPT$predict)
head(submit)
names(submit)<-c("Trip_ID","Surge_Pricing_Type")
write.csv(submit,"submission.csv",row.names = F)
#Scoring Done

#Cleanup
h2o.shutdown(prompt = F)
rm(list = ls())
#Cleanup Done

#Pending is Variable Selection based on importance(RF or More Detailed DL??)
#Then find hyper parameters for the Deep Learning model using the subset
#Then train the model and score the test.

#Following lines of codes are Ruff.

# ####Variable Reduction!
# combi.imp<-rbind(training.imp,testing.imp)
# #Variable IMP Selected+Surge Pricing Type
# combi.final<-combi.imp
# #combi.final<-combi.imp[,c(top.var.a,'Surge_Pricing_Type')]
# summary(combi.final)
# 
# training<-combi.final[(1:train.row.length),]
# testing<-combi.final[-(1:train.row.length),]
# 
# #Remove all elements except train and test.
# # list<-ls()
# # l.imp<-list[c(-19,-14)]
# # rm(list=l.imp)
# #Merge End
# 
# ##Modeling===========
# setwd("D:/DataScience/Analytics Vidhya/mlmh")
# 
# library(h2o)
# localH2O <- h2o.init(nthreads = -1,max_mem_size="3G")
# #Looks like we need to use H20 Package to work on data of this size.
# training.h2o <- as.h2o(train)
# testing.h2o <- as.h2o(test)
# names(training.h2o)
# #dependent variable (Surge_Pricing_Type)
# y.dep <- 8
# #independent variables
# x.indep <- c(1:7)
# summary(training.h2o)
# rfmodelbase <- h2o.randomForest(x=x.indep, y=y.dep, training_frame=training.h2o)
# h2o.performance (rfmodelbase)
# #Nice# Error Rate reduced to 24%, Hit Rates increased to 75%, 93%. Better than Base Model.
# #Deep Learning
# dlmodelbase <- h2o.deeplearning(x=x.indep, y=y.dep, training_frame=training.h2o)
# h2o.performance (dlmodelbase)
# #Nice# Error Rate reduced to 24%, Hit Rates increased to 75%, 94%. Better than RF Model.
# #This will not ensure it will do well on test set! Still..
# submit.predict.SPT <- as.data.frame(h2o.predict(dlmodelbase, testing.h2o[,c(1:7)]))
# submit<-c(TripID,submit.predict.SPT$predict)
# names(submit)<-c("Trip_ID","Surge_Pricing_Type")
# write.csv(submit,"submission.csv",row.names = F)
#h2o.shutdown(prompt = F)
# #-------------##