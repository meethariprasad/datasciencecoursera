#Create Combi of trainining and testing
#Replace NA from Combi
#Find outliers and remove
#We will do proper cross validation by splitting train


# Data:Variable
# Loan_ID:Unique Loan ID
# Gender:Male/ Female
# Married
# Applicant married (Y/N)
# Dependents
# Number of dependents
# Education
# Applicant Education (Graduate/ Under Graduate)
# Self_Employed
# Self employed (Y/N)
# ApplicantIncome
# Applicant income
# CoapplicantIncome
# Coapplicant income
# LoanAmount
# Loan amount in thousands
# Loan_Amount_Term
# Term of loan in months
# Credit_History
# credit history meets guidelines
# Property_Area
# Urban/ Semi Urban/ Rural
# Loan_Status
# Loan approved (Y/N)
# 
# Note: 
# Evaluation Metric is accuracy i.e. percentage of loan approval you correctly predict.
# You are expected to upload the solution in the format of "sample_submission.csv"
# 



setwd("D:/DataScience/Analytics Vidhya/Loan Prediction")

#Training
training<-read.csv("train.csv",stringsAsFactors = TRUE,na.strings = c("", " "))
train.row.length<-length(training$Loan_ID)
testing<-read.csv("test.csv",stringsAsFactors = TRUE,na.strings = c("", " "))
#Preserving Test ID
Loan_ID<-testing$Loan_ID

#Remove ID
names.lnt<-length(names(training))
training<-training[,2:names.lnt]
names(training)
head(training,5)
summary(training)
names(training)
names(testing)

testing<-testing[names(training)[-12]]
testing$Loan_Status<-NA
head(testing)

combi<-rbind(training,testing)

# train<-combi[(1:train.row.length),]
# test<-combi[-(1:train.row.length),]
# 
# head(train)
# head(test)


combi$Gender<-as.factor(combi$Gender)
combi$Married<-as.factor(combi$Married)
combi$Education<-as.factor(combi$Education)
combi$Self_Employed<-as.factor(combi$Self_Employed)
combi$Credit_History<-as.factor(combi$Credit_History)
#combi$Loan_Status<-ifelse(combi$Loan_Status=="Y",1,0)
combi$Loan_Status<-as.factor(combi$Loan_Status)
summary(combi)

#NA Handling
library(missForest)
combi.imp <- missForest(combi)
summary(combi.imp$ximp)
combi.imp<-combi.imp$ximp

#Outlier Management. Replace the outliers with mean.

#Borrowed this function from a forum.
#Thanks https://datascienceplus.com/identify-describe-plot-and-removing-the-outliers-from-the-dataset/

outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  #response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  response<-"y"
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(dt))
  } else{
    cat("Nothing changed", "n")
    return(invisible(var_name))
  }
}

#Iterating by trial and error method till we remove all outliers in continuous
#columns
for(i in 1:4){i<-i
outlierKD(combi.imp,ApplicantIncome)
outlierKD(combi.imp,CoapplicantIncome)
outlierKD(combi.imp,LoanAmount)
}

###

summary(combi.imp)

#NA Handling. Assuming Outlier NA will be filled with Predictive outcomes.
library(missForest)
combi.imp.two <- missForest(combi.imp)
summary(combi.imp.two$ximp)
combi.imp<-combi.imp.two$ximp
#
str(combi.imp)

#Feature Engineering
#Assume you are Bank Manager
#I think about howmuch total income of the applicants per month, devide it by
#number of dependents multiply it by Loan Term and compare it with 
#Loan Amount Asked. 
#Typically Loan Amount Eligibility is 60*Net Monthly Take Home eligibility.
#Typically if Loan Amount is more then I will not give loan.

#Let us combine ApplicantIncome+CoapplicantIncome & Remove these two variables
combi.imp$Dependents<-ifelse(combi.imp$Dependents=="3+",3,combi.imp$Dependents)
combi.imp$totalincome<-((combi.imp$ApplicantIncome+combi.imp$CoapplicantIncome)*(1/1000)*(1/combi.imp$Dependents)*combi.imp$Loan_Amount_Term)
names(combi.imp)
head(combi.imp,5)
combi.imp<-combi.imp[c(-6,-7,-9)]
names(combi.imp)
head(combi.imp,5)
combi.imp$Loan_Status<-ifelse(combi.imp$Loan_Status=="Y",1,0)
combi.imp$Loan_Status<-as.factor(combi.imp$Loan_Status)
table(combi.imp$Loan_Status)
test.imp<-combi.imp[-(1:train.row.length),]
train.imp<-combi.imp[(1:train.row.length),]

head(train.imp)
head(test.imp)
#Just to check if imputation itself resulted a good prediction
#table(test.imp$Loan_Status)
#0: 81, doesn't look like. We are looking around 70-75
#Ok. Now important Variables
library(randomForest)
library(caret)
var.imp<-varImp(randomForest(Loan_Status~.,data=train.imp))
var.imp.order<-var.imp[order(-var.imp$Overall),,drop=FALSE]
var.imp.order
#After observing Get top 3 rows
top.imp.var<-head(var.imp.order,4)
top.imp.var<-row.names(top.imp.var[,,drop=FALSE])
top.imp.var
#Let us hope for the best :-)

very.imp.var<-top.imp.var
very.imp.var
#Subset Train.imp and Test.imp Set for these variable only
train.imp.sub<-train.imp[c(very.imp.var,"Loan_Status")]
head(train.imp.sub)
head(train.imp.sub,5)
#Sync Test
test.imp.sub<-test.imp[very.imp.var]
names(test.imp.sub)
names(train.imp.sub)
head(train.imp.sub,5)
#Cross Validation. Let us do properway!
set.seed(107)
inTrain <- createDataPartition(y = train.imp.sub$Loan_Status, p = .75, list = FALSE)
train.imp.sub.mdl <- train.imp.sub[ inTrain,]
test.imp.sub.mdl <- train.imp.sub[-inTrain,]

head(train.imp.sub.mdl,5)

table(train.imp.sub.mdl$Loan_Status)
#Without PCA
#1 Logistic
mdl_glm<-glm(Loan_Status~.,data=train.imp.sub.mdl,family=binomial(link='logit') )
Loan_Status<-predict(mdl_glm, test.imp.sub.mdl,type='response')
#par()
plot(Loan_Status)
#Loan_Status<-ifelse(Loan_Status>0.2,"Y","N")
#table(Loan_Status)
#1.1 Logistic. Performance Evaluvation

test.imp.sub.mdl$Pred.Loan_Status<-Loan_Status

library(pROC)

roc1 <- roc(response =test.imp.sub.mdl$Loan_Status  ,predictor =test.imp.sub.mdl$Pred.Loan_Status , auc=TRUE)
roc1
#79% GLM AUC. Not good.

#Random Forest
library(randomForest)
mdl_rf<-randomForest(Loan_Status~.,data=train.imp.sub.mdl,ntree=15000,mtry=3)
#mdl<-randomForest(Loan_Status~Credit_History+ApplicantIncome+LoanAmount+CoapplicantIncome,data=train.imp.sub,ntree=20000)
Loan_Status<-predict(mdl_rf, test.imp.sub.mdl,type="prob")
head(Loan_Status)
Loan_Status<-Loan_Status[,2]
head(Loan_Status)
plot(Loan_Status)
test.imp.sub.mdl$Pred.Loan_Status<-Loan_Status

library(pROC)
roc2 <- roc(response =test.imp.sub.mdl$Loan_Status  ,predictor =test.imp.sub.mdl$Pred.Loan_Status , auc=TRUE)
roc2
#76.93 % Auc. Not good!
roc1
roc.test(roc1, roc2)

# AUC of roc1 AUC of roc2 
# 0.7902778   0.7693452 

#Looks like glm is performing better

#===================================



#Checking Boosting Algorithms
library(caret)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
# C5.0
set.seed(seed)
library(C50)
fit.c50 <- train(Loan_Status~., data=train.imp.sub.mdl, method="C5.0", metric=metric, trControl=control)
# Stochastic Gradient Boosting
set.seed(seed)
fit.gbm <- train(Loan_Status~., data=train.imp.sub.mdl, method="gbm", metric=metric, trControl=control, verbose=FALSE)
# summarize results
boosting_results <- resamples(list(c5.0=fit.c50, gbm=fit.gbm))
summary(boosting_results)
dotplot(boosting_results)
#Accuracy of GBM looks like 82.28  on Cross Validated Data.
#Remember that this is on subset of training data.
#Should we do a roc.test for these two models?
#Let us do it

Loan_Status<-predict(fit.gbm, test.imp.sub.mdl,type="prob")
head(Loan_Status)
Loan_Status<-Loan_Status[,2]
head(Loan_Status)
plot(Loan_Status)
test.imp.sub.mdl$Pred.Loan_Status<-Loan_Status

library(pROC)
roc3 <- roc(response =test.imp.sub.mdl$Loan_Status  ,predictor =test.imp.sub.mdl$Pred.Loan_Status , auc=TRUE)
roc3
roc.test(roc2,roc3)
#0.76 is the test data AUC performance on gbm



# Checking Bagging algorithms
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
# Bagged CART
set.seed(seed)
fit.treebag <- train(Loan_Status~., data=train.imp.sub.mdl, method="treebag", metric=metric, trControl=control)
# Random Forest
set.seed(seed)
fit.rf <- train(Loan_Status~., data=train.imp.sub.mdl, method="rf", metric=metric, trControl=control)
# summarize results
bagging_results <- resamples(list(treebag=fit.treebag, rf=fit.rf))
summary(bagging_results)
dotplot(bagging_results)
#Looks like RF is giving 82.49 performance in bagging.
#Remember that this is on subset of training data.
#Should we do a roc.test for these two models?
Loan_Status<-predict(fit.rf, test.imp.sub.mdl,type="prob")
head(Loan_Status)
Loan_Status<-Loan_Status[,2]
head(Loan_Status)
plot(Loan_Status)
test.imp.sub.mdl$Pred.Loan_Status<-Loan_Status

library(pROC)
roc4 <- roc(response =test.imp.sub.mdl$Loan_Status  ,predictor =test.imp.sub.mdl$Pred.Loan_Status , auc=TRUE)
roc4
roc.test(roc4,roc1)
#Random Forest Performance looks better at 79%








# Stacking algorithms

# create submodels
control <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions='final', classProbs=TRUE)
algorithmList <- c('lda', 'rf', 'glm', 'nnet', 'svmRadial')
set.seed(seed)
train.imp.sub.mdl$Loan_Status
train.imp.sub.mdl$Loan_Status<-ifelse(train.imp.sub.mdl$Loan_Status==1,"Y","N")
train.imp.sub.mdl$Loan_Status

models_stack <- caretList(Loan_Status~., data=train.imp.sub.mdl, trControl=control, methodList=algorithmList)
results <- resamples(models_stack)
summary(results)
#Looks like glm, rf,svm has good predictions

dotplot(results)

#Select Least Corelated models
modelCor(results)
splom(results)
#Not sure there are any uncorelated models. I am not sure
#Stack Results will improve performance. Let us see.

#We will take fairly un corelated but almost similiar accuracy rf & nnet
#And rebuild model stack
algorithmList <- c('rf', 'nnet')
models_stack <- caretList(Loan_Status~., data=train.imp.sub.mdl, trControl=control, methodList=algorithmList)
results <- resamples(models_stack)
summary(results)
#RF is 82.7 % and nnet is 82%

# stack using glm
stackControl <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
set.seed(seed)
stack.glm <- caretStack(models_stack, method="glm", metric="Accuracy", trControl=stackControl)
print(stack.glm)
#82.52% Accuracy in Stack CV. Not bad. Let us see how it works on test portion
#of train!

#Let us predict on test portion of training data and find accuracy
predict.glm.cv.stack<-predict(stack.glm, newdata=test.imp.sub.mdl, type="prob")
plot(predict.glm.cv.stack)
#Predicted.loan.status<-ifelse(predict.glm.cv.stack>0.2,"Y","N")
#Confusion Matrix
roc.stack<-roc(predictor=predict.glm.cv.stack,response=test.imp.sub.mdl$Loan_Status,auc=TRUE)

roc.stack
#Stacked Model:0.8034
roc1
#Logistic:0.7903
roc2
#RF:AUC=0.7963


#Analysis:
#Eventhough RF is giving better result, stacked ensemble tends
#to remove overfit of data as per literature.

#So Selected Stacked model as final model and
#Applying it irrespective of leaderboard result.

#Before that let us build one more stack using RF and see if it 
#is better than glm stack.


# stack using random forest
set.seed(seed)
stack.rf <- caretStack(models_stack, method="rf", metric="Accuracy", trControl=stackControl)
print(stack.rf)

#Looks like stack.rf giving not better accuracy on train part at 77%
#So selecting stack.glm

head(train.imp.sub,1)
head(train.imp.sub.mdl,1)
train.imp.sub$Loan_Status<-ifelse(train.imp.sub$Loan_Status==1,"Y","N")
head(test.imp.sub)
head(train.imp.sub)

#Let us train the model on complete train and do stack prediction with glm.
control <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)

#Loan_Status<-ifelse(Loan_Status=="Y","N","Y")

######Predictions from here onwards
algorithmList <- c('rf', 'nnet')
models_stack <- caretList(Loan_Status~., data=train.imp.sub, trControl=control, methodList=algorithmList)
results <- resamples(models_stack)
summary(results)

# stack using glm or nnet
stackControl <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
set.seed(seed)
stack.glm <- caretStack(models_stack, method="gbm", metric="Accuracy", trControl=stackControl)
print(stack.glm)


#Let us predict on test data
predict.glm.cv.stack<-predict(stack.glm, newdata=test.imp.sub, type="prob")
plot(predict.glm.cv.stack)
boxplot(predict.glm.cv.stack)
summary(predict.glm.cv.stack)
Loan_Status<-ifelse(predict.glm.cv.stack>0.7390,"Y","N")
table(Loan_Status)
submit<-data.frame(Loan_ID=Loan_ID,Loan_Status=Loan_Status)
head(submit,5)
write.csv(submit,"submit_stack.csv",row.names=FALSE)
