# Problem Statement
# A retail company "ABC Private Limited" wants to understand the customer 
#purchase behaviour (specifically, purchase amount) against various products 
#of different categories. They have shared purchase summary of various customers
#for selected high volume products from last month.
# The data set also contains customer demographics (age, gender, marital status,
#city_type, stay_in_current_city), product details (product_id and product 
#category) and Total purchase_amount from last month.
# Now, they want to build a model to predict the purchase amount of customer 
#against various products which will help them to create personalized offer 
#for customers against different products.
# Data
# Variable	Definition
# User_ID	User ID
# Product_ID	Product ID
# Gender	Sex of User
# Age	Age in bins
# Occupation	Occupation (Masked)
# City_Category	Category of the City (A,B,C)
# Stay_In_Current_City_Years	Number of years stay in current city
# Marital_Status	Marital Status
# Product_Category_1	Product Category (Masked)
# Product_Category_2	Product may belongs to other category also (Masked)
# Product_Category_3	Product may belongs to other category also (Masked)
# Purchase	Purchase Amount (Target Variable)
# Your model performance will be evaluated on the basis of your prediction of 
#the purchase amount for the test data (test.csv), which contains similar 
#data-points as train except for their purchase amount. Your submission needs 
#to be in the format as shown in "SampleSubmission.csv".
# We at our end, have the actual purchase amount for the test dataset, 
#against which your predictions will be evaluated. 
#Submissions are scored on the root mean squared error (RMSE). 
#RMSE is very common and is a suitable general-purpose error metric. 
#Compared to the Mean Absolute Error, RMSE punishes large errors:
#   
#   Where y hat is the predicted value and y is the original value.
# Please note :
#   
#   Public leaderboard is based on 30% of the test dataset, while 70% of the dataset is used for Private Leaderboard.
# The final results would be declared only on Private Leaderboard
#https://datahack.analyticsvidhya.com/contest/black-friday/

#Step 1: DataLoad & Exploratory Analysis& Decisions & Implementation
#Step 2: Model Training and Cross Validation
#Step 3: Ensemble using Approximate Weighted Average (Or NN with 1 hiddenlayer?)
#Step 4: Submission
#Step 5: Rework, if any.

#Step1:Data Load & Exploratory Analysis & Decisions & Implementation========
  setwd("D:/DataScience/Analytics Vidhya/friday")
  set.seed(1234)
  library(h2o)
  library(audio)
  h2o.removeAll()
  #If there are any running H2o.Shut it down.
  h2o.shutdown(prompt = F)
  #Wait for sometime for all closures
  wait(10)
  localH2O <- h2o.init(nthreads = -1,max_mem_size="3G")

        #Data Load
        training<-read.csv("train.csv",stringsAsFactors = TRUE,na.strings = c("", " "))
        testing<-read.csv("test.csv",stringsAsFactors = TRUE,na.strings = c("", " "))
        #Save number of rows of Training to get Training back from combination.
        train.row.length<-nrow(training)
        
        #Analysis of Training data.
        str(training)
        
        #Creating Combination of Train and test and doing analysis
          #Making names(training)==names(testing). Adding missing Var from Test.
            testing$Purchase<-NA
            combi<-rbind(training,testing)
            summary(combi)
            #Make All Selected Columns except Purchase as Factor
            selCols = names(combi)[1:11]
            combi[selCols] <- lapply(combi[selCols], as.factor)
            #Check if class of variables got changed.
            sapply(combi, class)#Done.Except Purchase, all are factors
        #Summary of Combi
            summary(combi)
            #Let us mutate all factor variables with numerical alternatives.
            #Not Needed for User ID & Product ID as it has too many factors.
            #Gender,Age,City Category,Stay In Current City Years Need Change
            levels(combi$Gender)<-c(0,1)
            levels(combi$Age)<-c(17,25,35,45,50,55,60)
            levels(combi$City_Category)<-c(0,1,2)
            levels(combi$Stay_In_Current_City_Years)<-c(0,1,2,3,4)
            summary(combi)
            #Looks like NA itself forming class in P2 and P3.
            #Let us try to encode NA Presense and Absense.
            #To Do that we need to unfactor the Factor variable.
            library(dmm)
            combi$Product_Category_2<-unfactor(combi$Product_Category_2)
            combi$Product_Category_3<-unfactor(combi$Product_Category_3)
            combi$Product_Category_2<-replace(combi$Product_Category_2,which(is.na(combi$Product_Category_2)),0)
            combi$Product_Category_3<-replace(combi$Product_Category_3,which(is.na(combi$Product_Category_3)),0)
            combi$Product_Category_2<-replace(combi$Product_Category_2,which(!(combi$Product_Category_2==0)),1)
            combi$Product_Category_3<-replace(combi$Product_Category_3,which(!(combi$Product_Category_3==0)),1)
            combi$Product_Category_2<-factor(combi$Product_Category_2)
            combi$Product_Category_3<-factor(combi$Product_Category_3)
            summary(combi)
            #Do we really think Age & Stay in Current City Year is Categorical?
            #No. I strongly assume them to be a continuous in nature
            combi$Age<-as.numeric(as.character(combi$Age))
            combi$Stay_In_Current_City_Years<-as.numeric(as.character(combi$Stay_In_Current_City_Years))
            summary(combi)
            
            #Is all NA values Handled?: Yes.
            #Is all variables are satisfactorily matching to appropriate class?:Yes
            #Can we remove ID Variables?
            #Discussion: If you see ID Variables(USER & Product), 
            #these are not unique rows.They are actually repeating multiple times.
            #So removing them might not improve the value.
            #To Substantiate our claim we can check VARIMP of GB.
            #Also see with them how much we are getting the RMSE performance.
            
        #Spilt Combi to Training and Testing again and Split Training
            #to Train, Valid and Test. h2o.performance against test
            training<-combi[c(1:train.row.length),]
            testing<-combi[-c(1:train.row.length),]
            
            training.h2o <- as.h2o(training)
            testing.h2o <- as.h2o(testing)
            split <- h2o.splitFrame( training.h2o, c(0.7, 0.2))
            train <- h2o.assign( split[[1]], "train" ) # 70%
            valid <- h2o.assign( split[[2]], "valid" ) # 20%
            test  <- h2o.assign( split[[3]], "test" )  # 10%
            
           
# #Step2:Model Training and Cross Validation========
            
            #Let us check Variable Importance using GBM to
            y="Purchase"
            x<-setdiff(names(training),y)
            rf.base<-h2o.randomForest(x=x,y=y,
                                      training_frame=train[1:1000,],
                                      validation_frame = valid[1:1000,]
                                      )
            vip<-h2o.varimp(rf.base)
            #Important variables which ar greater than 5%
            vip.imp<-vip[which(vip$percentage*100>5),]$variable
            #Top Important Variables to consider
            vip.imp
            x<-vip.imp
            
            #Let us train our model with these parameters and check cv,test performance.
            
            #We will go for well known Boosting & DL Models
            gbm1 = h2o.gbm( x=x,    y = "Purchase",
                                    training_frame =train,
                                    validation_frame =valid ,
                                    max_depth = 3,
                                    distribution = "gaussian",
                                    ntrees =500,
                                    learn_rate = 0.05,
                                    nbins_cats = 5891
            )
            
            gbm2 = h2o.gbm( x=x,    y = "Purchase",
                                    training_frame =train,
                                    validation_frame =valid ,
                                    max_depth = 3,
                                    distribution = "gaussian",
                                    ntrees =600,
                                    learn_rate = 0.03,
                                    nbins_cats = 5891
            )
            #GBM1 
            h2o.performance(gbm1,newdata = test)
            #RMSE on Test: 2468
            #GBM2
            h2o.performance(gbm2,newdata = test)
            #RMSE on Test: 2461
            
            
# 
# #Step3:Ensemble using Approximate Weighted Average (Or NN with 1 hiddenlayer?)========
# Not yet done
            submit.predict.bestdl1 <- as.data.frame(h2o.predict(gbm1, testing.h2o[vip.imp]))
            submit.predict.bestdl1$predict<-ifelse(submit.predict.bestdl1$predict<0,0,submit.predict.bestdl1$predict)
            summary(submit.predict.bestdl1$predict)
            
            submit.predict.bestdl2 <- as.data.frame(h2o.predict(gbm2, testing.h2o[vip.imp]))
            submit.predict.bestdl2$predict<-ifelse(submit.predict.bestdl2$predict<0,0,submit.predict.bestdl2$predict)
            summary(submit.predict.bestdl2$predict)
            
            submit.predict.bestdl$predict<-0.8*submit.predict.bestdl1$predict+0.2*submit.predict.bestdl2$predict
            
# #Step4:Submission========
            
            predict.bestdl<-data.frame(testing$User_ID,testing$Product_ID,submit.predict.bestdl$predict)
            names(predict.bestdl)<-c("User_ID","Product_ID","Purchase")
            write.csv(predict.bestdl,"bfv1.csv",row.names = F)
            #Private LB Score:2483. Top 35
# #Step5: Rework, if any=======
# 
