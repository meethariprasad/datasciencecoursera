#Read this before you start anything
#http://www.clockbackward.com/2009/06/18/ordinary-least-squares-linear-regression-flaws-problems-and-pitfalls/
#Best Source for Reading:https://web.stanford.edu/~mrosenfe/soc_meth_proj3/matrix_OLS_NYU_notes.pdf
# http://stat.smmu.edu.cn/DOWNLOAD/ebook/econometric.pdf 

#Data: http://people.sc.fsu.edu/~jburkardt/datasets/regression/x31.txt
#The correct solution is (-3,4,-1).

library(readr)
data.df <- read_csv("https://raw.githubusercontent.com/meethariprasad/datasciencecoursera/master/Learning/lr.csv")

#Remove unneccesary Index(rownumber) column position
data.df<-data.df[2:5]

#Using default package available.
lm.mdl<-lm(data.df$X5~.,data.df)
summary(lm.mdl)

#Decomposing deep in to the algorithm used above.
#Solving Linear Least Squares using Normal Method, with QR Decomposition
#via Householder Algorithm.

#coefficients [b]=inv(t(X)(Matrix Multiplication)(X))(Matrix Multiplication)t(X)(Matrix Multiplication)y
Y<-as.matrix(data.df[4])
#But note that if you need to express Y=Xb, the coefficient of b_0 must be X_0 
#which is 1
X_0<-as.data.frame(c(1,1,1,1,1,1,1,1,1,1))
X<-(cbind(X_0,data.df[1:3]))
names(X)<-c("X1","X2","X3","X4")
X<-as.matrix(X)
#Create copy for final evaluvations
A<-X
#Method 1 Normal Method to find b
#Solving without QR Decomosition
#Component 0: (Matrix Multiplication)=>  %*% 
#Component 1: inv(t(X)(Matrix Multiplication)(X)) => solve
#Component 2: t(X)(Matrix Multiplication)y

#Finding Coeff by inversing Normal Eqn
coeff_inverse<-round(solve(t(A)%*%A)%*%(t(A)%*%Y))

#Solving with QR Decomosition
#The solution of the Normal equation also can be achieved using QR decomosition
#using numerically stable House Holder Algorithm

#In this method, matrix X is represented as A which is decomposed in to 
#Matrices Q&R
# => The normal equation converts in to: R%*%b=transpose(Q)%*%label
#House holder is one of the many algorithms which can get Q&R vector for A

#Householder
#A=QR=> A=(H1H2H3..HP)%*%R
# Householder refection:H 
#H_i = I - c_i*v_i%*%transpose(v_i)
#c_i=2/(transpose(v_i)%*%(v_i))
#House Holder Vector: v_i=x_i+||x_i||%*%e_i
# Best Video on HHolder Calculation https://www.youtube.com/watch?v=DstGBMyWIQM
#But it explains symmetric matrix. For non symmetric numeric calculations
#check http://www.math.usm.edu/lambers/mat610/sum10/lecture9.pdf
# Intention is to find iteratively H1H2H3H3H4..HP such that (H1H2H3..HP)%*%A=R
# R is upper triangular matrix

#We need tofind H1,H2,H3,H4. Let us start with H1

#House Holder Vector: v_i=x_i(+/-)||x_i||%*%e_i
# i=Even=> -, i=Odd=>+
x1<-as.matrix(X[,1])
deter_x<-sqrt(sum(x1^2))
n=dim(x1)[1]
deter_e1<-as.matrix(c(deter_x,rep(0,n-1)))
v1=x1+deter_e1
#c_i=2/(transpose(v_i)%*%(v_i))
c1<-as.numeric(2/(t(v1)%*%v1))
#H_i = I - c_i*v_i%*%transpose(v_i)
I<-diag(n)
H1<-I-c1*(v1%*%t(v1))
R1<-H1%*%X
#Check R1 and see if it is Upper Triangle Matrix
R1
#We will take rest of the interesting portion of matrix R1.
n=dim(R1)[1]
X<-as.matrix(as.data.frame(cbind(R1[2:n,2],R1[2:n,3],R1[2:n,4])))
x1<-as.matrix(X[,1])
deter_x<-sqrt(sum(x1^2))
n=dim(x1)[1]
deter_e1<-as.matrix(c(deter_x,rep(0,n-1)))
v1=x1-deter_e1
#c_i=2/(transpose(v_i)%*%(v_i))
c1<-as.numeric(2/(t(v1)%*%v1))
#H_i = I - c_i*v_i%*%transpose(v_i)
I<-diag(n)
H2<-I-c1*(v1%*%t(v1))
R2<-H2%*%X

#Check R2 and see if it is Upper Triangle Matrix, if no go for R3
n=dim(R2)[1]
X<-as.matrix(as.data.frame(cbind(R2[2:n,2],R2[2:n,3])))
x1<-as.matrix(X[,1])
deter_x<-sqrt(sum(x1^2))
n=dim(x1)[1]
deter_e1<-as.matrix(c(deter_x,rep(0,n-1)))
v1=x1+deter_e1
#c_i=2/(transpose(v_i)%*%(v_i))
c1<-as.numeric(2/(t(v1)%*%v1))
#H_i = I - c_i*v_i%*%transpose(v_i)
I<-diag(n)
H3<-I-c1*(v1%*%t(v1))
R3<-H3%*%X
R3

#Check R3 and see if it is Upper Triangle Matrix, if no go for R4
n=dim(R3)[1]
X<-as.matrix(as.data.frame(cbind(R3[2:n,2])))
x1<-as.matrix(X[,1])
deter_x<-sqrt(sum(x1^2))
n=dim(x1)[1]
deter_e1<-as.matrix(c(deter_x,rep(0,n-1)))
v1=x1-deter_e1
#c_i=2/(transpose(v_i)%*%(v_i))
c1<-as.numeric(2/(t(v1)%*%v1))
#H_i = I - c_i*v_i%*%transpose(v_i)
I<-diag(n)
H4<-I-c1*(v1%*%t(v1))
R4<-H4%*%X
R4
#As we can see R4 has all values except first element as zero
#Let us replace Matrices iteratively in R1 from R2 to R4 and round it of
R1[2:10,2:4]<-R2
R1[3:10,3:4]<-R3
R1[4:10,4]<-R4
R<-round(R1,5)
R
#We have our R matrix from house holder theorem
R<-R[1:4,1:4]


#A=QR,Q=inv(R)%*%A
#Q=H1%*%H2%*%H3%*%H4
H1_COM<-H1
# 
H_temp<-diag(10)
n=dim(H_temp)[1]
dim(H_temp[2:n,2:n])
dim(H2)
H_temp[2:n,2:n]<-H2
H2_COM<-H_temp
H2_COM

H_temp<-diag(10)
n=dim(H_temp)[1]
dim(H_temp[3:n,3:n])
dim(H3)
H_temp[3:n,3:n]<-H3
H3_COM<-H_temp
H3_COM

H_temp<-diag(10)
n=dim(H_temp)[1]
dim(H_temp[4:n,4:n])
dim(H4)
H_temp[4:n,4:n]<-H4
H4_COM<-H_temp
Q=H1_COM%*%H2_COM%*%H3_COM%*%H4_COM

#Get Square matrix of R[1:4,1:4]
#We have Q and R. We need to find coefficients
#Select top 4 in both sides. Why it gives right results in RHS of matrix?
coeff_manual<-round(solve(R[1:4,1:4]) %*% (t(Q) %*% Y)[1:4,])
coeff_lmmdl<-as.data.frame(as.numeric(round(lm.mdl$coefficients)))
coeff_inverse

#Compare the coefficients found using Different Methods.
compare=data.frame(cbind(coeff_inverse,coeff_lmmdl,coeff_manual))
names(compare)<-c("Inverse","function_lm","manual")

#Understanding lm Output
summary(lm.mdl)

#Residuals: http://stattrek.com/regression/residual-analysis.aspx?Tutorial=AP
# Residuals:
#   Min     1Q Median     3Q    Max 
# -5.323 -4.687  1.864  3.038  6.496 
#Basically it is Y-F(Y) where F(Y)=-1+(-2)X2+(4)X3+(-1)X4
lm.mdl$residuals
# Coefficients:
#               Estimate    Std. Error   t value     Pr(>|t|)    
# (Intercept)    -1.0850     1.9503       -0.556     0.59810    
#   X2           -2.9008     0.1791       -16.196    3.52e-06 ***
#   X3            3.9478     0.2385        16.550    3.10e-06 ***
#   X4           -0.9344     0.1866       -5.008     0.00243 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Std Error of Coefficients: Ok we have coefficients. 
#The standard deviation of the estimate of a regression coefficient measures how precisely the model estimates the coefficient's unknown value. The standard error of the coefficient is always positive.

# Use the standard error of the coefficient to measure the precision of 
# the estimate of the coefficient. The smaller the standard error, the more 
# precise the estimate. Dividing the coefficient by its standard error calculates 
# a t-value. If the p-value associated with this t-statistic is less than your 
# alpha level, you conclude that the coefficient is significantly different 
# from zero.

#Standard Error is  a measure of the statistical accuracy of an estimate, 
#equal to the standard deviation of the theoretical distribution of a 
#large population of such estimates.
#Now standard deviation is square root of variance.
#So if we get variance of coefficients, we will get standard error.
# To get variance we will create Variance-CoVaraiance Matrix of estimated 
#coefficients, say b.
#http://stats.stackexchange.com/questions/44838/how-are-the-standard-errors-of-coefficients-calculated-in-a-regression
#https://onlinecourses.science.psu.edu/stat501/sites/onlinecourses.science.psu.edu.stat501/files/pt2_multiple_linear_regression.pdf

#V(b) = MSE*(inverse of (t(X)%*%X))

X<-A
N <- nrow(X)
p <- ncol(X)
XtXinv <- solve(crossprod(X))
#http://genomicsclass.github.io/book/pages/standard_errors.html
resid <- Y - X %*% XtXinv %*% crossprod(X,Y)
s <- sqrt( sum(resid^2)/(N-p))
ses <- s*sqrt(diag(XtXinv))
ses

#t value
#We need to check if the coefficient found is of any significance
#or Another way of saying if the variable has any effect on regression.
# Null Hypothesis here is, The true beta = beta_0 = 0, means no effect.
# The probability of t statistic if above threshold fails to reject null hypothesis
# The probability of t statistic if below threshold fails to accept Null.
#T statistic is Coeff-0/(standard error)
#https://en.wikipedia.org/wiki/T-statistic 
#http://reliawiki.org/index.php/Simple_Linear_Regression_Analysis
tstat<-compare$manual/ses
tstat
#Pr(>|t|). Find this from T table
#Degree of freedom
#http://www.unc.edu/~nielsen/soci708/mod11/soci708mod11.pdf
df<-nrow(X)-(ncol(X)-1)-1
#http://stackoverflow.com/questions/41505213/how-to-manually-compute-the-p-value-of-t-statistic-in-linear-regression
P<-2 * pt(-abs(tstat), df)
level_of_significance=0.05
P>level_of_significance
#As you can see, ignoring constant, all intercepts fail to accept Null Hypothesis
# All variables are statistically significant
#Signif. codes:
#Tells you at which level of the significance value belongs. Just to help you.

# Residual standard error: 5.382 on 6 degrees of freedom
#http://stats.stackexchange.com/questions/57746/what-is-residual-standard-error
#The residual standard error you've asked about is nothing more 
#than the positive square root of the mean square error.
Sum_Residual_Square<-sum(lm.mdl$residuals^2)
AVG_Sum_Residual_Square<-Sum_Residual_Square/df
Residual_Standard_Error<-sqrt(AVG_Sum_Residual_Square)
Residual_Standard_Error
#Multiple R-squared:  0.9902
#The coefficient of multiple correlation, denoted R, is a scalar that is 
# defined as the Pearson correlation coefficient between the predicted and 
# the actual values of the dependent variable in a linear regression model 
# that includes an intercept.
#R-squared = Explained variation / Total variation
#R squared = 1-(SS(Residual)/(SS(Total)))
SS_Residual<-sum(lm.mdl$residuals^2)
SS_Total<-sum((Y-mean(Y))^2)
coeff_of_deter<-1-((SS_Residual)/(SS_Total))
#Adjusted R-squared:  0.9853 
#http://thestatsgeek.com/2013/10/28/r-squared-and-adjusted-r-squared/
#Adjusted R squared
nominator<-SS_Residual/(nrow(X)-ncol(Y)-1)
denominator<-SS_Total/(nrow(X)-1)
Adj_R_Squared<-1-(nominator/denominator)
Adj_R_Squared
# F-statistic: 202.3 on 3 and 6 DF

# For a multiple regression model with intercept, 
# we want to test the following null hypothesis and alternative hypothesis:
# DFE = n - p  and DFM = p - 1
# Whether Collectively Variables has any effect on the regression!
#   H0:   ??1 = ??2 = ... = ??p-1 = 0
# 
# H1:   ??j ??? 0, for at least one value of j
# 
# This test is known as the overall F-test for regression. 
# 
# Here are the five steps of the overall F-test for regression 
# 
# State the null and alternative hypotheses:
#   
#   H0:   ??1 = ??2 = ... = ??p-1 = 0 
# 
# H1:   ??j ??? 0, for at least one value of j
# 
# Compute the test statistic assuming that the null hypothesis is true:
#   
#   F = MSM / MSE = (SSM/DFM) / (SSE/DFE)=(explained variance) / (unexplained variance)
# 
# Find a (1 - ??)100% confidence interval I for (DFM, DFE) degrees of freedom using an F-table or statistical software. 
# 
# Accept the null hypothesis if F ??? I; reject it if F ??? I. 
# 
# Use statistical software to determine the p-value.
#F = MSM / MSE = (SSM/DFM) / (SSE/DFE)
# Corrected Degrees of Freedom for Model:   DFM = p - 1 
# Degrees of Freedom for Error:   DFE = n - p 
# Mean of Squares for Model:   MSM = SSM / DFM 
# Mean of Squares for Error:   MSE = SSE / DFE
# SSM: Sum of Squares for Model Against Average: SSM = SS_Total
# Sum of Squares for Model Against Error: SSE = Sum_Residual_Square

DFM = ncol(X) - 1
DFE = nrow(X) - ncol(X)
F=((SS_Total)/(DFM))/((Sum_Residual_Square)/(DFE))
I<-0:qf(0.95, DFM, DFE)
#Check if F belongs to the interval
F==I
#All are False.
#You can reject the Null Hypothesis.

# p-value: 2.046e-06 not supporting Null Hypothesis.
PF<-1-pf(F, DFM, DFE, lower.tail=F)
PF

#What Next?

#Gradient Descent Method.

#Read this before you start anything
#http://www.clockbackward.com/2009/06/18/ordinary-least-squares-linear-regression-flaws-problems-and-pitfalls/
#Best Source for Reading:https://web.stanford.edu/~mrosenfe/soc_meth_proj3/matrix_OLS_NYU_notes.pdf
# http://stat.smmu.edu.cn/DOWNLOAD/ebook/econometric.pdf 

#Data: http://people.sc.fsu.edu/~jburkardt/datasets/regression/x31.txt
#The correct solution is (-3,4,-1).

library(readr)
data.df <- read_csv("https://raw.githubusercontent.com/meethariprasad/datasciencecoursera/master/Learning/lr.csv")

#Remove unneccesary Index(rownumber) column position
data.df<-data.df[2:5]
#Independent Variables +intercept term
X<-cbind(Intercept=rep(1,nrow(data.df)),data.df[1:3])
X<-as.matrix(X)
#Dependent Variable
Y<-as.matrix(data.df[4])

#Batch Gradient Descent Algorithm

#We need to change it in vectorized format.

#Start with a Random parameters say 0,0,0,0 to begin with for 3 independent variables & Intercept.
theta<-list()
#Iteration<-1

theta[[1]]<-as.matrix(c(0,0,0,0))
#Start with a random learning rate say 0.1
alpha<-0.01
#Calculate initial cost function cost  
#Cost function for multivariate regression
#J(theta) =1/2m*transpose((X)%*%theta-Y)%*%(X%*%theta-Y)
J<-numeric()
#predictedvalue=h(x) over theta
m=nrow(X)
predictedvalue<-X%*%theta[[1]]
actualvalue<-Y
#Square of a Column vector Dot product of two vectors
sum_of_squared_error<-(t(predictedvalue-actualvalue)%*%(predictedvalue-actualvalue))
J[1]<-(1/(2*m))*sum_of_squared_error
#920.25 is initial cost

#Update all parameters simultaneously as per Gradient Descent Algorithm

temp<-numeric()
#The updates are going wrong as multiplication of additional term x on each row of h-y is needed
temp[1]<-theta[[1]][[1]]-(alpha/m)*sum((X%*%theta[[1]]-actualvalue)*as.matrix(X[,1]))

temp[2]<-theta[[1]][[2]]-(alpha/m)*sum((X%*%theta[[1]]-actualvalue)*as.matrix(X[,2]))

temp[3]<-theta[[1]][[2]]-(alpha/m)*sum((X%*%theta[[1]]-actualvalue)*as.matrix(X[,3]))

temp[4]<-theta[[1]][[2]]-(alpha/m)*sum((X%*%theta[[1]]-actualvalue)*as.matrix(X[,4]))


theta[[2]]<-as.matrix(temp)

#J[2]
#We will update theta with newly find one. theta 2

predictedvalue<-X%*%theta[[2]]
actualvalue<-Y
#Square of a Column vector Dot product of two vectors
sum_of_squared_error<-(t(predictedvalue-actualvalue)%*%(predictedvalue-actualvalue))
J[2]<-(1/(2*m))*sum_of_squared_error
#J[2] drastically reduced to 96.7 Nice improvement. We are going good.

#Similiary find J[3], J[4] and so on till whatever iteration you want
#After one phase cost function will not decrease beyond 0.001
#That is where we say we reached global minimum of convex function J

#Wanna do all above four steps in a single shot!?
#Have fun with Linear Algebra!

theta[[2]]<-theta[[1]]-(alpha/m)*t(t(X%*%theta[[1]]-actualvalue)%*%X)

predictedvalue<-X%*%theta[[2]]
actualvalue<-Y
#Square of a Column vector Dot product of two vectors
sum_of_squared_error<-(t(predictedvalue-actualvalue)%*%(predictedvalue-actualvalue))
J[2]<-(1/(2*m))*sum_of_squared_error

#Let us loop it over.

#Step 1 Populate Initial Values of Theta and Cost
theta[[1]]<-as.matrix(c(0,0,0,0))
predictedvalue<-X%*%theta[[1]]
actualvalue<-Y
sum_of_squared_error<-(t(predictedvalue-actualvalue)%*%(predictedvalue-actualvalue))
J[1]<-(1/(2*m))*sum_of_squared_error

#Mention How many interations you need.

iterations<-2
#Step 2: loop it Over to get Theta iterations+1 and J iterations+1
for(i in 1:iterations){
  j<-i+1
  theta[[j]]<-theta[[i]]-(alpha/m)*t(t(X%*%theta[[i]]-actualvalue)%*%X)
  
  predictedvalue<-X%*%theta[[j]]
  actualvalue<-Y
  #Square of a Column vector Dot product of two vectors
  sum_of_squared_error<-(t(predictedvalue-actualvalue)%*%(predictedvalue-actualvalue))
  J[j]<-(1/(2*m))*sum_of_squared_error
}

#Let us make a function out of it.
#A function which will return us a list of theta and J values for each list.
#Input for the function will be Matrix X with Intercept, Matrix Y, iterations

get_gradient<-function(X,Y,iterations,alpha){
  
  m<-nrow(X)
  #Step 1 Populate Initial Values of Theta and Cost
  theta<-list()
  theta[[1]]<-as.matrix(rep(0,ncol(X)))
  predictedvalue<-X%*%theta[[1]]
  actualvalue<-Y
  sum_of_squared_error<-(t(predictedvalue-actualvalue)%*%(predictedvalue-actualvalue))
  J[1]<-(1/(2*m))*sum_of_squared_error
  
  iterations<-iterations
  #Step 2: loop it Over to get Theta iterations+1 and J iterations+1
  for(i in 1:iterations){
    j<-i+1
    theta[[j]]<-theta[[i]]-(alpha/m)*t(t(X%*%theta[[i]]-actualvalue)%*%X)
    
    predictedvalue<-X%*%theta[[j]]
    actualvalue<-Y
    #Square of a Column vector Dot product of two vectors
    sum_of_squared_error<-(t(predictedvalue-actualvalue)%*%(predictedvalue-actualvalue))
    J[j]<-(1/(2*m))*sum_of_squared_error
  }
  
  results<-list()
  results[[1]]<-theta
  results[[2]]<-J
  return(results)
  
}

#Set for high iteration. Observe that Coefficients stop changing much and also cost value.
k<-get_gradient(X,Y,iterations=30,alpha=0.01)
#Plot your cost values from result.
plot(k[[2]]) 
#As you can see that after 3rd iteration it starts to converge.
#But remember GD doesn't immediately reach to to global minima but reaches near to it.
##The correct solution is (-3,4,-1). Let us see if GD is giving us or how near it can go.
k[[1]][[3]]
#You can see that stable coefficients are almost near to what we expected.
# k[[1]][[3]]
# X5
# Intercept -0.113822
# X2        -2.951618
# X3         3.205256
# X4        -0.722127

#See what reducing the learning rate does to convergence.
k2<-get_gradient(X,Y,iterations=1000,alpha=0.001)
#Plot your cost values from result.
plot(k2[[2]]) 
plot(k2[[2]][c(1:100)]) 
#It is pretty slow convergence. Isn't it? But it reaches for sure.
k2[[1]][[30]]



#Congrats. We successfully implemented Gradient Descent for Multiple Linear Regression. Awesome!

#There are few of problems though!


#Problem 1: In Big Data, the Batch Gradient Descent will be slow because you can see that 
#it is calculating sum over 1 to m for every time for single Parameter updates.

#Instead of this how about updating parameters for each row, hence eliminating the sum operation
#But isn't it goint to be more costly to calculate theta parameters for every row?



#Solution 1:
#After this we will see Stochastic Gradient Descent

#The logic is instead of calculating sum for all train examples for one parameter update,
#start updating the parameter for once row of training example.
#Now calculate the cost for that one training row with new theta values.
#Go for next row and update the theta values and calculate the cost for that row.
#Like this in one iteration, you will update the theta m times, which is number of rows.
#Plot the cost to see if it converges.
#If it is not converging go for one more iteration.

#Remember that since we are adjusting theta for every row than overall, 
#cost might increase in between.
#Then it will decrease in some cases.
#Then it goes near to global optima and moves around there.
#And yes, by randomly shuffling the rows, you might hit luck earlier in your iterations day!

#Again to recap: What is the difference again getween GD and SGD?
#In GD: Every iteration means updating only one new theta value.
#You will continue iterations and find theta untill you find cost value converges.
#In every iterations entire matrix of data X gets multiplied with previous theta value.
#Since it is only one theta update per iteration it might take multiple iterations to converge.
#Also remember that there is sum of all row costs has to happen for each theta calculations.

#In SGD: Every iteration means theta gets updated as many times as rows.
#Means if there are 1,00,000 rows, theta(remember theta is not 1, but all parameter matrix) 
#gets updated 1,00,000 times per iteration.

#Let us start with row 1, shall we? And the story begins
#Cost for each row: Cost(theta,row(x,y))=(1/2)row(h(x)-y)^2
#Train Cost function:J(theta)<-(1/m)sum over 1 to m(Cost(theta,row(x,y)))
#How to update theta?

#Repeat iterations
#for i = 1 to number of rows{
#theta(j)<-theta(j)-learningrate(h(x)-y)x(j)
#j is the columnnumber, 0 to n, 0 is for intercept
# }

#All right. Once we do code it in simple way, then we will see vectorized operations.
iterations<-1
m<-nrow(X)
theta<-list()
theta[[1]]<-as.matrix(c(0,0,0,0))
alpha<-0.01
#First row, start with first column j
i=1
j=1
#First row predicted value for theta 0,0,0,0
h_xi<-t(theta[[i]])%*%as.matrix(X[i,])

#Check for Cost before updating theta values
Cost<-numeric()

#First Row actual value
Y[i,]
#Cost  of first row for the theta 0,0,0,0
Cost[i]<-(1/2)*(h_xi-Y[i,])^2

#J=1 gives our first intercept coefficient change for row 1.
j=1
##First Column(Intecept), First Row value
X[i,j]

theta[[i+1]]<-matrix()
theta[[i+1]][[j]]<-theta[[i]][[j]]-alpha*(h_xi-Y[i,])*X[i,j]
#We need to update 3 more parameter updates for row 1 considering we have 4 independent variables
j=2
theta[[i+1]][[j]]<-theta[[i]][[j]]-alpha*(h_xi-Y[i,])*X[i,j]
j=3
theta[[i+1]][[j]]<-theta[[i]][[j]]-alpha*(h_xi-Y[i,])*X[i,j]
j=4
theta[[i+1]][[j]]<-theta[[i]][[j]]-alpha*(h_xi-Y[i,])*X[i,j]
theta[[i+1]]<-as.matrix(theta[[i+1]])
#Store this first iteration to check when you create loop to verify theta values.
Compare<-theta[[i+1]]
#Ok. Now for every row you get parameters equal to number of columns in feature.


#Continue the same process above for all rows and plot the cost value.
#In case of millions of rows, take the average of say 1000 rows and see plot makes sense.
#If not see with 5000 or 10000 rows average plot
#If the graph shows diverging change learning rate.
#And run for more iterations if needed! It is expected to get in one pass itself the optimal values.

#Ok. You can always vectorize the code to reduce the number of lines.
#Let us make a function out of it.

get_sgd<-function(X,Y,rounds,alpha){
  #Shuffle the data.
  
  internal_mat<-cbind(X,Y)
  #Shuffling
  internal_mat<-internal_mat[sample.int(nrow(internal_mat)),]
  X<-internal_mat[,1:(ncol(internal_mat)-1)]
  Y<-as.matrix(internal_mat[,ncol(internal_mat)])
  theta<-matrix()
  Cost<-numeric()
  Costdf<-data.frame()
  J<-numeric()
  rounds<-rounds
  alpha<-alpha
  theta<-matrix(0, ncol = 1, nrow = ncol(X))
  for(iterations in 1:rounds){
    #Initialize theta iterations and update the first column with lastest update
    for(i in 1:nrow(Y)){
      h_xi<-t(theta)%*%as.matrix(X[i,])
      diff<-h_xi-Y[i,]
      Cost[i]<-(1/2)*(diff)^2
      Costdf<-rbind(Costdf,data.frame(iterations,round(Cost[i])))
      theta<-(as.matrix(theta)-alpha*t(diff%*%X[i,]))
    }
    
    J[iterations]<-sum(Cost)/(nrow(X))
  }
  result_list<-list()
  result_list[[1]]<-Costdf
  result_list[[2]]<-J
  result_list[[3]]<-theta
  return(result_list)
}#End of function

s<-get_sgd(X=X,Y=Y,rounds = 100,alpha = 0.001)

#Watch out for last few columns of results to see diverging or converging
#s[[1]] : Costs over iterations
plot((s[[1]]))
#s[[2]] : J Values
plot(s[[2]])
#Looks like in 5th iteration itself we might have reached global minimum
#Final theta at round 100.
s[[3]]
#Run only 4-5 iterations and find the theta there.
round(get_sgd(X=X,Y=Y,rounds = 4,alpha = 0.001)[[3]])
#We reached the goal in 4 iterations.


#You can see the result is almost near to the correct solution is (-3,4,-1) for theta.X2,X3,X4.
#Don't try to put effort to over fit it as stochastic gradient J goes in and around global minima
# Briefly, when the learning rates {\displaystyle \eta } \eta decrease with an appropriate rate, 
# and subject to relatively mild assumptions, stochastic gradient descent converges almost surely 
# to a global minimum when the objective 
# function is convex or pseudoconvex, and otherwise converges almost surely to a local minimum


#Awesome. You can celebrate saying you had fun with SGD. Stochastic
#But why that word stochasitc?
#That is because random shuffling rule as step 1.
#We have not put that in out function code.
#Let us try it out to check out if convergence can be done soon
data.df<-data.df[2:5]
#Shuffle the data
data.df<-data.df[sample(nrow(data.df)),]
#Independent Variables +intercept term
X<-cbind(Intercept=rep(1,nrow(data.df)),data.df[1:3])
X<-as.matrix(X)
#Dependent Variable
Y<-as.matrix(data.df[4])
new_s<-get_sgd(X=X,Y=Y,rounds = 10,alpha = 0.001)

#Check the convergence on cost function.
s[[1]]
#Converging around iteration 5 somewhere in 3rd value, J started to work around global minimum

new_s[[1]]
#You can see in iteration 5 somewhere in 6th value, J started to work around global  minimum


#You can see manually done compare and via loop producing same results. Great!
s[[2]][[1]][,2]
Compare
#The correct solution is (-3,4,-1) for theta.X2,X3,X4.

#After this next step is to work on Mini Batch Gradient Descent
#instead of 1 at a time we will do sum over 10 at a time.


#Problem 2: The existance of outliers and biased variables tend to affect regularization. We need to penalize our model to reduce these effects.

#Solutions


#Solution 2:
#Regularization
#Logistic Regression
#Ridge Regression

#Lasso Regression

#Elastic Net

#Anamoly Detection
# setwd("D:/DataScience/2Course Era Data Science-Machiene Learning/coursera-stanford-master/coursera-stanford-master/machine_learning/exercises/matlab/machine-learning-ex8/ex8")
# library(R.matlab)
# anamolydatax<-data.frame(readMat("ex8data2.mat"))
# summary(anamolydatax)
# anamolydatax$yval<-as.factor(anamolydatax$yval)
# 
# library(h2o)
# h2o.init(nthreads = -1)
# anamolydata.hex<-as.h2o(anamolydatax)
# h2o.frames<-h2o.splitFrame(data=anamolydata.hex,ratios = c(0.1,0.8))
# summary(h2o.frames[[1]])#Valid
# summary(h2o.frames[[2]])#Train
# summary(h2o.frames[[3]])#Test
# #Better to use stratified cross validation
# features<-h2o.names(anamolydata.hex)[!h2o.names(anamolydata.hex) %in% "yval"]
# y<-"yval"
# gbm.imbalance<-h2o.gbm(
#   training_frame = h2o.frames[[2]]
#   ,validation_frame = h2o.frames[[1]]
#   ,x=features
#   ,y=y
#   ,nfolds=10
#   #,fold_assignment="Stratified"
#   #,balance_classes=T
#   ,seed=1234
# )
# gbm.imbalance
# 

