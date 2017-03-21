#Read this before you start anything
#http://www.clockbackward.com/2009/06/18/ordinary-least-squares-linear-regression-flaws-problems-and-pitfalls/
#Best Source for Reading:https://web.stanford.edu/~mrosenfe/soc_meth_proj3/matrix_OLS_NYU_notes.pdf
# http://stat.smmu.edu.cn/DOWNLOAD/ebook/econometric.pdf 

setwd("D:/DataScience/1Data Science Specialization Capstone/RMachine Learning")
#Data: http://people.sc.fsu.edu/~jburkardt/datasets/regression/x31.txt
#The correct solution is (-3,4,-1).

library(xlsx)
#I saved the the data in second sheet of regression.xlsx file.
data.df<-read.xlsx("regression.xlsx",2,header = F)
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
#You can reject the Full Hypothesis.

# p-value: 2.046e-06 not supporting Null Hypothesis.
PF<-1-pf(F, DFM, DFE, lower.tail=F)
PF

#What Next?

#Ridge Regression

#Lasso Regression

#Elastic Net

#Gradient Descent Method.


