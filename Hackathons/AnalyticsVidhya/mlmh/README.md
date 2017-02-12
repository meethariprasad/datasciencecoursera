Problem Statement
Welcome to Sigma Cab Private Limited - a cab aggregator service. Their customers can download their app on smartphones and book a cab from any where in the cities they operate in. They, in turn search for cabs from various service providers and provide the best option to their client across available options. They have been in operation for little less than a year now. During this period, they have captured surge_pricing_type from the service providers.

You have been hired by Sigma Cabs as a Data Scientist and have been asked to build a predictive model, which could help them in predicting the surge_pricing_type pro-actively. This would in turn help them in matching the right cabs with the right customers quickly and efficiently.    

    
Data
Variable
Definition
Trip_ID
ID for TRIP (Can not be used for purposes of modelling)
Trip_Distance
The distance for the trip requested by the customer
Type_of_Cab
Category of the cab requested by the customer
Customer_Since_Months
Customer using cab services since n months; 0 month means current month
Life_Style_Index
Proprietary index created by Sigma Cabs showing lifestyle of the customer based on their behaviour
Confidence_Life_Style_Index
Category showing confidence on the index mentioned above
Destination_Type
Sigma Cabs divides any destination in one of the 14 categories.
Customer_Rating
Average of life time ratings of the customer till date
Cancellation_Last_1Month
Number of trips cancelled by the customer in last 1 month
Var1, Var2 and Var3
Continuous variables masked by the company. Can be used for modelling purposes
Gender
Gender of the customer
Surge_Pricing_Type
Predictor variable can be of 3 types
 
 
Note: 
Evaluation Metric is accuracy i.e. percentage of Surge_Price_Category you correctly predict.
You are expected to upload the solution in the format of "sample_submission.csv"
Public and Private split is 25:75

