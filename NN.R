#clean environment 
rm(list=ls())

#display work items
ls()
#get working directory
getwd()
#set working directory
setwd("D:\\Bigdata\\project\\madrid-airbnb-data (1)")


############################################################################################


#install.packages("ROCR")
#install.packages("corrplot")
#install.packages("fastDummies")
#install.packages("MASS")
#install.packages("car")
#install.packages("RSNNS")
#install.packages("BBmisc")
library(neuralnet)
library(ROCR)
library(corrplot)
library(fastDummies)
library(MASS)
library(car)

require(RSNNS)

require(BBmisc)
###########################################################################################



dfm <- read.csv("listings_detailed.csv")


pricing<-data.frame( price<-dfm$price)
pricing<-na.omit(pricing)
pricing$price = as.numeric(gsub("\\$", "", pricing$price))



data <- data.frame(
  host_is_superhost=as.numeric(dfm$host_is_superhost),
  host_identity_verified=as.numeric(dfm$host_identity_verified),
  #neighbourhood=dfm$neighbourhood,
  zipcode=dfm$zipcode,
  accommodates=dfm$accommodates,
  bedrooms=dfm$bedrooms,
  beds=dfm$beds,
  bathrooms=dfm$bathrooms,
  number_of_reviews=dfm$number_of_reviews,
  #roomtype=dfm$room_type,
  price=dfm$price,
  guests_included=dfm$guests_included,
  minimum_nights=dfm$minimum_nights,
  maximum_nights=dfm$maximum_nights,
  review_scores_rating=dfm$review_scores_rating,
  review_scores_accuracy=dfm$review_scores_accuracy,
  review_scores_cleanliness=dfm$review_scores_cleanliness,
  review_scores_checkin=dfm$review_scores_checkin,
  review_scores_communication=dfm$review_scores_communication,
  review_scores_location=dfm$review_scores_location,
  review_scores_value=dfm$review_scores_value,
  instant_bookable=dfm$instant_bookable,
  cancellation_policy=dfm$cancellation_policy
)

data$price = as.numeric(gsub("\\$", "", data$price))
data<-dummy_cols(data,remove_first_dummy=TRUE,remove_selected_columns=TRUE)

data<- data[data$price<200,]



str(data)

# Random sampling
samplesize = 0.8 * nrow(data)
set.seed(80)
index = sample( seq_len ( nrow ( data ) ), size = samplesize )

# Create training and test set
datatrain = data[ index, ]
datatest = data[ -index, ]

data<- na.omit(data)
## Scale data for neural network

max = apply(data , 2 , max)
min = apply(data, 2 , min)
scaled<-normalize(data, method="range")
scaled$price<-data$price
params<-getNormParameters(normalizeData(data[,-9], type="0_1"))
attr(data, "normParams")
data$price
scaled$price
#scaled = as.data.frame(data)


table(is.na(scaled))
# creating training and test set
trainNN = scaled[index , ]
table(is.na(trainNN))

testNN = scaled[-index , ]
#maxtest = apply(testNN , 2 , max)
#mintest = apply(testNN, 2 , min)


testNN<- na.omit(testNN)
trainNN <- na.omit(trainNN)
# fit neural network
set.seed(2)




#################################










#########################

NN = neuralnet(price~
                 host_is_superhost+
                 host_identity_verified+
                 #neighbourhood=dfm$neighbourhood+
                 zipcode+
                 accommodates+
                 bedrooms+
                 beds+
                 bathrooms+
                 number_of_reviews+
                 #roomtype+
                 guests_included+
                 minimum_nights+
                 maximum_nights+
                 review_scores_rating+
                 review_scores_accuracy+
                 review_scores_cleanliness+
                 review_scores_checkin+
                 review_scores_communication+
                 review_scores_location+
                 review_scores_value+
                 instant_bookable_t+
                 cancellation_policy_moderate+
                 cancellation_policy_strict+
                 cancellation_policy_strict_14_with_grace_period+
                 cancellation_policy_super_strict_30+            
                 cancellation_policy_super_strict_60 ,trainNN, hidden = 3,act.fct="logistic", linear.output = T,lifesign = "full",stepmax=1e7,threshold=200)

# plot neural network
plot(NN)



summary(NN)


predict_testNN = compute(NN, testNN[,-9])
str(predict_testNN)

#dpredict <- denormalizeData(predict_testNN$net.result, params)
#dtest <- denormalizeData(testNN$price, params)
dpredict <- (predict_testNN$net.result )

#predict_testNN2 = (predict_testNN$net.result * (maxtest- mintest) )+mintest
#testNN2 = (testNN$price * (maxtest- mintest) )+mintest

plot(testNN$price, dpredict, col='blue', pch=16, ylab = "predicted rating NN", xlab = "real rating")

abline(0,1)

# Calculate Root Mean Square Error (RMSE)
RMSE.NN = (sum((testNN$price - dpredict)^2) / nrow(testNN)) ^ 0.5
