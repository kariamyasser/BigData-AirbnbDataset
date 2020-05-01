#clean environment 
rm(list=ls())

#display work items
ls()
#get working directory
getwd()
#set working directory
setwd("D:\\Bigdata\\project\\madrid-airbnb-data (1)")



#install.packages("randomForest")
#install.packages("caTools")
#install.packages("anchors")
library(randomForest)
require(caTools)

library(fastDummies)


dfm <- read.csv("listings_detailed.csv", header=TRUE, sep=",")

#replace.value( data, data$roomtype, from="Private room", to="Privateroom", verbose = FALSE)
check<-dfm$room_type=="Private room"


dfm2<-read.csv("listings.csv")

data2<- data.frame(
  Id=dfm2$id,
  price=dfm2$price,
  latitude=dfm2$latitude,
  longitude=dfm2$longitude,
  minimum_nights=dfm$minimum_nights,
  calculated_host_listings_count=dfm2$calculated_host_listings_count,
  reviews_per_month=dfm2$reviews_per_month,
  availability_365=dfm2$availability_365
  
)

data <- data.frame(
  Id=dfm$id,
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
  #price=dfm$price,
  guests_included=dfm$guests_included,
  #minimum_nights=dfm$minimum_nights,
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
data<-merge(data, data2)
data$price = as.numeric(gsub("\\$", "", data$price))
data<-dummy_cols(data,remove_first_dummy=TRUE,remove_selected_columns=TRUE)
boxplot(data$price)
data<- data[data$price<200,]


str(data)

data<-na.omit(data)


max = apply(data , 2 , max)
min = apply(data, 2 , min)
#scaled = as.data.frame(scale(data, center = min, scale = max - min))
scaled=data
samp <- sample(1:nrow(scaled), size=round(0.7*nrow(scaled)), replace=FALSE)

train <- scaled[samp,]  #Only takes rows that are in samp
test <- scaled[-samp,] #Omits the rows that were in samp
train<-na.omit(train)
test<-na.omit(test)
test$price
dim(train)
dim(test)
rf <- randomForest(
  price ~ host_is_superhost+
    host_identity_verified+
    #neighbourhood+
    zipcode+
    accommodates+
    #roomtype+
    availability_365+
    reviews_per_month+
    calculated_host_listings_count+
    longitude+
    latitude+
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
    cancellation_policy_super_strict_60 ,
  data=train
)

summary(rf)
pred = predict(rf, newdata=test)
sum(((test$price - pred)>50)==TRUE)




# Calculate Root Mean Square Error (RMSE)
RMSE.NN = (sum((test$price - pred)^2) / nrow(test)) ^0.5
ypred <- pred
ytrue=test$price
par(mfrow=c(1,1))
plot(ytrue, ytrue, type="l", xlab="true y", ylab="predicted y")
points(ytrue, ypred)


