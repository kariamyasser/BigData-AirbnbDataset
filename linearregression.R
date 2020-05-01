#clean environment 
rm(list=ls())

#display work items
ls()
#get working directory
getwd()
#set working directory
setwd("D:\\Bigdata\\project\\madrid-airbnb-data (1)")



##########################################################################################


library(fastDummies)

dfm <- read.csv("listings_detailed.csv")
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

#traindata <- data.frame(nobed=dfm$beds,nobedrooms=dfm$bedrooms,bedtype=dfm$bed_type,state=dfm$city,propertytype=dfm$property_type,roomtype=dfm$room_type,reviewscore=dfm$review_scores_rating)
pricedf <-data.frame(price=dfm$price)

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
table(is.na(data$price))
table(is.na(data2$price))
data<-merge(data, data2)
data<-na.omit(data)


##########################################################################################


#data<-na.omit(data)

table(is.na(data))
str(data)
data$price = as.numeric(gsub("\\$", "", data$price))
data<- data[data$price<200,]

data<-dummy_cols(data,remove_first_dummy=TRUE,remove_selected_columns=TRUE)



data<-na.omit(data)
samplesize = 0.9 * nrow(data)
set.seed(80)
str(data)

index = sample( seq_len ( nrow ( data ) ), size = samplesize )

# Create training and test set
datatrain = data[ index, ]
datatest = data[ -index, ]
str(datatrain)
#+dfm$bedrooms+dfm$bed_type+dfm$city+dfm$property_type+dfm$room_type+dfm$review_scores_rating
model <- lm(datatrain$price~
              datatrain$host_is_superhost+
              datatrain$host_identity_verified+
              #datatrain$neighbourhood+
              datatrain$zipcode+
              datatrain$accommodates+
              #datatrain$roomtype+
              datatrain$availability_365+
              datatrain$reviews_per_month+
              datatrain$calculated_host_listings_count+
              datatrain$longitude+
              datatrain$latitude+
              datatrain$guests_included+
              datatrain$minimum_nights+
              datatrain$maximum_nights+
              datatrain$review_scores_rating+
              datatrain$review_scores_accuracy+
              datatrain$review_scores_cleanliness+
              datatrain$review_scores_checkin+
              datatrain$review_scores_communication+
              datatrain$review_scores_location+
              datatrain$review_scores_value+
              datatrain$instant_bookable_t+
              datatrain$cancellation_policy_moderate+
              datatrain$cancellation_policy_strict+
              datatrain$cancellation_policy_strict_14_with_grace_period+
              datatrain$cancellation_policy_super_strict_30+            
              datatrain$cancellation_policy_super_strict_60 
)


summary(model)
#options(na.action="na.pass", na.detail.response=TRUE)
datatrain=datatest
ypred <- predict(model,datatrain)
ytrue=datatrain$price
par(mfrow=c(1,1))
plot(ytrue, ytrue, type="l", xlab="true y", ylab="predicted y")
points(ytrue, ypred)

# Calculate Root Mean Square Error (RMSE)

RMSE.NN = (sum((ypred - ytrue)^2) / nrow(datatrain)) ^ 0.5
