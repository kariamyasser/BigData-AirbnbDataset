#In this tutorial we are going to examine a data series about monthly car sales 
require(forecast)
require(TSPred)
library(plyr)

#clean environment 
rm(list=ls())
#display work items
ls()
#get working directory
getwd()
#set working directory
setwd("D:\\MY DATA\\CUFE-CHS 2020\\cce\\Senior 2\\2nd semester\\Big Data\\Project")


#Import the time series data in 'tractor-sales.csv' file. 
data <- read.table("calendar.csv", header=TRUE, sep=",")
data <- data[c("date","price")]

#REMOVE THE DOLLAR SIGN
data$price <-as.numeric(sub('[$]', '', data$price))
summary(data)

#REMOVE NA's
clean_data<-data[!(is.na(data$price) | data$price==""), ]
summary(clean_data)

#get mean for each day

mean_data <- ddply(clean_data, .(date), function(x) mean(x$price) )
summary(mean_data)

#ORDER DATA
#ordered_data <- mean_data[order(as.Date(mean_data$date, format="%Y-%m-%d")),]

#CREATE THE TIME SERIES
#get the start date index
start <- as.numeric(format(as.Date("2019-09-19"), "%j"))
series <- ts(mean_data[,2], frequency=365, start=c(2019, start)) 
series

summary(series)
#(3) Visualise the time series. 
par(mfrow=c(1,1))
plot(series, xlab="Date", ylab = "Prices")

#(6)
plot(diff(series), ylab="Differenced Prices")


#(8)
plot(diff(log10(series)),ylab="Differenced Log (prices)")


#(10) Fitting an ARIMA Model 
ARIMAfit <- auto.arima(log10(series), approximation=FALSE, trace=TRUE)
summary(ARIMAfit)

#(11) Use the ARIMA model to forecast values.
pred <- predict(ARIMAfit, n.ahead = 30)
summary(series)


#(13) library TSPred
plotarimapred(log10(series), ARIMAfit, xlim=c(2019.7,2021), range.percent = 0.2)

#(14) Forecast for a longer range. 
plotarimapred(log10(series), ARIMAfit, xlim=c(2019.7,2021.7), range.percent = 0.2)
