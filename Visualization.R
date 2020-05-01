rm(list=ls())
setwd("E:\\CUFE CHS Materials\\Senior 2\\Semester 10\\Big data\\Project")

library(plyr)
library(ggplot2)
require(TSPred)

## Let's first read the data 
## ~~~~~~~~~~~~~~~~~~~~~~~~~
data <- read.csv("listings.csv", header=TRUE, sep=",")
head(data)
summary(data)

## Now, let's do some visualisations
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## 1) Let's see the rank of neighborhood groups accoring to the number of listings
## From the graph we can see that "Centro", which is the city center, has way more listings than any other
## neighbourhood. This means that the city center has a lot of offerings, thus you can get 
## a good price, and wide varity of options.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
barplot(head(sort(table(data$neighbourhood_group), decreasing = TRUE), 10), 
        main = "Listings per neighborhood")

# Get the names of five most frequent neighbourhoods
most_freq <- names(head(sort(table(data$neighbourhood_group), decreasing = TRUE), 5))

# Make a cop yof the data

sub_data <- read.csv("listings.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)

# Define not in function

'%!in%' <- function(x,y)!('%in%'(x,y))

# Get indices of listings with neighbourhoods not in the most common ones

idx <- sub_data$neighbourhood_group %!in% most_freq

# Set them to Others

sub_data$neighbourhood_group[idx] <- "Others"

# Plot the pie chart

pie(table(sub_data$neighbourhood_group), labels = paste(round(prop.table(table(sub_data$neighbourhood_group))*100), "% ",
 names(table(sub_data$neighbourhood_group)), sep = ""), main="Percentage of listings for each neighbourhood")


## 2) Number of listings per room type
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## We can notice that entire home/apartments are the majority with 60%, and big share also
## for private rooms. While, shared rooms and hotel rooms have very little share.
##
## This means that if you are looking for an entire home or appartment or a private room,
## to rent in Madrid, Airbnb will be a good place to search in.

pie(table(data$room_type), labels = paste(round(prop.table(table(data$room_type))*100), "% ",
    names(table(data$room_type)), sep = ""), main = "Percentage of listings per room type")


## 3) Lets see how different neighbourhods and room types are distributed together.
## We can see that entire homes/apts and private rooms are most common, while in "Centro"
## the city center, most of listings are entire homes/apts.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Get names of neighbourhoods

x <- names(table(sub_data$neighbourhood_group))

# Prepare the plot place-holders for the graph

Neighbourhoods <- c(rep(x[1], 4), rep(x[2], 4), rep(x[3], 4), rep(x[4], 4),rep(x[5], 4), rep(x[6], 4))

# get room types

Type <- rep(names(table(data$room_type)) , 6)

# Fill the values for each room type for each neighbourhood

values <- c()

for (n in x){
  
  tmp_values <- c()
  
  for (room_type in names(table(data$room_type))){
    
    tmp <- sub_data$neighbourhood_group == n & sub_data$room_type == room_type
    
    tmp_values <- append(tmp_values, length(tmp[tmp==TRUE]))
  
  }
  
  tmp_values = tmp_values / sum(tmp_values)
  values <- append(values, tmp_values)
}

plot_vals <- data.frame(Neighbourhoods, Type, values)


ggplot(plot_vals, aes(fill=Type, y=values, x=Neighbourhoods)) + 
  geom_bar(position="stack", stat="identity")


## 4) Do users rent more than one property ?
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## We want to see the percentage of users who rent more than one property.
## 
## From the pie chart, we can see that 20 % of the users rent more than one 
## place, which is not a small percentage.

host_ids <- table(data$host_id)

percentage <- table(host_ids > 1)

pie(percentage, labels = paste(round(prop.table(percentage)*100), "% ", c("No", "Yes"), sep = ""),  main = "Does the user rent more than one place?")


## 5) Percentage of listings of users who rent more than one place
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## To see the full picture, we want to see the percentage of listings which
## belong to users who rent more than one place.
##
## From the chart, we can see that a surprising fact. Despite a 20% percentage
## of users who have more than one property, they correspond to 55% of all listings.
## This means more than half the listings are of users who rent more than
## one property.

host_ids <- table(data$host_id)

num_multi <- sum(host_ids[host_ids > 1])

num_single <- sum(host_ids[host_ids == 1])

pie(c(num_multi, num_single), labels = paste(round(prop.table(c(num_multi, num_single))*100), "% ",
c("belongs to user renting multiple properties", "belongs to user renting one property"), sep = ""), main = "Percentage of listings per single or multi owners")


## 6) Number of listings for each user
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## To get a more clear picture, lets see the distribution of number
## of owned properties by multi-owners
##
## We can see that half the multi-owners rent two properties, while 
## the other half rent more than two, with around 20% renting five or more

bi_prop <- length(names(host_ids[host_ids == 2]))

tri_prop <- length(names(host_ids[host_ids == 3]))

quad_prop <- length(names(host_ids[host_ids == 4])) 

other_prop <- length(names(host_ids[host_ids >= 5]))

props <- c(bi_prop, tri_prop, quad_prop, other_prop)

pie(props, labels = paste(round(prop.table(props)*100), "% ",
 c(" Two properties", " Three properties", " Four properties", " Five+ properties"), sep = ""),
 main = "Number of properties per multi-owners")


## 7) NUmber of listings per user with 5+ listings
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## let's dig more and see the listings distribution per thos 20%
##
## We can see that many hosts have very large number of properties, 
## and this indicates the insight we have suspected from the begining.
## The insight is that Airbnb is full of agencies and companies renting 
## properties for tourists.

sort(host_ids, decreasing = TRUE)

## 8) Let's see the distribution of prices of listings. 
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## We can see that there are a lot of outliers, so we use data with prices less
## than 300 which seems a resonable price

boxplot(data$price, main="Listings prices")$stats

summary(data$price < 300)

boxplot(subset(data, price <= 300, select=c(price)), main="Listings prices less than 300$")$stats

hist(subset(data, price <= 300, select=c(price))$price, main = "Prices histogram")


## 9) Security deposit
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data <- read.csv("listings_detailed.csv", header=TRUE, sep=",")
barplot(table(as.numeric(gsub('[$,]', '', data$security_deposit))), main="Histogram of security deposit")

## 10) Reviews trend
## ~~~~~~~~~~~~~~~~~
data <- read.csv("reviews.csv", header=TRUE, sep=",")
time_series <- table(data$date)
time_series <- ts(as.data.frame(time_series)[,2], frequency = 365, start = c(2010,1))
par(mfrow=c(1,1))
plot(time_series, xlab="Years", ylab = "Number of reviews", main="Reviews time series")





