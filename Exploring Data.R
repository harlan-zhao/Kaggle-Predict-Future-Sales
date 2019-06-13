# Exploring and Cleaning Data
train <- read.csv("sales_train_v2.csv")
items <- read.csv("items.csv")
library(dplyr)
merged.train <- merge.data.frame(train, items, by = c("item_id"))
merged.train$item_name <- NULL
head(merged.train)
str(merged.train)
rm(train)
rm(items)
train <- as.data.frame(merged.train)
library(lubridate)
train$date <- dmy(train$date)
str(train)

# Feature Engineering
train$year <- year(train$date)
train$month <- month(train$date)
train$day <- day(train$date)
train$weekday <- weekdays(train$date)
train$year <- as.factor(train$year)
train$weekday <- as.factor(train$weekday)
