rm(list=ls())
getwd()

library(dplyr)
library(tidyr)
library(randomForest)


## load data 
features <- read.csv('features.csv')
str(features)

stores <- read.csv('stores.csv')
str(stores)

train <- read.csv('train.csv')
str(train)

test <- read.csv('test.csv')
str(test)


## join "features" and "stores" to "train" and "test"
train <- train %>%
  left_join(features, by = c("Store","Date")) %>%
  left_join(stores, by = "Store") %>%
  data.frame


## join "features" and "stores" to "train" and "test"
test <- test %>%
  left_join(features, by = c("Store","Date")) %>%
  left_join(stores, by = "Store") %>%
  data.frame

## Create a new columns on test table
test$Weekly_Sales <- NA


## Combine test and train data for data cleaning
data <- rbind(train, test)


## Wrangling. Create new variables, "quarter", "store_type" and "holiday"
data <- data %>%
  mutate(month = substr(Date, 6, 7),
         day = substr(Date, 9, 10),
         quarter = ifelse(month == '01'| month == '02' | month == '03', 1,
                          ifelse(month == '04'| month == '05' | month == '06', 2,
                                 ifelse(month == '07'| month == '08' | month == '09', 3,
                                        ifelse(month == '10'| month == '11' | month == '12', 4, month)))),
         store_type = ifelse(Type == 'A', 1,
                             ifelse(Type == 'B', 2, Type)),
         holiday = ifelse(IsHoliday.x == 'FALSE', 0,
                          ifelse(IsHoliday.x == 'TRUE', 1, IsHoliday.x)),
         Store = as.factor(Store),
         month = as.factor(month),
         quarter = as.factor(quarter))


## Get test and train data
train.clean <- data %>% filter( !is.na(Weekly_Sales) ) %>% filter(Date > '2010-12-31')
test.clean <- data %>% filter( is.na(Weekly_Sales) )

## Set up train and validation data

samplesize <- sample(nrow(train.clean), nrow(train.clean)*0.8)

Train <- train.clean[samplesize,]
validation <- train.clean[-samplesize,]

## Modeling
lm.fit <- lm(Weekly_Sales~ Store + Dept + holiday + quarter,data = Train)
summary(lm.fit)

## Predict validation data
validation$prediction <- predict(lm.fit, newdata = validation)

## Measure the sum of squared errors and mean absolute error between predicted sales and actual sales
mean(abs(validation$prediction - validation$Weekly_Sales))


## After deciding certain critical variables, using all train data to build model.
lm.fit.all <- lm(Weekly_Sales~ Store + Dept + holiday + quarter,data = train.clean)
summary(lm.fit.all)

## Predict test data
test$Weekly_Sales <- predict(lm.fit.all, newdata = test.clean)

## Create a file for submission
submission <- test[,c('Id', 'Weekly_Sales')]
write.csv(submission, file = 'submission.csv', quote=FALSE, row.names=FALSE)



## random forest
set.seed(71)

## Train model
random.all = randomForest(Weekly_Sales~Store + Dept + holiday + quarter + Temperature 
                          + Fuel_Price + CPI + Unemployment + store_type
                          , data = train.clean
                          , importance=TRUE,proximity=TRUE)

## To see which variables are important
improtance.variable = round(importance(random.all),2)
improtance.variable


## Predict test data
test$Weekly_Sales <- predict(random.all, newdata = test.clean)

## Create a file for submission
submission <- test[,c('Id', 'Weekly_Sales')]
write.csv(submission, file = 'submission.csv', quote=FALSE, row.names=FALSE)

