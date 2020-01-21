#remove all the objects stored
rm(list=ls())

#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

lapply(x, require, character.only = TRUE)

rm(x)

#set current working directory
setwd("/DataScience/Project")

bike_data = read.csv("day.csv")

##sapply(bike_data, class)

# Missing value analysis
#Check for missing value
Null_Count =  sum(is.na(bike_data))


#Outlier Analysis
boxplot(bike_data$weathersit)
boxplot(bike_data$temp)
boxplot(bike_data$hum)
boxplot(bike_data$windspeed)

new_Data = subset(bike_data, select = c(temp,hum, windspeed) )

cnames = colnames(new_Data)

#create NA on Outliers
for(i in cnames){
  val = new_Data[,i][new_Data[,i] %in% boxplot.stats(new_Data[,i])$out]
  print(length(val))
  new_Data[,i][new_Data[,i] %in% val] = NA
}

bike_data$hum = new_Data$hum
bike_data$windspeed = new_Data$windspeed

Null_Count =  sum(is.na(bike_data))

#Remove rows containing NA
##bike_data = bike_data[complete.cases(bike_data),]

# 1) Remove NA with median
bike_data$windspeed[is.na(bike_data$windspeed)] = median(bike_data$windspeed, na.rm = T)
bike_data$hum[is.na(bike_data$hum)] = median(bike_data$hum, na.rm = T)

#Feature engineering
#Conver data types
bike_data$season <- as.factor(bike_data$season)
bike_data$yr <- as.factor(bike_data$yr)
bike_data$mnth <- as.factor(bike_data$mnth)
bike_data$weekday <- as.factor(bike_data$weekday)
bike_data$weathersit <- as.factor(bike_data$weathersit)
library(anytime)
bike_data$dteday <- anydate(bike_data$dteday)

# Feature selection
new_Data = subset(bike_data, select = c(windspeed,hum,temp,atemp,casual,registered))
library(corrgram)
corrgram(new_Data, order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

#Remove variables
Bike_Clean_Data = subset(bike_data, select = -c(instant,atemp, holiday, workingday, casual, registered))

#Model development

# Set train and test data
train_index = sample(1:nrow(Bike_Clean_Data), 0.8 * nrow(Bike_Clean_Data))
train = Bike_Clean_Data[train_index,]
test = Bike_Clean_Data[-train_index,]

# 1) Decision Tree
#Load Libraries
library(rpart)
library(MASS)
fit = rpart(cnt ~ ., data = train, method = "anova")

summary(fit)

#Predict for new test cases
predictions_DT = predict(fit, test[,-10])

regr.eval(test[,10], predictions_DT, stats = c('mae', 'rmse', 'mape', 'mse'))

#MAPE
#calculate MAPE
MAPE = function(y, yhat){
  #mean(abs((y - yhat)/y))
  mean(abs((y -yhat))/y)
}

MAPE(test[,10], predictions_DT)

write.csv(train,"train.csv")
write.csv(test,"test.csv")

#2) linear regression

library(sp)
library(raster)
library(usdm)

lm_model = lm(cnt ~.,data = train)
summary(lm_model)
Prediction_LR = predict(lm_model, test[,1:9])

MAPE(test[,10], Prediction_LR)

# 3) Random forest 
library(randomForest)

#random forest
rf <- randomForest(cnt ~ .,data=train, importance=TRUE, ntree=1000)
pred = predict(rf, newdata=test[-10])

regr.eval(test[,10], pred, stats = c('mae', 'rmse', 'mape', 'mse'))
MAPE(test[,10], pred)
