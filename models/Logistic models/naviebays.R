rm(list = ls())
data = read.csv("FlightDelays.csv")
head(data, 10)
str(data)

summary(data)
sum(is.na(data))

data$Weather <- as.factor(data$Weather)
data$DAY_WEEK <- as.factor(data$DAY_WEEK)

data1 = data
data1$Flight.Status = if_else(data$Flight.Status == 1, "on-time", "delayed")
str(data1)

data1$Flight.Status = as.factor(data1$Flight.Status)

data1$levels = if_else(data1$DEP_TIME >=600 & data$DEP_TIME <= 1200, "level1",
   if_else(data1$DEP_TIME >=1200 & data1$DEP_TIME <= 1800, "level2",
           if_else(data1$DEP_TIME >=1800 & data1$DEP_TIME <= 2100, "level3", "level4"))

summary(data1)
data1$levels = as.factor(data1$levels)   
data1[ , "DEP_TIME"] = NULL

library(caret)

train_rows <- createDataPartition(y=data1$Flight.Status, p=0.7, list=F)
train <- data1[train_rows,]
test <- data1[-train_rows, ]

library(e1071)
model_nb <- naiveBayes(train$Flight.Status~., train)
print(model_nb)

preds <- predict(model_nb, train)
confusionMatrix(data = preds, reference = train$Flight.Status)


preds <- predict(model_nb, test)
confusionMatrix(data = preds, reference = test$Flight.Status)

