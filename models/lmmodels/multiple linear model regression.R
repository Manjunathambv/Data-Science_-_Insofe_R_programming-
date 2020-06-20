rm(list = ls())
data = read.csv("housing_data.csv")
dim(data)
names(data)

head(data, 10)
str(data)

attribute_names = colnames(data)
target_names = "MV" 
independent_attributes = setdiff(attribute_names, target_names)

character_names = c("CHAS" , "RAD")
numeric_variable = setdiff(independent_attributes, character_names)

target_names
independent_attributes
numeric_variable
character_names

data[,character_names] = lapply(data[,character_names], as.factor)
str(data)
head(data)

## splitting the data into train and test
library(caret)
set.seed(1234)
splitting = createDataPartition(y = data$MV, p = 0.7, list = FALSE)
head(splitting)
train_data = data[splitting, ]
test_data = data[-splitting, ]

head(train_data)
tail(test_data)

#Data Preprocessing
summary(train_data)
colSums(is.na(train_data))

##missing value is imputed
imputed_model = preProcess(x = train_data, method = "medianImpute")
sum(is.na(train_data))

train_imputed = predict(object = imputed_model, newdata = train_data)
sum(is.na(train_imputed))
colSums(is.na(train_imputed))

test_imputed = predict(object = imputed_model, newdata = test_data)
sum(is.na(test_imputed))
colSums(is.na(test_imputed))

# for categorical data
library(DMwR)
train = centralImputation(train_imputed)
test = centralImputation(test_imputed)
colSums(is.na(train))
colSums(is.na(test))

## relationship between independent and dependent variable
library(corrplot)
corrplot::corrplot(cor(train[,c(numeric_variable,target_names)], use = "complete.obs"), method = "number")


std_model = preProcess(train[, numeric_variable], method = c("center", "scale"))

train[,numeric_variable] = predict(object = std_model, newdata = train[,numeric_variable])
test[,numeric_variable] = predict(object = std_model, newdata = test[,numeric_variable])


mlr_model = lm(formula = MV~., data = train)

summary(mlr_model)
par(mfrow = c(2,2))

plot(mlr_model)

library(MASS)

stepAIC_model = stepAIC(mlr_model, direction = "both")
summary(stepAIC_model)
par(mfrow = c(2,2))

plot(stepAIC_model)

library(car)
vif(mlr_model)


vif(stepAIC_model)

mlr_model_VIF <- lm(formula = MV ~ CRIM + ZN + CHAS + NOX + RM + DIS + RAD + PT + B + LSTAT, data = train)

summary(mlr_model_VIF)
vif(mlr_model_VIF)

train_pred = predict(mlr_model_VIF, train[, independent_attributes])
test_pred  = predict(mlr_model_VIF, test[, independent_attributes])


regr.eval(trues = train$MV, preds =train_pred)

regr.eval(trues = test$MV, preds =test_pred)
