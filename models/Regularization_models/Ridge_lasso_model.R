data = read.csv("labour_income.csv")

head(data, 10)
str(data)

summary(data)
sum(is.na(data))
colSums(is.na(data))


#split the data into train and testing
library(caret)
set.seed(1234)
splitting_data = createDataPartition(y = data$wages, p = 0.7, list = FALSE)
train_data = data[splitting_data, ]
test_data = data[-splitting_data, ]

std_train_data = preProcess(x = train_data[,c("education", "age", "sex", "language")], method = c("center", "scale"))
train_data1 = predict(object = std_train_data, newdata = train_data)
test_data1 = predict(object = std_train_data, newdata = test_data)

dummy_obj <- dummyVars(~., data = train_data1)
train_data2 = as.data.frame(predict(dummy_obj, train_data1))
str(train_data2)
head(train_data2, 10)
test_data2 = as.data.frame(predict(dummy_obj, test_data1))

X_train <- as.matrix(train_data2[, -1])
dim(X_train)
y_train <- as.matrix(train_data2[, 1])
X_test <- as.matrix(test_data2[, -1])
y_test <- as.matrix(test_data2[, 1])

library(glmnet)
model_lasso = cv.glmnet(x = X_train, y = y_train, nfolds = 4, alpha = 1, type.measure = "mse")
plot(model_lasso)
plot(model_lasso$glmnet.fit, xvar="lambda", label=TRUE)

print(model_lasso$lambda.min)
coef(model_lasso)

model_ridge <- cv.glmnet(X_train, y_train, alpha = 0, type.measure = "mse", nfolds = 4)
plot(model_ridge)
plot(model_ridge$glmnet.fit, xvar="lambda", label=TRUE)

print(model_ridge$lambda.min)
coef(model_ridge)


lasso_model <- glmnet(X_train, y_train, lambda = model_lasso$lambda.min, alpha = 1)
coef(lasso_model)

preds_lasso_test <- predict(model_lasso, X_test)
preds_lasso_train <- predict(model_lasso, X_train)


ridge_model <- glmnet(X_train, y_train, lambda = model_ridge$lambda.min, alpha = 0)
coef(ridge_model)

preds_ridge_test <- predict(ridge_model, X_test)
preds_ridge_train <- predict(ridge_model, X_train)

library(DMwR)
regr.eval(trues = y_test, preds = preds_lasso_test)

