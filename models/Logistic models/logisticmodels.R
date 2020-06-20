rm(list = ls())
data = read.table(file = "bank.txt", header = TRUE, sep = ";")
head(data)
summary(data)
str(data)


sum(is.na(data))
colSums(is.na(data))

library(caret)
set.seed(1234)
spliiting = createDataPartition(y = data$y, p = 0.7, list = FALSE)
train_data = data[spliiting, ]
test_data = data[-spliiting, ]


log_model = glm(y~., data = train_data, family = binomial)
summary(log_model)

log_odds_train = predict(log_model, newdata = train_data)
prob_train = predict(log_model, type = "response")


print(log_odds_train[1:6])
print(prob_train[1:6])

library(ROCR)
pred = prediction(predictions = prob_train, labels = train_data$y)
head(pred)

perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, colorize = TRUE, print.cutoffs.at = seq(0,1,0.05))

perf_auc = performance(pred, measure = "auc")
class(perf_auc)

auc = perf_auc@y.values[[1]]
print(auc)


prob_test = predict(log_model, test_data, type = "response")
preds_test = ifelse(prob_test > 0.1, "yes", "no")

test_data_labs = test_data$y
conf_matrix = table(test_data_labs, preds_test)
print(conf_matrix)

library('e1071')
confusionMatrix(as.factor(preds_test), test_data$y, positive = "yes")


confusionMatrix(as.factor(preds_test), test_data$y, positive = "no")

