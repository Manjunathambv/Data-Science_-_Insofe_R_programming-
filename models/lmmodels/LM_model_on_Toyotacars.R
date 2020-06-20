rm(list = ls())
data = read.csv("Toyota_SimpleReg.csv", header = TRUE)

head(data)

data1 = data[,c("Price", "Age_06_15")]
head(data1)
names(data1)[names(data1) == "Age_06_15"] = "Age"
head(data1)
summary()

data2 = data[,c("Price", "Age_06_15")]
head(data2)

library(dplyr)
data2 = dplyr::rename(.data = data2, "Age" = "Age_06_15")
head(data2)

summary(data2)
colSums((is.na(data2)))

plot(x = data2$Age, y = data2$Price, type = "p")

cov(data2)
cor(data2)

library(ggplot2)
ggplot2::ggplot(data2) + aes(x = Age, y = Price) + geom_point()
## split the data into train and test
sample_data = sample(2, nrow(data2), replace = TRUE, prob = c(0.8, 0.2))
traindata = data2[sample_data == 1, ]
testdata = data2[sample_data == 2, ]

LinReg = lm(Price ~ Age, data = traindata)
coefficients(LinReg)
summary(LinReg)

plot(data2$Age,data2$Price,
     xlab="Age of the Car",
     ylab="Price in ($)",main="Car Price Vs. Age: Best fit line", 
     col= "blue")
abline(LinReg,col="red",lwd=1)


head(LinReg$residuals)

# To extract the predictions
head(LinReg$fitted.values)

par(mfrow = c(2,2))           
plot(LinReg) 


train_prediction = predict(LinReg, traindata)  # Fitted values
train_actual = traindata$Price 


test_prediction = predict(LinReg, testdata)  # Fitted values
test_actual = testdata$Price                 # Actual values


library(DMwR)
#Error verification on train data
regr.eval(train_actual, train_prediction)


#Error verification on test data
regr.eval(test_actual, test_prediction)


Conf_Pred = data.frame(predict(LinReg, 
                               testdata, 
                               interval="confidence",level=0.95))

Pred_Pred = data.frame(predict(LinReg, 
                               testdata, 
                               interval="prediction",level=0.95))

names(Conf_Pred)
head(Conf_Pred)


plot(testdata$Age, testdata$Price,
     main = "Price and Age, with Regression Line and Intervals",
     xlab = "Age (Years)", ylab = "Price  ($)", 
     col = 'brown')
points(testdata$Age, Conf_Pred$fit, type="l", col="green", lwd=2)
points(testdata$Age, Conf_Pred$lwr, pch="-", col="red", lwd=4)
points(testdata$Age, Conf_Pred$upr, pch="-", col="red", lwd=4)
points(testdata$Age, Pred_Pred$lwr, pch="-", col="blue", lwd=4)
points(testdata$Age, Pred_Pred$upr, pch="-", col="blue", lwd=4)

grid(10,10,lwd =1)

