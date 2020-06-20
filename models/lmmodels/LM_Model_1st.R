rm(list = ls())
data = read.csv("BigMac-NetHourlyWage.csv", header = TRUE)
head(data)

library(ggplot2)
ggplot2::ggplot(data = data) + aes(BigMacPriceDollars, NetHourlyWageDollars)+
  geom_point() + theme_bw()

plot(x = data$BigMacPriceDollars, y = data$NetHourlyWageDollars )

cov(data$NetHourlyWageDollars, data$BigMacPriceDollars)
cor(data$NetHourlyWageDollars, data$BigMacPriceDollars)


summary(data)
str(data)

data1 = data[,c("BigMacPriceDollars", "NetHourlyWageDollars")]
head(data1)

sml_model = lm(formula = data$NetHourlyWageDollars~. ,data = data1)
summary(sml_model)

predict(sml_model$residuals)

plot(data1$BigMacPriceDollars, data1$NetHourlyWageDollars, 
     col = "brown", lwd = 2, 
     xlab="BigMacPriceDollars", ylab="NetHourlyWageDollars",
     main="NetHourlyWageDollars vs BigMacPriceDollars: Best fit line")
abline(sml_model, col="blue", lty=1, lwd=2);
grid(10, 10, lwd=1)

plot(sml_model$residuals, ylab="Residuals", 
     main="Residuals", col = 'brown', lwd = 2)

plot(sml_model$fitted.values,
     sml_model$residuals,
     main = "Residual vs Predicted values",
     col = 'brown',lwd = 2,
     xlab ="Predicted Values / Fitted Values", ylab = "Residuals")
abline(h = 0,col = 'blue',lwd  =2)
grid(10,10,lwd=1)

par(mfrow = c(2,2)) # par helps us set graphical parameters, refer to help for more info on this
plot(sml_model)
