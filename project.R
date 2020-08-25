library(datasets)

data("longley")

plot(x = longley$Employed,y = longley$GNP)
plot(x = longley$Employed,y = longley$GNP.deflator)
plot(x = longley$Employed,y = longley$Unemployed) 
plot(x = longley$Employed,y = longley$Armed.Forces) 
plot(x = longley$Employed,y = longley$Population)
plot(x = longley$Employed,y = longley$Year)


#taking GNP,GNP.deflator and Population as variables most correlated to Employed

mod1 = lm(Employed ~ GNP,data = longley) #B0 = 51.843,B1 = 0.034
mod2 = lm(Employed ~ GNP.deflator,data = longley) #B0 = 33.189,B1 = 0.316
mod3 = lm(Employed ~ Population,data = longley) #B0 = 8.3807,B1 = 0.484

plot(mod1$residuals)
plot(mod2$residuals)
plot(mod3$residuals)
abline(h = 0, lty = 3)

plot(longley$Employed,mod1$fitted.values)
plot(longley$Employed,mod2$fitted.values)
plot(longley$Employed,mod3$fitted.values)

library(tidyverse)
fit.stats <- as.data.frame(cbind(longley$Employed,mod1$fitted.values)) %>% #swap second column with fitted values from each model
  rename(Actuals = V1, Predicted.Values = V2) %>%
  mutate(error = Actuals - Predicted.Values,
         squared.error = error^2)
SSE <- sum(fit.stats$squared.error)
RMSE <- sqrt(mean(fit.stats$squared.error))

#SSE of 1st model: 6.036 RMSE: 0.614
#SSE of 2nd model: 10.611 RMSE: 0.814
#SSE of 3rd model: 14.365 RMSE: 0.947 

#After checking residual plots,prediction plot and checking SSE and RMSE of all the three models, we can conclude that first model is the best model.

y <- as.matrix(longley$Employed)

X <- as.matrix(cbind(rep(1,length(y)),longley$GNP))

B <- solve(t(X) %*% X) %*% t(X) %*% y #B0 = 51.843,#B1 = 0.034


pred <- longley %>%
  mutate(Intercept = B[1],Employed.pred = Intercept+B[2]*GNP)


plot(longley$Employed,pred$Employed.pred) 

