library(forecast)
library(astsa)

#Read the data
stocks <- read.csv("D:/Berkeley/Stat153_TimeSeries/Project/stocks.csv")

#Plotting the given data
plot.ts(stocks$Price,ylab = "Stock Prices", xlab= "Time Period (2015-2019)")

#VST - log transformation
log_stocks = log(stocks$Price)
plot.ts(log_stocks, ylab = "Log (Stock Prices)", xlab= "Time Period (2015-2019)")

#Differencing to see if removing linear trend makes the data stationary
D1X = diff(log_stocks)
plot.ts(D1X)

# ACF/PACF of differenced series
a = acf2(D1X,main="Differenced Series")

#Models Considered 

#Model A: Linear Trend with AR(1)  
time = 1:length(log_stocks)
modelA = lm(log_stocks~time)
residA = modelA$residuals

acf2(residA,max.lag = 50,main = "ACF/PACF for Residuals of Linear Model")

ar1 = arima(residA, order = c(1,0,0), include.mean = FALSE)
abline(a=modelA$coefficients[1],b=modelA$coefficients[2],col='red')
forecasts_modelA= predict(ar1,n.ahead = 10)

new_time = 1195:1204
predictions = modelA$coefficients[1] + modelA$coefficients[2]*new_time + forecasts_modelA$pred
var = forecasts_modelA$se

exp(predictions)
exp(predictions+ 2*var)
exp(predictions- 2*var)

plot(stocks$Price[1000:1194],type='l',xlab='Time Period',ylab='Stock Price',xlim=c(0,204),ylim = c(18,25))
lines(195:204,exp(predictions),col=2)
lines(195:204,exp(predictions + 2*var),lty=2,col=2)
lines(195:204,exp(predictions - 2*var),lty=2,col=2)

#Model B: SARIMA(d=1,D=1,Q=1,S=5)

# Forecasts and Confidence Interval - Model B

modelB = sarima(log_stocks,p=0,d=1,q=0,P=0, D=1, Q=1,S=5)

predictions = sarima.for(log_stocks, n.ahead=10, p=0, d=1, q=0, P=0, D=1, Q=1,
                         S=5)$pred
var = sarima.for(log_stocks, n.ahead=10, p=0, d=1, q=0, P=0, D=1, Q=1,
                 S=5)$se

exp(predictions)
exp(predictions+ 2*var)
exp(predictions- 2*var)

write.csv(exp(predictions),"stocks_3035332938_NA_NA_NA_NA.csv",row.names = FALSE,quote = FALSE)

plot(stocks$Price[1000:1194],type='l',xlab='Time Period',ylab='Stock Price',xlim=c(0,204),ylim = c(18,25))
lines(195:204,exp(predictions),col=2)
lines(195:204,exp(predictions + 2*var),lty=2,col=2)
lines(195:204,exp(predictions - 2*var),lty=2,col=2)

#Cross- Validation - MSE 
mse_log_modelA = list()
mse_log_modelB= list()
mse_modelA=list()
mse_modelB=list()

for (i in seq(1000,1184,10)) {
  
  train_set <- window(log_stocks,end=i)
  test_set <- window(log_stocks,start=i+1,end=i+10)
  
  #AR(1)
  time = 1:length(train_set)
  req_model = lm(train_set~time)
  req_resid = req_model$residuals
  forecast_residual <- sarima.for(req_resid, n.ahead=10, p=1, d=0, q=0)$pred
  
  new_time = (length(train_set)+1):(length(train_set)+10)
  forecast1 = req_model$coefficients[1] + req_model$coefficients[2]*new_time +
    forecast_residual
  
  #SARIMA(0,1,0)(0,1,1)[5]
  forecast2 <- sarima.for(train_set, n.ahead=10, p=0, d=1, q=0, P=0, D=1, Q=1,
                          S=5)$pred
  
  #Mean Squared Error for Logged Prediction
  mse_log_modelA =  c(mse_log_modelA,mean((forecast1 - test_set)^2))
  mse_log_modelB =  c(mse_log_modelB,mean((forecast2 - test_set)^2))
  
  #Mean Squared Error for Actual Data Point Prediction
  mse_modelA =  c(mse_modelA,mean((exp(forecast1) - exp(test_set))^2))
  mse_modelB =  c(mse_modelB,mean((exp(forecast2) - exp(test_set))^2))
  
}

#Log MSE
mean(unlist(mse_log_modelA))
mean(unlist(mse_log_modelB))

#MSE
mean(unlist(mse_modelA))
mean(unlist(mse_modelB))
