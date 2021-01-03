#bibliotecas necessarias para o projeto 
require(tseries)
require(FitAR)
require(forecast)
require(FitAR)

#Accidental Deaths in the US 1973-1978
#A time series giving the monthly totals of accidental deaths in the USA. 
#The values for the first six months of 1979 are 7798 7406 8363 8460 9217 9316.
USAccDeaths
usaTimes<-ts(USAccDeaths, frequency = 12, start = c(1973,1))
plot.ts(usaTimes)
compUsaTime.ts = decompose(usaTimes, type="mult")
plot(compUsaTime.ts)


usa.Trend <- compUsaTime.ts$trend 
usa.Seasonal <- compUsaTime.ts$seasonal
usa.Random <- compUsaTime.ts$random


x = usaTimes- compUsaTime.ts$seasonal
usaTimes_stationary <- diff(x, differences=1)
plot(usaTimes_stationary)

layout(1:2)
acf(usaTimes_stationary,lag.max = 40)
pacf(usaTimes_stationary,lag.max = 40)


fitARIMA = arima(usaTimes, order=c(1,1,1),seasonal= list(order = c(1,0,0), period = 12),method="ML")
fitARIMA

res=fitARIMA$residuals
plot(res)

layout(1:2)
acf(res,lag.max = 40)
pacf(res,lag.max = 40)

Box.test(res,type="Ljung-Box")

model=auto.arima(usaTimes, trace=TRUE)
model

plot(model$residuals)

Box.test(model$residuals,type="Ljung-Box")


predicted_values = forecast(model,h=100, level=c(99.5))
plot(predicted_values)
