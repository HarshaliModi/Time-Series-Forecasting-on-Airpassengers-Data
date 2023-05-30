#Importing the dataset
getwd()
setwd('C:\\Users\\Harshali\\Documents\\Datasets')
tsdata<- read.csv("AirPassengers.csv", header = TRUE)
View(tsdatats)

#Converting data frame into time series class
class(tsdata)
tsdata<-ts(tsdata$X.Passengers, start = 1949, end = 1960, frequency = 12)
class(tsdata)
plot(tsdata, Xlab = "Time", ylab = "Number of Passengers", main = "AirPassenger Time Series")

#Required to predict future values
install.packages("forecast")
library(forecast)
#Required for performing Auto Correlation Function, Partial Auto Correlation Function, Augmented Dickey Fuller Test as well as ARIMA function 
install.packages("tseries")
library(tseries)

#Checking whether the data is stationary or not
acf(tsdata)
pacf(tsdata)
adf.test(tsdata)

#Performing to make the data stationary
statts<-auto.arima(tsdata, ic="aic", trace = TRUE)
statts

#Checking acf() and pacf() after performing auto arima function
acf(ts(statts$residuals))
pacf(ts(statts$residuals))

#Predicting future values on stationary data
forecastts<-forecast(statts, level = c(95), h=5*12)
forecastts
plot(forecastts)

#Validating the model
Box.test(forecastts$residuals, lag = 5, type = "Ljung-Box")
