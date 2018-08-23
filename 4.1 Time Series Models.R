#Times Series
#clear environment
rm(list=ls())

install.packages('forecast')
install.packages('TTR')
install.packages('corrplot')


#Load Data
#USD to CAD exchange rate from investing.com
library(readr)
library(forecast) 
library(TTR)     
library('corrplot')


#data pulled from investing.com
usdcad<- read_csv("C:/Users/ykchi/Documents/Workshop_Shared_Part II/Data/USD CAD Historical Data.csv")

usdcad
summary(usdcad)
plot(usdcad$Price)
#declare time series
usdcad$Price<-ts(usdcad$Price, start=c(2006,01,01), frequency=52)


####PLEASE DON'T GO TRADE STOCKS WITH THIS EXTREMELY SIMPLIFIED MODEL

#Graph (lower case L for type)
plot(usdcad$Price,main="USD/CAD Exchange Rate", xlab="Date", col = 'blue')



dj_decomp<-decompose(usdcad$Price)
?decompose
plot(dj_decomp)

tsdisplay(usdcad$Price)

#If plotting issues
dev.off()
par(mar = c(0, 0, 0, 0))
#what is ACF and PACF?

#autocorrelation function and partial autocorrelation function
#how to read it?

#Is your data stationary?
#What is a stationary seies?


#Seasonality Decomposition
usdcad_decom<-decompose(usdcad$Price)
usdcad_decom
plot(usdcad_decom)

#same thing with another function (time permitting)
plot(stl(usdcad$Price, s.window = "period"))


seasonplot(usdcad$Price)
plot(usdcad$Price)

#what is the future, how do we know what is going to happen?
#predictable components: last period, growth rate, seasonality
#rando8m/unpredictable components
#basic mean, naive, drift model

#Simple time seires model
#MA

usdcad_sma_10 <- SMA(usdcad$Price, 10)
usdcad_sma_25 <- SMA(usdcad$Price, 25) 
usdcad_sma_50 <- SMA(usdcad$Price, 50) 
plot(usdcad$Price)
plot(usdcad_sma_10, type= 'l')
plot(usdcad_sma_25, type= 'l')
plot(usdcad_sma_50, type= 'l')


#forecasting
usdcad_sma_forecast <- forecast(usdcad_sma_10, 10)
plot(usdcad_sma_forecast)

#which timeline you use matters
usdcad_sma_15 <- SMA(usdcad$Price, 15)   
usdcad_sma_forecast <- forecast(usdcad_sma_15, 16)
plot(usdcad_sma_forecast)

#check, don't copy this!
usdcad_2017 <- read_csv("C:/Users/Eric/Desktop/Data for Class/usdcad_2017.csv")
plot(usdcad_sma_forecast$mean)
par(new=T)
plot(usdcad_2017$Price[1:16], col='red', type = 'l')


#AR Model - Autoregressive Model
#Better for "non stationarity"
#Prediction
#what is autoregressive

usdcad_ar<-ar.ols(usdcad$Price, order.max = 200)
usdcad_ar
summary(usdcad_ar)
usdcad_ar_forecast<-forecast(usdcad_ar, 16)
plot(usdcad_ar_forecast)


#quick check, you don't have this
plot(usdcad_ar_forecast$mean)
par(new=T)
plot(usdcad_2017$Price[1:16], col='red', type = 'l')



#Oil Data
oil_d <-read_csv("C:/Users/Eric/Desktop/Data for Class/Crude Oil WTI Futures Historical Data.csv")
oil_d$Price<-ts(oil_d$Price, start=c(2006,01,01), frequency=52)
plot(oil_d$Price, type = "l")


library(vars)
endo<-data.frame(oil_d$Price, usdcad$Price)
var1<-VAR(y=endo, p= 5)
summary(var1)

var1_predict<-predict(var1, n.ahead=16)
plot(var1_predict$fcst$usdcad.Price[,1], type= 'l')

#check!
par(new=T)
plot(usdcad_2017$Price[1:16], col='red', type = 'l')



#Prediction with seasonality built in ->holt winters or ARIMA with Seasonality



#a realy good guide (http://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html#decomposing-non-seasonal-data)
#another good guide (https://www.r-bloggers.com/time-series-analysis-using-r-forecast-package/)