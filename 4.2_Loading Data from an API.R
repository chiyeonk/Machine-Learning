#looking up data from quandl

#rm(list = ls())
library(Quandl)
library(ggplot2)

Quandl.api_key("Gx-7xmJmNay_idJZhc7D")


startdate <- "2013-1-1"
enddate <- "2016-10-31"


#calling data
skew <- Quandl("CBOE/SKEW", start_date = startdate, end_date = enddate, type= 'xts')
plot(skew)



housingmarket <- Quandl("YALE/RBCI")
summary(housingmarket)
plot(housingmarket)

#plotting correlation plot, not working yet
#library(corrplot)