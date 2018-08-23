#Quick Review on Statistics
rm(list = ls())

#Data
#types of files-Excel, CSV
#SQL Querries 
install.packages('readr')
library(readr)


### a lot of times, finding the data can be the hardest part####

#load the file

house<-read_csv("C:/Users/Eric/Desktop/Data for Class/Housing Training Set.csv")



View(house)


#data manipulation
head(house)
tail(house)

house$SalePrice


house[10,5]

house[9,]
house$LotArea[10]

#from (https://www.kaggle.com/c/house-prices-advanced-regression-techniques)

#what's in the data file?
#good practice is to have a data description file
summary(house)
colnames(house)
#give this a quick read


####################################################################


#$$$$$$$$$$$$$$$$$$$$$$$$$$ what does that mean?
house$LotArea[50]

#any predictions before we plot them?

#Is there a relationship between sales price and lot area. 
plot(house_cleaned$LotArea,house_clean$SalePrice)

#some parameters 
plot(house_cleaned$LotArea,house_clean$SalePrice, main = "sales price vs lot area", col = 'red')








cor(house$LotArea,house$SalePrice) 


     

plot(house$LotArea, house$SalePrice, data=house[which(house$YrSold!="2010"),])

#analyze your own graph and show the class! 
#Group Activities: explain to your partner what you did and what (business) implications you can draw from it






#what is correlation
#on the board!
#find two variables that you think may correlate and show the class!


cor(house$SalePrice, house$LotArea)

#what else do you want to look at?


#extra
#corrgram
#install.packages('corrgram')
library(corrgram)
house_corr<-data.frame(house$LotArea, house$SalePrice, house$OverallQual, house$Fireplaces, house$BedroomAbvGr, house$YrSold)
corrgram(house_corr)



#Taking out the outliers
house_cleaned<-house[which(house$LotArea<50000),]
plot( house_cleaned$LotArea, house_cleaned$SalePrice)
cor(house_cleaned$SalePrice, house_cleaned$LotArea)



#exxtra
#################################################################
#dealing with NAs
#house_cleaned<-na.omit(house_cleaned)

#other plots
plot(house_cleaned$LotArea,house_cleaned$SalePrice)
plot(house_cleaned$LotArea,house_cleaned$SalePrice, col=house_cleaned$BedroomAbvGr)
plot(house_cleaned$BedroomAbvGr,house_cleaned$SalePrice)
cor(house_cleaned$LotArea,house_cleaned$SalePrice)










#####creatnig a new variable
house_cleaned$Age<-
##how to do it???


house_cleaned$Age<-2018-house_cleaned$YearBuilt

house_cleaned$Age[1]
house_cleaned$YearBuilt[1]



#any predictions?
plot(house_cleaned$Age,house_cleaned$SalePrice)
house_cleaned$Age<-2017-house_cleaned$YearBuilt



#Averages
mean(house$SalePrice)
median(house$SalePrice)
mode(house$SalePrice)



?mode
#did mode work?

#from the web
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}





getmode(house$SalePrice)

#min or max
min(house$SalePrice)
max(house$SalePrice)


#standard deviation...what is it?? Very important!
sd(house$SalePrice)





##### a little bit more advanced
install.packages('psych')
library(psych)



#check out what it is online, the pdf!
#check out the describe function
#?describe

describe(house)
#give this a quick read

#check out description of this function



#understanding a package using CRAN
#looking at a function



