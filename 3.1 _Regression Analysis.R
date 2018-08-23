#####super quick regression analysis


rm(list=ls())
library(readr)
library('corrplot')


#same dataset
house_train <- read_csv("C:/Users/ykchi/Documents/Workshop_Shared_Part II/Data/Housing Training Set.csv",col_names = TRUE)
house_test <- read_csv("C:/Users/ykchi/Documents/Workshop_Shared_Part II/Data/Housing Testing Set.csv",col_names = TRUE)

#decide what you want to run the regression on

#the lm function
reg1<-lm(SalePrice~LotArea, data= house_train)

summary(reg1)

#salesprice = 158800 + 2.1 (LOT AREA)house_
house_train1= data.frame(house_train$LotArea, house_train$SalePrice)
M <- cor(house_train1)
corrplot(M, method = "circle")

plot(house_train$LotArea, house_train$SalePrice)


#what to analyze, coefficient, R-Squared, P-Value (the stars), F-Stat


####################################################
#predict function



reg1<-lm(SalePrice~LotArea, data= house_train)
prd1<-predict(reg1, data.frame(LotArea=c(1000)), interval="confidence")
prd1





prd2<-predict(reg1, house_test, interval="confidence")
prd2
#############################################################

#find another simple regression
#show!

#drawing conclusions, correlation not causation
#causation needs insights 

reg2<-lm(house_train$SalePrice~house_train$LotArea+(house_train$Fireplaces))
summary(reg2)


#Numeric Fireplace vs factor fireplace.
reg2<-lm(house_train$SalePrice~house_train$LotArea+as.factor(house_train$Fireplaces))

summary(reg2)


#House train?
house$Age<-2017-house$YearBuilt


#Playing around with variables
reg2<-lm(house$SalePrice~house$LotArea+as.factor(house$LotConfig)+house$Age)
         
reg3<-lm(house$SalePrice~house$LotArea+as.factor(house$LotConfig)+house$Age+as.factor(house$RoofStyle)+as.factor(house$CentralAir)+as.factor(house$BedroomAbvGr)+house$Fireplaces+house$YrSold+as.factor(house$MoSold))
summary(reg2)
summary(reg3)

#what's a good r-squared
#try your own!

