#Same dataset!

#cluster analysis
#Neural Network
library(readr)
library(neuralnet)
library(clue)

house <- read_csv("C:/Users/Eric/Desktop/Data for Class/Housing Training Set.csv")
house$age<-2017-house$YearBuilt

  reg2<-lm(house$SalePrice~house$LotArea+as.factor(house$LotConfig)+house$age+as.factor(house$RoofStyle)+as.factor(house$CentralAir)+as.factor(house$BedroomAbvGr)+house$Fireplaces+house$YrSold+as.factor(house$MoSold)+house$OverallQual)
  
library(MASS)
new_reg2 <- stepAIC(reg2, direction="both")
summary(new_reg2)

#NN 
house$age<-scale(2017-house$YearBuilt, center = T, scale = T)

house$SalePrice<-scale(house$SalePrice, center = TRUE, scale = TRUE)
house$LotArea<-scale(house$LotArea, center = TRUE, scale = TRUE)

nn <- neuralnet(SalePrice~LotArea+age,data=house,hidden=2, act.fct = 'logistic')
plot(nn)

#how about 3 hidden layer


#it's not as crazy as you think it is to use!
#the background coding is though 
#check source code and essay accompanying

