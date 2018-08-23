#Cluster Analysis
rm(list=ls())

#Data Load
library(readr)
library(clue)
library(ggplot2)

house_train <- read_csv("C:/Users/Eric/Dropbox/AAARL_Shared/Workshop/Workshop Curriculum/Part III_ Tools in Machine Learning/Data for Part III/Housing Training Set.csv")
house_test<-read_csv("C:/Users/Eric/Dropbox/AAARL_Shared/Workshop/Workshop Curriculum/Part III_ Tools in Machine Learning/Data for Part III/Housing Testing Set.csv")

#age variable
house_train$Age<-2017-house_train$YearBuilt
house_test$Age<-2017-house_test$YearBuilt

#Scatter Plot
plot(house_train$SalePrice, house_train$LotArea)





#variables you want to cluster
housecluster<-na.omit(data.frame(house_train$`1stFlrSF`, house_train$SalePrice, house_train$Age, house_train$Fireplaces, house_train$BedroomAbvGr))

housecluster.test<-na.omit(data.frame(house_test$`1stFlrSF`, house_test$Age, house_test$Fireplaces, house_test$BedroomAbvGr))



#Simple k-means Cluster
cluster1<-kmeans(housecluster, centers = 4, algorithm = 'Lloyd', iter.max = 200)
cluster1

plot(housecluster$house_train.SalePrice, housecluster$house_train.Age,col=cluster1$cluster)


#Optimal number of clusters
library(fpc)
ncluster<-pamk(housecluster,krange=1:10,criterion="ch",critout=TRUE)
#the higher Calinski & Harabasz (CH) value, the better
#there are many scores and indices to determine what is the best number of clusters...read up on the documentation to decide which is most appropriate!

ncluster

cluster2<-kmeans(housecluster, centers = 8, algorithm = 'Lloyd', iter.max = 200)
cluster2

#cluster visualization
#original
plot(housecluster$house_train.SalePrice, housecluster$house_train.Age)

#cluster one
plot(housecluster$house_train.SalePrice, housecluster$house_train.Age,col=cluster1$cluster)

#cluster two
plot(housecluster$house_train.SalePrice, housecluster$house_train.Age,col=cluster2$cluster)

x#cluster one prediction
predc<-cl_predict(cluster1, newdata=housecluster.test)

predc

ggplot(housecluster, aes(housecluster$house_train..1stFlrSF., housecluster$house_train.Age, color = cluster1$cluster)) + geom_point() + xlab('Square Footage of 1st Floor')+ylab('Age')+ggtitle('Housing Cluster')  

#cluster two prediction
predc<-cl_predict(cluster2, newdata=housecluster.test)
ggplot(housecluster, aes(housecluster$house_train..1stFlrSF., housecluster$house_train.Age, color = cluster2$cluster)) + geom_point() + xlab('Square Footage of 1st Floor')+ylab('Age')+ggtitle('Housing Cluster')  


#what kinds of insights could you derive from this?
#understand the 
#neighbourhoods?
#Recommendations?

#automatic grouping of houses and send to different real estate agents. 
#similar clusters for recommendations




