rm(list=ls())

dating<-read_csv("C:/Users/Eric/Desktop/Data for Class/Speed Dating Data _ Subset.csv")
summary(dating)

dating<-na.omit(dating)

## 75% of the sample size
smp_size <- floor(0.75 * nrow(dating))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(dating)), size = smp_size)

train <- dating[train_ind, ]
test <- dating[-train_ind, ]
####

table(train$dec_o)
table(test$dec_o)
train$field_cd<-as.factor(train$field_cd)
test$field_cd<-as.factor(test$field_cd)

#fit logit model
logit1<-glm(dec_o~int_corr+attr_o, data= train, family= binomial(logit)  )
summary(logit1)



#try your own!
logit2<-glm(dec_o~int_corr+samerace+field_cd+attr_o, data= train, family= binomial(logit)  )
summary(logit1)

#with everything
logit_everything<-glm(dec_o~., data= train, family= binomial(logit)  )


#prediction
predicted<-plogis(predict(logit1, test))
predicted  

test$prob<-predicted
  
#Accuracy
test$prob<-ifelse(test$prob>0.5, 1, 0)
table(test$dec_o, test$prob)
nrow(test)
(838+591)/2095

#reference
  #http://r-statistics.co/Logistic-Regression-With-R.html


#quick nn example
#remember how to install packages?
#install.packages('neuralnet)
library(neuralnet)
nn_1 <- neuralnet(dec_o~int_corr+attr_o,data=train, hidden=3, act.fct = 'logistic')
plot(nn_1)

#prediction
predicted_nn<-compute(nn_1, test[,c(2,4)])
#watch the compute function, might be overwritten  
predicted_nn 
plot(predicted_nn$net.result)

test$prob_nn<-predicted_nn$net.result

#Accuracy
test$prob_nn<-ifelse(test$prob_nn>0.5, 1, 0)
table(test$dec_o, test$prob_nn)
nrow(test)
(393+350)/1029

