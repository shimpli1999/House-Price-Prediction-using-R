#Project1
#Shimpli Borkar
#house price prediction
library(tidyverse)
library(ISLR2)
library(stargazer)
library(caret)
library(leaps)
library(Amelia)
library(ggplot2)
library(lmtest)
library(Hmisc)
library(tseries)
library(caTools)
library(broom)

hodata<-read_csv("C:\\Users\\Lenovo\\Downloads\\house.csv")
summary(hodata)
view(hodata)
head(hodata)
summary(hodata)
dim(hodata)
sapply(hodata,class)
houseframe<- data.frame(hodata)
missmap(hodata,col=c('yellow','black'),y.at=1,y.labels='',legend=TRUE)
colSums(is.na(houseframe))

cor(hodata$medv, hodata$chas)
cor(hodata$medv, hodata$crim)
cor(hodata$medv, hodata$zn)
cor(hodata$medv, hodata$nox)
cor(hodata$medv, hodata$rm)
cor(hodata$medv, hodata$dis)
cor(hodata$medv, hodata$ptratio)
cor(hodata$medv, hodata$lstat)
cor(hodata$medv, hodata$indus)
cor(hodata$medv, hodata$age)
cor(hodata$medv, hodata$tax)
cor(hodata$medv, hodata$rad)
rcorr(as.matrix(houseframe),type='spearman')
#checking normality
hist.data.frame(hodata)
#checking linearity
plot(hodata)

split = sample.split(hodata$medv, SplitRatio = 0.8)
training_set = subset(hodata, split == TRUE)
test_set = subset(hodata, split == FALSE)
view(training_set)
view(test_set)
dim((training_set))
dim(test_set)

largemod<-lm(medv ~ crim+zn+chas+nox+rm+dis+ptratio+lstat+indus+age+tax+rad ,data = training_set)
summary(largemod)
glance(largemod)

largemodResids <- largemod$residuals
largemodFitted <- largemod$fitted.values
plot(largemodFitted,largemodResids)
hist(largemodResids)
qqnorm(largemodResids)

stargazer(largemod, type="text")

subsetmod <- regsubsets(medv ~ crim+zn+chas+nox+rm+dis+ptratio+lstat+indus+age+tax+rad, data = training_set)
summary(subsetmod)
plot(subsetmod, scale = "adjr2" )

reducedmod<- lm(medv ~ crim+zn+chas+nox+rm+dis+ptratio+lstat,data = training_set)
summary(reducedmod)
glance(reducedmod)
anova(reducedmod,largemod)
#pvalue should be less than 0.5 here 

interactionmod<-lm(medv ~ (crim+zn+chas+nox+rm+dis+ptratio+lstat)^2 ,data = training_set)
summary(interactionmod)

redinmodel<-lm(medv ~ crim+zn+chas+nox+rm+dis+ptratio+lstat+rm*ptratio+rm*lstat,data = training_set)
coef(redinmodel)
summary(redinmodel)

anova(redinmodel,interactionmod)

remodResids <- redinmodel$residuals
redmodFitted <- redinmodel$fitted.values
plot(redmodFitted,remodResids)
hist(remodResids)
qqnorm(remodResids)

glance(redinmodel)

largeCVModel <- train(
 form =medv ~ crim+zn+chas+nox+rm+dis+ptratio+lstat+rm*ptratio+rm:lstat,
 data = training_set,
 method = "lm",
 trControl = trainControl(method = "cv", number = 10)
 )
largeCVModel
summary(largeCVModel)

predictionvalue<-predict(largeCVModel,test_set)
plot(predictionvalue,test_set$medv,xlab="Predicted MEDV value", ylab="Actual MEDV Value",col="Blue")
test_set$predictionvalue<-predict(largeCVModel,test_set)
acpred<-data.frame(test_set$medv,test_set$predictionvalue)
names(acpred)<-c("medv","predictionvalue")
correlation_accuracy<-cor(acpred)
correlation_accuracy
predict(largeCVModel, test_set, interval="predict") 
