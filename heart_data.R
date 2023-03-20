library(tidyverse)
dataset=read_csv('heart.data.csv')

#Explore the data
head(dataset)
glimpse(dataset)
length(dataset)
summary(dataset)

#Find missing values
colSums(is.na(dataset))

#Impute missing values for each column
ggplot(data=dataset,
       aes(biking))+
  geom_density()

biking_median=median(dataset$biking, na.rm=TRUE)
dataset$biking=ifelse(is.na(dataset$biking),
                     biking_median,
                     dataset$biking)
colSums(is.na(dataset))

ggplot(data=dataset,
       aes(smoking))+
  geom_density()

smoking_median=median(dataset$smoking, na.rm=TRUE)
dataset$smoking=ifelse(is.na(dataset$smoking),
                      smoking_median,
                      dataset$smoking)

ggplot(data=dataset,
       aes(heart.disease))+
  geom_density()

disease_median=median(dataset$heart.disease, na.rm=TRUE)
dataset$heart.disease=ifelse(is.na(dataset$heart.disease),
                      disease_median,
                      dataset$heart.disease)

colSums(is.na(dataset))

#Splitting data
library(caTools)

split=sample.split(dataset$heart.disease, SplitRatio=.8) #80% training, 20% testing

#True for training set and false for test set
training_set=subset(dataset, split=TRUE)
test_set=subset(dataset, split=FALSE)

#Fitting multiple linear regression
MLR=lm(formula=heart.disease~ ., data= training_set)
summary(MLR)
#14.9585-0.2(biking)+0.179(smoking)
#Biking and smoking are stastically significant for heart disease

#Mean square error
summ=summary(MLR)
MSE=(mean(summ$residuals^2))
paste('Mean Square Error:', MSE)

#R-Squared
summary(MLR)

#Testing Set Prediction
y_pred=predict(MLR, newdata=test_set)
data=data.frame(test_set$heart.disease, y_pred)
head(data)

#Validation
new=read_csv("Heart_validation.csv")
new_x=new[c(1:2)]
new_x

data.frame(new[c(3)], predict(MLR, newdata=new_x))
