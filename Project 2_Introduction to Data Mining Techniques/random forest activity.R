###
# Module 2 in-class activity
###

#Using the following data and code, answer these questions:

#1.) What is the accuracy for predicting each of the three species? 0.625

#2.) What are the most important features? specificity, neg pred value, 

#3.) What happens if you reduce or increase the number of trees used by randomForest?

rm(list=ls())

library(tidyverse)
library(caret)
library(randomForest)
library(mlbench)

# Loading data
data(Glass)

# Structure and brief EDA 
str(Glass)
head(Glass)
dim(Glass)
View(Glass)

# Split the data into training and test set
set.seed(123)
training.samples <- Glass$Type %>% 
  createDataPartition(p = 0.8, list = FALSE)
train  <- Glass[training.samples, ]
test <- Glass[-training.samples, ]

# Fitting Random Forest to the train dataset
classifier_RF = randomForest(x = train[1:9],
                             y = train$Type,
                             ntree = 20)

classifier_RF

# Predicting the Test set results
y_pred = predict(classifier_RF, newdata = test[1:9])

# Confusion Matrix
confusion_mtx = table(test$Type, y_pred)
confusion_mtx

# A more detailed confusion matrix via "caret"
cf <- caret::confusionMatrix(data=y_pred,
                             reference=test$Type)
print(cf)

# Plotting model
plot(classifier_RF)

# Importance plot
importance(classifier_RF)

# Variable importance plot
varImpPlot(classifier_RF)
