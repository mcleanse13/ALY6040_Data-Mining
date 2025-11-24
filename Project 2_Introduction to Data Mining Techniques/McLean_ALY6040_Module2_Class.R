cars <- read.csv(file = "car_evaluation.csv")

str(cars)
table(cars$decision)

nrow(cars) - sum(complete.cases(cars))

cars <- cars[ cars$decision %in% c("unacc", "acc"), ]

table(cars$decision, cars$safety)

set.seed(12345)
train <- sample(1:nrow(cars), size = ceiling(0.80*nrow(cars)), replace = FALSE)
cars_train <- cars[train,]
cars_test <- cars[-train, ]

tree <- rpart(decision~.,
              data=cars_train,
              method = "class")
rpart.plot(tree, nn=TRUE)

pred <- predict(object=tree, cars_test[1:6], type="class")

t <- table(cars_test$decision, pred)
confusionMatrix(t)
matrix(t)

library(tidyverse)
library(caret)
library(randomForest)
library(mlbench)
install.packages("randomForest")
install.packages("mlbench")

rm(list=ls())

data("PimaIndiansDiabetes2", package = "mlbench")

set.seed(12345)
train <- sample(1:nrow(PimaIndiansDiabetes2), size = ceiling(0.80*nrow(PimaIndiansDiabetes2)), replace = FALSE)
train.data <- PimaIndiansDiabetes2[train,]
test.data <- PimaIndiansDiabetes2[-train, ]

model <- train(
  diabetes ~., data=train.data, method = "rf",
  trControl = trainControl("cv", number = 10),
  importance = TRUE
)

glass <- read.csv("random forest activity.csv")
