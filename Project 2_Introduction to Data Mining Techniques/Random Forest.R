library(tidyverse)
library(caret)
library(randomForest)
library(mlbench)

rm(list=ls())

# Load the data and remove NAs
data("PimaIndiansDiabetes2", package = "mlbench")
PimaIndiansDiabetes2 <- na.omit(PimaIndiansDiabetes2)
# Inspect the data
sample_n(PimaIndiansDiabetes2, 3)

# Split the data into training and test set
set.seed(123)
training.samples <- PimaIndiansDiabetes2$diabetes %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- PimaIndiansDiabetes2[training.samples, ]
test.data <- PimaIndiansDiabetes2[-training.samples, ]

# Fit the model on the training set
model <- train(
  diabetes ~., data = train.data, method = "rf",
  trControl = trainControl("cv", number = 10),
  importance = TRUE
)

# Best tuning parameter
model$bestTune

# Final model
model$finalModel

# Make predictions on the test data
predicted.classes <- model %>% predict(test.data)
head(predicted.classes)

# Compute model accuracy rate
mean(predicted.classes == test.data$diabetes)

importance(model$finalModel)

# Plot MeanDecreaseAccuracy
varImpPlot(model$finalModel, type = 1)
# Plot MeanDecreaseGini
varImpPlot(model$finalModel, type = 2)

varImp(model)

models <- list()
for (nodesize in c(1, 2, 4, 8)) {
  set.seed(123)
  model <- train(
    diabetes~., data = na.omit(PimaIndiansDiabetes2), method="rf", 
    trControl = trainControl(method="cv", number=10),
    metric = "Accuracy",
    nodesize = nodesize
  )
  model.name <- toString(nodesize)
  models[[model.name]] <- model
}
# Compare results
resamples(models) %>% summary(metric = "Accuracy")




###
# example of using RF on continuous output
###

# Load the data
data("Boston", package = "MASS")
# Inspect the data
sample_n(Boston, 3)
# Split the data into training and test set
set.seed(123)
training.samples <- Boston$medv %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- Boston[training.samples, ]
test.data <- Boston[-training.samples, ]

# Fit the model on the training set
set.seed(123)
model <- train(
  medv ~., data = train.data, method = "rf",
  trControl = trainControl("cv", number = 10)
)
# Best tuning parameter mtry
model$bestTune
# Make predictions on the test data
predictions <- model %>% predict(test.data)
head(predictions)
# Compute the average prediction error RMSE
RMSE(predictions, test.data$medv)

