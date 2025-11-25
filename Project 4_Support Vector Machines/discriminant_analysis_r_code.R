library(tidyverse)
library(caret)
library(MASS)
library(mlbench)
theme_set(theme_classic())


# Load the data
data(DNA)
head(DNA)
dim(DNA)
# SpDNA# Split the data into training (80%) and test set (20%)
set.seed(123)
training.samples <- DNA$Class %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data <- DNA[training.samples, ]
test.data <- DNA[-training.samples, ]

# Why don't we need to preprocess here??? Normalize or scale?

###
# Linear discriminant analysis - LDA
###

# Fit the model
model <- lda(Class~., data = train.data)
# Make predictions
predictions <- model %>% predict(test.data)
# Model accuracy
mean(predictions$class==test.data$Class)
# dive into the model
model


plot(model)

names(predictions)


# Predicted classes
head(predictions$class, 6)
# Predicted probabilities of class memebership
head(predictions$posterior, 6) 
# Linear discriminants
head(predictions$x, 3) 

lda.data <- cbind(train.data, predict(model)$x)
ggplot(lda.data, aes(LD1, LD2)) +
  geom_point(aes(color = Class))



#############################################################################################
#Quadratic discriminant analysis - QDA
#no assumption that the covariance matrix of the classes is the same
library(MASS)
# Fit the model
model <- qda(Class~., data = train.data)
model
# Make predictions
predictions <- model %>% predict(test.data)
# Model accuracy
mean(predictions$class == test.data$Class)


##############################################################################################
#Mixture discriminant analysis - MDA
# Each class is assumed to be a gaussian mixture of subclasses
library(mda)
# Fit the model
model <- mda(Class~., data = train.data)
model
# Make predictions
predicted.classes <- model %>% predict(test.data)
# Model accuracy
mean(predicted.classes == test.data$Class)


##############################################################################################
#Flexible discriminant analysis - FDA
# nonlinear combinations of predictors is used for boundaries

# Fit the model
model <- fda(Class~., data = train.data)
# Make predictions
predicted.classes <- model %>% predict(test.data)
# Model accuracy
mean(predicted.classes == test.data$Class)
model

###############################################################################################
#Regularized discriminant analysis
# regularization helps when p >> n

library(klaR)
# Fit the model
model <- rda(Class~., data = train.data)
model
# Make predictions
predictions <- model %>% predict(test.data)
# Model accuracy
mean(predictions$class == test.data$Class)

