library(tidyverse)
library(caret)
library(MASS)
theme_set(theme_classic())

# Load the data and take a minute to understand it
df <- read.csv("car_silhouette.csv", header = TRUE)
summary(df)
dim(df)
unique(df$class)

# Split the data into training (80%) and test set (20%)
set.seed(123)
training.samples <- df$class %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data <- df[training.samples, ]
test.data <- df[-training.samples, ]

# normalize the data based on the params of the training set
# Estimate preprocessing parameters
preproc.param <- df %>% 
  preProcess(method = c("center", "scale"))
# Transform the data using the estimated parameters
train.transformed <- preproc.param %>% predict(train.data)
test.transformed <- preproc.param %>% predict(test.data)

#check our work (why only use training data?)
apply(train.transformed[1:18], 2, mean)
apply(train.transformed[1:18], 2, sd)

# Fit an LDA model to the training data and predict performance on the test set
# Fit the model
model <- lda(class~., data = train.data)
# Make predictions
predictions <- model %>% predict(test.data)
# Model accuracy
mean(predictions$class==test.data$class)
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
  geom_point(aes(color = class))

# Try another discriminant analysis model and see how well it performs
#Regularized discriminant analysis
# regularization helps when p >> n

library(klaR)
# Fit the model
models <- rda(class~., data = train.data)
models
# Make predictions
predictions <- models %>% predict(test.data)
# Model accuracy
mean(predictions$class == test.data$class)
