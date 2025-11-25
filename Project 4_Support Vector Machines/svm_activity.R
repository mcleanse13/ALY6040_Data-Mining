library(caret)
library(caTools)
library(e1071)
library(dplyr)

# Importing the dataset
dataset = read.csv('star_classification.csv')

head(dataset)
summary(dataset)

#drop columns that aren't numerical or useful (all fields with ID, plate, MJD)
dataset <- select(dataset, -c(dataset$obj_ID, dataset$run_ID, dataset$rerun_ID, dataset$field_ID,
                              dataset$spec_obj_ID, dataset$fiber_ID, dataset$plate,
                              dataset$MJD))
View(dataset)
#train/test split
set.seed(123)
split = sample.split(dataset$class, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#fit an SVM model to training data
classifier = svm(formula = cam_col ~ .,
                 data = dataset,
                 type = 'C-classification',
                 kernel = 'linear')

classifier

plot(classifier, training_set)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])

#accuracy
mean(y_pred == test_set$cam_col)

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)
cm

#take a subsample to tune params
training_set_tuning <- sample_frac(training_set, 0.2)

#try tuning the svm model params to improve performance
tune_classifier = tune.svm(cam_col ~.,
                           data = dataset,
                           type = 'C-classification',
                           kernel = 'linear',
                           cost = c(.01, .1, .5, 1, 2.5, 5, 7.5, 10, 20, 50),
                           scale = FALSE)

tune_classifier

# use those suggested parameters
classifier_2 = svm(formula = as.factor(class) ~ .,
                   data = dataset,
                   type = 'C-classification',
                   kernel = 'linear',
                   cost = ???,
                   gamma = ???)

# Predicting the Test set results


#accuracy


#try it with a different kernel
