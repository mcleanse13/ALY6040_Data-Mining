library(caret)
library(caTools)
library(e1071)

# Importing the dataset
dataset = read.csv('social.csv')

head(dataset)
summary(dataset)

#keep only numerical variables
dataset = dataset[3:5]

#Encoding the target feature as factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))
dataset$Purchased = as.factor(dataset$Purchased)

#plot the data
ggplot(dataset,aes(x=EstimatedSalary,y=Age,color=Purchased)) + 
  geom_point() + theme_bw()

#train/test split
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#Feature Scaling via caTools
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

#fit an SVM model to training data
classifier = svm(formula = Purchased ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')

classifier

plot(classifier, training_set)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])

#accuracy
mean(y_pred == test_set$Purchased)

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)
cm

#try tuning the svm model params to improve performance
tune_classifier = tune.svm(Purchased~.,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear',
                 cost = c(.01, .1, .5, 1, 2.5, 5, 7.5, 10, 20, 50),
                 scale = FALSE)

tune_classifier

# use those suggested parameters
classifier_2 = svm(formula = Purchased ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear',
                 cost = 0.5)

# Predicting the Test set results
y_pred_2 = predict(classifier_2, newdata = test_set[-3])

#accuracy
mean(y_pred_2 == test_set$Purchased)

###
#try a radial kernel
###

tune_classifier = tune.svm(Purchased~.,
                           data = training_set,
                           type = 'C-classification',
                           kernel = 'radial',
                           cost = c(.01, .1, .5, 1, 2.5, 5, 7.5, 10, 20, 50), 
                           gamma = c(0.1,.5, 1,5, 10,20,50,100), 
                           scale = FALSE)

tune_classifier

classifier_r = svm(formula = Purchased ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'radial',
                 gamma = 0.1,
                 cost = 7.5)

plot(classifier_r, training_set)

y_pred_r = predict(classifier_r, newdata = test_set[-3])
mean(y_pred_r == test_set$Purchased)

###
#try a polynomial kernel
###

tune_classifier = tune.svm(Purchased~.,
                           data = training_set,
                           type = 'C-classification',
                           kernel = 'polynomial',
                           cost = c(.01, .1, .5, 1, 2.5, 5), 
                           gamma = c(0.1,.5, 1,5, 10), 
                           scale = FALSE)

tune_classifier

classifier_p = svm(formula = Purchased ~ .,
                   data = training_set,
                   type = 'C-classification',
                   kernel = 'polynomial',
                   gamma = 0.5,
                   cost = 5)

plot(classifier_p, training_set)

y_pred_p = predict(classifier_p, newdata = test_set[-3])
mean(y_pred_p == test_set$Purchased)
