library(datasets)
library(ggplot2)
library(dplyr)
#Import file
boston <- read.csv('boston_housing.csv')

#Summary statistics of the dataset
View(boston)
summary(boston)
head(boston)
tail(boston)
head(boston, 10)
dim(boston)
str(boston)
attributes(boston)
names(boston)
range(boston$crim)

#Statistics using random attributes from dataset
boston[1:5, ]
boston[1:10, "ptratio"]
quantile(boston$age)
quantile(boston$dis, c(0.1, 0.3, 0.65))
var(boston$crim)
mean(boston$tax)
median(boston$lstat)

#Checking for outlier, duplicates, or any missing information in dataset
sum(is.na(boston))
colSums(is.na(boston))
sum(duplicated(boston))

#Covariance and Correlations of each attribute
cov(boston[,1:14])
cor(boston[,1:14])

# Fitting a linear regression model
model <- lm(crim ~ ptratio + b + lstat + medv, data = boston)

# Summarizing the model to view the coefficients and other statistics
summary(model)

#Data visualizations of the attributes with the highest correlations and covariances
#Histogram of the 'tax' attribute
hist(boston$tax)    

#Scatterplots of the attributes with the highest correlations and covariances
plot(density(boston$crim))
plot(boston$tax, boston$rad)
plot(boston$nox, boston$indus)
plot(boston$tax, boston$indus) 
plot(boston$rm, boston$medv) 
plot(boston$rad, boston$crim)
plot(boston$dis, boston$zn)
plot(boston$age, boston$indus)
plot(boston$age, boston$nox)
plot(boston$age, boston$lstat) 
plot(boston$nox, boston$rad)
plot(boston$lstat, boston$indus)
plot(jitter(boston$rm), jitter(boston$medv))

#Table function showing the most common values in the 'nox' attribute
table(boston$nox)

#Pie chart of the 'rad' attribute
pie(table(boston$rad))

#Bar plots of the 'indus' attribute
barplot(table(boston$indus))

#Boxplots of all attributes and the individual attributes with the most outliers
boxplot(boston)
boxplot(boston$tax)
boxplot(boston$b)
boxplot(boston$crim)
boxplot(boston$zn)

#Scatterplot using the smoothed density color representation
smoothScatter(boston$age, boston$nox)

#Scatterplot matrices using combinations of four attributes
pairs(boston[1:4])
pairs(boston[5:8])
pairs(boston[9:12])
pairs(boston[11:14])

#Least squares fit on two attributes
lsfit(boston$age, boston$lstat)$coefficients

#Scatterplot using the ggplot object on the attributes 'rad' and 'tax'
ggplot(boston, aes(x = tax, y = rad)) +
  geom_point(size = 3, alpha = 0.7) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  
  labs(title = "Tax Rates Versus Radial Highway Access",
       x = "Property Tax Rates",
       y = "Radial Highways Access") +
  theme_minimal()

#Scatterplot using the ggplot object on the attributes 'rm' and 'medv'
ggplot(boston, aes(x = rm, y = medv)) +
  geom_point(size = 3, alpha = 0.7) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  
  labs(title = "Price of Homes Per Room Average",
       x = "Average Number of Rooms Per Dwelling",
       y = "Med. Value of Homes") +
  theme_minimal()
