library(datasets)
data("iris")

head(iris)
tail(iris)
head(iris, 10)

iris[1:6, ] #slice data the following way
dim(iris) #check dimensionality
#functions that return the structure and attributes to data
str(iris)
attributes(iris)

#A random sample of the data can be retrieved with function sample()
idx <- sample(1:nrow(iris), 5)
idx
iris[idx, ]

iris[1:10, "Sepal.Length"]

iris[1:10, 1]
summary(iris)

quantile(iris$Sepal.Length)
quantile(iris$Sepal.Length, c(0.1, 0.3, 0.65))

var(iris$Petal.Length)
mean(iris$Sepal.Length)
median(iris$Sepal.Length)

hist(iris$Sepal.Length)      
plot(density(iris$Sepal.Length))
table(iris$Species)
pie(table(iris$Species))
barplot(table(iris$Sepal.Length))

cov(iris$Sepal.Length, iris$Petal.Length)
cov(iris[,1:4])

cor(iris$Sepal.Length, iris$Sepal.Length)
cor(iris[,1:4])

aggregate(Sepal.Length ~ Species, summary, data = iris)
boxplot(Sepal.Length ~ Species, data=iris, xlab = "Species", ylab = "Sepal Length")
pairs(iris[1:4])

