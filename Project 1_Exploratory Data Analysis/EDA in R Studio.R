rm(list = ls())

#The iris dataset has been used for classification in many research publications. 
#It consists of 50 samples from each of three classes of iris 
#flowers [Frank and Asuncion, 2010]. One class is linearly separable from the other
#two, while the latter are not linearly separable from each other.

#load the library containing example datasets
library(datasets)
#load the iris dataset
data(iris)

#The first or last rows of data can be retrieved with 
#head() or tail(), which by default return the first or last 6 rows. 
#Alternatively, we can get a certain number of rows by setting the 2nd
#parameter to both functions.

head(iris)
tail(iris)
head(iris,10)

#You can also slice data the following way.
#Note that the first row is 1, not 0.
iris[1:6, ]

#check dimensionality
dim(iris)
#Check the name of variables
names(iris)

#Functions str() and attributes() return the structure and attributes of data.
str(iris)
attributes(iris)


#A random sample of the data can be retrieved with function sample().
#In the code below, we draw a sample of 5 rows.
idx <- sample(1:nrow(iris), 5)
idx
iris[idx, ]

#We can also retrieve the values of a single column. 
#For example, the first 10 values of Sepal.Length can be obtained in 
#three different ways below.
iris[1:10, "Sepal.Length"]

iris[1:10, 1]

iris$Sepal.Length[1:10]


###################################################################
#Distribution of every numeric variable can be checked with function summary(), 
#which returns the minimum, maximum, mean, median, and the first (25%) and 
#third (75%) quartiles. Take Sepal.Length as an example, the result below shows that,
#its minimum value is 4.3 and the maximum 7.9. Its first quartile 
#(\1st Qu.") is 5.1, which means that 25% out of all records have Sepal.Length 
#below 5.1. Similarly, a value of 6.4 in the third quartile (\3rd Qu.") 
#indidates that 75% out of all records have Sepal.Length below 6.4. 
#It has a median of 5.8, which means that half of records have Sepal.Length 
#below 5.8. The value of mean shows that the arithemetic mean (calculated by 
#adding all values together and dividing by the number of values) of Sepal.Length
#is 5.843.
###################################################################

#For factors (or categorical variables), it shows the frequency of every level. 
#In the result below, we can see that each one of the three Species, 
#\setosa", \versicolor" and \virginica", has 50 observations.


summary(iris)


#The mean, median and range can also be obtained respectively with functions with 
#mean()
#median()
#range()
#Quartiles and percentiles are supported by function 
#quantile() 

quantile(iris$Sepal.Length)
quantile(iris$Sepal.Length, c(0.1, 0.3, 0.65))

var(iris$Sepal.Length)   #variance of sepal length
mean(iris$Sepal.Length)  #mean of sepal length
median(iris$Sepal.Length) #median of sepal length
range(iris$Sepal.Length)  #range of sepal length

#Base R can do simple plotting like this for EDA
hist(iris$Sepal.Length) #histogram of sepal length
plot(density(iris$Sepal.Length)) #density of sepal length
table(iris$Species) #table of iris
pie(table(iris$Species)) #pie chart
barplot(table(iris$Species)) #barplot


#After checking the distributions of individual variables, we then investigate 
#the relationships between two variables. Below we calculate covariance and 
#correlation between variables with 
#cov()
#and 
#cor()

cov(iris$Sepal.Length, iris$Petal.Length)
cov(iris[,1:4])

cor(iris$Sepal.Length, iris$Petal.Length)
cor(iris[,1:4])



###########################################################################
#Correlation shows whether and how strongly a pair of variables are related 
#to each other. It ranges from -1 to +1. The closer the correlation is to +1 
#(or -1), the more strongly the two variables are positively (or negatively) 
#related. When it is close to zero, it means that there is no relationship
#between them. From above results, we can see that the correlation between 
#Sepal.Length and Petal.Length is 0.87, which means that they are positively 
#related to each other. Similarly, Sepal.Length and Petal.Width are highly 
#related, as well as Petal.Length and Petal.Width.
#In contrast, Sepal.Width is weakly negatively related with the other three
###########################################################################

#Next, we compute the stats of Sepal.Length of every Species with aggregate()

aggregate(Sepal.Length ~ Species, summary, data=iris)

######################################################################################
#We then use function boxplot() to plot a box plot, also known as box-and-whisker plot, to
#show the median, first and third quartile of a distribution 
#(i.e., the 50%, 25% and 75% points in cumulative distribution), and outliers. 
#The bar in the middle is the median. The box shows the interquartile range (IQR), 
#which is the range between the 75% and 25% observation. The result shows that 
#the three species are of dierent distributions in their Sepal.Length. 
#\Virginica" tends to have large Sepal.Length, \setosa" has small Sepal.Length 
#and \versicolor" sits in between. It suggests that the varialbe can be used 
#to predict the species of flowers.
################################################################################

boxplot(Sepal.Length ~ Species, data=iris, xlab="Species", ylab="Sepal.Length")

#A scatter plot can be drawn for two numeric variables with plot() as below. 
#Using function with(), we don't need to add 
#\iris$" before variable names. In the code below, the colors (col)
#and symbols (pch) of points are set to Species

with(iris, plot(Sepal.Length, Sepal.Width, col=Species, pch=as.numeric(Species)))

## same function as above
plot(iris$Sepal.Length, iris$Sepal.Width, col=iris$Species, pch=as.numeric(iris$Species))


#When there are many points, some of them may overlap. We can use jitter() 
#to add a small amount of noise to the data before plotting.

plot(jitter(iris$Sepal.Length), jitter(iris$Sepal.Width))


#A smooth scatter plot can be plotted with function smoothScatter(), which 
#a smoothed color density representation of the scatterplot, obtained through 
#a kernel density estimate.

smoothScatter(iris$Sepal.Length, iris$Sepal.Width)

#A matrix of scatter plots can be produced with function pairs(), where each 
#sub figure is the scatter plot of a pair of variables.
pairs(iris[1:4])


#####################################################################
#You will have noticed from the plot above that petal length and petal width
#are highly correlated over all species. How about running a linear regression? 
#First of all, using the "least squares fit" function lsfit gives this

lsfit(iris$Petal.Length, iris$Petal.Width)$coefficients

#The function lsfit is a bit of a "one trick pony" and its a lot more flexible 
#to use a linear model instead (function lm). For this example you get exactly 
#the same thing when we model petal width depending on petal length 
#(written as Petal.Width ~ Petal.Length:

lm(Petal.Width ~ Petal.Length, data=iris)$coefficients
plot(iris$Petal.Length, iris$Petal.Width, pch=21, bg=c("red","green3","blue")
     [unclass(iris$Species)], main="Iris Data", xlab="Petal length", 
     ylab="Petal width")
abline(lm(Petal.Width ~ Petal.Length, data=iris)$coefficients, col="black")

plot(iris$Sepal.Width, iris$Sepal.Length, pch=21, bg=c("red","green3","blue")
     [unclass(iris$Species)], main="Iris Data Sepal.Length ~ Sepal.Width", 
     xlab="Sepal Width", 
     ylab="Sepal Length")
abline(lm(Sepal.Length ~ Sepal.Width, data=iris)$coefficients, col="black")


#You get more than just that with a linear model:

summary(lm(Petal.Width ~ Petal.Length, data=iris))

#Looking at those p-values, this simple model seems to fit the data very well

#The main point about using a linear model is we can consider more complicated 
#examples. What about the sepal length as a function of the sepal width?

plot(iris$Petal.Length, iris$Petal.Width, pch=21, bg=c("red","green3","blue")
     [unclass(iris$Species)], main="Iris Data Petal.Width ~ Petal.Length", xlab="Petal length", 
     ylab="Petal width")
abline(lsfit(iris$Petal.Length, iris$Petal.Width)$coefficients, col="black")

#It very clear that the linear model Sepal.Length ~ Sepal.Width (black line) 
#is not doing a very good job, even without looking at the statistics
#Look at those lousy p-values:

summary(lm(Sepal.Length ~ Sepal.Width, data=iris))


#What happens if we divide the data up by species, and run three separate linear 
#regressions?
plot(iris$Sepal.Width, iris$Sepal.Length, pch=21, bg=c("red","green3","blue")
     [unclass(iris$Species)], main="Iris Data", xlab="Petal length", ylab="Sepal length")
abline(lm(Sepal.Length ~ Sepal.Width, data=iris)$coefficients, col="black")
abline(lm(Sepal.Length ~ Sepal.Width, data=iris[which(iris$Species=="setosa"),])
       $coefficients, col="red")
abline(lm(Sepal.Length ~ Sepal.Width, data=iris[which(iris$Species=="versicolor"),])
       $coefficients, col="green3")
abline(lm(Sepal.Length ~ Sepal.Width, data=iris[which(iris$Species=="virginica"),])
       $coefficients, col="blue")

lm(Sepal.Length ~ Sepal.Width, data=iris[which(iris$Species=="setosa"),])$coefficients

lm(Sepal.Length ~ Sepal.Width, data=iris[which(iris$Species=="versicolor"),])$coefficients

lm(Sepal.Length ~ Sepal.Width, data=iris[which(iris$Species=="virginica"),])$coefficients

#The equivalent linear model would be something like Sepal.Length ~ Petal.Length:
#Species + Species - 1, which gives identical coefficients 

lm(Sepal.Length ~ Sepal.Width:Species + Species - 1, data=iris)$coefficients

#What are these new terms? Because Species is a categorical input variable 
#(a factor in R's terminology) it can't be used directly in a linear model 
#as they need actual numbers (a linear model is basically a matrix equation). 
#So, the following "dummy variables" have been invented for each data point 
#(which are just numbers)

#Speciessetosa = 1 if Species is "setosa", 0 otherwise
#Speciesversicolor = 1 if Species is "versicolor", 0 otherwise
#Speciesvirginica = 1 if Species is "virginica", 0 otherwise
#Sepal.Width:Speciessetosa = Sepal.Width if Species is "setosa", 0 otherwise
#Sepal.Width:Speciesversicolor = Sepal.Width if Species is "versicolor", 0 otherwise
#Sepal.Width:Speciesvirginica = Sepal.Width if Species is "virginica", 0 otherwise
#Using the summary command on the linear model object gives:

summary(lm(Sepal.Length ~ Sepal.Width:Species + Species - 1, data=iris))

#Just look at those p-values! Every single term has an excellent p-value, 
#as does the model as a whole. And the residual standard error has also been halved.

#In this case, the Sepal.Length ~ Sepal.Width:Species + Species - 1 model is 
#clearly much better than just Sepal.Length ~ Sepal.Width.

#On the other hand, what about this choice instead: 
#Sepal.Length ~ Sepal.Width + Species. In fact, this is what the AIC 
#(Akaike Information Criterion) step function gives you if you start with all 
#possible interactions between sepal width and species, which is written 
#Sepal.Length ~ Sepal.Width * Species (using a asterix instead of a plus or colon) 
#in R:

summary(step(lm(Sepal.Length ~ Sepal.Width * Species, data=iris)))


#How can we interpret this? Recall we did three species specific regressions? 
#If you imagine applying a shift of 1.4587 to the Iris versicolor (green) 
#points, and a shift of 1.9468 to the Iris virginica (blue) points then their 
#individual best fit lines would more of less agree (with a gradient of say 0.8036):

#Or, to put that another way, this model predicts that 
#Sepal.Length = (0.8036 * Sepal.Width) + 2.2514 plus an additional 1.4587 
#if the species is Iris versicolor or 1.9468 if the species is Iris virginica.

#Note that using summary(step(lm(Sepal.Length ~ Sepal.Width * Species - 1, data=iris)))
#gives an equivalent model, but like the case discussed below would use a dummy 
#variable for each of the three species, rather than an intercept term and two 
#dummy variables.
#See help(lm), help(step) for more information, and perhaps also help(glm) too
#Note on Intercept Coefficients
#Recall that I just introduced a model of the form 
#Sepal.Length ~ Sepal.Width:Species + Species - 1, which gave identical 
#coefficients to those found doing species specific regressions:

lm(Sepal.Length ~ Sepal.Width:Species + Species - 1, data=iris)$coefficients

#The use of the "- 1" in the model above told R not to automatically include 
#a default intercept term. The alternative is the following:

lm(Sepal.Length ~ Sepal.Width:Species + Species, data=iris)$coefficients

#This time the dummy variables are:

#(Intercept) = 1, regardless of Species
#Speciesversicolor = 1 if Species is "versicolor", 0 otherwise
#Speciesvirginica = 1 if Species is "virginica", 0 otherwise
#Sepal.Width:Speciessetosa as before
#Sepal.Width:Speciesversicolor as before
#Sepal.Width:Speciesvirginica as before
#Notice that the new (Intercept) term and the old Speciessetosa have the 
#same coefficient, 2.6390012. Similarly note that for Speciesversicolor 
#the old value of 3.5397347 equals 2.6390012 + 0.9007335 (the intercept plus 
#the new coefficient), and likewise for Speciesvirginica, the old value of 
#3.9068365 equals 2.6390012 + 1.2678352. This all makes sense - these are 
#basically equivalent models but with different dummy variables. Anyway, I found 
#the first case slightly easier to explain.


