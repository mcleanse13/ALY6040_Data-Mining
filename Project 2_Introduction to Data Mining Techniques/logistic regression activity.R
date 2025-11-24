install.packages('mlbench')
install.packages('MASS')
install.packages('pROC')
library(mlbench)
library(MASS)
library(pROC)

#load some UCLA admittance data
df <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
str(df)

#1.) Visualize and summarize the data to better understand it.

summary(df)
sum(is.na(df))
plot(df)
boxplot(df)

#2.) Look at a contingency table for admit versus rank.

Contable = table(df$admit, df$rank)
print(Contable)

#3.) Turn rank into a factor instead of a numerical value (hint: use as.factor)



#4.) Fit a logistic regression model on admit.
# Splitting dataset
split <- sample.split(df, SplitRatio = 0.8)
split

train_reg <- subset(mtcars, split == "TRUE")
test_reg <- subset(mtcars, split == "FALSE")

# Training model
logistic_model <- glm(vs ~ wt + disp,
                      data = train_reg,
                      family = "binomial")
logistic_model

# Summary
summary(logistic_model)
logit <- glm(
summary(logit)


#5.) What is the probability that Dave will get into UCLA? His data is provided below.
dave <- data.frame(gre=790,gpa=3.8,rank=as.factor(1))
predict(logit,dave, type = 'response')



