#ALY 6040 Module 3 Technique Project
#load libraries and data
require(arules)

#Summary statistics of Netflix dataset
netflix <- read.csv("netflix.csv")
View(netflix)
summary(netflix)
head(netflix)
dim(netflix)

# Read your dataset into a transaction object 
#ChatGpt reference from lines 11 to 25
# Replace "your_dataset.csv" with the filename of your dataset
movies <- read.transactions("netflix.csv", format = "basket", sep = ",", rm.duplicates = TRUE)

# Inspect the transaction object
summary(movies)

# Perform association rule mining
rules <- apriori(movies, parameter = list(support = 0.1, confidence = 0.5))

# Inspect the discovered rules
inspect(rules)

# number of items in each observation
size(head(movies))
LIST(head(movies, 3))

# calculates support for frequent items
frequentItems <- eclat (movies, parameter = list(supp = 0.07, 
                                                    maxlen = 15))
inspect(frequentItems)

# plot frequent items
itemFrequencyPlot(movies, topN=10, type="absolute", 
                  main="Item Frequency")

# Min Support as 0.01, confidence as 0.8.
rules <- apriori (movies, parameter = list(supp = 0.01, 
                                              conf = 0.8))
# 'high-confidence' rules.
rules_conf <- sort (rules, by="confidence", decreasing=TRUE)
# show the support, lift and confidence for all rules
inspect(head(rules_conf)) 

#rules with highest lift
rules_lift <- sort (rules, by="lift", decreasing=TRUE)
# show the support, lift and confidence for all rules
inspect(head(rules_lift)) 

# maxlen = 3 limits the elements in a rule to 3
rules <- apriori(movies, parameter = list (supp = 0.001, 
                                              conf = 0.5, 
                                              maxlen=3))

#remove subset rules
length(rules)
subsetRules <- which(colSums(is.subset(rules, rules)) > 1) # get subset rules in vector
length(subsetRules)
rules <- rules[-subsetRules] # remove subset rules. 
length(rules)

# get rules that watched a production with 'Rajiv Chilaka'
rules <- apriori (data=movies, 
                  parameter=list (supp=0.001,conf = 0.08), 
                  appearance = list (default="lhs",rhs="Rajiv Chilaka"), 
                  control = list (verbose=F))

# 'high-confidence' rules.
rules_conf <- sort (rules, by="lift", decreasing=TRUE)
inspect(head(rules_conf))

# those who watched a production with 'Rajiv Chilaka' also watched...
rules <- apriori (data=movies, parameter=list (supp=0.001,
                                                  conf = 0.15,
                                                  minlen=2), 
                  appearance = list(default="rhs",lhs="Rajiv Chilaka"), 
                  control = list (verbose=F)) 

# 'high-confidence' rules
rules_conf <- sort (rules, by="confidence", decreasing=TRUE)
inspect(head(rules_conf))

#visualize rules
library(arules)
library(arulesViz)
library(RColorBrewer)

rules <- apriori(movies, parameter = list(supp = 0.01, conf = 0.2))

inspect(rules[1:10])

arules::itemFrequencyPlot(movies, topN = 20, 
                          col = brewer.pal(8, 'Pastel2'),
                          main = 'Relative Item Frequency Plot',
                          type = "relative",
                          ylab = "Item Frequency (Relative)")

plot(rules, measure = c("support", "lift"), shading = "confidence")

plot(rules, method = "grouped")
