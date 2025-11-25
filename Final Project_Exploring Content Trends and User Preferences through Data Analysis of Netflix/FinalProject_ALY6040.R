#ALY 6040 Final Project
#Milestone 1: EDA
# Load required libraries
library(tidyverse)
library(lubridate)
library(dplyr)

# Load the dataset
netflix_df <- read.csv("netflix.csv")

# Display the structure of the dataset
str(netflix_df)

# Display the first few rows of the dataset
head(netflix_df)

# Summary statistics
summary(netflix_df)
View(netflix_df)

# Data Preprocessing
# Convert 'type' and 'rating' to factor
netflix_df$type <- as.factor(netflix_df$type)
netflix_df$rating <- as.factor(netflix_df$rating)

# Convert 'date_added' to Date type
netflix_df$date_added <- as.Date(netflix_df$date_added, format = "%B %d, %Y")

# Handle missing values
# Check for missing values
colSums(is.na(netflix_df))

#Remove rows that don't contain a rating
netflix <- netflix[!(is.na(netflix$rating) | netflix$rating == "" | grepl("\\d+ min", netflix$rating)), ]
View(netflix)

# Since 'director' and 'cast' have missing values and it's difficult to impute, we'll replace missing with 'Unknown'
netflix$director[is.na(netflix$director)] <- "Unknown"
netflix$cast[is.na(netflix$cast)] <- "Unknown"

# Exploratory Data Analysis (EDA)
# Distribution of content type (Movie/TV Show)
ggplot(netflix, aes(x = type, fill = type)) +
  geom_bar() +
  labs(title = "Distribution of Content Type on Netflix",
       x = "Type",
       y = "Count")

# Calculate frequency of each rating category (Code from ChatGPT)
rating_counts <- table(netflix$rating)

# Convert rating to factor and reorder levels by count in descending order (Code from ChatGPT)
netflix$rating <- factor(netflix$rating, levels = names(sort(rating_counts, decreasing = TRUE)))

# Distribution of ratings
ggplot(netflix, aes(x = rating, fill = rating)) +
  geom_bar() +
  labs(title = "Distribution of Ratings on Netflix",
       x = "Rating",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Release Year distribution
ggplot(netflix, aes(x = release_year)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Release Year Distribution",
       x = "Release Year",
       y = "Count") +
  theme_minimal()

# Convert 'duration' to numeric
netflix$duration <- as.numeric(gsub(" min", "", netflix$duration))

# Runtime distribution
ggplot(netflix, aes(x = duration)) +
  geom_histogram(binwidth = 10, fill = "green", color = "black") +
  labs(title = "Runtime Distribution",
       x = "Runtime (minutes)",
       y = "Count") +
  theme_minimal()

# Top 10 directors with the most content
top_directors <- netflix %>%
  filter(director != "Unknown") %>%
  count(director, sort = TRUE) %>%
  slice_max(n = 10, order_by = n)

ggplot(top_directors, aes(x = reorder(director, n), y = n)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  labs(title = "Top 10 Directors with Most Content",
       x = "Director",
       y = "Count") +
  theme_minimal()

# Top 10 genres
genres <- netflix %>%
  separate_rows(listed_in, sep = ", ") %>%
  count(listed_in, sort = TRUE) %>%
  slice_max(n = 10, order_by = n)

ggplot(genres, aes(x = reorder(listed_in, n), y = n)) +
  geom_bar(stat = "identity", fill = "purple") +
  coord_flip() +
  labs(title = "Top 10 Genres",
       x = "Genre",
       y = "Count") +
  theme_minimal()

# Save the cleaned dataset
write.csv(netflix, "cleaned_netflix.csv", row.names = FALSE)

#Module 3: Data Mining Techniques
#Clustering - Catherine Smereena Dommaty
# Load required packages
library(factoextra)
library(NbClust)
library(dplyr)

# Load the dataset
netflix <- read.csv("netflix.csv")

# Data Preprocessing
# Convert 'type' and 'rating' to factor
netflix$type <- as.factor(netflix$type)
netflix$rating <- as.factor(netflix$rating)

# Convert 'date_added' to Date type
netflix$date_added <- as.Date(netflix$date_added, format = "%B %d, %Y")

# Handle missing values
netflix$director[is.na(netflix$director)] <- "Unknown"
netflix$cast[is.na(netflix$cast)] <- "Unknown"

# Filter out missing values and convert to numeric
netflix <- netflix %>%
  filter(!is.na(release_year) & !is.na(duration)) %>%
  mutate(release_year = as.numeric(release_year),
         duration = as.numeric(gsub(" min", "", duration)))

# Standardize the data
scaled_data <- scale(netflix[, c("release_year", "duration")])
# Check for missing or infinite values in the scaled data
any(is.na(scaled_data))
any(is.infinite(scaled_data))

# If there are missing or infinite values, remove them before visualizing
scaled_data <- na.omit(scaled_data)

# Elbow method
fviz_nbclust(scaled_data, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(scaled_data, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

# Gap statistic
fviz_nbclust(scaled_data, kmeans, method = "gap_stat") +
  labs(subtitle = "Gap statistic method")
# Standardize the data
scaled_data <- scale(netflix[, c("release_year", "duration")])

# D index
set.seed(123)  # for reproducibility
res.dindex <- NbClust(scaled_data, distance = "euclidean", min.nc = 2, max.nc = 5, method = "Dindex")
fviz_nbclust(res.dindex)

# Visualize the D index
fviz_nbclust(res.dindex)

# Extract the coordinates of the final centers
round(res.dindex$All.index, digits = 2)

# Extract the coordinates of the final centers
round(res.hubert$centers, digits = 2)

# Hierarchical clustering
# Single linkage
hclust_single <- hclust(dist(scaled_data), method = "single")
plot(hclust_single, main = "Hierarchical clustering: single linkage")

# Complete linkage
hclust_complete <- hclust(dist(scaled_data), method = "complete")
plot(hclust_complete, main = "Hierarchical clustering: complete linkage")

# Average linkage
hclust_average <- hclust(dist(scaled_data), method = "average")
plot(hclust_average, main = "Hierarchical clustering: average linkage")

library(readr)
netflix <- read_csv("netflix.csv")
View(netflix)

# Load necessary libraries
library(MASS)  # for lda
library(caret) # for train-test split and accuracy calculation
library(ggplot2)
library(lattice)
library(dplyr) # for data manipulation

# Encode the 'rating' variable
netflix$rating_encoded <- as.numeric(factor(netflix$rating))

# Select and prepare features
netflix <- na.omit(netflix) # Remove rows with NA values
features <- netflix[, c("release_year", "rating_encoded")]

# Prepare target variable
target <- netflix$type

# Split data into training and test sets
set.seed(42)  # for reproducibility
train_indices <- createDataPartition(target, p = 0.7, list = FALSE)
train_data <- features[train_indices, ]
train_target <- target[train_indices]
test_data <- features[-train_indices, ]
test_target <- target[-train_indices]

# Fit LDA model
lda_model <- lda(train_target ~ ., data = data.frame(train_data, train_target))

# Predict on test data
predictions <- predict(lda_model, test_data)
predicted_classes <- predictions$class

# Calculate accuracy
accuracy <- sum(predicted_classes == test_target) / length(test_target)
print(paste("Accuracy:", accuracy))

# Load the necessary library
library(tidyverse)

# Convert 'rating' from categorical to numerical code
netflix <- netflix %>%
  mutate(rating_code = as.numeric(as.factor(rating)))

# Check for NA values and remove rows containing them
netflix <- drop_na(netflix, c(release_year, rating_code))

# Run a linear regression model
model <- lm(release_year ~ rating_code, data = netflix)

# Summary of the model
summary(model)

# Plotting
ggplot(netflix, aes(x = rating_code, y = release_year)) +
  geom_point(aes(color = rating), alpha = 0.5) +  # Points with different colors for each rating
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Regression line with standard error
  labs(title = "Relationship between Release Year and Rating Code",
       x = "Rating Code",
       y = "Release Year") +
  theme_minimal()  # A clean theme for the plot

# Creating a boxplot
ggplot(netflix, aes(x = rating, y = release_year, fill = rating)) +
  geom_boxplot() +  # Boxplot for each rating category
  labs(title = "Boxplot of Release Years by Rating",
       x = "Rating",
       y = "Release Year") +
  theme_minimal() +  # A clean theme for the plot
  scale_fill_brewer(palette = "Pastel1")  # Color palette for different ratings

#Association Mining - Sean McLean
#load libraries and data
install.packages("arulesViz")
library(arulesViz)
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
rules <- apriori(movies, parameter = list(support = 0.08, confidence = 0.4))

# Inspect the discovered rules
summary(rules)
inspect(rules)
plot(rules)

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

# get rules that watched a production with a "TV-MA" rating
rules <- apriori (data=movies, 
                  parameter=list (supp=0.001,conf = 0.05), 
                  appearance = list (default="lhs",rhs="TV-MA"), 
                  control = list (verbose=F))

# 'high-confidence' rules.
rules_conf <- sort (rules, by="lift", decreasing=TRUE)
inspect(head(rules_conf))

# those who watched a production with a "TV-MA" rating also watched...
rules <- apriori (data=movies, parameter=list (supp=0.001,
                                               conf = 0.15,
                                               minlen=2), 
                  appearance = list(default="rhs",lhs="TV-MA"), 
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

#Classification - Susheel Reddy
library(readr)
netflix <- read_csv("netflix.csv")
View(netflix)

# Load necessary libraries
library(MASS)  # for lda
library(caret) # for train-test split and accuracy calculation
library(dplyr) # for data manipulation

# Encode the 'rating' variable
netflix$rating_encoded <- as.numeric(factor(netflix$rating))

# Select and prepare features
netflix <- na.omit(netflix) # Remove rows with NA values
features <- netflix[, c("release_year", "rating_encoded")]

# Prepare target variable
target <- netflix$type

# Split data into training and test sets
set.seed(42)  # for reproducibility
train_indices <- createDataPartition(target, p = 0.7, list = FALSE)
train_data <- features[train_indices, ]
train_target <- target[train_indices]
test_data <- features[-train_indices, ]
test_target <- target[-train_indices]

# Fit LDA model
lda_model <- lda(train_target ~ ., data = data.frame(train_data, train_target))

# Predict on test data
predictions <- predict(lda_model, test_data)
predicted_classes <- predictions$class

# Calculate accuracy
accuracy <- sum(predicted_classes == test_target) / length(test_target)
print(paste("Accuracy:", accuracy))

# Load the necessary library
library(tidyverse)

# Convert 'rating' from categorical to numerical code
netflix <- netflix %>%
  mutate(rating_code = as.numeric(as.factor(rating)))

# Check for NA values and remove rows containing them
netflix <- drop_na(netflix, c(release_year, rating_code))

# Run a linear regression model
model <- lm(release_year ~ rating_code, data = netflix)

# Summary of the model
summary(model)

# Plotting
ggplot(netflix, aes(x = rating_code, y = release_year)) +
  geom_point(aes(color = rating), alpha = 0.5) +  # Points with different colors for each rating
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Regression line with standard error
  labs(title = "Relationship between Release Year and Rating Code",
       x = "Rating Code",
       y = "Release Year") +
  theme_minimal()  # A clean theme for the plot

# Creating a boxplot
ggplot(netflix, aes(x = rating, y = release_year, fill = rating)) +
  geom_boxplot() +  # Boxplot for each rating category
  labs(title = "Boxplot of Release Years by Rating",
       x = "Rating",
       y = "Release Year") +
  theme_minimal() +  # A clean theme for the plot
  scale_fill_brewer(palette = "Pastel1")  # Color palette for different ratings
