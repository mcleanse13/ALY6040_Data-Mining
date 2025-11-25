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

