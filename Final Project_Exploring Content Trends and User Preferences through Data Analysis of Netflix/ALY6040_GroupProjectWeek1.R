# Load required libraries
library(tidyverse)
library(lubridate)

# Load the dataset
netflix <- read.csv("netflix.csv")

# Display the structure of the dataset
str(netflix)

# Display the first few rows of the dataset
head(netflix)

# Summary statistics
summary(netflix)

# Data Preprocessing
# Convert 'type' and 'rating' to factor
netflix$type <- as.factor(netflix$type)
netflix$rating <- as.factor(netflix$rating)

# Convert 'date_added' to Date type
netflix$date_added <- as.Date(netflix$date_added, format = "%B %d, %Y")

# Handle missing values
# Check for missing values
colSums(is.na(netflix))

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

