library(tidyverse)

#install an older version of tidytext that still had twitter support...
packageurl <- "https://cran.r-project.org/src/contrib/Archive/tidytext/tidytext_0.3.3.tar.gz"
install.packages(packageurl, repos=NULL, type="source")
install.packages("tidytext")
library(tidytext)

#load a few other tidyverse packages for data munging
library(lubridate)
library(scales)
library(purrr)
library(broom)
library(tidytext)
###
# Code and data here taken from https://www.tidytextmining.com/twitter.html
###

#load the twitter data
tweets_julia <- read_csv("tweets_julia.csv")
tweets_dave <- read_csv("tweets_dave.csv")

#combine both dfs and add a "person" column
tweets <- bind_rows(tweets_julia %>% 
                      mutate(person = "Julia"),
                    tweets_dave %>% 
                      mutate(person = "David")) %>%
  mutate(timestamp = ymd_hms(timestamp))

#histogram of tweets
ggplot(tweets, aes(x = timestamp, fill = person)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  facet_wrap(~person, ncol = 1)

###
# Compare word frequencies
###

#list of characters to remove for tweets
remove_reg <- "&amp;|&lt;|&gt;"

#remove retweets, remove symbols common to tweets, unnest tokens
#this splits each line to be one single word from the tweet
tidy_tweets <- tweets %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"))

#look at overall frequency of words
frequency <- tidy_tweets %>% 
  count(person, word, sort = TRUE) %>% 
  left_join(tidy_tweets %>% 
              count(person, name = "total")) %>%
  mutate(freq = n/total)

frequency

#look at tf-idf instead
tweet_words <- frequency %>%
  bind_tf_idf(word, person, n)

tweet_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

#pivot frequency to a wide format with both frequencies as columns
frequency <- frequency %>% 
  select(person, word, freq) %>% 
  pivot_wider(names_from = person, values_from = freq) %>%
  arrange(Julia, David)

frequency

#plot these frequencies
ggplot(frequency, aes(Julia, David)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")

###
# Sentiment analysis
###

#load a known sentiment lexicon
get_sentiments("bing")

#join to our words, average sentiment by day
bing_sentiment <- tidy_tweets %>% 
  inner_join(get_sentiments("bing")) %>%
  mutate(method = "Bing et al.") %>%
  count(method, index = as.Date(timestamp), sentiment) %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

#plot sentiment over time
bing_sentiment %>%
ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

#common words and sentiments
bing_word_counts <- tidy_tweets %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts

bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

###
# Comparing word usage
###

#Look at just 2016 when both were active and data scientists
tidy_tweets <- tidy_tweets %>%
  filter(timestamp >= as.Date("2016-01-01"),
         timestamp < as.Date("2017-01-01"))

#filter out usernames with @, calculate log odds ratio for who said it
word_ratios <- tidy_tweets %>%
  filter(!str_detect(word, "^@")) %>%
  count(word, person) %>%
  group_by(word) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  pivot_wider(names_from = person, values_from = n, values_fill = 0) %>%
  mutate_if(is.numeric, list(~(. + 1) / (sum(.) + 1))) %>%
  mutate(logratio = log(David / Julia)) %>%
  arrange(desc(logratio))

#words with small logratio are likely to be said by both
word_ratios %>% 
  arrange(abs(logratio))

#look at where logratio differs the most
word_ratios %>%
  group_by(logratio < 0) %>%
  slice_max(abs(logratio), n = 15) %>% 
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ylab("log odds ratio (David/Julia)") +
  scale_fill_discrete(name = "", labels = c("David", "Julia"))

###
# Changes in word use
###

#bin tweets by month, count # of times used per bin, filter to common words
words_by_time <- tidy_tweets %>%
  filter(!str_detect(word, "^@")) %>%
  mutate(time_floor = floor_date(timestamp, unit = "1 month")) %>%
  count(time_floor, person, word) %>%
  group_by(person, time_floor) %>%
  mutate(time_total = sum(n)) %>%
  group_by(person, word) %>%
  mutate(word_total = sum(n)) %>%
  ungroup() %>%
  rename(count = n) %>%
  filter(word_total > 30)

words_by_time

#nest all data by word and person
nested_data <- words_by_time %>%
  nest(data = c(-word, -person)) 

nested_data

nested_data$data[1:5]

#make models for each of those little dataframes
nested_models <- nested_data %>%
  mutate(models = map(data, ~ glm(cbind(count, time_total) ~ time_floor, ., 
                                  family = "binomial")))

nested_models

#pull out statistically significant slopes from the models.
#where did the frequencies change the most?
slopes <- nested_models %>%
  mutate(models = map(models, tidy)) %>%
  unnest(cols = c(models)) %>%
  filter(term == "time_floor") %>%
  mutate(adjusted.p.value = p.adjust(p.value))

top_slopes <- slopes %>% 
  filter(adjusted.p.value < 0.05)

top_slopes

#plot David changes in frequency
words_by_time %>%
  inner_join(top_slopes, by = c("word", "person")) %>%
  filter(person == "David") %>%
  ggplot(aes(time_floor, count/time_total, color = word)) +
  geom_line(size = 1.3) +
  labs(x = NULL, y = "Word frequency")

#same for Julia
words_by_time %>%
  inner_join(top_slopes, by = c("word", "person")) %>%
  filter(person == "Julia") %>%
  ggplot(aes(time_floor, count/time_total, color = word)) +
  geom_line(size = 1.3) +
  labs(x = NULL, y = "Word frequency")

