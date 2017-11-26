library(tidyverse)
library(tidytext)
news <- read.csv("data/clean_posts.csv")


# Cleaning the Data -------------------------------------------------------



#Now I will only select the titles, score and author
titles <-data.frame(news$author,news$title_clean,news$score)

#Checking out how many posts have more than 1 upvote
titles2.0 <- subset(titles,news$score > 1) 
#4442 posts from 31713 posts have more than 1 upvote


#I will transfer the dataframes in only characters 
titles$news.author = as.character(titles$news.author)
titles$news.title = as.character(titles$news.title)

new_titles <- new_titles[c("news.author", "news.title", "news.score")]

#I will order it after their score
new_titles <- titles %>% 
  arrange(desc(news.score)) #%>% 
#select(-news.score)

#Now I will tidy the Data and put one word for one row

tidy_titles <- new_titles %>%
  # Group by the titles of the plays
  group_by(news.author) %>%
  # Define a new column linenumber
  mutate(linenumber = row_number()) %>%
  # Transform the non-tidy text data to tidy text data
  unnest_tokens(word, news.title) %>%
  ungroup()
  #  filter(news.title != "deleted") %>%
  
  # Pipe the tidy reddit data frame to the next line
tidy_titles %>% 
  # Use count to find out how many times each word is used
  count(word, sort = TRUE)
#Fun fact: "in" is the most used word ;) 


#I will order it after the score
reddit_df <- tidy_titles %>%
  arrange(desc(news.score))


# Sentiment Analysis ------------------------------------------------------

#I will combine our reddit data with the bing lexicon

reddit_sentiment <- reddit_df %>%
  # Implement sentiment analysis with the "bing" lexicon
  inner_join(get_sentiments("bing")) 

#Here we can see how many postive and negative words every author used
negative_positive <- data.frame(reddit_sentiment %>%
                                  #Find how many positive/negative words each play has
                                  count(news.author, sentiment))


#Counting the negative and positive words per author
sentiment_counts <- reddit_df %>%
  # Implement sentiment analysis using the "bing" lexicon
  inner_join(get_sentiments("bing")) %>%
  # Count the number of words by title, type, and sentiment
  count(news.author, news.score, sentiment)

#The negativity should be calculated!!
#I still have to create something there


#Now I will calculate how often the words are being used
word_counts <- reddit_df %>%
  # Implement sentiment analysis using the "bing" lexicon
  inner_join(get_sentiments("bing")) %>%
  # Count by word and sentiment
  count(word, sentiment)

#the 10 most used negative and positive words
top_words <- word_counts %>%
  # Group by sentiment
  group_by(sentiment) %>%
  # Take the top 10 for each sentiment
  top_n(10) %>%
  ungroup() %>%
  # Make word a factor in order of n
  mutate(word = reorder(word, n))

#plot it

ggplot(top_words, aes(word, n, fill = sentiment)) +
  # Make a bar chart with geom_col()
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +  
  coord_flip()


# Another approach with another dictionary („afinn“) ----------------------
#This dictionary has scores for each word from -5 to +5

#Aplying this dictionary to our reddit Data
#Added also a contribution column, so we can see how important each word is
sentiment_contributions <- reddit_df %>%
  # Count by title and word
  count(news.author, word, sort = TRUE) %>%
  # Implement sentiment analysis using the "afinn" lexicon
  inner_join(get_sentiments("afinn")) %>%
  # Group by title
  group_by(news.author) %>%
  # Calculate a contribution for each word in each title
  mutate(contribution = score * n/sum(n)) %>%
  ungroup() %>%
  arrange(desc(contribution))
#Maybe this will be useful later


# Doing it with the dictionary „nrc" --------------------------------------
#This dictionary has more catagories


reddit_sentiment2 <- reddit_df %>% 
  # Group by author
  group_by(news.author) %>% 
  # Define a new column author_total
  mutate(author_total = n()) %>%
  ungroup() %>%
  # Implement sentiment analysis with the NRC lexicon
  inner_join(get_sentiments("nrc"))


# Which author use the most negative words?
reddit_sentiment2 %>% 
  count(news.author, sentiment, author_total) %>%
  # Define a new column percent
  mutate(percent = n/author_total) %>%
  # Filter only for negative words
  filter( sentiment == "negative") %>%
  # Arrange by percent
  arrange(desc(percent))


reddit_sentiment2 %>%
  # Count by word and sentiment
  count(word, sentiment) %>%
  # Group by sentiment
  group_by(sentiment) %>%
  # Take the top 10 words for each sentiment
  top_n(10, n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  # Set up the plot with aes()
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip()


# Time Analysis -----------------------------------------------------------

# Load the lubridate package
library(lubridate)

#choosing the time date
#added the time colum to the new_titles
published_date <- news %>%
                  select(period_posted)

time_reddit <- cbind(new_titles, published_date)

#Clean it
tidy_time_reddit <- time_reddit %>%
  # Group by the titles of the plays
  group_by(news.author) %>%
  # Define a new column linenumber
  mutate(linenumber = row_number()) %>%
  # Transform the non-tidy text data to tidy text data
  unnest_tokens(word, news.title) %>%
  ungroup()

reddit_time_df <- tidy_time_reddit %>%
  arrange(desc(news.score))


#Now the time analysis starts
library(lubridate)


sentiment_by_time <- reddit_time_df %>%
  # Define a new column using floor_date()
  mutate(date = floor_date(period_posted, unit = "day")) %>%
  # Group by date
  group_by(date) %>%
  mutate(total_words = n()) %>%
  ungroup() %>%
  # Implement sentiment analysis using the NRC lexicon
  inner_join(get_sentiments("nrc"))

sentiment_by_time %>%
  # Filter for positive and negative words
  filter(sentiment %in% c("positive", "negative")) %>%
  # Count by date, sentiment, and total_words
  count(date, sentiment, total_words) %>%
  ungroup() %>%
  mutate(percent = n / total_words) %>%
  # Set up the plot with aes()
  ggplot(aes(date, percent, color = sentiment)) +
  geom_line(size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, lty = 2) +
  expand_limits(y = 0)







