library(tidyverse)
library(tidytext)
news <- read.csv("data/clean_posts.csv")


# Cleaning the Data -------------------------------------------------------



#Now I will only select the titles, score and author
titles <-data.frame(news$author,news$title,news$score)

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

# tweets_nrc has been pre-defined
tweets_nrc

joy_words <- tweets_nrc %>%
  # Filter to choose only words associated with joy
  filter(sentiment == "joy") %>%
  # Group by each word
  group_by(word) %>%
  # Use the summarize verb to find the mean frequency
  summarize(freq = mean(freq)) %>%
  # Arrange to sort in order of descending frequency
  arrange(desc(freq))    

# Load ggplot2
library(ggplot2)

joy_words %>%
  top_n(20) %>%
  mutate(word = reorder(word, freq)) %>%
  # Use aes() to put words on the x-axis and frequency on the y-axis
  ggplot(aes(word, freq)) +
  # Make a bar chart with geom_col()
  geom_col() +
  coord_flip()