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










