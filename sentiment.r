library(tidyverse)
library(tidytext)
news <- read.csv("data/clean_posts.csv")


# Cleaning the Data -------------------------------------------------------

#Now I will only select the titles, score and author
titles <-data.frame(news$author,news$title,news$score)

news_with_known_author <- news %>% filter(author != "[deleted]")
#Checking out how many posts have more than 1 upvote
titles2.0 <- subset(titles,news$score != 1)
#4442 posts from 31713 posts have more than 1 upvote


#I will transfer the dataframes in only characters
titles$news.author = as.character(titles$news.author)
titles$news.title = as.character(titles$news.title)

#new_titles <- new_titles[c("news.author", "news.title", "news.score",)]

#I will order it after their score
new_titles <- titles %>%
  arrange(desc(news.score)) #%>%
#select(-news.score)

#Now I will tidy the Data and put one word for one row

new_titles <- new_titles %>%
    mutate(title_number = rownames(new_titles))

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


#I will order it after the score
reddit_df <- tidy_titles %>%
  arrange(desc(news.score))


# Sentiment Analysis ------------------------------------------------------

#I will combine our reddit data with the bing lexicon
bing_sentiment <- reddit_df %>%
  # Implement sentiment analysis with the "bing" lexicon
  inner_join(get_sentiments("bing"))

# Afinn Dictionary --------------------------------------------------------

#positive and negative scores

reddit_sentiment_with_scores <- reddit_df %>%
  # Implement sentiment analysis with the "bing" lexicon
  inner_join(get_sentiments("afinn"))

#now we want to see if a news title is negative or positive by adding the sentiment score
score_title <- data.frame(reddit_sentiment_with_scores$score)


#Here we can see how many postive and negative words every author used
negative_positive <- data.frame(reddit_sentiment %>%
                                  #Find how many positive/negative words each author has
                                  count(news.author, sentiment))


#Counting the negative and positive words per author
sentiment_counts <- reddit_df %>%
  # Implement sentiment analysis using the "bing" lexicon
  inner_join(get_sentiments("afinn")) %>%
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
#This Graph shows the most used negative and positive words
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


plot_authors <- reddit_sentiment2 %>%
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



# Now the time analysis starts

library(lubridate)
reddit_time_df$period_posted <- as.POSIXct(reddit_time_df$period_posted)

sentiment_by_time <- reddit_time_df %>%
  # Define a new column using floor_date()
  mutate(date = floor_date(period_posted, unit = "days")) %>%
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
  geom_smooth(method = "lm", se = FALSE, lty = 10) +
  expand_limits(y = 0)
#We have a graph now, where we can see the negative and positive words over time


# How many words used in analysis
# Fraction of words used in sentiment analysis of a specific title
#hist(sentiment_by_time$percent)
#axis(side=1, at=seq(0,1, .1))

#ggplot(sentiment_by_time, aes(period_posted, percent)) +
  #geom_line(size = 1.5) +
  #geom_smooth(method = "lm", se = FALSE, lty = 2) #+

#  expand_limits(y = 0)




# Analyzing each author by positive and negative words --------------------

reddit_with_known_author <- reddit_sentiment2 %>%
                            filter(news.author != "[deleted]")

#arranging it after their linenumber(how many posts they have)

reddit_with_known_author = arrange(reddit_with_known_author, desc(linenumber))



top_author <- head(as.vector(unique(reddit_with_known_author$news.author)),n=10)


some_author <- filter(reddit_with_known_author, news.author %in% top_author)
head(some_author)
some_author %>%
  # Filter for only negative words
  filter(sentiment == "negative") %>%
  # Count by word and author
  count(word, news.author) %>%
  # Group by author
  group_by(news.author) %>%
  # Take the top 10 words for each author
  top_n(10, n) %>%
  ungroup() %>%
  mutate(word = reorder(paste(word, news.author, sep = "__"), n)) %>%
  # Set up the plot with aes()
  ggplot(aes(word, n, fill = news.author)) +
  geom_col(show.legend = FALSE) +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  facet_wrap(~ news.author, nrow = 2, scales = "free") +
  coord_flip()




# title sentiment aggregate (afinn) -----------------------------------------------
#We have a normal distribution
# Idea - high upvotes seem like normally distributed, do mle and maybe naive bayes


new_titles <- new_titles %>% 
  mutate(title_number = rownames(new_titles))



new_titles <- new_titles[2:4]
new_titles <- new_titles[-2]

merged <- merge(df, new_titles, by = "title_number")

merged = merge(merged, aggregate(score ~ title_number, merged, sum), by="title_number")


ggplot(merged, aes(score.y, news.score.x)) +
  labs( x = "sentiment value", y = "score") + 
  # Make a bar chart with geom_col()
  geom_col(show.legend = FALSE)
facet_wrap(~ sentiment, scales = "free")

#conlusion:
#the upvotes tend to be in general higher for negative posts
#if you look at the extreme ends the up votes are higher for positive posts


# Now we will look at the sentiment score and comments --------------------
descended_news <- news %>%
                  arrange(desc(score))

comments <- data.frame(descended_news$num_comments)

titles_with_comments <- cbind(arrange(new_titles),comments)

#we are merging the data frame
merged_comments <- merge(df, titles_with_comments, by = "title_number")

merged_comments = merge(merged_comments, aggregate(score.y ~ title_number, merged, sum), by="title_number")

#plotting it to see the relation between comments and sentiment

ggplot(merged_comments, aes(score.y, descended_news.num_comments)) +
  labs( x = "sentiment value", y = "comments") + 
  # Make a bar chart with geom_col()
  geom_col(show.legend = FALSE)
facet_wrap(~ sentiment, scales = "free")


# Now I will make a time analysis -----------------------------------------

time_posted <- data.frame(descended_news$period_posted)

reddit_time_df$period_posted <- as.POSIXct(reddit_time_df$period_posted)

titles_with_time <- cbind(new_titles,time_posted)

#Now we are merging

merged_time <- merge(df, titles_with_time, by = "title_number")

merged_time = merge(merged_time, aggregate(score.y ~ title_number, merged, sum), by="title_number")

merged_time$descended_news.period_posted <- as.POSIXct(merged_time$descended_news.period_posted)


ggplot(merged_time, 
  aes(descended_news.period_posted, score.y.x)) +
  geom_point()
  labs( x = "time", y = "sentiment value") + 
  # Make a bar chart with geom_col()
  geom_col(show.legend = FALSE)

class(merged_time$descended_news.period_posted)

names(merged_time)

#
library('tseries')
library(plyr)

merged_time$post_time = strptime(merged_time$descended_news.period_posted, "%Y-%m-%d %H")
merged_ts = merged_time %>%
  select(score.y.y) %>% 
  group_by(yr = year(post_time), mon = month(post_time), dy=day(post_time)) %>% 
  summarise(score = mean(score.y.y))

aggregate(merged_ts$score.y.y, by=(merged_ts$post_time), sum)

aggregate(score.y.y ~ post_time, merged_ts,sum)
merged_ts[ order(merged_ts$post_time , decreasing = FALSE ),]



# combining for a regression ----------------------------------------------



