library(tidyverse)
library(tidytext)
library(lubridate)

news <- read.csv("data/clean_posts.csv")


# Cleaning the Data -------------------------------------------------------

#Now I will only select the titles, score and author
titles <-news[c("author","title","score")]

# cast cols  as char
# assign index as column - to be used as reference for each unique title
# sort desc by score
titles = titles %>%
  mutate(author = as.character(author),
         title = as.character(title)) %>%
  mutate(title_id = rownames(titles)) %>%
  arrange(desc(score)) 

#Removing all the unknown author
news_author <- news %>% filter(author != "[deleted]")

#Checking out how many posts have more than 1 upvote
print("count posts > 1 upvote:")
print(nrow(filter(news,score > 1)))
#4442 posts from 31713 posts have more than 1 upvote

#Now I will tidy the Data and put one word for one row
# Group by the author and show words used in titles he posted
# Transform the non-tidy text data to tidy text data
df_words <- titles %>%
  group_by(author) %>%
  unnest_tokens(word, title) %>%
  ungroup()


# Sentiment Analysis ------------------------------------------------------

#I will combine our reddit data with the bing lexicon
# Implement sentiment analysis with the "bing" lexicon
bing_sentiment <- df_words %>%
  inner_join(get_sentiments("bing"))

#Now I will calculate how often the words are being used
# Implement sentiment analysis using the "bing" lexicon
# Count by word and sentiment
word_freq <- df_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment)

#the 10 most used negative and positive words
# Group by sentiment
# Take the top 10 for each sentiment
# Make word a factor in order of n

top_words <- word_freq %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n))

#plot it
#This Graph shows the most used negative and positive words
#with the bing dictionary
# Make a bar chart with geom_col()
ggplot(top_words, aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +
  coord_flip()


# Afinn Dictionary --------------------------------------------------------
# Another approach with another dictionary („afinn“) ----------------------
#This dictionary has scores for each word from -5 to +5

#Aplying this dictionary to our reddit Data
#Added also a contribution column, so we can see how important each word is
# Count by title and word
# Implement sentiment analysis using the "afinn" lexicon
# Group by title
# Calculate a contribution for each word in each title

sentiment_contributions <- df_words %>%
  count(author, word, sort = TRUE) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(author) %>%
  mutate(contribution = score * n/sum(n)) %>%
  ungroup() %>%
  arrange(desc(contribution))

#Result: We see a column with the words that contribute the most



# Doing it with the dictionary „nrc" --------------------------------------
#This dictionary has more catagories

# Group by author
# Define a new column author_total
# Implement sentiment analysis with the NRC lexicon

nrc_sentiment <- df_words %>%
  group_by(author) %>%
  mutate(author_total = n()) %>%
  ungroup() %>%
  inner_join(get_sentiments("nrc"))


# Which author use the most negative words?
# Define a new column percent
# Filter only for negative words
# Arrange by percent
most_negative_author <- nrc_sentiment %>%
  count(author, sentiment, author_total) %>%
  mutate(percent = n/author_total) %>%
  filter( sentiment == "negative") %>%
  arrange(desc(percent))


#PLOTTING which words are used most often for each sentiment
# Count by word and sentiment
# Group by sentiment
# Take the top 10 words for each sentiment
nrc_sentiment %>%
  count(word, sentiment) %>%
  group_by(sentiment) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%

# Set up the plot with aes()
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip()


# Time Analysis -----------------------------------------------------------
#choosing the time date
#added the time colum to the new_titles

published_date <- news[c("period_posted")]
time_reddit <- cbind(titles, published_date)

#Clean it
# Group by the titles of the plays
# Define a new column linenumber
# Transform the non-tidy text data to tidy text data
df_words_with_time <- time_reddit %>%
  group_by(author) %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(word, title) %>%
  ungroup() %>%
  arrange(desc(score))

reddit_time_df <- tidy_time_reddit %>%
  arrange(desc(news.score))

# Now the time analysis starts

#changing the class of the column "period_posted" to POSIXct
df_words_with_time$period_posted <- as.POSIXct(df_words_with_time$period_posted)



#PLOTTING the sentiment by time
# Define a new column using floor_date()
# Group by date
# Implement sentiment analysis using the NRC lexicon
sentiment_by_time <- df_words_with_time %>%
  mutate(date = floor_date(period_posted, unit = "days")) %>%
  group_by(date) %>%
  mutate(total_words = n()) %>%
  ungroup() %>%
  inner_join(get_sentiments("nrc"))

# Filter for positive and negative words
# Count by date, sentiment, and total_words
sentiment_by_time %>%
  filter(sentiment %in% c("positive", "negative")) %>%
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

reddit_with_known_author <- nrc_sentiment %>%
                            filter(author != "[deleted]")

#arranging it after their linenumber(how many posts they have)
reddit_with_known_author = arrange(reddit_with_known_author, desc(author_total))

#choosing the top 10 author that post the most
top_author <- head(as.vector(unique(reddit_with_known_author$author)),n=10)


some_author <- filter(reddit_with_known_author, author %in% top_author)
head(some_author)

#PLOTTING the most used negative words from authors that posted the most
# Filter for only negative words
# Count by word and author
# Group by author
# Take the top 10 words for each author
some_author %>%
  filter(sentiment == "negative") %>%
  count(word, author) %>%
  group_by(author) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(word = reorder(paste(word, author, sep = "__"), n)) %>%

# Set up the plot with aes()
  ggplot(aes(word, n, fill = author)) +
  geom_col(show.legend = FALSE) +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  facet_wrap(~ author, nrow = 2, scales = "free") +
  coord_flip()
#result: we see a plot with the most used negative words 
#from the 10 author that posted the most



# title sentiment aggregate (afinn) -----------------------------------------------
#We have a normal distribution
# Idea - high upvotes seem like normally distributed, do mle and maybe naive bayes

renamed_titles <- titles

names(renamed_titles)[names(renamed_titles) == "score"] <- "news.score"

tidy_renamed_titles <- renamed_titles %>%
  group_by(author) %>%
  unnest_tokens(word, title) %>%
  ungroup()

# Implement sentiment analysis with the "afinn" lexicon
# scale sentiments -5 to 5
afinn_sentiment <- tidy_renamed_titles %>%
  inner_join(get_sentiments("afinn"))

#scores from sentiment and scores from news is the same name
# Distribution of afinn ratings
ggplot(afinn_sentiment, aes(score, news.score)) +
  labs( x = "sentiment value", y = "score") + 
# Make a bar chart with geom_col()
geom_point()
facet_wrap(~ sentiment, scales = "free")


#new_titles_afinn <- new_titles_afinn[2:4]
#new_titles_afinn <- new_titles_afinn[-2]


merged_news <- merge(renamed_titles, afinn_sentiment, by = "title_id") %>%
  select(-news.score.x)
names(merged_news)[names(merged_news) == "score"] <- "sentiment_score"
names(merged_news)[names(merged_news) == "news.score.y"] <- "score"


merged_news_sum= merge(merged_news, aggregate(sentiment_score ~ title_id, merged_news, sum), by="title_id")
names(merged_news_sum)[names(merged_news_sum) == "sentiment_score.x"] <- "word_sentiment"
names(merged_news_sum)[names(merged_news_sum) == "sentiment_score.y"] <- "title_sentiment"


ggplot(merged_news_sum, aes(title_sentiment, score)) +
  labs( x = "sentiment value", y = "score") + 
# Make a bar chart with geom_col()
  geom_col(show.legend = FALSE)

#conlusion:
#the upvotes tend to be in general higher for negative posts
#if you look at the extreme ends the up votes are higher for positive posts


# Now we will look at the sentiment score and comments --------------------
descended_news <- news %>%
                  arrange(desc(score))

comments <- data.frame(descended_news$num_comments)

titles_with_comments <- cbind(arrange(new_titles),comments)

#we are merging the data frame
merged_comments <- merge(df, titles_with_comments, by = "title_id")

merged_comments = merge(merged_comments, aggregate(score.y ~ title_id, merged, sum), by="title_id")

#plotting it to see the relation between comments and sentiment

ggplot(merged_comments, aes(score.y, descended_news.num_comments)) +
  labs( x = "sentiment value", y = "comments") + 
  # Make a bar chart with geom_col()
  geom_col(show.legend = FALSE)
facet_wrap(~ sentiment, scales = "free")


# Now I will make a time analysis to see how sentiments change over time-----------------------------------------

time_posted <- data.frame(descended_news$period_posted)

reddit_time_df$period_posted <- as.POSIXct(reddit_time_df$period_posted)

titles_with_time <- cbind(new_titles,time_posted)

#Now we are merging

merged_time <- merge(df, titles_with_time, by = "title_id")

merged_time = merge(merged_time, aggregate(score.y ~ title_id, merged, sum), by="title_id")

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



