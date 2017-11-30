rm(list = ls())


# Data preparation --------------------------------------------------------
# Combines the original dataset with the aggregated sentiment score of a title

library("tidytext")
library("tidyverse")

df <- read_csv("data/clean_posts.csv")
sent <- read_csv("data/reddit_sentiment_with_scores.csv")

# Select the colums to be included in the df used for regression
titles <- data.frame (
  df$author,
  df$title_clean,
  df$score,
  df$num_comments,
  df$gilded,
  df$time_passed_days,
  df$ln_time_passed,
  df$period_posted,
  df$period_retrieved)

#I will order it after their score
new_titles <- titles %>%
  arrange(desc(df.score)) #%>%
#select(-news.score)

#Now I will tidy the Data and put one word for one row

new_titles <- new_titles %>%
  mutate(title_number = rownames(new_titles))

sent <- data.frame(sent[3], sent[6])
sent <- aggregate(score ~ title_number, sent, sum)
merged <- merge(sent, new_titles, by = "title_number")

merged <- merged %>%
  filter(df.author != "[deleted]")

mean_date <- mean(merged$df.period_posted)

merged_t <- data.frame(transpose(merged))
test_df <- merged

split_date 

# Regressions -------------------------------------------------------------




