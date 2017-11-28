rm(list = ls())

df <- read_csv("data/reddit_sentiment_with_scores.csv")
df <- df %>%
  filter(news.author != "[deleted]")

new_titles <- new_titles %>% 
  mutate(title_number = rownames(new_titles))

new_titles <- new_titles[2:4]
new_titles <- new_titles[-2]

merged <- merge(df, new_titles, by = "title_number")
