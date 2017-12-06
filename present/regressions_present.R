# Regressions for the presentaiton

rm(list = ls())


library("tidytext")
library("tidyverse")
library("plyr")

# Data Preparation
# Import of data
df <- read_csv("data/clean_posts.csv")
df_sent <- read_csv("data/clean_posts_sent.csv")
sent <- read_csv("data/reddit_sentiment_with_scores.csv")
df_orig <- read_csv("data/news_2016_12.csv")

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
  df$period_retrieved,
  df$domain)

# Order in descending score
new_titles <- titles %>%
  arrange(desc(df.score)) #%>%
# select(-news.score)

# Now I will tidy the Data and put one word for one row

new_titles <- new_titles %>%
  mutate(title_number = rownames(new_titles))

# Selecting only a column with sentimet value and a title number
sent <- data.frame(sent[3], sent[6])
# Sum the sentiment values alssociated with different words in one
# title.
sent <- aggregate(score ~ title_number, sent, sum)
# Merge two data frames by the title number to allocate the title
# sentiments
merged <- merge(sent, new_titles, by = "title_number")

# Remove the unknown authors
#merged1 <- merged %>%
#  filter(df.author != "[deleted]")

# Renaming columns
merged <- rename(
  merged,
  c(
    "score" = "sent_score",
    "df.author" = "author",
    "df.title_clean" = "title_clean",
    "df.score" = "score",
    "df.num_comments" = "num_comments",
    "df.gilded" = "gilted",
    "df.time_passed_days" = "time_passed_days",
    "df.ln_time_passed" = "ln_time_passed",
    "df.period_posted" = "period_posted",
    "df.period_retrieved" = "period_retrieved",
    "df.domain" = "domain")
)

test_df <- merged

# Transform the boolean values in columns to a dummy variable that will further
# be used in the regression.

# Date for the middle of the month
mean_date <- mean(merged$period_posted)



#write_csv(merged, "data/clean_posts_sent.csv")

# Regressions -------------------------------------------------------------
reg_score <- function(b){
  fit <-lm(merged$score ~ 0 + b, data = merged)
  summary(fit)
}
reg_score(merged$num_comments)

fit <-
  lm(score ~ 0 + num_comments,
     data = merged)
summary(fit)

plot(fit)
