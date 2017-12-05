rm(list = ls())


# Data preparation --------------------------------------------------------
# Combines the original dataset with the aggregated sentiment score of a title

library("tidytext")
library("tidyverse")
library("plyr")

df <- read_csv("data/clean_posts.csv")
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
  df$domain,
  df$saved,
  # transform "false" = 0, "true" = 1
  df$over_18,
  # transform "false" = 0, "true" = 1
  df$thumbnail # transform into a dummy "default" = 0, else = 1
)

# I will order it after their score
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
merged1 <- merged %>%
  filter(df.author != "[deleted]")

test_df <- merged

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
    "df.domain" = "domain",
    "df.saved" = "saved", # note it is actually always false
    "df.over_18" = "over_18",
    "df.thumbnail" = "thumbnail"
  )
)

# Transform the boolean values in columns to a dummy variable that will further
# be used in the regression.


merged <- merged %>%
  mutate(saved = as.integer(as.logical(saved)),
           over_18 = as.integer(as.logical(over_18)))
#+ sapply(thumbnail, if (thumbnail = "default")) 

# Date for the middle of the month
mean_date <- mean(merged$period_posted)


#write_csv(merged, "data/clean_posts_sent.csv")

# Regressions -------------------------------------------------------------
# Plot gives us about 5-6 different plots for evaluation. Ask Milan if he can
# interpret some.

fit <-
  lm(score ~ 0 + sent_score + num_comments + time_passed_days + gilted + saved + over_18,
     data = merged)
summary(fit)

plot(fit)

# Conclusion:
# There are two statistically significant coefficients: number of comments and
# gilted. The coeficient of sentiment score is negative, meaning that on average
# a predicted score is faling the more positive the sentiment of the title is.
# The gilted posts on average have higher score by 1.4K, ceteris paribus.
#
# There is however an endogeneity problem between score and number of comments.
#

fit1 <-
  lm(num_comments ~ sent_score + score + ln_time_passed + gilted,
     data = merged)
summary(fit1)
#plot(fit1)

# Conclusion:
# All coefficients are statistically significant. The positive sentiment on average
# reduces the predicted score, and as expected, there is a positive relation between
# number of coments and score.
#
# The fact that the post is gilted does not impact the number of comments as much as
# it affects the score.

fit2 <-
  lm(gilted ~ sent_score + score + time_passed_days + num_comments,
     data = merged)
summary(fit2)
#plot(fit2)
# Conclusion:
