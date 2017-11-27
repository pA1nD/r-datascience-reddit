# Cleaning and preparation of the data, function to split the set into a training and testing one


rm(list = ls())
library("jsonlite")
library("stringi")
#install.packages("stringr", dependencies = TRUE) # To work with strings
library("stringr")
library("tidyverse")



news <- read_csv("data/news_2016_12.csv")
df <- news
df_test <- news[1:100, ]

# df <- df_test


# Description of data
print(unique(df$author))# about 15K authors 

summary(news)
cor(df$score, df$ups)

unique(news$subreddit_id)
# Changing the date

#install.packages("lubridate")
library("lubridate")

# Origin Date
lubridate::origin # "1970-01-01 UTC"

# Conversion of the date
df <-
  mutate(
    df,
    period_posted = as.Date(df$created_utc, origin = "1970-01-01"),
    period_retrieved = as.Date(df$retrieved_on, origin = "1970-01-01")
    
  )
class(df$period_posted) = c('POSIXt', 'POSIXct')
class(df$period_retrieved) = c('POSIXt', 'POSIXct')

# Number of days passed between submition and extraction 
df <- df %>% 
  mutate(
    time_passed = retrieved_on - created_utc,
    time_passed_days = period_retrieved - period_posted,
    time_passed_days = as.integer(time_passed_days),
    ln_time_passed = log(time_passed_days) # ln of passed days
  )


# Function splitting the date into smaller increments
calc_numDate = function(df, col = "Date") {
  col = df[[col]]
  
  df = df %>%
    mutate(
      year = as.integer(str_sub(col, 1, 4)),
      month = as.integer(str_sub(col, 6, 7)),
      day = as.integer(str_sub(col, 9, 10)),
      hour = as.integer(str_sub(col, 12, 13)),
      minutes = as.integer(str_sub(col, 15, 16)),
      seconds = as.integer(str_sub(col, 18, 19)),
      DateNum = as.integer(year) + (as.integer(month) - 1) / 12
    )
  
  return(df)
}

df = calc_numDate(df, col = "period_posted")

# Selecting columns that will be dropped from the complete data set
col_to_drop <- c(

)

# Separation of data into training and testing datasets
set.seed(1)

test_n_train = function (df, percTrain){ #percTrain is a Percentage of observations used for training
  resh = sample(1:nrow(df1)) # Randomly arranged numbers from 1 to the length of the df
  
  iSplit = percTrain * nrow(df1) # Determine the i value that serves as a splitting point
  
  df_train = df1[1:iSplit ,] # Part of df used for training
  df_test = df1[(iSplit + 1):nrow(df1),] # Part of df used for testing
  
  nTrain = nrow(df_train) # Number of iterations is the number of rows in the df used for training
}







