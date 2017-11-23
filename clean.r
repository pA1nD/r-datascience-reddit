
# setup -------------------------------------------------------------------
rm(list = ls())
library("tidyverse")
library("lubridate")
library("Hmisc")
library("textcat")

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

resh = sample(1:nrow(df1)) # Ransomly arranged numbers from 1 to the length of the df

percTrain = 0.7 # Percentage of observations used for training
iSplit = percTrain * nrow(df1) # Determine the i value that serves as a splitting point

df_train = df1[1:iSplit ,] # Part of df used for training
df_test = df1[(iSplit + 1):nrow(df1),] # Part of df used for testing

nTrain = nrow(df_train) # Number of iterations is the number of rows in the df used for training

# Language of titles
#%nin% from Hmisc
# installed package cldr, can be downloaded from here: ftp://cran.r-project.org/pub/R/src/contrib/Archive/cldr


# Clean the date and select only the english ones
Clean_String <- function(string) {
  # Lowercase
  #temp <- tolower(string)
  temp <- string
  #' Remove everything that is not a number or letter (may want to keep more
  #' stuff in your actual analyses).
  temp <- stringr::str_replace_all(temp, "&.{1,5};", "")
  # Shrink down to just one white space
  temp <- stringr::str_replace_all(temp, "[\\s]+", " ") # remove trailing space
  # Split it
  #temp <- stringr::str_split(temp, " ")[[1]]
  # Get rid of trailing "" if necessary
  indexes <- which(temp == "")
  if (length(indexes) > 0) {
    temp <- temp[-indexes]
  }
  return(temp)
}

# Select the languages that are not going to pop up 
my.profiles <- ECIMCI_profiles[names(ECIMCI_profiles) %nin% c("afrikaans",
                                                              "basque",
                                                              "frisian","middle_frisian",
                                                              "latin",
                                                              "rumantsch",
                                                              "spanish",
                                                              "welsh",
                                                              "catalan",
                                                              "hungarian",
                                                              "romanian",
                                                              "scots",
                                                              "swedish")]
Clean_String(df1$title)
# parse
df1 <- df[1:17398, ]
df2 <- df[17399:34796, ]
df3 <- df[34797:52194, ]
df4 <- df[52195:69593, ]


titleClean = function(df){
  df <- df %>%
    mutate(lang = textcat(title)) %>%  # textcat identifies languages) %>%
    filter(lang == "english") %>% # filter out non-english languages
    mutate(title_clean = Clean_String(title))
}

dftest_clean <- titleClean(df_test)

df1_clean <- titleClean(df1) # note it takes about 10 min to calculate one 
df2_clean <- titleClean(df2) 
df3_clean <- titleClean(df3) 
df4_clean <- titleClean(df4) 
