rm(list=ls())

install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
library(MASS)
library(dplyr)
library(modelr)
library(forcats)

#load data set
library(readr)
clean_posts <- read_csv("data/clean_posts.csv")
View(clean_posts)

clean_posts <- clean_posts %>%
  mutate(index_author = 1:nrow(clean_posts))

# Retrieve Data of URL-----------------------------------------------------------------

library(readr)
TITLE_NEWS <- read_delim("data/titles.csv", 
                     ";", escape_double = FALSE, col_names = FALSE, 
                     trim_ws = TRUE)
View(TITLE_NEWS)


# Clean, add legend and merge tabs with URL  ----------------------------------------------------


names(TITLE_NEWS)[1]<-paste("Source URL")
names(TITLE_NEWS)[2]<-paste("source title")
names(clean_posts)[5]<-paste("Source URL")

TITLE_NEWS <- TITLE_NEWS %>%
  dplyr::select("Source URL", "source title")

TITLE_merged <- inner_join(TITLE_NEWS, clean_posts, by = "Source URL")

# create new tab with only relevant columns  ------------------------------

Title_analysis <- 
  TITLE_merged %>% dplyr::select("author",
                        "domain",
                        "Source URL",
                        "num_comments",
                        "score",
                        "title",
                        "source title",
                        "period_posted",
                        "period_retrieved", 
                        "index_author")


# Do people tend to repost it in a negative or positive way ---------------
# Sentiment analysis 

 
install.packages("tidytext") # contains several sentiment lexicons  
# that is why we are interested in it 

library(tidytext)

#We use afinn library because the sentiment is split in different emotions
#But this one has the particularity to have a certain scale 

get_sentiments("afinn")


# separate the data (in words) so that the library Afinn can be applied ---------------------

Title <- TITLE_merged %>% dplyr:: select("title", "index_author")
Source_title <- TITLE_merged %>% dplyr:: select("source title", "index_author")

#vector_title <- as.vector(Title$title)
#vector_source_title <- as.vector(Source_title$'source title')


title_words <- Title %>%
  unnest_tokens(word, title)

title_words

title_source_words <- Source_title %>%
  unnest_tokens(word, "source title")

title_source_words


# Compute the analysis of sentiment on the words --------------------------

# titles 

afinn <- title_words %>%
  inner_join(get_sentiments("afinn")) 


afinn_grouped_title <- title_words %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(index_author) %>%
  summarise(sentiment = sum(score)) 

#source titles 

afinn_source <- title_source_words %>%
  inner_join(get_sentiments("afinn")) 


afinn_grouped_source_title <- title_source_words %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(index_author) %>%
  summarise(sentiment_source = sum(score)) 

# Merge both results----------------------------------------------------------

merged_sentiment_titles <- inner_join(afinn_grouped_title, afinn_grouped_source_title, by = "index_author")

#plot teh difference between both
merged_sentiment_titles <- merged_sentiment_titles %>%
  dplyr::mutate(interpretation_level = sentiment - sentiment_source)

# Plot results  -----------------------------------------------------------

ggplot(merged_sentiment_titles) +
  geom_point(aes(index_author, interpretation_level)) +
  geom_smooth(mapping = aes(index_author, interpretation_level))

ggplot(merged_sentiment_titles) +
  geom_line(aes(index_author, interpretation_level)) 


# More plotting -----------------------------------------------------------

#Bar chart observing how many 0, >0 and <0 



merged_sentiment_titles_bar <- merged_sentiment_titles %>%
  mutate(category = sapply(merged_sentiment_titles$interpretation_level, switch, 
                           0 = "No interpretation"
                           1 = "positive interpretation" 
                           -1 = "negative interpretation"))





