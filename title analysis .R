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

summary(merged_sentiment_titles$interpretation_level)

ggplot(merged_sentiment_titles) +
  geom_point(aes(index_author, interpretation_level)) +
  geom_smooth(mapping = aes(index_author, interpretation_level))

ggplot(merged_sentiment_titles) +
  geom_line(aes(index_author, interpretation_level)) 

hist(merged_sentiment_titles$interpretation_level, main = "histogram of interpretation level", xlab = "interpretation level", col = "red", bin_with=30)

summary(merged_sentiment_titles$interpretation_level)

boxplot(merged_sentiment_titles$interpretation_level, main = "histogram of interpretation level", xlab = "interpretation level")


# More plotting -----------------------------------------------------------

merged_sentiment_titles <- merged_sentiment_titles %>%
  dplyr::mutate( interpretation1 = merged_sentiment_titles$interpretation_level * 0)
  

#loop 

for (i in 1:nrow(merged_sentiment_titles)) { 
  if (merged_sentiment_titles$interpretation_level[i] < 0) {
    merged_sentiment_titles$interpretation1[i] <- "Negative"
  } else if (merged_sentiment_titles$interpretation_level[i] > 0) {
    merged_sentiment_titles$interpretation1[i] <- "Positive"
  } else if (merged_sentiment_titles$interpretation_level[i] == 0) {
    merged_sentiment_titles$interpretation1[i] <- "No change"
  } else { "NA" 
    }
  }                                                                                                                0 = "unchanged"))

summary(merged_sentiment_titles$interpretation1)

ggplot(data = merged_sentiment_titles) +
  geom_bar(mapping = aes(x = interpretation1))

library(plyr)
length(which(merged_sentiment_titles$interpretation1 == "Positive"))

length(which(merged_sentiment_titles$interpretation1 == "Negative"))

length(which(merged_sentiment_titles$interpretation1 == "No change"))

number_of_interpretation <-c(489, 675, 4550)
kind_of_interpretation <- c("positive", "negative", "no change")
percentage <- round(number_of_interpretation/sum(number_of_interpretation)*100)
labels <- paste(kind_of_interpretation, percentage)
labels <- paste(labels,"%",sep="")

pie(number_of_interpretation,labels = labels, col=rainbow(length(labels)),
    main="Pie Chart of interpretation levels")

install.packages("plotrix")
library(plotrix)

pie3D(number_of_interpretation,labels=labels,explode=0.5,
      main="Pie Chart 3D of interpretation levels ")

#check  in book
ggplot(data = merged_sentiment_titles) + 
  stat_summary(
    mapping = aes(x = sentiment_level, y = interpretation_level)
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
    )

