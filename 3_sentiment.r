# Load Libraries
library(tidyverse)
library(ggplot2)
library(MASS)
library(dplyr)
library(modelr)
library(forcats)
library(readr)

library(tidytext)
library(plyr)
library(plotrix)


# Load Data
D <- read_csv("./data/clean_posts_sent.csv")
D2 <- read_csv("./data/clean_posts.csv")
View(D)

# Sentiment Analysis in the Sample --------------------------------------
# This graph is interesting to analyze the overall sentiment in the sample
# Interpretation: The Sentiment is Gaussian and therefore on expectation neutral
# (the mean is near zero and almost lines up with the median)

summary(D$sent_score) # checking the stats

# CHECK THAT!

ggplot(D, aes(x=sent_score))+
  geom_histogram(color="black", fill='white')+
  labs(x="Sentiment Score Per Post, n= 15400", y = 'Frequency', title= 'Histogram: Sentiment In The Set')+
  geom_vline(aes(xintercept=mean(D$sent_score), color='mean: -0.1513'), show.legend = TRUE, size=2)+
  geom_vline(aes(xintercept=median(D$sent_score), color='median: 0.0000'), show.legend = TRUE, size=1)+
  geom_vline(aes(xintercept=quantile(D$sent_score,0.25), color='1st quantile: -2.0000'), show.legend = TRUE, size=1)+
  geom_vline(aes(xintercept=quantile(D$sent_score,0.75), color='3rd quantile: 2.0000'), show.legend = TRUE, size=1)

# Camille's Sentiment Analysis --------------------------------------
D2 <- D2 %>%
  mutate(index_author = 1:nrow(D2))


# Retrieve Data of URL-----------------------------------------------------------------

TITLE_NEWS <- read_delim("./data/titles.csv", "; ", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)
View(TITLE_NEWS)


# Clean, add legend and merge tabs with URL  ----------------------------------------------------

names(TITLE_NEWS)[1]<-paste("Source URL")
names(TITLE_NEWS)[2]<-paste("source title")
names(D2)[5]<-paste("Source URL")

TITLE_NEWS <- TITLE_NEWS %>%
  dplyr::select("Source URL", "source title")

TITLE_merged <- inner_join(TITLE_NEWS, D2, by = "Source URL")


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


# Sentiment analysis ------------
# Do people tend to repost it in a negative or positive way?

# tidytext contains several sentiment lexicons. That is why we are interested in it.
# We use the afinn library because the sentiment is split in different emotions.
# But this one has the particularity to have a certain scale.
get_sentiments("afinn")


# Separate the data (in words) so that the library Afinn can be applied ---------------------

Title <- TITLE_merged %>% dplyr:: select("title", "index_author")
Source_title <- TITLE_merged %>% dplyr:: select("source title", "index_author")

title_words <- Title %>%
  unnest_tokens(word, title)

title_source_words <- Source_title %>%
  unnest_tokens(word, "source title")


# Compute the sentimentanalysis on the words --------------------------

# titles
afinn <- title_words %>%
  inner_join(get_sentiments("afinn"))

afinn_grouped_title <- title_words %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(index_author) %>%
  summarise(sentiment = sum(score))

# source titles
afinn_source <- title_source_words %>%
  inner_join(get_sentiments("afinn"))

afinn_grouped_source_title <- title_source_words %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(index_author) %>%
  summarise(sentiment_source = sum(score))


# Merge both results----------------------------------------------------------

merged_sentiment_titles <- inner_join(afinn_grouped_title, afinn_grouped_source_title, by = "index_author")

# Plot the difference between both
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

# Plot something  -----------------------------------------------------------

merged_sentiment_titles <- merged_sentiment_titles %>%
  dplyr::mutate( interpretation1 = merged_sentiment_titles$interpretation_level * 0)

for (i in 1:nrow(merged_sentiment_titles)) {
  if (merged_sentiment_titles$interpretation_level[i] < 0) {
    merged_sentiment_titles$interpretation1[i] <- "Negative"
  } else if (merged_sentiment_titles$interpretation_level[i] > 0) {
    merged_sentiment_titles$interpretation1[i] <- "Positive"
  } else if (merged_sentiment_titles$interpretation_level[i] == 0) {
    merged_sentiment_titles$interpretation1[i] <- "No change"
  } else 
    { "NA" }
  }

ggplot(data = merged_sentiment_titles) +
  geom_bar(mapping = aes(x = interpretation1))


# Plot something else  -----------------------------------------------------------
p = length(which(merged_sentiment_titles$interpretation1 == "Positive"))

n = length(which(merged_sentiment_titles$interpretation1 == "Negative"))

nc = length(which(merged_sentiment_titles$interpretation1 == "No change"))

number_of_interpretation <-c(p, n, nc)
kind_of_interpretation <- c("positive", "negative", "no change")
percentage <- round(number_of_interpretation/sum(number_of_interpretation)*100)
labels <- paste(kind_of_interpretation, percentage)
labels <- paste(labels,"%",sep="")

pie(number_of_interpretation,labels = labels, col=rainbow(length(labels)),
    main="Pie Chart of interpretation levels")

pie3D(number_of_interpretation,labels=labels,explode=0.5,
      main="Pie Chart 3D of interpretation levels ")
