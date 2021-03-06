# Analyzing each network station ------------------------------------
library(dplyr)
library("tidyverse")
library("tidytext")


# Cleaning the Data -------------------------------------------------------


network_station <- data.frame(news$domain)
names(network_station)[1] <- "domain"

networking_reddit <- cbind(new_titles, network_station)
networking_reddit <- select(networking_reddit, -news.author)

#Chancing the order of the columns
networking_reddit <- networking_reddit[c(5,2,3)]

#Putting one word in each row
tidy_networking_reddit <- networking_reddit %>%
  # Group by the titles of the plays
  group_by(domain) %>%
  # Define a new column linenumber
  mutate(linenumber = row_number()) %>%
  # Transform the non-tidy text data to tidy text data
  unnest_tokens(word, news.title) %>%
  ungroup()



# Which network uses the most negative words? -----------------------------

#inner_join on the dictionary "nrc"
domain_sentiment <- tidy_networking_reddit %>% 
  # Group by station
  group_by(domain) %>% 
  # Define a new column station_total
  mutate(domain_total = n()) %>%
  ungroup() %>%
  # Implement sentiment analysis with the NRC lexicon
  inner_join(get_sentiments("nrc"))

#Now we will analyze how negative each domain is
domain_negative <- domain_sentiment %>% 
  count(domain, sentiment, domain_total) %>%
  # Define a new column percent
  mutate(percent_neg = n / domain_total) %>%
  # Filter only for negative words
  filter(sentiment == "negative") %>%
  # Arrange by percent
  arrange(percent_neg)

#Here we will see the positive side of each domain
domain_positive <- domain_sentiment %>% 
  count(domain, sentiment, domain_total) %>%
  # Define a new column percent
  mutate(percent_pos = n / domain_total) %>%
  # Filter only for negative words
  select(domain, sentiment, percent_pos, n) %>%
  filter(sentiment == "positive") %>%
  # Arrange by percent
  arrange(percent_pos)

#merging the negative and positive dataframe
domain_all = merge(x = domain_positive, y = domain_negative, by = "domain", all = TRUE)

#choosing the top_domains (that published most news)
domain_sentiment = arrange(domain_sentiment, desc(linenumber))
top_domains <- head(as.vector(unique(domain_sentiment$domain)),n=10)

#We are only looking at some domain that are important in our opinion
some_domains <- filter(domain_sentiment, domain %in% top_domains)
head(some_domains)
some_domains %>%
  # Filter for only negative words
  filter(sentiment == "negative") %>%
  # Count by word and domain
  count(word, domain) %>%
  # Group by domain
  group_by(domain) %>%
  # Take the top 10 words for each domain
  top_n(10, n) %>%
  ungroup() %>%
  mutate(word = reorder(paste(word, domain, sep = "__"), n)) %>%
  # Set up the plot with aes()
  #This plot shows what kind of negative words the different news agencies use 
  ggplot(aes(word, n, fill = domain)) +
  geom_col(show.legend = FALSE) +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  facet_wrap(~ domain, nrow = 2, scales = "free") +
  coord_flip()





# Checking how popular each domain is for the authors ---------------------


#network_station <- news %>%
  select(domain)

#author_domain_df <- cbind(new_titles, network_station)

#author_domain_df <- author_domain_df[c(1,4,3,2)]

#author_domain_df <- author_domain_df %>%
                    (news.author)






