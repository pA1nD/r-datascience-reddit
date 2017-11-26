# Analyzing each network station ------------------------------------

library("tidyverse")
library("tidytext")


# Cleaning the Data -------------------------------------------------------


network_station <- news %>%
  select(domain)

networking_reddit <- cbind(new_titles, network_station)
networking_reddit <- select(networking_reddit, -news.author)

networking_reddit <- networking_reddit[c(3,2,1)]


tidy_networking_reddit <- networking_reddit %>%
  # Group by the titles of the plays
  group_by(domain) %>%
  # Define a new column linenumber
  mutate(linenumber = row_number()) %>%
  # Transform the non-tidy text data to tidy text data
  unnest_tokens(word, news.title) %>%
  ungroup()



# Which network uses the most negative words? -----------------------------

domain_sentiment <- tidy_networking_reddit %>% 
  # Group by station
  group_by(domain) %>% 
  # Define a new column station_total
  mutate(domain_total = n()) %>%
  ungroup() %>%
  # Implement sentiment analysis with the NRC lexicon
  inner_join(get_sentiments("nrc"))

#Now we analyze how negative each domain is
domain_negative <- domain_sentiment %>% 
  count(domain, sentiment, domain_total) %>%
  # Define a new column percent
  mutate(percent_neg = n / domain_total) %>%
  # Filter only for negative words
  filter(sentiment == "negative") %>%
  # Arrange by percent
  arrange(percent_neg)

#Here we see th positive side
domain_positive <- domain_sentiment %>% 
  count(domain, sentiment, domain_total) %>%
  # Define a new column percent
  mutate(percent_pos = n / domain_total) %>%
  # Filter only for negative words
  select(domain, sentiment, percent_pos, n) %>%
  filter(sentiment == "positive") %>%
  # Arrange by percent
  arrange(percent_pos)

domain_all = merge(x = domain_positive, y = domain_negative, by = "domain", all = TRUE)

domain_sentiment = arrange(domain_sentiment, desc(linenumber))
top_domains <- head(as.vector(unique(domain_sentiment$domain)),n=10)

#We are only looking at some domain that are important in our opinion
some_domains <- filter(domain_sentiment, domain %in% top_domains)
head(some_domains)
some_domains %>%
  # Filter for only negative words
  filter(sentiment == "positive") %>%
  # Count by word and domain
  count(word, domain) %>%
  # Group by domain
  group_by(domain) %>%
  # Take the top 10 words for each domain
  top_n(10, n) %>%
  ungroup() %>%
  mutate(word = reorder(paste(word, domain, sep = "__"), n)) %>%
  # Set up the plot with aes()
  ggplot(aes(word, n, fill = domain)) +
  geom_col(show.legend = FALSE) +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  facet_wrap(~ domain, nrow = 2, scales = "free") +
  coord_flip()


# Countin the words -------------------------------------------------------

countiing_words <- tidy_networking_reddit %>% 
  count(domain, word) %>%
  # Define a new column percent
  mutate(percent_pos = n / domain_total) %>%
  # Filter only for negative words
  select(domain, sentiment, percent_pos, n) %>%
  filter(sentiment == "positive") %>%
  # Arrange by percent
  arrange(percent_pos)








