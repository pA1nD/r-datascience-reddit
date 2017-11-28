rm(list = ls())

# Visuals -----------------------------------------------------------------
library("tidyverse")
#install.packages("igraph") 
library("igraph")
#install.packages("data.tree")
library("data.tree")
library("networkD3")

library("plyr")
library('visNetwork') 

df <- read_csv("data/sentiment_nrc.csv")
df <- df %>%
  filter(news.author != "[deleted]")

# Trying visual on small set 
df_test <- df[1:200, ]


# Playground area ----------------------------------------------------------
# To try: 1) Weight words by frequency; 2) Select top words; 3) Change hierarchy;
#         4) Add Positive & Negative sentiment level

# Sentiment/Sentiment detail/Word

# Define the hierarchy
df_test$pathString <- paste("Sentiment", 
                            df_test$sentiment,
                            df_test$word,
                            sep = "|")
useRdf <- df_test
# Convert to Node
useRtree <- as.Node(useRdf, pathDelimiter = "|")

# Plot with networkD3
useRtreeList <- ToListExplicit(useRtree, unname = TRUE)
radialNetwork(useRtreeList)

# Final Radial Visualisation ----------------------------------------------



# Network Visual ----------------------------------------------------------
# Using small test data frame
# Preparing the nodes (contains unique ID's)
nodes <- data.frame(unique(df_test$word))
nodes1 <- data.frame(unique(df_test$sentiment))
nodes1 <- rename(nodes1, c("unique.df_test.sentiment." = "unique.df_test.word."))
nodes <- rbind(nodes, nodes1)
nodes <- rename(nodes, c("unique.df_test.word." = "id"))

# Preparing the links (contains "from" and "to" columns)
links <- data.frame(df_test$word, df_test$sentiment)
links <- rename(links, c("df_test.word"="from"))
links <- rename(links, c("df_test.sentiment"="to"))

# Visual
visNetwork(nodes, links, width="100%", height="400px", main="Network!")

# Final Network Visual ----------------------------------------------------
# Using whole data frame
network_df <- df
# Word column contains names of the sentiment - edits the sentiment titles
network_df$sentiment1 <- paste(network_df$sentiment, "_sentiment")
nodes <- data.frame(unique(network_df$word))
nodes1 <- data.frame(unique(network_df$sentiment1))
nodes1 <- rename(nodes1, c("unique.network_df.sentiment1." = "unique.network_df.word."))
nodes <- rbind(nodes, nodes1)
nodes <- rename(nodes, c("unique.network_df.word." = "id"))

links <- data.frame(network_df$word, network_df$sentiment1)
links <- rename(links, c("network_df.word"="from"))
links <- rename(links, c("network_df.sentiment1"="to"))


visNetwork(nodes, links, width="100%", height="400px", main="Network!")
#=========================================================================

