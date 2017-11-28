rm(list = ls())

# Visuals -----------------------------------------------------------------
#install.packages("igraph") 
library(igraph)

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


# Final Visualisations ----------------------------------------------------

