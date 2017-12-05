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
#df <- df %>%
#  filter(news.author != "[deleted]")
df <- df %>% 
  mutate(
    sentiment_rad = sentiment
  )
df$sentiment <- paste(df$sentiment, "_sentiment")
# Trying visual on small set 

# Calculate the frequency of word
df_word_freq <- count(df$word)

df_word_freq <- rename(df_word_freq,
                    c("x" = "word",
                      "freq" = "word_freq"))
# Calculate the frequenct of sentiment
df_sent_freq <- count(df$sentiment)
df_sent_freq <- rename(df_sent_freq,
                    c("x" = "sentiment",
                      "freq" = "sent_freq"))
# Merge dataframes to contain weight
df_freq <- merge(df, df_word_freq, by = "word")
df_freq <- merge(df_freq, df_sent_freq, by = "sentiment")


# Radial Diagram ----------------------------------------------------------

# Sentiment/Sentiment detail/Word

# Define the hierarchy
df_test <- df_freq
df_test <- df_test %>% 
  arrange(desc(df_test$news.score))
df_test <- df_test[1:300, ]
df_test$pathString <- paste("Sentiment", 
                            df_test$sentiment_rad,
                            df_test$word,
                            sep = "|")
useRdf <- df_test
# Convert to Node
useRtree <- as.Node(useRdf, pathDelimiter = "|")

# Plot with networkD3
useRtreeList <- ToListExplicit(useRtree, unname = TRUE)
radial_diagram <- radialNetwork(useRtreeList, fontSize =9)
radial_diagram
# Saves network as HTML 
saveNetwork(radial_diagram, "Radial NRC Visual.html")

# NCR Network Visual ----------------------------------------------------------
# Preparing the nodes
# Nodes must contain the unique values
nodes <- data.frame(unique(df_test$word))
nodes <- nodes %>% 
  mutate(
    group = "word"
  )

nodes1 <- data.frame(unique(df_test$sentiment))
nodes1 <- nodes1 %>% 
  mutate(
    group = "sentiment"
  )

nodes1 <- rename(nodes1, c("unique.df_test.sentiment." = "unique.df_test.word."))
nodes <- rbind(nodes, nodes1)
nodes <- rename(nodes, c("unique.df_test.word." = "id"))

nodes <- nodes %>% 
  mutate(
    label = id
  )

# Preparing the links (contains "from" and "to" columns)
links <- data.frame(df_test$word, df_test$sentiment)
links <- rename(links, c("df_test.word"="from"))
links <- rename(links, c("df_test.sentiment"="to"))

# Visual
network <- visNetwork(
  nodes,
  links,
  width = "100%",
  height = "500px",
  main = "Visual Network")

# Saves the NTML
saveNetwork(network, "NRC Network Visual.html")
