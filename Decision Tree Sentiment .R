install.packages("rattle")
install.packages("rattle", repos="https://rattle.togaware.com", type="source")

install.packages("devtools")
devtools::install_bitbucket("kayontoga/rattle")

library(tidyverse)
library(tidytext)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
#run sentiment.r first 


#split the data
training_bing <- bing_sentiment[1:18000,]
testing_bing <- bing_sentiment[18000:27255,]

#tree model
tree <- rpart(sentiment ~ ., training_bing, method = "class")

# Draw the decision tree
rpart.plot(tree)
fancyRpartPlot(tree)

#Gedanken für morgen:
#-nimm die Titel heraus
#-Kommentare, upvotes, thumbnail, over_18, gilded behalten
#sentiment analysieren nicht durch Wörter, sondern durch andere Columns


# Experiment 2….. ---------------------------------------------------------


news <- read.csv("data/clean_posts.csv")

#now I will take the most interesting colums

news_most_interesting <- news[c(3,4,6,7,19)]





