# Setup -------------------------------------------------------------------
#source("clean.r")

library("tm")
#library("SnowballC")
#library("wordcloud")
library("RColorBrewer")
# main --------------------------------------------------------------------


# General Plotting --------------------------------------------------------


# Upvotes -----------------------------------------------------------------

#create upvotes tibble

dfUps <- df %>% 
  select(ups) %>% 
  #filter (up) %>% # we don't want -infinity for 0-upvoted posts (downvoted)
  mutate(ups = log(ups)) %>% #exponential dist
  arrange(desc(ups)) #arange descending

plot(dfUps$ups)
plot(dfUps$ups)