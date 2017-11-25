# Setup -------------------------------------------------------------------
#source("clean.r")

library("tm")
#library("SnowballC")
#library("wordcloud")
library("RColorBrewer")
# main --------------------------------------------------------------------
# Read clean data csv
df = read_delim("data/news_2016_12.csv")
df_clean = read_delim("data/clean_posts.csv", sep=",")

# General Plotting --------------------------------------------------------


# Upvotes -----------------------------------------------------------------

#create upvotes tibble
print("hello")

plotUps <- function(df){
  dfUps <- df %>% 
    select(ups) %>% 
    mutate(ups = log(ups)) %>% #exponential dist
    arrange(desc(ups)) #arange descending
  
  par(mfrow=c(2,1))
  # plot all upvotes
  plot(dfUps$ups, type="l", col="red", ylab="log total Upvotes")
  #plot only entries with upvotes > 1, log itt, sort decreasing order
  dfUpsGr1 = dfUps[which(dfUps$ups > 0), "ups"]
  plot(dfUpsGr1$ups, type="l", col="red", ylab="log Upvotes > 1") # most posts only have 1 upvote, >0 fol LN scale
  
  # calculate upvotes>1 / total Upvotes
  print(length(dfUpsGr1$ups) / length(dfUps$ups) * 100 ) 
}
print("% ups > 1 for unclean data")
plotUps(df)
print("% ups > 1 for clean data")
plotUps(df_clean)
# Influencer : Post language
# can see that the removal of non-english posts have a positive effect on upvotes
# The percentage of posts with upvotes > 1 increases from 9 to 14% 
# after the removal of non-english posts


# Binning -----------------------------------------------------------------

# might want only posts with upvotes > 1 - use log scale

binUps = function(df){
  maxInt= ceiling(max(df))
  binList = seq(1, maxInt)
  binUps = matrix(0, nrows=2, ncol=maxInt)
  binUps[,1] = binList
  for(i in bins){
    
  }
}
