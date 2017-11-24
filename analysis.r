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

plotUps <- function(df){
  dfUps <- df %>% 
    select(ups) %>% 
    mutate(ups = log(ups)) %>% #exponential dist
    arrange(desc(ups)) #arange descending
  
  par(mfrow=c(2,1))
  # plot all upvotes
  plot(dfUps$ups, type="l", col="red")
  #plot only entries with upvotes > 1, log itt, sort decreasing order
  plot(sort(log(which(dfUps$ups > 0)), decreasing = TRUE), type="l", col="red") # most posts only have 1 upvote, >0 fol LN scale
  
  # calculate upvotes>1 / total Upvotes
  print(length(which(dfUps$ups > 0))/ length(dfUps$ups))
}
print("unclean data")
plotUps(df)
print("clean data")
plotUps(df1_clean)

# can see that the removal of non-english posts have a positive effect on upvotes
# The percentage of posts with upvotes > 1 increases from 9 to 14-15% 
# after the removal of non-english posts
plotUps(df2_clean)
