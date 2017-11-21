library(rjson)
data <- fromJSON(sprintf("[%s]", paste(readLines("data/reddit_sample.json"),collapse=",")))