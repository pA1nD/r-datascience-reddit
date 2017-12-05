library("httr")
df <- read_delim("data/news_2016_12.csv", ",")

news_links <- df$url

smmry_url_query <- 
news_links[1]