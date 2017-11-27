df_clean = read_delim("data/clean_posts.csv", ",")
df_clean = select(df_clean, url)
write_csv(df_clean, "scraper/urls.txt", append=FALSE, col_names = FALSE)
