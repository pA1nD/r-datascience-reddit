library(ggplot2)   

df <- read_csv("data/clean_posts.csv")

# create date variable for the x-axis
df$date <- as.Date(df$period_posted, format = "%Y-%m-%d")
# get H:M:S components
df$hm <- format(df$period_posted, "%H:%M:%S")
# allocate a weekday
df$days <- weekdays(df$date)

df <- df %>% 
  group_by(df$days) %>% 
  mutate(
    week_time_frequence = count(df$hm)
    )

df_freq <- count(df$period_posted)

df_freq <- rename(df_freq,
                       c("x" = "time",
                         "freq" = "time_freq"))

# create y-axis breaks and labels
lab <- with(df, paste(format(df$period_posted, "%H"), "00", sep = ":"))

gg <- ggplot(data = df, aes(x = date, y = hm, fill = value)) +
  geom_tile() +
  scale_y_discrete(breaks = lab)

gg



ggplot(df, aes(x = hm, y = date)) + 
  geom_tile(aes(fill = Freq)) + 
  scale_fill_gradient(name = 'Total Motor Vehicle Thefts', 
                      low = 'white', 
                      high = 'red') + 
  theme(axis.title.y = element_blank())



