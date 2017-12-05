# Load Libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(MASS)
library(readr)
library(modelr)
library(forcats)


# Load Data
D <- read_csv("./data/clean_posts_sent.csv")
D2 <- read_csv("./data/clean_posts.csv")
View(D)


# Slicing: Authors and their activity-----------------------------------------------------------------
activity_per_author = D %>% group_by(author) %>% count(author) #checking out many articles
activity_per_author = activity_per_author %>% filter(author!='[deleted]') #cutting out the deleted ones


# Plotting ----------------------------------------------------------------

#These boxplots and histograms are there to visualize the log-distributions
#of our choosen variables of interest. We can gauge through them how much
#our variables of interest are skewed (tailed)
#Interpretation: All plots show a massive skew eventhough the the data is log10.
#All ploted variables are right side skewed which shows that the sample consists MANY
#low discrete values and marginal number of outliers.

par(mfrow=c(2,3))

boxplot(log10(D$num_comments+1), main= "Boxplot: log10(Number of comments+1)", xlab="n = 15400")

boxplot(log10(D$score+1), main= "Boxplot: log10(Score Number+1)", xlab = "n = 15400")

boxplot(log10(activity_per_author$n), main='Box: log10(Activity per author)', xlab= "n = 5799")

hist(log10(x=D$num_comments+1),main = "Histogram of number of comments(log10)", col = 'red', xlab ="log10(Number of Comments + 1)")

hist(log10(x=D$score+1),main = "Histogram of number of ups(log10)", col='red', xlab = "log10(Score Number + 1)")

hist(log10(activity_per_author$n), main = 'Histogram of activity per author (log10)', col = 'red', xlab = "log10(activity per author)")


# Checkin the stats to get away from log10 scaling and get an idea about the real skew ---------------------------------------------------------------
# IGNORE the MEAN - not descriptive given the skew we have - Only used summary()
# instead of quantiles() to illustrate the nonsense of the mean to class during the presentation

stats_D_scores=summary(D$score)
stats_D_num_comments=summary(D$num_comments)
stats_activity_p_author=summary(activity_per_author$n)


# Regression PLOT: Effect of Number of comments on score number----------------
# Purpose of this regression is to see how activity (comments) influences score

xlab= 'Number Of Comments'
ylab='Score Number'

ggplot(D) +
  # scatterplot
  geom_point(aes(num_comments, score), color = "green4") +
  #add curve that shows statistical relationship between variables
  geom_smooth(aes(num_comments, score), method = "lm" ,color = "red3") +
  labs(x = xlab, y = ylab, title = "Reg: Comment Number vs. Score Number") +
  theme(
    title=element_text(size=8, face="bold"),
    axis.text=element_text(size=6),
    axis.title=element_text(size=6,face="bold"))


# Estimator results
reg = lm(D$score ~ D$num_comments)
summary(reg)

#Regression Interpretation: One comment more yields on average a 4.9 increase in the
#score number - result is statistically significant given the p-value of < 2.2e-16

# Regression Time Passed to Upvotes: -------

D2 <- D2 %>%
  mutate(index_author = 1:nrow(D2))

new_clean_2variables <- D2 %>%
  dplyr::select(ups, time_passed_days) %>%
  filter(time_passed_days < 21) %>%
  filter(ups > 1)  %>%
  mutate(ups = log(ups))

  ggplot(new_clean_2variables) +
    geom_point(aes(time_passed_days, ups))+
    geom_smooth(mapping = aes(time_passed_days, ups))

fit <- lm(ups ~ time_passed_days, data = new_clean_2variables)
summary(fit)

ggplot(new_clean_2variables, aes(x = time_passed_days, y = ups)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red")

# Regression Interpretation: For every additional day, a post gets an additional
# 0.09013 upvote. The result is statistically significant given the p-value < 0.02902.
