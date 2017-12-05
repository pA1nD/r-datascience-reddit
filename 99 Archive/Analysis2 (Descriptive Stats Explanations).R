install.packages('tidyverse')
library(tidyverse)
library(dplyr)
library(ggplot2)
library(MASS)

#load data set
library(readr)
clean_posts <- read_csv("./data/clean_posts_sent.csv")
View(clean_posts)

D = clean_posts

# Slicing: Authors and their activity-----------------------------------------------------------------


activity_per_author = D %>% group_by(author) %>% count(author) #checking out many articles
#each author has

activity_per_author = activity_per_author %>% filter(author!='[deleted]')#cutting out the deleted ones


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
  geom_smooth(aes(num_comments, score), method = "lm" ,color = "red3")+
  labs(x = xlab, y = ylab,
       title = "Regression: Number Of Comments vs. Score Number")


# Estimator results
reg = lm(D$score ~ D$num_comments)
summary(reg)

#Regression Interpretation: One comment more yields on average a 4.9 increase in the
#score number - result is statistically significant given the p-value of < 2.2e-16

# Sentiment Analysis in the Sample --------------------------------------
# This graph is interesting to analyze the overall sentiment in the sample
# Interpretation: The Sentiment is Gaussian and therefore on expectation neutral
# (the mean is near zero and almost lines up with the median)

summary(D$sent_score) # checking the stats

#par(mfrow=c(1,1))

#hist(D$sent_score, main = "Sentiment Analysis In The Set", xlab = "Sentiment score per post, n = 15400", col = "orange")

ggplot(D, aes(x=sent_score))+
  geom_histogram(color="black", fill='white')+
  labs(x="Sentiment Score Per Post, n= 15400", y = 'Frequency', title= 'Histogram: Sentiment In The Set')+
  geom_vline(aes(xintercept=mean(D$sent_score), color='mean: -0.1513'), show.legend = TRUE, size=2)+
  geom_vline(aes(xintercept=median(D$sent_score), color='median: 0.0000'), show.legend = TRUE, size=1)+
  geom_vline(aes(xintercept=quantile(D$sent_score,0.25), color='1st quantile: -2.0000'), show.legend = TRUE, size=1)+
  geom_vline(aes(xintercept=quantile(D$sent_score,0.75), color='3rd quantile: 2.0000'), show.legend = TRUE, size=1)



# Correlation Matrix -------------
# Main purpose of the corr. matrix is to see if there are any other correlations
# between our variables

#Interpretation: number of comments, score, and time passed all slightly
#correlate negatively (-0.04%) with sentiment score. Number of comments strongly
#correlates positively with score (as shown in the regression above)

install.packages('corrplot')
library(corrplot)

par(mfrow=c(1,1))

numericalVariables = D[ ,c(2,5,6,8)]

corrMat = cor(numericalVariables)

corrplot(corrMat, method = 'pie')
