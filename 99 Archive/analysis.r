install.packages('tidyverse')
library(tidyverse)
library(dplyr)
library(ggplot2)
library(MASS)

#load data set
library(readr)
clean_posts <- read_csv("~/Desktop/DS Group Project/clean_posts_sent.csv")
View(clean_posts)

D = clean_posts

# Authors and their activity-----------------------------------------------------------------


activity_per_author = D %>% group_by(author) %>% count(author) #checking out many articles
#each author has

activity_per_author = activity_per_author %>% filter(author!='[deleted]')#cutting out the deleted ones



# Inspecting the data and chopping it due to extreme skew in the distribution ----------------------------------------------------------------

#summary(D$num_comments) #inspecting the statistics of the number of commments
                        #makes me think about cutting either the 0s out or use log on y axis

#summary(D$ups) #inspecting the number of ups

#ninetynine_th_percentile_com = quantile(D$num_comments,c(.99)) # 99% of the posts contain 212 comments or less
#ninetynine_th_percentile_ups = quantile(D$ups,c(.99))#99% of posts have 695 ups or less

#D = D %>% filter(num_comments<213, ups<696) # chopping the last percentile off to get rid of outliers

# Plotting ----------------------------------------------------------------
# Getting a feel on which log base to use for scaling
# log10(100)
# log10(42)
# log(42)
# log(100,20)


par(mfrow=c(2,3))

boxplot(log10(D$num_comments+1), main= "Boxplot: log10(Number of comments+1)", xlab="n = 15400")

boxplot(log10(D$score+1), main= "Boxplot: log10(Score Number+1)", xlab = "n = 15400")

boxplot(log10(activity_per_author$n), main='Box: log10(Activity per author)', xlab= "n = 5799")

hist(log10(x=D$num_comments+1),main = "Histogram of number of comments(log10)", col = 'red', xlab ="log10(Number of Comments + 1)")

hist(log10(x=D$score+1),main = "Histogram of number of ups(log10)", col='red', xlab = "log10(Score Number + 1)")

hist(log10(activity_per_author$n), main = 'Histogram of activity per author (log10)', col = 'red', xlab = "log10(activity per author)")


# Stats to get away from log10 scaling and get an idea about the real skew ---------------------------------------------------------------
# IGNORE the MEAN - complete bogus given the skew we have - Only used summary()
# instead of quantiles() to illustrate the nonsense of the mean to class during the presentation


stats_D_scores=summary(D$score)
stats_D_num_comments=summary(D$num_comments)
stats_activity_p_author=summary(activity_per_author$n)



# ONLY INTERESTING FOR PLOT: Effect of Number of comments on Number of Ups-----------------

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



# Sentiment Analysis in the Sample --------------------------------------

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
install.packages('corrplot')
library(corrplot)

par(mfrow=c(1,1))

numericalVariables = D[ ,c(2,5,6,8)]

corrMat = cor(numericalVariables)

corrplot(corrMat, method = 'pie')
  

