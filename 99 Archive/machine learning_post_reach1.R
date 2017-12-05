rm(list=ls())

#install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
library(dplyr)
library(forcats)
library(sqldf)
library(chron)
library(readr)
library(tidytext)


clean_posts <- read_csv("data/clean_posts.csv", local = locale(encoding="latin1"))

clean_posts <- clean_posts %>%
  dplyr:: mutate(index_author = 1:nrow(clean_posts))

clean_posts_limited <- clean_posts %>%
  dplyr:: select("title_clean", "ups", "period_posted","index_author", "num_comments")


clean_posts_limited <-  clean_posts_limited %>%
  mutate(time = strftime(clean_posts$period_posted, format="%H:%M:%S"))

 class(clean_posts_limited$time)
 
 # turn into nurmeric format  ----------------------------------------------
 
 clean_posts_limited <- clean_posts_limited %>%
   mutate(time1 = as.numeric(times(clean_posts_limited$time)))
 
 clean_posts_limited <- clean_posts_limited %>%
   mutate(time2 = clean_posts_limited$time1 * 24)
  
hist(clean_posts_limited$time2, main = "histogram of posting time", xlab = "posting period", col = "red", bin_with=1)

boxplot(clean_posts_limited$time2, main = "histogram of posting time", xlab = "posting time")

summary(clean_posts_limited$time2)

# machine learning algo ---------------------------------------------------

#clean the DF 

DF <- clean_posts_limited %>%
  dplyr::select("title_clean", "ups", "time2", "index_author","num_comments")

# add sentiment -----------------------------------------------------------

# define it as good post when upvote >= 2 

#We use afinn library because the sentiment is split in different emotions
#But this one has the particularity to have a certain scale 

get_sentiments("afinn")

Title_sentiment <- DF %>% dplyr:: select("title_clean", "index_author")

Title_sentiment <- Title_sentiment %>%
  unnest_tokens(word, title_clean)

Title_sentiment <- Title_sentiment %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(index_author) %>%
  summarise(sentiment = sum(score)) 

DF1 <- inner_join(Title_sentiment, DF, by = "index_author")

summary(DF1$ups)
summary(DF1$num_comments)

#do people tend to post positive posts when they are going to work? 

ggplot(DF1) +
  geom_line(aes(time2, sentiment))+
  geom_smooth(aes(time2, sentiment))
# define “good” post ------------------------------------------------------

#let's define a good post according to no meana and no median (loyt of noisy data)

hist(DF1$num_comments, main = "histogram of comments", xlab = "number of comments", col = "red", bin_with=1)

hist(DF1$ups, main = "histogram of upvotes", xlab = "number of upvotes", col = "red", bin_with=1)

DF1 <- DF1 %>%
  mutate(categories = DF1$ups *0) 

for (i in 1:nrow(DF1)) { 
    if (DF1$ups[i] <= 1) {
      DF1$categories[i] <- -1
    } else if (DF1$ups[i] > 1) {
      DF1$categories[i] <- 1
    } else { "NA" 
    }
}       

length(which( DF1$categories == -1))
length(which( DF1$categories == 1))

hist(DF1$categories, main = "categories", xlab = "categories", col = "red", bin_with=1)

#add column with length of title 
DF1 <- DF1 %>%
  dplyr::mutate(length_of_title = DF1$ups *0)

class(DF1$title_clean)

for (i in 1 : nrow(DF1)) { 
  DF1$length_of_title[i] <- sum(nchar(DF1$title_clean[i])) 
} 


# logistic regression and perceptron 
# logistic regression as probabilistic way 
# LDA analysis and perceptron 
# Use perceptron first 

# Select the features and according categories 

DF <- DF1 %>%
  dplyr:: select("index_author", "time2", "length_of_title", "sentiment", "categories")

#start the machine learning algorithm 
# Parameters

eta = 0.001   # The learning rate
percTrain = 0.7 # Percentage of observations used for training

# Preparations ------------------------------------------------------------

#add the lda precition knowing Coefficients of linear discriminants:
#LD1
#sentiment       -0.3304467205
#time2            0.0223520284
#length_of_title  0.0003399004


# Select only 3 features, apart from time, length of title and sentiment 
features = names(DF)[2:4]

targetVar = "categories"

standFun = function(x){
  out = (x - mean(x))/sd(x)
  return(out)
}

DF[[features[1]]] = standFun(DF[[features[1]]])
DF[[features[2]]] = standFun(DF[[features[2]]])
DF[[features[3]]] = standFun(DF[[features[3]]])

# We select a percentage of the sample for training
iSplit = percTrain*nrow(DF)
Dtrain = DF[1:iSplit , ] # 70% of rows for training 

Dtest = DF[(iSplit+1):nrow(DF), ] #30% of rows for testing 

nTrain = nrow(Dtrain)

# Initialize learning process (training) ---------------------------------------------

indexList = rep(NA, nTrain) #Vector that replicates NA  
#by perceptron it is when -1 or 1 
epochs = 4
e = 0
wList = matrix(NA, ncol=4, nrow = nTrain * epochs)
colnames(wList) = c("w0", "w1", "w2", "w3") #wird die weights anzupassen 

# More empty containers
misclasList = indexList #initialize 
falPosList = indexList
falNegList = indexList

# The initial weights, initialized to 0 (= completely ignorant)
w = c(0,0,0,0) #random 

# Learning/Training ----------------------------------------------------------------
while(e < epochs) {
for (i in 1:nTrain) { # iteration 
  
  x_i = as.numeric(Dtrain[features][i, ])  # The x data from obs i from perimeter + concave (could take more, still linear)
  
  y_i = Dtrain$categories[i]  # The y data from obs i --> you get -1 or 1 
  
  index_i = w[1] + w[2]*x_i[1] + w[3]*x_i[2] + w[4]*x_i[3]
  # Dont'get confused about the weird indexing, 
  #R has no index 0  - with weights initiated as 0 
  
  pred_i = ifelse(index_i>=0, 1, -1)  
  
  # We are not only interested in the prediction to the current i
  # (which is used for updating the weights),
  # but how we would fare with the current i based on the ENTIRE training sample!
  
  Dtrain$index = w[1] + w[2]*Dtrain[[features[1]]] + w[3]*Dtrain[[features[2]]] + w[4]*Dtrain[[features[3]]] 

  #you take the whole data stored in the whole column (because two klammern)
  #and you try to see what is the misclassified rate for every weights applied to the whole column
  # then you adjust
  
  
  Dtrain$prediction = ifelse(Dtrain$index >=0, 1, -1)
  Dtrain$error  = Dtrain$categories - Dtrain$prediction
  
  # Ratio of misclassified cases
  misclas = round(sum(Dtrain$error!=0)/nrow(Dtrain)*100, digits = 2)
  
  # False positives
  x = ifelse(Dtrain$prediction == 1 & Dtrain$categories == -1, 1, 0)
  falPos = round(sum(x)/length(Dtrain$categories[Dtrain$categories == -1])*100, digits = 1)
  #rounden 
  
  # False negatives
  x = ifelse(Dtrain$prediction == -1 & Dtrain$categories == 1, 1, 0)
  falNeg = round(sum(x)/length(Dtrain$categories[Dtrain$categories == 1])*100, digits = 1)
  
  # Bookkeeping current w values, before they are updated
  wList[i, ] = w
  #liste von indexes letzte 3 Gewichte die wir haben 
  
  # UPDATING: this is the CORE ENGINE of the whole thing!!!
  
  w[-1] = w[-1] + eta*(y_i - pred_i) * x_i   # updating the weights for the two features
  
  w[1] = w[1] + eta*(y_i - pred_i)  # updating the constant/bias
  
  
  # Bookkeeping: Filling the containers
  
  indexList[i] = index_i # all three weights 
  misclasList[i] = misclas # misclas mit jedem Weight vom letztem iteration
  falPosList[i] = falPos # same
  falNegList[i] = falNeg # same
  
}
  e = e+ 1
}

wList = wList %>%
  as.tibble(.) %>%
  mutate(iter = 1:nTrain)


Y = tibble(iter = 1:nTrain, misclasList, falPosList, falNegList)

Y = Y %>% 
  inner_join(wList, by = "iter")

trainDiag = c(falPos = falPos, falNeg = falNeg, misclas = misclas)

# Plotting the learning process (training) -------------------------------------------

toPlot <- Y %>%
  gather(names(Y)[2:4], key = "series", value = "value")


toPlot <- toPlot %>%
  mutate(Legend = 
           sapply(series, switch,
                  misclasList = "Misclassified (all, in %)",
                  falPosList = "False positives (in %)",
                  falNegList = "False negatives (in %)"))


ggplot(toPlot, aes(iter, value)) +
  geom_line(aes(color = Legend, linetype = Legend, size = Legend, alpha = Legend))+ 
  scale_colour_manual(values = c("blue", "red3", "black")) + 
  scale_linetype_manual(values=c("solid", "solid", "solid")) + 
  scale_size_manual(values=c(1, 1, 1)) +
  scale_alpha_manual(values = c(0.5, 0.5, 0.5), guide = F) +
  labs(x = "Iteration", y = "Perc. of cases",
       title = "Perceptron learning: Training (reddit title data)",
       subtitle = paste0("Learning rate = ", eta, ", ", 
                         percTrain*100, " % of sample used for training")) + 
  theme_bw() +
  theme(legend.title=element_blank()) 

# Testing/evaluation ------------------------------------------------------

# Get the appropriate values for w
w = as.numeric(wList[nTrain, 1:4])


Dtest$index = w[1] + w[2]*Dtest[[features[1]]] + w[3]*Dtest[[features[2]]] + w[4]*Dtest[[features[3]]]


Dtest$prediction = ifelse(Dtest$index >=0, 1, -1)

Dtest$error  = Dtest$categories - Dtest$prediction

# Ratio of misclassified cases
misclas = round(sum(Dtest$error!=0)/nrow(Dtest)*100, digits = 2)
misclas

# False positives
x = ifelse(Dtest$prediction == 1 & Dtest$categories == -1, 1, 0)
falPos = round(sum(x)/length(Dtest$categories[Dtest$categories == -1])*100, digits = 1)
falPos

# False negatives
x = ifelse(Dtest$prediction == -1 & Dtest$categories == 1, 1, 0)
falNeg = round(sum(x)/length(Dtest$categories[Dtest$categories == 1])*100, digits = 1)
falNeg

testDiag = c(falPos = falPos, falNeg = falNeg, misclas = misclas)
testDiag




