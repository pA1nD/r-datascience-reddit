
rm(list=ls())

install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
library(MASS)
library(dplyr)
library(modelr)
library(forcats)

#load data set
library(readr)
clean_posts <- read_csv("data/clean_posts.csv")
View(clean_posts)

clean_posts <- clean_posts %>%
  mutate(index_author = 1:nrow(clean_posts))

# exploration: correlation coef for some variables  -----------------------

new_clean_2variables <- clean_posts %>%
  dplyr::select(ups, time_passed_days) 

ggplot(new_clean_2variables)+ 
  geom_point(aes(time_passed_days, ups))+
  geom_smooth(mapping = aes(time_passed_days, ups))

#Regression
fit <- lm(ups ~ time_passed_days, data = new_clean_2variables)
summary(fit)

ggplot(new_clean_2variables, aes(x = time_passed_days, y = ups)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

#essai

ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

fit1 <- lm(ups ~ time_passed_days, data = new_clean_2variables)
ggplotRegression(fit1)



