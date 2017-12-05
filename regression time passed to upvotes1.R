
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

clean_posts <- clean_posts %>%
  mutate(index_author = 1:nrow(clean_posts))

# exploration: correlation coef for some variables  -----------------------

new_clean_2variables <- clean_posts %>%
  dplyr::select(ups, time_passed_days) %>%
  filter(time_passed_days < 21) %>%
  filter(ups > 1)  %>%
  mutate(ups = log(ups))

  ggplot(new_clean_2variables) + 
    geom_point(aes(time_passed_days, ups))+
    geom_smooth(mapping = aes(time_passed_days, ups))

#Regression
fit <- lm(ups ~ time_passed_days, data = new_clean_2variables)
summary(fit)

ggplot(new_clean_2variables, aes(x = time_passed_days, y = ups)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

