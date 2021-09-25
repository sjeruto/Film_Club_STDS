#load packages
library(tidyverse)
library(wbstats)
library(lubridate)
library(stringr)
require(likert)


#import the tmdb files
tmdb_language <- read.csv('tmdb_language.csv')
tmdb_nolanguage <- read.csv('tmdb_nolanguage.csv')
tmdb_individual <- read.csv('tmdb_individual.csv')




#way1
glm(maturecontent_sum/(maturecontent_sum+notmature_sum) ~ income_level 
    + budget_mean 
    + gender_Y 
    + gender_N 
    + gender_Neut
    + lgbtqi_Y
    + lgbtqi_N
    + lgbtqi_Neut
    + religion_Y
    + religion_N
    + religion_Neut, family=binomial, data=tmdb_nolanguage, weights=(maturecontent_sum+notmature_sum)) %>%
  summary()


#way2
glm(cbind(maturecontent_sum,notmature_sum)~ income_level 
    + budget_mean 
    + gender_Y 
    + gender_N 
    + gender_Neut
    + lgbtqi_Y
    + lgbtqi_N
    + lgbtqi_Neut
    + religion_Y
    + religion_N
    + religion_Neut, family=binomial, data=tmdb_nolanguage) %>%
  summary()


#way3
glm(mature ~ income_level 
    + budget 
    + gender_Y 
    + gender_N 
    + gender_Neut
    + lgbtqi_Y
    + lgbtqi_N
    + lgbtqi_Neut
    + religion_Y
    + religion_N
    + religion_Neut, family=binomial, data=tmdb_individual) %>%
  summary()
