library(tidyverse)

#import the tmdb files
tmdbdummy <- read.csv('tmdbdummy.csv')

data <- tmdbdummy %>% select(
  dummy
  ,production_countries.name
  ,original_language
  ,income_level
  ,budget
  ,gender_Y
  ,gender_N  
  ,gender_Neut
  ,lgbtqi_Y
  ,lgbtqi_N
  ,lgbtqi_Neut
  ,religion_Y
  ,religion_N
  ,religion_Neut)


glm1 = glm(dummy ~ . -production_countries.name -original_language, family = binomial(logit), data = data, na.action = na.exclude)
summary(glm1)


