library(tidyverse)

#import the tmdb files
tmdbjoined <- read.csv('tmdbjoined.csv')

data <- tmdbjoined %>% select(
  genres.name
  ,production_countries.name
  ,original_language
  ,income_level
  ,budget
  ,gender_VI
  ,gender_SI
  ,gender_NTI
  ,gender_NIAA
  ,gender_DK
  ,gender_R)

#convert adult to binary
levels(data$genres.name)


glm1 = glm(adult ~ production_countries.name + gender_VI, family = binomial(logit), data = data)
summary(glm1)
