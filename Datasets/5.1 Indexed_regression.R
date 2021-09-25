##Load the necessary packages
library(tidyverse)
library(dplyr)
library(broom)
theme_set(theme_classic())
library(tidyverse)
library(wbstats)
library(lubridate)
library(stringr)
require(likert)

# Load gender data 
Gender_df<- read.csv("GENDER_EQUALITY.csv")
Gender_df



## SELECT HIGHEST WEIGHTED ESTIMATE
new_df1 <- Gender_df%>%
  group_by(country) %>%
  summarise(weighted_estimate=max(weighted_estimate))



### MERGE the two datasets with country as unique identifier
Gender_New<- merge (Gender_df, new_df1)
Gender_New <- Gender_New[,-c(2,3,5,6)]



########Load lgbtqi data
LGBTQI_df<- read.csv("HOMOSEXUALITY.csv")
LGBTQI_df



## SELECT HIGHEST WEIGHTED ESTIMATE
new_df2 <- LGBTQI_df%>%
  group_by(country) %>%
  summarise(weighted_estimate=max(weighted_estimate))



### MERGE the two datasets with country as unique identifier
LGBTQI_New<- merge (LGBTQI_df, new_df2)
LGBTQI_New <- LGBTQI_New[,-c(2,3,5,6)]




######## Load religioin data
Religion_df1<- read.csv("RELIGION_IMPORT.csv")
Religion_df1



## SELECT HIGHEST WEIGHTED ESTIMATE
new_df3 <- Religion_df1%>%
  group_by(country) %>%
  summarise(weighted_estimate=max(weighted_estimate))



### MERGE the two datasets with country as unique identifier
Religion_New<- merge (Religion_df1, new_df3)
Religion_New <- Religion_New[,-c(2,3,5,6)]



##Write csv
write.csv(Religion_New, "Religion_New.csv")
write.csv(LGBTQI_New, "LGBTQI_New.csv")
write.csv(Gender_New, "Gender_New.csv")



####Merge the 3 dataframes;



Religion_LGBTQI_Gender <- merge(merge(Religion_New, LGBTQI_New), Gender_New)

#fix country names to match across all datasets
#convert country names to match for join
Religion_LGBTQI_Gender$country <- str_replace(Religion_LGBTQI_Gender$country, "United States", "United States of America")



## Rename column countries to production_countries.name so we can merge the data based on production_countries.name
Religion_LGBTQI_Gender <- Religion_LGBTQI_Gender %>%
  rename(production_countries.name = country)



## Drop columns in the tmdb_nolanguage data
tmdb_dropped_columns <- tmdb_nolanguage[,-c(1,8,9,10,11,12,13,14,15,16)]
tmdb_dropped_columns <- tmdb_dropped_columns[,-c(5)]





##Final merge data; indexes and tmdb data;
New_Religion_LGBTQI_Gender <- merge(tmdb_dropped_columns,Religion_LGBTQI_Gender)

## convert budget mean to millions
New_Religion_LGBTQI_Gender$budget_mean <- round(New_Religion_LGBTQI_Gender$budget_mean / 1000, 2)



#### Run the logistic regression
model_glm_2 <- glm(cbind(maturecontent_sum, notmature_sum)~ income_level 
                   + budget_mean + I(RELIGION_IMPORT) + I(HOMOSEXUALITY), family=binomial, data=New_Religion_LGBTQI_Gender)



model_glm_2

