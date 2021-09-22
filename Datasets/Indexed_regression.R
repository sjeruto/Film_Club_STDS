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
```
# Load data 
df_GE <- read.csv("GENDER_EQUALITY.csv")
df_GE

## checking the structure of data
str(df_GE)

## SELECT HIGHEST WEIGHTED ESTIMATE
new_df <- df_GE%>%
          group_by(country) %>%
          summarise(weighted_estimate=max(weighted_estimate))

##MERGE
mpya <- merge (df_GE, new_df)
Gender_df <- mpya[,-c(2,3,5,6)]

########Load lgbtqi data
df_LGBTQI<- read.csv("HOMOSEXUALITY.csv")
df_LGBTQI

##
new_df_1 <- df_LGBTQI%>%
  group_by(country) %>%
  summarise(weighted_estimate=max(weighted_estimate))

##MERGE
mpya_1 <- merge (df_LGBTQI, new_df_1)
#Delete column

LGBTQI_df <- mpya_1[,-c(2,3,5,6)]

######## Load religioin data
df_Religion<- read.csv("RELIGION_IMPORT.csv")
df_Religion

##
new_df_2 <- df_Religion%>%
  group_by(country) %>%
  summarise(weighted_estimate=max(weighted_estimate))

##MERGE
mpya_2 <- merge (df_Religion, new_df_2)

####To delete a column
Religion_df <- mpya_2[,-c(2,3,5,6)]

####Merge the 3 dataframes;
tmdb_nolanguage<- read.csv("tmdb_nolanguage.csv")

Merged_df <- merge(merge(Religion_df, LGBTQI_df), Gender_df)

New_merged_df <- merge(tmdb_nolanguage,Merged_df)

#Drop columns;
DF_Fy<- New_merged_df[,-c(1,6,8,9,10,11,12,13,14,15,16,17)]

str(DF_Fy)

(l <- sapply(DF_Fy, function(x) is.factor(x)))
m <- DF_Fy[, l]
ifelse(n <- sapply(m, function(x) length(levels(x))) == 1, "DROP", "NODROP")

summary(DF_Fy)
#### Run the logistic regression
model_glm_1 <- glm(cbind(maturecontent_sum, notmature_sum) ~ income_level 
                 + budget_mean + HOMOSEXUALITY + RELIGION_IMPORT, family=binomial, data=DF_Fy)

model_glm_1
summary(model_glm_1)

##Write csv
write.csv(DF_Fy, "Indexed.csv")
