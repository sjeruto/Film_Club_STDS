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
library(car)
#install.packages("gam")
library(gam)
library(mgcv)
#install.packages("olsrr")
library(olsrr)

#tmdb data
tmdb_nolanguage<- read.csv("tmdb_nolanguage.csv")

#happiness data
happiness<- read.csv("happiness.csv")
happiness$ï..Country.name <- str_replace(happiness$ï..Country.name, "United States", "United States of America")


#population data
population <- wb_data("SP.POP.TOTL", start_date = 2020, end_date = 2021)
population <- population %>% select (country, SP.POP.TOTL)
population <- data.frame(lapply(population, function(x) {
  gsub("United States", "United States of America", x)
}))
population <- data.frame(lapply(population, function(x) {
  gsub("Slovak Republic", "Slovakia", x)
}))
population <- data.frame(lapply(population, function(x) {
  gsub("Korea, Rep.", "South Korea", x)
}))
population <- data.frame(lapply(population, function(x) {
  gsub("Russian Federation", "Russia", x)
}))


# Load gender data 
Gender_df<- read.csv("GENDER_EQUALITY.csv")
Gender_df


#pivot wider the survey responses
gender <- Gender_df %>%
  select(country, GENDER_EQUALITY, weighted_n)
gender_pivot <- pivot_wider(gender
                            ,id_cols = country
                            ,names_from = GENDER_EQUALITY
                            ,values_from = weighted_n)
names(gender_pivot) <- c("country", "gender_VI", "gender_SI", "gender_NTI", "gender_NIAA", "gender_DK", "gender_R")

gender_pivot$gender_Neut <- (gender_pivot$gender_DK + gender_pivot$gender_R)
gender_pivot$gender_DK <- NULL
gender_pivot$gender_R <- NULL
gender_pivot$gender_Y <- (gender_pivot$gender_VI + gender_pivot$gender_SI)
gender_pivot$gender_VI <- NULL
gender_pivot$gender_SI <- NULL
gender_pivot$gender_N <- (gender_pivot$gender_NTI + gender_pivot$gender_NIAA)
gender_pivot$gender_NTI <- NULL
gender_pivot$gender_NIAA <- NULL



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
write.csv(gender_pivot, "Gender_New.csv")



####Merge the 3 dataframes;
Religion_LGBTQI_Gender <- merge(merge(Religion_New, LGBTQI_New), gender_pivot)

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
New_Religion_LGBTQI_Gender <- left_join(New_Religion_LGBTQI_Gender, happiness, by= c("production_countries.name" = "ï..Country.name"))
New_Religion_LGBTQI_Gender <- New_Religion_LGBTQI_Gender[,c(1,2,3,4,5,6,7,8,9,10,11,12,16,17,18,19,20,21)]
New_Religion_LGBTQI_Gender <- left_join(New_Religion_LGBTQI_Gender, population, by= c("production_countries.name" = "country"))


New_Religion_LGBTQI_Gender$gender_Neut[is.na(New_Religion_LGBTQI_Gender$gender_Neut)] <- 0
New_Religion_LGBTQI_Gender$total_survey <- (New_Religion_LGBTQI_Gender$gender_Neut + New_Religion_LGBTQI_Gender$gender_N + New_Religion_LGBTQI_Gender$gender_Y)
New_Religion_LGBTQI_Gender$SP.POP.TOTL <- as.numeric(as.character(New_Religion_LGBTQI_Gender$SP.POP.TOTL))
New_Religion_LGBTQI_Gender$quotient <-  New_Religion_LGBTQI_Gender$SP.POP.TOTL / 1000
New_Religion_LGBTQI_Gender$extrapolated_genderY <- New_Religion_LGBTQI_Gender$gender_Y * (New_Religion_LGBTQI_Gender$SP.POP.TOTL / New_Religion_LGBTQI_Gender$total_survey)
New_Religion_LGBTQI_Gender$per_thousand_genderY <- New_Religion_LGBTQI_Gender$extrapolated_genderY / New_Religion_LGBTQI_Gender$quotient


## convert budget mean to millions
New_Religion_LGBTQI_Gender$budget_mean <- round(New_Religion_LGBTQI_Gender$budget_mean / 1000, 2)
New_Religion_LGBTQI_Gender$gender_Neut <-NULL
New_Religion_LGBTQI_Gender$gender_N <-NULL
New_Religion_LGBTQI_Gender$gender_Y <-NULL

## test for linearity in the continuous variables & logit
gam_mod <- gam(cbind(maturecontent_sum, notmature_sum) ~ 
               #s(Ladder.score, bs = 'cr', k = 6) 
                s(Freedom.to.make.life.choices, bs = 'cr', k = 6)
               + s(Logged.GDP.per.capita, bs = 'cr', k = 6)
               #+ s(Healthy.life.expectancy, bs = 'cr', k = 6)
               + s(Social.support, bs = 'cr', k = 6)
               + s(budget_mean, bs = 'cr', k = 6)
               + s(per_thousand_genderY, bs = 'cr', k = 6)
               , data=New_Religion_LGBTQI_Gender
               , method = "REML"
               , family = binomial("logit"))

gam.check(gam_mod)
plot(gam_mod, pages =1, all.terms = TRUE, residuals = TRUE, pch = 1, cex = 1, shade = TRUE, shade.col = "lightpink", shift = coef(gam_mod)[1])
summary(gam_mod)

#test for multicollinearity
cor(New_Religion_LGBTQI_Gender[,c("Freedom.to.make.life.choices","Logged.GDP.per.capita","Social.support", "budget_mean", "per_thousand_genderY")])
multico<- lm(Freedom.to.make.life.choices ~ Logged.GDP.per.capita + Social.support + budget_mean + per_thousand_genderY, data = New_Religion_LGBTQI_Gender) 
summary(multico)
ols_coll_diag(multico)

# Assessing Outliers

outlierTest(fit) # Bonferonni p-value for most extreme obs
qqPlot(fit, main="QQ Plot") #qq plot for studentized resid
leveragePlots(fit) # leverage plots


#### Run the logistic regression
model_glm_2 <- glm(cbind(maturecontent_sum, notmature_sum) ~ income_level 
                   + budget_mean + I(RELIGION_IMPORT) + I(HOMOSEXUALITY), family=binomial, data=New_Religion_LGBTQI_Gender)
summary(model_glm_2)


model_glm_3 <- glm(cbind(maturecontent_sum, notmature_sum) ~ income_level 
                   + Ladder.score + Freedom.to.make.life.choices + Logged.GDP.per.capita + Healthy.life.expectancy
                   + Social.support
                   + budget_mean + I(RELIGION_IMPORT) + I(HOMOSEXUALITY) + extrapolated_genderY, family=binomial, data=New_Religion_LGBTQI_Gender)
summary(model_glm_3)

