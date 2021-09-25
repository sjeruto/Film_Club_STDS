#load packages
library(tidyverse)
library(wbstats)
library(lubridate)
library(stringr)
require(likert)


#import the happiness data
happiness <- read.csv('happiness.csv')

#Code to get countries data
income_level <- select(wb_countries(), country, income_level)
population <- wb_data("SP.POP.TOTL", start_date = 2020, end_date = 2021)
population <- population %>% select (country, SP.POP.TOTL)
Country_Data <- left_join(income_level, population)


#import culture questions
gender <- read.csv('GENDER_EQUALITY.csv')
lgbtqi <- read.csv('HOMOSEXUALITY.csv')
religion <- read.csv('RELIGION_IMPORT.csv')


#import the tmdb files
file1 <- read.csv('tmdb clean 1-126118 range.csv')
file2 <- read.csv('tmdb clean 126119-199999 range.csv')
file3 <- read.csv('tmdb clean 200000-299999 range.csv')
file4 <- read.csv('tmdb clean 300000-371114 range.csv')
file5 <- read.csv('tmdb clean 371114-399999 range.csv')
file6 <- read.csv('tmdb clean 400000-499999 range.csv')

#merge files
combined <- rbind2(file1, file2)
combined <- rbind2(combined, file3)
combined <- rbind2(combined, file4)
combined <- rbind2(combined, file5)
combined <- rbind2(combined, file6)

#create dummy variable for year of release
combined$release_date <- lubridate::dmy(combined$release_date)
combined$release_date[combined$release_date == ""] <- NA
combined$year <- year(combined$release_date)

#filter for last 10 years
tenyears <- combined %>%
  filter(year > 2010)

#select relevant data
tmdb <- tenyears %>%
  select(production_countries.name
         ,original_language
         ,budget
         ,genres.name
         ,adult)

#drop NA values
tmdb <- na.omit(tmdb)

#create a dummy variable for 
mature <- c("Action", "Crime", "Thriller", "Horror", "War")

#remove pornography
tmdbdummy <- tmdb %>%
  filter(adult == "FALSE")

tmdbdummy <- tmdb %>%
  mutate(maturecontent = ifelse(genres.name %in% mature, 1, 0))

tmdbdummy <- tmdbdummy %>%
  mutate(notmature = ifelse(tmdbdummy$mature == 0, 1, 0))

tmdbdummy$genres.name <- NULL
tmdbdummy$adult <- NULL

tmdb <- tmdbdummy %>%
  select(production_countries.name, budget, maturecontent, notmature) %>%
  group_by(production_countries.name) %>%
  summarise(across(everything(), list(mean = mean, sum = sum)))

tmdb$budget_sum <- NULL
tmdb$maturecontent_mean <- NULL
tmdb$notmature_mean <- NULL

tmdb$budget_mean <- round(tmdb$budget_mean / 1000, 2)

#pivot wider the survey responses
gender <- file7 %>%
  select(country, GENDER_EQUALITY, weighted_n)
gender_pivot <- pivot_wider(gender
                            ,id_cols = country
                            ,names_from = GENDER_EQUALITY
                            ,values_from = weighted_n)
names(gender_pivot) <- c("country", "gender_VI", "gender_SI", "gender_NTI", "gender_NIAA", "gender_DK", "gender_R")

#pivot wider the survey responses
lgbtqi <- file8 %>%
  select(country, HOMOSEXUALITY, weighted_n)
lgbtqi_pivot <- pivot_wider(lgbtqi
                            ,id_cols = country
                            ,names_from = HOMOSEXUALITY
                            ,values_from = weighted_n)
names(lgbtqi_pivot) <- c("country", "lgbtqi_Y", "lgbtqi_N", "lgbtqi_DK", "lgbtqi_R")

#pivot wider the survey responses
religion <- file9 %>%
  select(country, RELIGION_IMPORT, weighted_n)
religion_pivot <- pivot_wider(religion
                              ,id_cols = country
                              ,names_from = RELIGION_IMPORT
                              ,values_from = weighted_n)
names(religion_pivot) <- c("country", "religion_VI", "religion_SI", "religion_NTI", "religion_NIAA", "religion_DK", "religion_R")

#survey questions joined
survey_joined <- left_join(gender_pivot, lgbtqi_pivot, by= "country")
survey_joined <- left_join(survey_joined, religion_pivot, by= "country")

#convert country names to match for join
survey_joined$country <- str_replace(survey_joined$country, "United States", "United States of America")
happiness$ï..Country.name <- str_replace(happiness$ï..Country.name, "United States", "United States of America")

clean_country <- data.frame(lapply(Country_Data, function(x) {
  gsub("United States", "United States of America", x)
}))
clean_country <- data.frame(lapply(clean_country, function(x) {
  gsub("Slovak Republic", "Slovakia", x)
}))
clean_country <- data.frame(lapply(clean_country, function(x) {
  gsub("Korea, Rep.", "South Korea", x)
}))
clean_country <- data.frame(lapply(clean_country, function(x) {
  gsub("Russian Federation", "Russia", x)
}))


#join country_data with tmdb
tmdbjoined <- left_join(tmdb, happiness, by= c("production_countries.name" = "ï..Country.name"))

#join country_data with tmdb
tmdbjoined <- left_join(tmdbjoined, clean_country, by= c("production_countries.name" = "country"))

#join country_data with tmdb
tmdbjoined <- inner_join(tmdbjoined, survey_joined, by= c("production_countries.name" = "country"))


#combine "Don't Know" & "Refused" to answer for all culture questions
tmdbjoined$gender_Neut <- (tmdbjoined$gender_DK + tmdbjoined$gender_R)
tmdbjoined$lgbtqi_Neut <- (tmdbjoined$lgbtqi_DK + tmdbjoined$lgbtqi_R)
tmdbjoined$religion_Neut <- (tmdbjoined$religion_DK + tmdbjoined$religion_R)

tmdbjoined$gender_DK <- NULL
tmdbjoined$gender_R <- NULL
tmdbjoined$lgbtqi_DK <- NULL
tmdbjoined$lgbtqi_R <- NULL
tmdbjoined$religion_DK <- NULL
tmdbjoined$religion_R <- NULL


#combine "Very Important" & "Somewhat Important" to answer for all culture questions
tmdbjoined$gender_Y <- (tmdbjoined$gender_VI + tmdbjoined$gender_SI)
tmdbjoined$religion_Y <- (tmdbjoined$religion_VI + tmdbjoined$religion_SI)

tmdbjoined$gender_VI <- NULL
tmdbjoined$gender_SI <- NULL
tmdbjoined$religion_VI <- NULL
tmdbjoined$religion_SI <- NULL

#combine "Very Important" & "Somewhat Important" to answer for all culture questions
tmdbjoined$gender_N <- (tmdbjoined$gender_NTI + tmdbjoined$gender_NIAA)
tmdbjoined$religion_N <- (tmdbjoined$religion_NTI + tmdbjoined$religion_NIAA)

tmdbjoined$gender_NTI <- NULL
tmdbjoined$gender_NIAA <- NULL
tmdbjoined$religion_NTI <- NULL
tmdbjoined$religion_NIAA <- NULL

tmdbjoined <- tmdbjoined[,c(1,2,3,4,5,6,10,11,12,13,24,25,26,29,27,31,28,33,32,30,34)]

tmdbjoined$lgbtqi_Neut[is.na(tmdbjoined$lgbtqi_Neut)] <- 0
tmdbjoined$total_survey <- (tmdbjoined$lgbtqi_Y + tmdbjoined$lgbtqi_N + tmdbjoined$lgbtqi_Neut)
tmdbjoined$SP.POP.TOTL <- as.numeric(as.character(tmdbjoined$SP.POP.TOTL))
tmdbjoined$quotient <-  tmdbjoined$SP.POP.TOTL / 1000
tmdbjoined$extrapolated_genderY <- tmdbjoined$gender_Y * (tmdbjoined$SP.POP.TOTL / tmdbjoined$total_survey)
tmdbjoined$per_thousand_genderY <- tmdbjoined$extrapolated_genderY / tmdbjoined$quotient
tmdbjoined$extrapolated_lgbtqiY <- tmdbjoined$lgbtqi_Y * (tmdbjoined$SP.POP.TOTL / tmdbjoined$total_survey)
tmdbjoined$per_thousand_lgbtqiY <- tmdbjoined$extrapolated_lgbtqiY / tmdbjoined$quotient
tmdbjoined$extrapolated_religionY <- tmdbjoined$religion_Y * (tmdbjoined$SP.POP.TOTL / tmdbjoined$total_survey)
tmdbjoined$per_thousand_religionY <- tmdbjoined$extrapolated_religionY / tmdbjoined$quotient


model_glm_3 <- glm(cbind(maturecontent_sum, notmature_sum)~ income_level + budget_mean 
                   + per_thousand_lgbtqiY + per_thousand_genderY + per_thousand_religionY
                   + Ladder.score + Freedom.to.make.life.choices + Logged.GDP.per.capita + Healthy.life.expectancy
                   + Social.support, 
                   family=binomial, data=tmdbjoined)

model_glm_3

plot(model_glm_3)