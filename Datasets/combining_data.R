#load packages
library(tidyverse)
library(wbstats)
library(lubridate)

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


#Code to get countries data
Country_Data <- select(wb_countries(),iso3c, country, income_level)

#import culture questions
file7 <- read.csv('GENDER_EQUALITY.csv')
file8 <- read.csv('HOMOSEXUALITY.csv')
file9 <- read.csv('RELIGION_IMPORT.csv')

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
survey_joined <- data.frame(lapply(survey_joined, function(x) {
  gsub("United States", "United States of America", x)
}))
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
tmdbjoined <- left_join(tmdb, clean_country, by= c("production_countries.name" = "country"))

#join country_data with tmdb
tmdbjoined <- inner_join(tmdbjoined, survey_joined, by= c("production_countries.name" = "country"))

## write to csv
write.csv(tmdbjoined, "tmdbjoined.csv")

#create a dummy variable
mature <- c("Action", "Crime", "Thriller", "Horror", "War")
tmdbdummy <- tmdbjoined %>%
  mutate(dummy = ifelse(genres.name %in% mature, 1, 0))

tmdbdummy$genres.name <- NULL
tmdbdummy$adult <- NULL


## write to csv
write.csv(tmdbdummy, "tmdbdummy.csv")
