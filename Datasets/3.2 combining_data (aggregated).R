#load packages
library(tidyverse)
library(wbstats)
library(lubridate)
library(stringr)
require(likert)

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

#create a dummy variable for 
mature <- c("Action", "Crime", "Thriller", "Horror", "War")

tmdbdummy <- tmdb %>%
  mutate(maturecontent = ifelse(genres.name %in% mature | adult == "TRUE", 1, 0))

tmdbdummy <- tmdbdummy %>%
  mutate(notmature = ifelse(tmdbdummy$mature == 0, 1, 0))

tmdbdummy$genres.name <- NULL
tmdbdummy$adult <- NULL

#aggregate the tmdb data
tmdb <- tmdbdummy %>%
  select(production_countries.name, budget, maturecontent, notmature) %>%
  group_by(production_countries.name) %>%
  summarise(across(everything(), list(mean = mean, sum = sum)))

tmdb$budget_sum <- NULL
tmdb$maturecontent_mean <- NULL
tmdb$notmature_mean <- NULL


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
write.csv(tmdbjoined, "tmdbjoined_nolanguage.csv")


tmdbdummy <- tmdbjoined

#combine "Don't Know" & "Refused" to answer for all culture questions
tmdbdummy$gender_Neut <- (tmdbdummy$gender_DK + tmdbdummy$gender_R)
tmdbdummy$lgbtqi_Neut <- (tmdbdummy$lgbtqi_DK + tmdbdummy$lgbtqi_R)
tmdbdummy$religion_Neut <- (tmdbdummy$religion_DK + tmdbdummy$religion_R)

tmdbdummy$gender_DK <- NULL
tmdbdummy$gender_R <- NULL
tmdbdummy$lgbtqi_DK <- NULL
tmdbdummy$lgbtqi_R <- NULL
tmdbdummy$religion_DK <- NULL
tmdbdummy$religion_R <- NULL


#combine "Very Important" & "Somewhat Important" to answer for all culture questions
tmdbdummy$gender_Y <- (tmdbdummy$gender_VI + tmdbdummy$gender_SI)
tmdbdummy$religion_Y <- (tmdbdummy$religion_VI + tmdbdummy$religion_SI)

tmdbdummy$gender_VI <- NULL
tmdbdummy$gender_SI <- NULL
tmdbdummy$religion_VI <- NULL
tmdbdummy$religion_SI <- NULL

#combine "Very Important" & "Somewhat Important" to answer for all culture questions
tmdbdummy$gender_N <- (tmdbdummy$gender_NTI + tmdbdummy$gender_NIAA)
tmdbdummy$religion_N <- (tmdbdummy$religion_NTI + tmdbdummy$religion_NIAA)

tmdbdummy$gender_NTI <- NULL
tmdbdummy$gender_NIAA <- NULL
tmdbdummy$religion_NTI <- NULL
tmdbdummy$religion_NIAA <- NULL


## write to csv
write.csv(tmdbdummy, "tmdb_nolanguage.csv")
write.csv(combined, "combined.csv")

#unique countries
countries <- unique(tmdbdummy$production_countries.name)




#plot
ggplot(tmdb_nolanguage, aes(maturecontent_sum/(maturecontent_sum + notmature_sum), budget_mean)) + 
  geom_point(alpha = 2/10) +
  scale_color_manual(values=c('darkred','navy'))+
  theme_minimal()+
  ggtitle("mature v budget")+
  xlab("mature content") + 
  ylab("budget_mean")+
  theme(text=element_text(size=14,family="CM Roman"),  legend.position="none")
