#-----STEP 1: PREPARE ENVIRONMENT                                          ####

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
library(performance)



#-----STEP 2: PREPARE TMDB DATA                                            ####

#import TMDB data
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

#select variables to be used in the model
tmdb <- tenyears %>%
  select(production_countries.name
         ,original_language
         ,budget
         ,genres.name
         ,adult)

#drop NA values
tmdb <- na.omit(tmdb)

#create a definition of mature film genres
mature <- c("Action", "Crime", "Thriller", "Horror", "War")

#remove pornography from the dataset
tmdbdummy <- tmdb %>%
  filter(adult == "FALSE")

#create a dummy variable for mature & not mature content
tmdbdummy <- tmdb %>%
  mutate(maturecontent = ifelse(genres.name %in% mature, 1, 0))
tmdbdummy <- tmdbdummy %>%
  mutate(notmature = ifelse(tmdbdummy$mature == 0, 1, 0))

#drop columns no longer required
tmdbdummy$genres.name <- NULL
tmdbdummy$adult <- NULL

#aggregate the dataset and drop new columns not required
tmdb <- tmdbdummy %>%
  select(production_countries.name, budget, maturecontent, notmature) %>%
  group_by(production_countries.name) %>%
  summarise(across(everything(), list(mean = mean, sum = sum)))

tmdb$budget_sum <- NULL
tmdb$maturecontent_mean <- NULL
tmdb$notmature_mean <- NULL

# convert budget variable to '000s
tmdb$budget_mean <- round(tmdb$budget_mean / 1000, 2)



#-----STEP 3: PREPARE CULTURAL DATA                                        ####

#import culture questions
gender <- read.csv('GENDER_EQUALITY.csv')
lgbtqi <- read.csv('HOMOSEXUALITY.csv')
religion <- read.csv('RELIGION_IMPORT.csv')

#pivot wider the survey responses
gender <- gender %>%
  select(country, GENDER_EQUALITY, weighted_n)
gender_pivot <- pivot_wider(gender
                            ,id_cols = country
                            ,names_from = GENDER_EQUALITY
                            ,values_from = weighted_n)
names(gender_pivot) <- c("country", "gender_VI"
                         , "gender_SI", "gender_NTI"
                         , "gender_NIAA", "gender_DK"
                         , "gender_R")


# Select the highest weighted estimate
new_df <- lgbtqi%>%
  group_by(country) %>%
  summarise(weighted_estimate=max(weighted_estimate))

### merge the two datasets with country as unique identifier
lgbtqi<- merge (lgbtqi, new_df)
lgbtqi <- lgbtqi[,-c(2,3,5,6)]

# Select the highest weighted estimate
new_df <- religion%>%
  group_by(country) %>%
  summarise(weighted_estimate=max(weighted_estimate))

### merge the two datasets with country as unique identifier
religion<- merge (religion, new_df)
religion <- religion[,-c(2,3,5,6)]

#survey questions joined
survey_joined <- left_join(gender_pivot, lgbtqi, by= "country")
survey_joined <- left_join(survey_joined, religion, by= "country")


#convert country names to match for join
survey_joined$country <- str_replace(survey_joined$country
                                     , "United States"
                                     , "United States of America")




#-----STEP 4: PREPARE COUNTRY DATA                                        ####

#Code to get countries data
income_level <- select(wb_countries(), country, income_level)
population <- wb_data("SP.POP.TOTL", start_date = 2020, end_date = 2021)
population <- population %>% select (country, SP.POP.TOTL)
Country_Data <- left_join(income_level, population)

#convert country names to match for join
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


#-----STEP 5: PREPARE HAPPINESS DATA                                       ####
#import the happiness data
happiness <- read.csv('happiness.csv')

#convert country names to match for join
happiness$ï..Country.name <- str_replace(happiness$ï..Country.name
                                         , "United States"
                                         , "United States of America")

#-----STEP 6: PREPARE HOFSTEDE DATA                                        ####
#import the happiness data
hofstede <- read.csv('hofstedes culture dimensions.csv', sep =";", stringsAsFactors = FALSE)

#convert country names to match for join
hofstede$country <- str_replace(hofstede$country
                                         , "U.S.A."
                                         , "United States of America")
hofstede$country <- str_replace(hofstede$country
                                , "Africa East"
                                , "Kenya")
hofstede$country <- str_replace(hofstede$country
                                , "Africa West"
                                , "Tunisia")
hofstede$country <- str_replace(hofstede$country
                                , "Slovak Rep"
                                , "Slovakia")
hofstede$country <- str_replace(hofstede$country
                                , "Czech Rep"
                                , "Czech Republic")
hofstede$country <- str_replace(hofstede$country
                                , "Korea South"
                                , "South Korea")
hofstede$country <- str_replace(hofstede$country
                                , "Arab countries"
                                , "Lebanon")
hofstede$country <- str_replace(hofstede$country
                                , "Great Britain"
                                , "United Kingdom")

hofstede <- hofstede[,c(2:8)]


#-----STEP 7: MERGE ALL DATA                                               ####
#join country_data with tmdb
tmdbjoined <- left_join(tmdb, happiness
                        , by= c("production_countries.name" = "ï..Country.name"))

#join country_data with tmdb
tmdbjoined <- left_join(tmdbjoined, clean_country
                        , by= c("production_countries.name" = "country"))

#join country_data with tmdb
tmdbjoined <- inner_join(tmdbjoined, survey_joined
                         , by= c("production_countries.name" = "country"))

#join country_data with tmdb
tmdbjoined <- inner_join(tmdbjoined, hofstede
                         , by= c("production_countries.name" = "country"))



#-----STEP 8: CONSTRUCT VARIABLES                                          ####

#combine "Don't Know" & "Refused to answer" for all culture questions
tmdbjoined$gender_Neut <- (tmdbjoined$gender_DK + tmdbjoined$gender_R)

#drop responses no longer required
tmdbjoined$gender_DK <- NULL
tmdbjoined$gender_R <- NULL

#combine "Very Important" & "Somewhat Important" for all culture questions
tmdbjoined$gender_Y <- (tmdbjoined$gender_VI + tmdbjoined$gender_SI)

#drop responses no longer required
tmdbjoined$gender_VI <- NULL
tmdbjoined$gender_SI <- NULL

#combine "Not That Important" & "Not Important at all" for all culture questions
tmdbjoined$gender_N <- (tmdbjoined$gender_NTI + tmdbjoined$gender_NIAA)

#drop responses no longer required
tmdbjoined$gender_NTI <- NULL
tmdbjoined$gender_NIAA <- NULL

#drop other columns not required
colnames(tmdbjoined)
tmdbjoined <- tmdbjoined[,c("production_countries.name"
                            ,"maturecontent_sum"
                            ,"notmature_sum"
                            ,"budget_mean"
                            ,"Ladder.score"
                            ,"Logged.GDP.per.capita"                      
                            ,"Social.support"                             
                            ,"Healthy.life.expectancy"                   
                            ,"Freedom.to.make.life.choices"               
                            ,"Generosity"                                 
                            ,"Perceptions.of.corruption"
                            ,"income_level"                              
                            ,"SP.POP.TOTL"                                
                            ,"pdi"                                        
                            ,"idv"                                        
                            ,"mas"                                       
                            ,"uai"                                        
                            ,"ltowvs"                                     
                            ,"ivr"
                            ,"gender_Neut"                                
                            ,"gender_Y"                                   
                            ,"gender_N"
                            ,"HOMOSEXUALITY"                              
                            ,"RELIGION_IMPORT")]

#convert NA response counts to 0
tmdbjoined$gender_Neut[is.na(tmdbjoined$gender_Neut)] <- 0

#calculate a survey total respondents column
tmdbjoined$total_survey <- (tmdbjoined$gender_Y + tmdbjoined$gender_N + tmdbjoined$gender_Neut)


#convert survey responses to population total
tmdbjoined$SP.POP.TOTL <- as.numeric(as.character(tmdbjoined$SP.POP.TOTL))
tmdbjoined$extrapolated_genderY <- tmdbjoined$gender_Y * (tmdbjoined$SP.POP.TOTL / tmdbjoined$total_survey)

#remove unneeded responses
tmdbjoined$gender_Neut <- NULL
tmdbjoined$gender_N <- NULL
tmdbjoined$gender_Y <- NULL
tmdbjoined$total_survey <- NULL


#-----STEP 9: CHECK MODEL ASSUMPTIONS                                      ####

#generate the model
model_glm_4 <- glm(cbind(maturecontent_sum, notmature_sum)~ 
                     income_level
                   + HOMOSEXUALITY 
                   + RELIGION_IMPORT
                   + extrapolated_genderY
                   + pdi                                        
                   #+ idv                                        
                   #+ mas                                       
                   #+ uai                                        
                   + ltowvs                                     
                   + ivr
                   #+ Ladder.score - exclude this is made up of the other scores below 
                   #+ Freedom.to.make.life.choices 
                   #+ Generosity
                   #+ Healthy.life.expectancy - exclude this no logical reason why this would play a role
                   #+ Social.support - exclude this social support wouldn't effect movie production unless it related to grants not included here
                   #+ Logged.GDP.per.capita
                   + budget_mean 
                   #+ Generosity - not sure how giving would impact movie production                                 
                   #+ Perceptions.of.corruption # could relate to propaganda
                   #+ SP.POP.TOTL # country population                             
                   ,family=binomial, data=tmdbjoined)


summary(model_glm_4)

plot(model_glm_4, pages =1, all.terms = TRUE, pch = 1)

# Predict the probability (p) of mature films production
tmdbjoined$probabilities <- predict(model_glm_4, tmdbjoined, type = "response") ##34 values
tmdbjoined$predicted.classes <- ifelse(tmdbjoined$probabilities > 0.2, "pos", "neg")


######    To check for linearity

# Select only numeric predictors
mydata <- tmdbjoined %>%
  dplyr::select_if(is.numeric) 
predictors <- colnames(mydata)

# Bind the logit and tidying the data for plot
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

# Create scatter plot
ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")



######    To check for multi-collinearity

multico <- na.omit(tmdbjoined)

cor(multico[,c("pdi"                                        
  ,"idv"                                        
  , "mas"                                       
  , "uai"                                        
  , "ltowvs"                                     
  , "ivr"
  #, "Ladder.score" - remove, biproduct of other variables
  , "Freedom.to.make.life.choices" 
  , "Healthy.life.expectancy"
  , "Social.support"
  #, "Logged.GDP.per.capita" -  remove, multicollinear 
  , "budget_mean" 
  , "Generosity"                                
  , "Perceptions.of.corruption"
  #, "SP.POP.TOTL" - remove, basis for next variable
  , "extrapolated_genderY"
  )])

multico1<- lm(pdi ~ idv + mas + uai + ltowvs + ivr 
             #+ Ladder.score #- remove, biproduct of other variables
             + Freedom.to.make.life.choices
             #+ Healthy.life.expectancy#-  remove, multicollinear 
             #+ Social.support         #-  remove, multicollinear
             #+ Logged.GDP.per.capita  #-  remove, multicollinear 
             + budget_mean
             + Generosity 
             + Perceptions.of.corruption 
             #+ SP.POP.TOTL #- remove, basis for next variable
             + extrapolated_genderY
             + RELIGION_IMPORT
             + income_level
             + HOMOSEXUALITY
  , data = multico) 

summary(multico1)
ols_coll_diag(multico1)

# plot results
if (require("see")) {
  x <- check_collinearity(multico1)
  plot(x)
}


#### ###Influential Values
#To check for the most extreme values in the data by visualizing the Cooks distance values. 
#To check for 3 largest values;
plot(model_glm_4, which = 4, id.n = 3)

#The following R code computes the standardized residuals .std.resid and the Cooks distance .cooksd using the R function augment()
# Extract model results
model.data <- augment(model_glm_4) %>% 
  mutate(index = 1:n()) 

#The data for the top 3 largest values, according to the Cooks distance, 
model.data %>% top_n(3, .cooksd)

#Plot the Standardized Residuals:
## To do this, we need one dependent variable column; so merge with tmdb_individual data to obtain the mature column;

newdata <- tmdbjoined %>%
  pivot_longer(tmdbjoined, cols = c(maturecontent_sum, notmature_sum),
                 names_to = "mature")

# Now, plot
ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = mature), alpha = .5) +
  theme_bw()
