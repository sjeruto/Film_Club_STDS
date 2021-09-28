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

str(tmdb)


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
# Select the highest weighted estimate
religion$RELIGION_IMPORT <- replace(religion$RELIGION_IMPORT, religion$RELIGION_IMPORT == "Very important", "Yes")
religion$RELIGION_IMPORT <- replace(religion$RELIGION_IMPORT, religion$RELIGION_IMPORT == "Somewhat important", "Yes")
religion$RELIGION_IMPORT <- replace(religion$RELIGION_IMPORT, religion$RELIGION_IMPORT == "Not too important", "No")
religion$RELIGION_IMPORT <- replace(religion$RELIGION_IMPORT, religion$RELIGION_IMPORT == "Not at all important", "No")
religion$RELIGION_IMPORT <- replace(religion$RELIGION_IMPORT, religion$RELIGION_IMPORT == "Dont know (DO NOT READ)", "Neutral")
religion$RELIGION_IMPORT <- replace(religion$RELIGION_IMPORT, religion$RELIGION_IMPORT == "Refused (DO NOT READ)", "Neutral")

new_df <- pivot_wider(religion, id_cols=c(country, RELIGION_IMPORT), names_from = RELIGION_IMPORT, values_from = weighted_estimate, values_fn = sum)

new_df <- new_df %>% pivot_longer(
  cols = c("Yes", "No", "Neutral"),
  names_to = "RELIGION_IMPORT",
  names_prefix = "",
  values_to = "weighted_estimate",
  values_drop_na = TRUE
)

new_df2 <- new_df%>%
  group_by(country) %>%
  summarise(weighted_estimate=max(weighted_estimate))

### merge the two datasets with country as unique identifier
religion<- merge (new_df, new_df2)
religion <- religion[,-c(2)]

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



#-----STEP 5: PREPARE HOFSTEDE DATA                                        ####
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


#-----STEP 6: MERGE ALL DATA                                               ####
#join country_data with tmdb
tmdbjoined <- left_join(tmdb, clean_country
                        , by= c("production_countries.name" = "country"))

#join country_data with tmdb
tmdbjoined <- inner_join(tmdbjoined, hofstede
                         , by= c("production_countries.name" = "country"))


#join country_data with tmdb
tmdbjoined <- inner_join(tmdbjoined, survey_joined
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
tmdbjoined <- tmdbjoined[,c("production_countries.name"
                            ,"maturecontent_sum"
                            ,"notmature_sum"
                            ,"budget_mean"
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


#use log transpormation on the new gender response, this overcomes issues with 
#outliers in later testing
tmdbjoined_log <- tmdbjoined
tmdbjoined_log$extrapolated_genderY <- log(tmdbjoined$extrapolated_genderY)


#-----STEP 9: CHECK MODEL ASSUMPTIONS                                      ####

#generate the model
model_glm_3 <- glm(cbind(maturecontent_sum, notmature_sum)~ 
                     income_level
                   + HOMOSEXUALITY 
                   + RELIGION_IMPORT
                   + extrapolated_genderY
                   + pdi                                        
                   + idv                                        
                   + mas                                       
                   + uai                                        
                   + ltowvs                                     
                   + ivr
                   + budget_mean 
                   ,family=binomial, data=tmdbjoined_log)


summary(model_glm_3)

anova(model_glm_3, model_glm_4, test = "Chisq")

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
  , "budget_mean" 
  #, "SP.POP.TOTL" - remove, basis for next variable
  , "extrapolated_genderY"
  )])

multico1<- lm(pdi ~ 
             # idv 
             + mas 
             + uai 
             + ltowvs 
             + ivr 
             + budget_mean
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
plot(model_glm_4, which = 4, id.n = 10)

#The following R code computes the standardized residuals .std.resid and the Cooks distance .cooksd using the R function augment()
# Extract model results
model.data <- augment(model_glm_4) %>% 
  mutate(index = 1:n()) 

model.data <- cbind(model.data, tmdbjoined[-c(13,21,26,32),1])

model.data <- model.data %>% 
  mutate(index = production_countries.name) 


#The data for the top 3 largest values, according to the Cooks distance, 
model.data %>% top_n(3, .cooksd)

#Plot the Standardized Residuals:
## To do this, we need one dependent variable column; so merge with tmdb_individual data to obtain the mature column;

# Now, plot
ggplot(model.data, aes(index, .std.resid)) + 
  geom_point() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  geom_hline(yintercept=2, linetype="dashed", color="red", size=.5) +
  geom_hline(yintercept=-2, linetype="dashed", color="red", size=.5)

# Remove Japan & South Korea
tmdbjoined_nooutliers <- tmdbjoined[-c(15),]

#-----STEP 10: FINAL MODEL                                                 ####

#generate the model
model_glm_5 <- glm(cbind(maturecontent_sum, notmature_sum)~ 
                     income_level
                   + HOMOSEXUALITY 
                   + RELIGION_IMPORT
                   + extrapolated_genderY
                   + pdi
                   + ltowvs                                     
                   + ivr
                   + budget_mean 
                   ,family=binomial, data=tmdbjoined_nooutliers)


summary(model_glm_5)
