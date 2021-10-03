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
library(leaflet)
library(rgdal)


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



#-----STEP 6: PREPARE HOFSTEDE DATA                                        ####
#import the data
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
tmdbjoined <- left_join(tmdb, clean_country
                        , by= c("production_countries.name" = "country"))

#join country_data with tmdb
tmdbjoined <- inner_join(tmdbjoined, survey_joined
                         , by= c("production_countries.name" = "country"))

#join country_data with tmdb
tmdbjoined <- inner_join(tmdbjoined, hofstede
                         , by= c("production_countries.name" = "country"))




# Create Chart for Culture Data 


# Homosexuality

library(tidyverse)
library(ggthemes)
library(sqldf)
library(lattice)
library(gridExtra)
library(cowplot)
library(ggpubr)



homo <- read_csv("HOMOSEXUALITY.csv")

homo_clean <- homo %>%
    select("country", "HOMOSEXUALITY", "weighted_estimate") %>%
    mutate(HOMOSEXUALITY=if_else(HOMOSEXUALITY=="Homosexuality should be accepted by society", "Yes", "No")) %>%
    mutate(response=case_when(HOMOSEXUALITY=="No" ~ -1*weighted_estimate, HOMOSEXUALITY=="Yes" ~ weighted_estimate)) %>%
    group_by(country,HOMOSEXUALITY)


country <- 
    read_csv("tmdbjoined.csv") %>%
    select("production_countries.name", "income_level") %>%
    mutate(country= case_when(production_countries.name =="United States of America" ~ "United States",
                              production_countries.name !="United States of America"~production_countries.name)) %>%
    unique() %>%
    filter(income_level !="NA")




homo_combined <- sqldf("SELECT 
                       country, HOMOSEXUALITY, response, income_level
                       FROM homo_clean
                       LEFT JOIN country USING(country)")


homo_combined_HI <- homo_combined %>%
    filter(income_level=='High income')

homo_combined_UMI <- homo_combined %>%
    filter(income_level=='Upper middle income')

homo_combined_LMI <- homo_combined %>%
    filter(income_level=='Lower middle income')


# X Axis Breaks and Labels 
brks <- seq(-1, 1, 0.2)
lbls = paste0(as.character(100*(c(seq(1, 0, -0.2), seq(0.1, 1, 0.2)))), "%")





#Plot - High Income
Plot_HI <-
ggplot(homo_combined_HI , aes(x = reorder(country, response), y = response, fill = HOMOSEXUALITY, group=income_level)) +   # Fill column
    geom_bar(stat = "identity", width = .6) +   # draw the bars
    scale_y_continuous(breaks = brks,   # Breaks
                       labels = lbls) + # Labels
    coord_flip() +  # Flip axes
    labs(title="Homosexuality should be accepted by society?\n \n High Income Countries",
         y="Response", x="Country") +
    theme_tufte() +  # Tufte theme from ggfortify
    theme(plot.title = element_text(hjust = .5, color="#F7F5E6"), 
          axis.ticks = element_blank(),
          plot.background = element_rect(fill="#0D2A47"),
          axis.title.x =element_blank(),
          axis.title.y = element_blank(),
          axis.text = element_text(colour = "#F7F5E6"),
          legend.text=element_text(colour = "#F7F5E6"),
          legend.position="top",
          legend.title=element_blank(),
          axis.text.x=element_blank()) +   # Centre plot title
    scale_fill_manual("HOMOSEXUALITY", values = c("Yes" = "#F7F5E6", "No" = "#E15759")) 






#Plot - Upper middle income

Plot_UMI <-
ggplot(homo_combined_UMI, aes(x = reorder(country, response), y = response, fill = HOMOSEXUALITY, group=income_level)) +   # Fill column
    geom_bar(stat = "identity", width = .6) +   # draw the bars
    scale_y_continuous(breaks = brks,   # Breaks
                       labels = lbls) + # Labels
    coord_flip() +  # Flip axes
    labs(title="\n Upper Middle Income Countries",
         y="Response", x="Country") +
    theme_tufte() +  # Tufte theme from ggfortify
    theme(plot.title = element_text(hjust = .5, color="#F7F5E6"), 
          axis.ticks = element_blank(),
          plot.background = element_rect(fill="#0D2A47"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(colour = "#F7F5E6"),
          legend.title = element_blank(),
          legend.text=element_text(colour = "#F7F5E6"),
          legend.position="none") +  # Centre plot title
    scale_fill_manual("HOMOSEXUALITY", values = c("Yes" = "#F7F5E6", "No" = "#E15759")) 



#Plot - Lower middle income
Plot_LMI <-
ggplot(homo_combined_LMI, aes(x = reorder(country, response), y = response, fill = HOMOSEXUALITY, group=income_level)) +   # Fill column
    geom_bar(stat = "identity", width = .6) +   # draw the bars
    scale_y_continuous(breaks = brks,   # Breaks
                       labels = lbls) + # Labels
    coord_flip() +  # Flip axes
    labs(title="\n Lower Middle Income Countries",
         y="Response", x="Country") +
    theme_tufte() +  # Tufte theme from ggfortify
    theme(plot.title = element_text(hjust = .5, color="#F7F5E6"), 
          axis.ticks = element_blank(),
          plot.background = element_rect(fill="#0D2A47"),
          axis.title.x = element_text(colour = "#F7F5E6"),
          axis.title.y = element_blank(),
          axis.text.x = element_text(colour = "#F7F5E6"),
          axis.text.y = element_text(colour = "#F7F5E6"),
          legend.position="none",
          plot.margin =margin(0,0.5,0,0.5, "cm")) +  # Centre plot title
    scale_fill_manual("HOMOSEXUALITY", values = c("Yes" = "#F7F5E6", "No" = "#E15759")) 



figure <- ggarrange(Plot_HI, Plot_UMI,Plot_LMI + font("x.text", size = 10),
                    ncol = 1, nrow = 3, heights=c(2,1,1)) 

figure


ggsave("Homosexuality", plot=figure,device="jpeg",dpi=700)




## Gender


gender <- read_csv("GENDER_EQUALITY.csv")

gender_clean <- gender %>%
    select("country", "GENDER_EQUALITY", "weighted_estimate") %>%
    mutate(GENDER_EQUALITY=if_else(GENDER_EQUALITY==("Very important" |"Somewhat important"), "Yes", "No")) %>%
    mutate(response=case_when(GENDER_EQUALITY=="No" ~ -1*weighted_estimate, GENDER_EQUALITY=="Yes" ~ weighted_estimate)) %>%
    group_by(country,GENDER_EQUALITY)


country <- 
    read_csv("tmdbjoined.csv") %>%
    select("production_countries.name", "income_level") %>%
    mutate(country= case_when(production_countries.name =="United States of America" ~ "United States",
                              production_countries.name !="United States of America"~production_countries.name)) %>%
    unique() %>%
    filter(income_level !="NA")




gender_combined <- sqldf("SELECT 
                       country, GENDER_EQUALITY, response, income_level
                       FROM gender_clean
                       LEFT JOIN country USING(country)")


gender_combined_HI <- gender_combined %>%
    filter(income_level=='High income')

gender_combined_UMI <- gender_combined %>%
    filter(income_level=='Upper middle income')

gender_combined_LMI <- gender_combined %>%
    filter(income_level=='Lower middle income')


# X Axis Breaks and Labels 
brks <- seq(-1, 1, 0.2)
lbls = paste0(as.character(100*(c(seq(1, 0, -0.2), seq(0.1, 1, 0.2)))), "%")





#Plot - High Income
Plot_Gender_HI <-
    ggplot(gender_combined_HI , aes(x = reorder(country, response), y = response, fill = GENDER_EQUALITY, group=income_level)) +   # Fill column
    geom_bar(stat = "identity", width = .6) +   # draw the bars
    scale_y_continuous(breaks = brks,   # Breaks
                       labels = lbls) + # Labels
    coord_flip() +  # Flip axes
    labs(title="Is Gender Equality Important?\n \n High Income Countries",
         y="Response", x="Country") +
    theme_tufte() +  # Tufte theme from ggfortify
    theme(plot.title = element_text(hjust = .5, color="#F7F5E6"), 
          axis.ticks = element_blank(),
          plot.background = element_rect(fill="#0D2A47"),
          axis.title.x =element_blank(),
          axis.title.y = element_blank(),
          axis.text = element_text(colour = "#F7F5E6"),
          legend.text=element_text(colour = "#F7F5E6"),
          legend.position="top",
          legend.title=element_blank(),
          axis.text.x=element_blank()) +   # Centre plot title
    scale_fill_manual("GENDER_EQUALITY", values = c("Yes" = "#F7F5E6", "No" = "#E15759")) 






#Plot - Upper middle income

Plot_Gender_UMI <-
    ggplot(homo_combined_UMI, aes(x = reorder(country, response), y = response, fill = GENDER_EQUALITY, group=income_level)) +   # Fill column
    geom_bar(stat = "identity", width = .6) +   # draw the bars
    scale_y_continuous(breaks = brks,   # Breaks
                       labels = lbls) + # Labels
    coord_flip() +  # Flip axes
    labs(title="\n Upper Middle Income Countries",
         y="Response", x="Country") +
    theme_tufte() +  # Tufte theme from ggfortify
    theme(plot.title = element_text(hjust = .5, color="#F7F5E6"), 
          axis.ticks = element_blank(),
          plot.background = element_rect(fill="#0D2A47"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(colour = "#F7F5E6"),
          legend.title = element_blank(),
          legend.text=element_text(colour = "#F7F5E6"),
          legend.position="none") +  # Centre plot title
    scale_fill_manual("GENDER_EQUALITY", values = c("Yes" = "#F7F5E6", "No" = "#E15759")) 



#Plot - Lower middle income
Plot_Gender_LMI <-
    ggplot(homo_combined_LMI, aes(x = reorder(country, response), y = response, fill = GENDER_EQUALITY, group=income_level)) +   # Fill column
    geom_bar(stat = "identity", width = .6) +   # draw the bars
    scale_y_continuous(breaks = brks,   # Breaks
                       labels = lbls) + # Labels
    coord_flip() +  # Flip axes
    labs(title="\n Lower Middle Income Countries",
         y="Response", x="Country") +
    theme_tufte() +  # Tufte theme from ggfortify
    theme(plot.title = element_text(hjust = .5, color="#F7F5E6"), 
          axis.ticks = element_blank(),
          plot.background = element_rect(fill="#0D2A47"),
          axis.title.x = element_text(colour = "#F7F5E6"),
          axis.title.y = element_blank(),
          axis.text.x = element_text(colour = "#F7F5E6"),
          axis.text.y = element_text(colour = "#F7F5E6"),
          legend.position="none",
          plot.margin =margin(0,0.5,0,0.5, "cm")) +  # Centre plot title
    scale_fill_manual("GENDER_EQUALITY", values = c("Yes" = "#F7F5E6", "No" = "#E15759")) 



figure_gender <- ggarrange(Plot__Gender_HI, Plot__Gender_UMI,Plot_Gender_LMI + font("x.text", size = 10),
                    ncol = 1, nrow = 3, heights=c(2,1,1)) 




ggsave("GENDER_EQUALITY", plot=figure,device="jpeg",dpi=700)






# Religion


religion <- read_csv("RELIGION_IMPORT.csv")

religion_clean <- religion %>%
    select("country", "RELIGION_IMPORT.csv", "weighted_estimate") %>%
    mutate(RELIGION_IMPORT=if_else(RELIGION_IMPORT==("Very important" |"Somewhat important"), "Yes", "No")) %>%
    mutate(response=case_when(RELIGION_IMPORT=="No" ~ -1*weighted_estimate, RELIGION_IMPORT=="Yes" ~ weighted_estimate)) %>%
    group_by(country,RELIGION_IMPORT)


country <- 
    read_csv("tmdbjoined.csv") %>%
    select("production_countries.name", "income_level") %>%
    mutate(country= case_when(production_countries.name =="United States of America" ~ "United States",
                              production_countries.name !="United States of America"~production_countries.name)) %>%
    unique() %>%
    filter(income_level !="NA")




religion_combined <- sqldf("SELECT 
                       country, RELIGION_IMPORT, response, income_level
                       FROM gender_clean
                       LEFT JOIN country USING(country)")


religion_combined_HI <- religion_combined %>%
    filter(income_level=='High income')

religion_combined_UMI <- religion_combined %>%
    filter(income_level=='Upper middle income')

religion_combined_LMI <- religion_combined %>%
    filter(income_level=='Lower middle income')


# X Axis Breaks and Labels 
brks <- seq(-1, 1, 0.2)
lbls = paste0(as.character(100*(c(seq(1, 0, -0.2), seq(0.1, 1, 0.2)))), "%")





#Plot - High Income
Plot_Religion_HI <-
    ggplot(religion_combined_HI , aes(x = reorder(country, response), y = response, fill = RELIGION_IMPORT, group=income_level)) +   # Fill column
    geom_bar(stat = "identity", width = .6) +   # draw the bars
    scale_y_continuous(breaks = brks,   # Breaks
                       labels = lbls) + # Labels
    coord_flip() +  # Flip axes
    labs(title="Is Religion Important To You?\n \n High Income Countries",
         y="Response", x="Country") +
    theme_tufte() +  # Tufte theme from ggfortify
    theme(plot.title = element_text(hjust = .5, color="#F7F5E6"), 
          axis.ticks = element_blank(),
          plot.background = element_rect(fill="#0D2A47"),
          axis.title.x =element_blank(),
          axis.title.y = element_blank(),
          axis.text = element_text(colour = "#F7F5E6"),
          legend.text=element_text(colour = "#F7F5E6"),
          legend.position="top",
          legend.title=element_blank(),
          axis.text.x=element_blank()) +   # Centre plot title
    scale_fill_manual("RELIGION_IMPORT", values = c("Yes" = "#F7F5E6", "No" = "#E15759")) 






#Plot - Upper middle income

Plot_Religion_UMI <-
    ggplot(religion_combined_UMI, aes(x = reorder(country, response), y = response, fill = RELIGION_IMPORT, group=income_level)) +   # Fill column
    geom_bar(stat = "identity", width = .6) +   # draw the bars
    scale_y_continuous(breaks = brks,   # Breaks
                       labels = lbls) + # Labels
    coord_flip() +  # Flip axes
    labs(title="\n Upper Middle Income Countries",
         y="Response", x="Country") +
    theme_tufte() +  # Tufte theme from ggfortify
    theme(plot.title = element_text(hjust = .5, color="#F7F5E6"), 
          axis.ticks = element_blank(),
          plot.background = element_rect(fill="#0D2A47"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(colour = "#F7F5E6"),
          legend.title = element_blank(),
          legend.text=element_text(colour = "#F7F5E6"),
          legend.position="none") +  # Centre plot title
    scale_fill_manual("RELIGION_IMPORT", values = c("Yes" = "#F7F5E6", "No" = "#E15759")) 



#Plot - Lower middle income
Plot_ReligionLMI <-
    ggplot(religion_combined_LMI, aes(x = reorder(country, response), y = response, fill = RELIGION_IMPORT, group=income_level)) +   # Fill column
    geom_bar(stat = "identity", width = .6) +   # draw the bars
    scale_y_continuous(breaks = brks,   # Breaks
                       labels = lbls) + # Labels
    coord_flip() +  # Flip axes
    labs(title="\n Lower Middle Income Countries",
         y="Response", x="Country") +
    theme_tufte() +  # Tufte theme from ggfortify
    theme(plot.title = element_text(hjust = .5, color="#F7F5E6"), 
          axis.ticks = element_blank(),
          plot.background = element_rect(fill="#0D2A47"),
          axis.title.x = element_text(colour = "#F7F5E6"),
          axis.title.y = element_blank(),
          axis.text.x = element_text(colour = "#F7F5E6"),
          axis.text.y = element_text(colour = "#F7F5E6"),
          legend.position="none",
          plot.margin =margin(0,0.5,0,0.5, "cm")) +  # Centre plot title
    scale_fill_manual("RELIGION_IMPORT", values = c("Yes" = "#F7F5E6", "No" = "#E15759")) 



figure_gender <- ggarrange(Plot_Religion_HI, Plot_Religion_UMI,Plot_Religion_LMI + font("x.text", size = 10),
                           ncol = 1, nrow = 3, heights=c(2,1,1)) 




ggsave("RELIGION_IMPORT", plot=figure,device="jpeg",dpi=700)



#Choropleth map 
#Load World Map
world_spdf <- readOGR( 
  "world_shape_file" , 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

#PREPARE HOFSTEDE DATA
#import the HOFSTEDE data
hofstede <- read.csv('hofstedes culture dimensions.csv', sep =";", stringsAsFactors = FALSE)

#convert ctr names to match for join
hofstede$ctr <- str_replace(hofstede$ctr
                            , "ALG", "DZA")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "AFE", "KEN")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "AUL", "AUS")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "BAN", "BGD")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "BOS", "BIH")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "BUL", "BGR")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "BUF", "BFA")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "CHI", "CHN")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "COS", "CRI")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "CRO", "HRV")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "ECA", "ECU")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "DEN", "DNK")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "ARA", "LBN")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "ECA", "ECU")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "SLV", "SVN")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "SAL", "SLV")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "SLK", "SVK")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "GER", "DEU")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "GRE", "GRC")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "GUA", "GTM")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "HOK", "HKG")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "ICE", "ISL")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "IDO", "IDN")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "IRA", "IRN")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "IRE", "IRL")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "KYR", "KGZ")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "LAT", "LVA")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "LIT", "LTU")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "MAL", "MYS")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "MOL", "MDA")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "MOR", "MAR")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "NET", "NLD")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "NIG", "NGA")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "PHI", "PHL")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "POR", "PRT")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "PUE", "PRI")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "ROM", "ROU")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "SER", "SRB")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "SIN", "SGP")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "SAF", "ZAF")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "SPA", "ESP")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "SWI", "CHE")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "TAI", "TWN")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "TAN", "TZA")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "TRI", "TTO")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "URU", "URY")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "VIE", "VNM")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "ZAM", "ZMB")
hofstede$ctr <- str_replace(hofstede$ctr
                            , "ZIM", "ZWE")

hofstede <- hofstede[,c(1:8)]

# Tidy into data frame
df <- hofstede %>% select(1,3,7, 8)                   # drops extra columns
names(df) <- c("ISO3", "pdi", "ltowvs", "ivr")  # rename columns
df <- df %>% drop_na("pdi", "ltowvs", "ivr")                     # drop additional invalid rows


#merge csv with shapefile
x <- merge(world_spdf, df, by ="ISO3", type = "inner")

#dplyr function to remove the NAs
x <- x[!is.na(x@data$pdi), ]  
x <- x[!is.na(x@data$ltowvs), ] 
x <- x[!is.na(x@data$ivr), ]  

# Create a color palette for the map:
mypalette <- colorNumeric( palette="viridis", domain=x@data$pdi, na.color="transparent")

# Create a color palette with handmade bins.--------------------------------------------------------
library(RColorBrewer)
mybins <- c(0,20,40,60,80,100,Inf)
mypalette <- colorBin( palette="YlOrBr", domain=x@data$pdi, na.color="transparent", bins=mybins)

# Prepare the text for tooltips:
mytext <- paste(
  "Country: ", x@data$NAME,"<br/>", 
  "Power distance: ", x@data$pdi, "<br/>", 
  "Long-term orientation: ", x@data$ltowvs, "<br/>", 
  "Indulgence: ", x@data$ivr, 
  sep="") %>%
  lapply(htmltools::HTML)

# Final Map- pdi
leaflet(x) %>% 
  addTiles()  %>% 
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( 
    fillColor = ~mypalette(pdi), 
    stroke=TRUE, 
    fillOpacity = 0.9, 
    color="white", 
    weight=0.3,
    label = mytext,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  ) %>%
  addLegend( pal=mypalette, values=~pdi, opacity=0.9, title = "Power Distance", position = "bottomleft" ) 

# Final Map- ltowvs
leaflet(x) %>% 
  addTiles()  %>% 
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( 
    fillColor = ~mypalette(ltowvs), 
    stroke=TRUE, 
    fillOpacity = 0.9, 
    color="white", 
    weight=0.3,
    label = mytext,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  ) %>%
  addLegend( pal=mypalette, values=~ltowvs, opacity=0.9, title = "Long-term orientation", position = "bottomleft" ) 

# Final Map- ivr
leaflet(x) %>% 
  addTiles()  %>% 
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( 
    fillColor = ~mypalette(ivr), 
    stroke=TRUE, 
    fillOpacity = 0.9, 
    color="white", 
    weight=0.3,
    label = mytext,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  ) %>%
  addLegend( pal=mypalette, values=~ivr, opacity=0.9, title = "Indulgence", position = "bottomleft" ) 





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
tmdbjoined$extrapolated_genderY <- log(tmdbjoined$extrapolated_genderY)


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
                   + budget_mean 
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
  , "budget_mean" 
  #, "SP.POP.TOTL" - remove, basis for next variable
  , "extrapolated_genderY"
  )])

multico1<- lm(pdi ~ #idv 
             #+ mas 
             #+ uai 
             + ltowvs 
             + ivr 
             + budget_mean
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
tmdbjoined_nooutliers <- tmdbjoined[-c(15,27),]

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
                   ,family=binomial, data=tmdbjoined)


summary(model_glm_5)
coef(model_glm_5)


#-----STEP 11: INTERPRET                                                 ####

#install.packages("DescTools")
#install.packages("manipulate")
library(DescTools)
library(manipulate)

confint(model_glm_5)
exp(coef(model_glm_5))
exp(cbind(OR=coef(model_glm_5),confint.default(model_glm_5)))

#generate an intercept only model
tmdbnull <- tmdbjoined[-c(13,21,27,32),]
model_5null <- glm(cbind(maturecontent_sum, notmature_sum)~1,
                   family=binomial, data=tmdbnull)

anova(model_5null, model_glm_5, test= "LRT")

# To check for variable importance 
caret::varImp(model_glm_5)
