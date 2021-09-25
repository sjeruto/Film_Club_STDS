#load packages
library(tidyverse)
library(gtools)
library(lubridate)


#import the files
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


## country ratio
country <- combined %>%
  select(production_countries.name, genres.name, original_title) %>%
  group_by(production_countries.name, genres.name) %>%
  summarise(n = n())

## country ratio
combined$release_date <- lubridate::dmy(combined$release_date)
combined$release_date[combined$release_date == ""] <- NA
combined$year <- year(combined$release_date)

country <- combined %>%
  filter(year > 2010)

country <- country %>%
  select(production_countries.name, genres.name, original_title) %>%
  group_by(production_countries.name, genres.name) %>%
  summarise(n = n())

country_pivot <- pivot_wider(country
                             ,id_cols = production_countries.name
                             ,names_from = genres.name
                             ,values_from = n)

country_pivot$total <- rowSums(country_pivot[,-1], na.rm=TRUE)

country_pivot <- pivot_longer(country_pivot,
                              cols = -c(production_countries.name|total),
                              names_to = "genre",
                              values_to = "value")

country_pivot$total[is.na(country_pivot$total)] <- 0
country_pivot$value[is.na(country_pivot$value)] <- 0

country_pivot <- country_pivot %>% filter(total > 50)

country_pivot$perc <- round((country_pivot$value /country_pivot$total * 100), 2)

country_pivot <- country_pivot %>%
  select(production_countries.name, genre, perc)

country_pivot <- pivot_wider(country_pivot
                             ,id_cols = production_countries.name
                             ,names_from = genre
                             ,values_from = perc)

country_perc <- pivot_longer(country_pivot,
                              cols = -(production_countries.name),
                              names_to = "genre",
                              values_to = "value")

country_perc <- country_perc %>%
  filter(production_countries.name %in% countries)

country_perc <- country_perc %>%
  filter(genre != "NA")



#plot heatmap of country genre popularity
ggplot(country_perc, aes(production_countries.name, genre, fill=value)) +
  geom_tile() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position="none") +
  ggtitle("Genre Popularity by Country
          ") +
  xlab("") + 
  ylab("")+
  theme(text=element_text(size=14,family="CM Roman"))+
  scale_fill_gradient(low="white", high="navy")
  

