#load packages
library(httr)
library(tidyverse)
library(gtools)
library(lubridate)


api_key <- "56c0a3f1756884f78f46e17b35da9ff0"
url <- "https://api.themoviedb.org/3/movie/"

#test on one film
tmdb <- GET(paste0(url,
                   as.character(500),
                   "?api_key=",
                   api_key))
movie <- content(tmdb, "parse")
movie <- as.matrix(unlist(movie))
movie <- as.data.frame(movie)
movie <- cbind(newColName = rownames(movie), movie)
rownames(movie) <- 1:nrow(movie)
movie <- pivot_wider(movie, id_cols=NULL, names_from=newColName, values_from=2)



#test on two films
tmdb <- GET(paste0(url,
                   as.character(501),
                   "?api_key=",
                   api_key))
movie2 <- content(tmdb, "parse")
movie2 <- as.matrix(unlist(movie2))
movie2 <- as.data.frame(movie2)
movie2 <- cbind(newColName = rownames(movie2), movie2)
rownames(movie2) <- 1:nrow(movie2)
movie2 <- pivot_wider(movie2, id_cols=NULL, names_from=newColName, values_from=2)

## convert result to an R data frame
tmdb <- smartbind(movie, movie2)

## range
start = 126120
end = 199999


for (i in start:end) {
  get_movie <- GET(paste0(url, as.character(i), "?api_key=", api_key))
  movie <- content(get_movie, "parse")
  movie <- as.matrix(unlist(movie))
  movie <- as.data.frame(movie)
  movie <- cbind(newColName = rownames(movie), movie)
  rownames(movie) <- 1:nrow(movie)
  movie <- pivot_wider(movie, id_cols=NULL, names_from=1, values_from=2)
  tmdb <- smartbind(tmdb, movie)  
}

## clean dataset
df <- tmdb %>%
  select(imdb_id
         , original_title
         , adult
         , genres.name
         , original_language
         , popularity
         , production_countries.name
         , release_date
         , budget
         , revenue
         , vote_average
         , vote_count)
df <- df %>%
  filter(! is.na(imdb_id))


## write to csv update the record
raw <- paste0("tmdb raw ",start,"-",end," range.csv")
clean <- paste0("tmdb clean ",start,"-",end," range.csv")

write.csv(tmdb, raw)
write.csv(df, clean)



## country ratio
country <- df %>%
  select(production_countries.name, genres.name, original_title) %>%
  group_by(production_countries.name, genres.name) %>%
  summarise(n = n())

## country ratio
df$release_date[df$release_date == ""] <- NA
df$year <- year(df$release_date)

country <- df %>%
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
