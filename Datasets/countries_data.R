install.packages("wbstats")

library(wbstats)
library(tidyverse)

#Code to get countries data
Country_Data <- select(wb_countries(),iso3c, country, income_level)

country_Data

#A tibble: 299 x 3
#iso3c country                       income_level       
#<chr> <chr>                         <chr>              
#1 ABW   Aruba                       High income        
#2 AFE   Africa Eastern and Southern Aggregates         
#3 AFG   Afghanistan                 Low income         
#4 AFR   Africa                      Aggregates         
#5 AFW   Africa Western and Central  Aggregates         
#6 AGO   Angola                      Lower middle income
#7 ALB   Albania                     Upper middle income
#8 AND   Andorra                     High income        
#9 ARB   Arab World                  Aggregates         
#10 ARE   United Arab Emirates       High income       
