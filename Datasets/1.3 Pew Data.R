pacman::p_load(tidyverse, here, haven)
library("Amelia")
# https://medium.com/pew-research-center-decoded/using-tidyverse-tools-with-pew-research-center-survey-data-in-r-bdfe61de0909
#Pew Religious data survey. Download this dataset "Pew-Research-Center-Global-Attitudes-Spring-2019-Survey-Data" from PEW (https://www.pewresearch.org/global/dataset/september-2019-u-s-survey-data/)
Pew_Data <- read_sav(here("Data", "Pew Research Center Global Attitudes Spring 2019 Dataset WEB.sav"), 
                     user_na = TRUE) %>%
                      as_factor()

#Q85. How important is religion in your life: very important, somew
Religion_d <- Pew_Data %>% 
  select(country, RELIGION_IMPORT) 

# RELIGION_IMPORT <-
  Pew_Data %>% 
  group_by(country, RELIGION_IMPORT) %>% 
  summarise(weighted_n = sum(weight)) 
  # pivot_wider(names_from = RELIGION_IMPORT, values_from = n) 

#Views on homosexuality

HOMOSEXUALITY <- Pew_Data %>% 
  group_by(country, HOMOSEXUALITY) %>% 
  summarise(weighted_n = sum(weight)) 

# View on Gender

GENDER_EQUALITY <- Pew_Data %>% 
  group_by(country, GENDER_EQUALITY) %>% 
  summarise(weighted_n = sum(weight)) 

# Write into CSV
write.csv(RELIGION_IMPORT, "RELIGION_IMPORT.csv")
write.csv(HOMOSEXUALITY, "HOMOSEXUALITY.csv")
write.csv(GENDER_EQUALITY, "GENDER_EQUALITY.csv")


# If you want to get the percentage proportions 

RELIGION_IMPORT_P <- Pew_Data %>%  
  ##by taking the sum of the weights
  group_by(country, RELIGION_IMPORT) %>% 
  summarise(weighted_n = sum(weight)) %>% 
  ##add the weighted_group_size to get the total weighted n and 
  ##divide weighted_n by weighted_group_size to get the proportions 
  mutate(weighted_group_size = sum(weighted_n), 
         weighted_estimate = weighted_n / weighted_group_size
  )

HOMOSEXUALITY_P <- Pew_Data %>%  
  ##by taking the sum of the weights
  group_by(country, HOMOSEXUALITY) %>% 
  summarise(weighted_n = sum(weight)) %>% 
  ##add the weighted_group_size to get the total weighted n and 
  ##divide weighted_n by weighted_group_size to get the proportions 
  mutate(weighted_group_size = sum(weighted_n), 
         weighted_estimate = weighted_n / weighted_group_size
  )

GENDER_EQUALITY_P <- Pew_Data %>%  
  ##by taking the sum of the weights
  group_by(country, GENDER_EQUALITY) %>% 
  summarise(weighted_n = sum(weight)) %>% 
  ##add the weighted_group_size to get the total weighted n and 
  ##divide weighted_n by weighted_group_size to get the proportions 
  mutate(weighted_group_size = sum(weighted_n), 
         weighted_estimate = weighted_n / weighted_group_size
  )

# Write into CSV
write.csv(RELIGION_IMPORT_P, "RELIGION_IMPORT.csv")
write.csv(HOMOSEXUALITY_P, "HOMOSEXUALITY.csv")
write.csv(GENDER_EQUALITY_P, "GENDER_EQUALITY.csv")