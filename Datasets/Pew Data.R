pacman::p_load(tidyverse, here, haven)
library("Amelia")

#Pew Religious data survey. Download this dataset "Pew-Research-Center-Global-Attitudes-Spring-2019-Survey-Data" from PEW 
Pew_Data <- read_sav(here("Data", "Pew Research Center Global Attitudes Spring 2019 Dataset WEB.sav"), 
                     user_na = TRUE) %>%
                      as_factor()

#Q85. How important is religion in your life: very important, somew
Religion_d <- Pew_Data %>% 
  select(country, RELIGION_IMPORT) 

Religion_Piv <- Pew_Data %>% 
  group_by(country, RELIGION_IMPORT) %>% 
  summarise(n=n()) %>% 
  pivot_wider(names_from = RELIGION_IMPORT, values_from = n) 