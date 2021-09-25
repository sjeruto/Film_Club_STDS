install.packages("rsmdx")
library(rsmdx)

##OECD Data
income <-
readSDMX( providerId= "OECD",
            resource ="data",
            flowRef = "IDD",
            key = "/",
            key.mode = "SDMX",
            dsd = F) #This is where we say we want the data dictionary

#puts XML in dataframe. 
incomeid <-as.data.frame(income, labels = F)

#clean the data
gini <-incomeid %>% 
  filter(MEASURE == "GINI" , AGE == "TOT" , obsTime == "2018") %>%
  select(LOCATION, obsValue)
names(gini) <- c("Location", "GINI")


##OECD Data
gdp <-
  readSDMX( providerId= "OECD",
            resource ="data",
            flowRef = "SNA_TABLE1",
            key = "/",
            key.mode = "SDMX",
            dsd = T) #This is where we say we want the data dictionary

#puts XML in dataframe. 
gdpid <-as.data.frame(income, labels = F)

#clean the data
gini <- gdpid %>% 
  filter(MEASURE == "GINI" , AGE == "TOT" , obsTime == "2018") %>%
  select(LOCATION, obsValue)
names(gini) <- c("Location", "GINI")