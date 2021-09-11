library(tidyverse)
library(skimr)
library(summarytools)
library(corrplot)
library(Hmisc) # describe()
library(funModeling) # profiling_num()
library(dlookr) # plot_normality(), normality()
library(DataExplorer) # plot_str()
library(plotly) # ggplotly(), plot_ly()
library(dummies) # creating dummy variables


#import
combined <- read.csv('combined.csv')
tmdbdummy <- read.csv('tmdbdummy.csv')
tmdbdummy$X <- NULL

# Explore structure of csv file
str(tmdbdummy)
glimpse(tmdbdummy)
summary(tmdbdummy)
skim(tmdbdummy) #numerical attributes from summary
dfSummary(tmdbdummy, graph.magnif = 0.75, valid.col = FALSE, style = "grid") # need to re-configure
describe(tmdbdummy)
normality(tmdbdummy)
profiling_num(tmdbdummy)
nrow(tmdbdummy)
table(tmdbdummy$mature)



#plot budget vs vote_avg.
ggplot(combined, aes(vote_average, budget, colour = genres.name)) + 
  geom_point() +
  ggtitle("budget vs average vote")

#plot budget vs revenue
ggplot(combined, aes(revenue, budget, colour = genres.name)) + 
  geom_point()+
  ggtitle("budget vs revenue")

#all genres separately
for (var in unique(combined$genres.name)) {
  dev.new()
  print( ggplot(combined[combined$genres.name==var,], aes(budget, revenue)) 
         + geom_point() 
         + ggtitle(var))
}

#country by mature vs not
ggplot(tmdbdummy, aes(x=production_countries.name, y=mature, fill=mature))+
  geom_col(position="fill")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#country by mature vs not
ggplot(tmdbdummy, aes(x=production_countries.name, y=mature, fill=mature))+
  geom_col(position="fill")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


plot_missing(tmdbdummy) # checks for NA data or missing values
plot_normality(tmdbdummy)
