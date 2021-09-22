library(tidyverse)
library(skimr)
library(summarytools)
library(corrplot)
library(Hmisc)        # describe()
library(funModeling)  # profiling_num()
library(dlookr)       # plot_normality(), normality()
library(DataExplorer) # plot_str()
library(plotly)       # ggplotly(), plot_ly()
library(dummies)      # creating dummy variables
library(likert)
library(scales)

#import
combined <- read.csv('combined.csv')
tmdbdummy <- read.csv('tmdbdummy.csv')
tmdbdummy$X <- NULL

# Explore structure of csv file----
str(tmdbdummy)       #variable types look correct, budget looks like lots of 0s
glimpse(tmdbdummy)   #
summary(tmdbdummy)   
skim(tmdbdummy)      #numerical attributes from summary
dfSummary(tmdbdummy, graph.magnif = 0.75, valid.col = FALSE, style = "grid") # need to re-configure
describe(tmdbdummy)
normality(tmdbdummy)
profiling_num(tmdbdummy)
nrow(tmdbdummy)
table(tmdbdummy$mature)


#plot budget vs vote_avg.----
dollar <- label_dollar(prefix = "$", scale = 0.000001, suffix = "M",)
combined$avg <- ifelse(combined$budget > 13000000 & combined$vote_average < 5.4, "0", "1")
combined <- combined %>%
  filter(budget > 0 & revenue > 0)

ggplot(combined, aes(budget, vote_average, color = avg)) + 
  geom_point(alpha = 2/10) +
  scale_color_manual(values=c('darkred','navy'))+
  theme_minimal()+
  ggtitle("Budget vs. Rating")+
  xlab("Budget") + 
  ylab("Rating")+
  # scale_x_continuous(labels = dollar)+
  scale_y_continuous(labels = label_number(accuracy = 0))+
  theme(text=element_text(size=14,family="CM Roman"),  legend.position="none")

  
#plot budget vs vote_avg.
dollar <- label_dollar(prefix = "$", scale = 0.000001, suffix = "M",)

combined$profit <- (combined$revenue - combined$budget)
combined$profitable <- ifelse(combined$profit < 0, "0", "1")
combined <- combined %>%
  filter(budget > 0 & revenue > 0)

ggplot(combined, aes(budget, profit, color = profitable)) + 
  geom_point(alpha = 2/10) +
  scale_color_manual(values=c('darkred','navy'))+
  theme_minimal()+
  ggtitle("Budget vs. Profit")+
  xlab("Budget") + 
  ylab("Profit")+
  scale_x_continuous(labels = dollar)+
  scale_y_continuous(labels =dollar)+
  theme(text=element_text(size=14,family="CM Roman"),  legend.position="none")


#plot budget vs profit
no_nas <- na.omit(combined)
no_nas <- no_nas %>%
  filter(budget > 0 | revenue > 0)
ggplot(no_nas, aes(budget, (revenue-budget), colour = genres.name)) + 
  geom_point()+
  ggtitle("budget vs profit")

#all genres separately
for (var in unique(combined$genres.name)) {
  dev.new()
  print( ggplot(combined[combined$genres.name==var,], aes(budget, revenue)) 
         + geom_point() 
         + ggtitle(var))
}


tmdbdummy$mature <- as.factor(tmdbdummy$mature)
#country by mature vs not
ggplot(tmdbdummy, aes(x=production_countries.name, y=mature, fill=mature))+
  geom_col(position="fill")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#country by mature vs not
ggplot(tmdbdummy, aes(x=income_level, y=mature, fill=mature))+
  geom_col(position="fill")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



plot_missing(tmdbdummy) # checks for NA data or missing values
plot_normality(tmdbdummy)

#mature vs. lgbtqi
ggplot(tmdbdummy, aes(lgbtqi_Y, gender_Y)) + 
  geom_point()+
  ggtitle("Progressive across lgbtqi & gender")

ggplot(tmdbdummy, aes(lgbtqi_Y, religion_Y)) + 
  geom_point()+
  ggtitle("Progressive across lgbtqi & religion") 

ggplot(tmdbdummy, aes(gender_Y, religion_Y)) + 
  geom_point()+
  ggtitle("Progressive across gender & religion")
