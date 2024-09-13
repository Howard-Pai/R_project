library(tidyverse)
library(pwr)

data <- read.csv("hw6-fb.csv")
str(data)
data$visit_date = as.Date(data$visit_date)
data$condition = as.factor(data$condition)
#data$clicked_article = as.factor(data$clicked_article)
#data$clicked_like = as.factor(data$clicked_like)
#data$clicked_share = as.factor(data$clicked_share)
data$gender = as.factor(data$gender)

data %>% 
  group_by(condition) %>% 
  summarise(mean_click_article = mean(clicked_article))

data %>% 
  group_by(condition) %>% 
  summarise(mean_click_like = mean(clicked_like))

data %>% 
  group_by(condition) %>% 
  summarise(mean_click_share = mean(clicked_share))

t.test(data[data$condition == "tips",]$clicked_article,
       data[data$condition == "tools",]$clicked_article,
       alternative = "two.sided")


t.test(data[data$condition == "tips",]$clicked_like,
       data[data$condition == "tools",]$clicked_like,
       alternative = "two.sided")

t.test(data[data$condition == "tips",]$clicked_share,
       data[data$condition == "tools",]$clicked_share,
       alternative = "two.sided")

aov.model <- aov(clicked_like ~ visit_date+condition+time_spent_homepage_sec+gender,data)
summary(aov.model)

interaction.model <- aov(clicked_like ~ condition*visit_date +condition*time_spent_homepage_sec+condition*gender,data)
summary(interaction.model)

daily.clicked_like <- data %>% 
  group_by(visit_date,condition) %>% 
  summarise(mean_click_like = mean(clicked_like))

ggplot(daily.clicked_like,aes(x=visit_date,y=mean_click_like,color = condition))+
  geom_point()+geom_line()+
  xlab("Date")+ylab("Mean Click Like")+
  ggtitle("Time Series Plot of click like rate: group by article's condition")+
  theme_bw()
