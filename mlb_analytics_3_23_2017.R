library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

#setwd("~/Desktop/baseballdatabank-2017.1/MLB_Analytics")

list.files()

appearances <- read_csv('Appearances.csv')
str(appearances)
appearances %>%
  mutate(GS = as.numeric(as.character(GS))) %>%
  filter(G_p == 0) %>%
  filter(yearID >= 1947) %>%
  filter(G_batting >= 50) %>%
  mutate(starter_score = GS / G_all) %>%
  arrange(starter_score) 


appearances %>%
  mutate(GS = as.numeric(as.character(GS))) %>%
  filter(G_p == 0) %>%
  filter(yearID >= 1997) %>%
  filter(GS == 0) %>%
  arrange(desc(G_all))

View(appearances)

batting <- read_csv('Batting.csv')
View(batting)

players <- read.csv('Master.csv')
View(batting)
batting_appear <- appearances %>%
  filter(yearID >= 1980) %>%
  inner_join(batting, by= c('playerID' = 'playerID', 'yearID' = 'yearID')) %>%
  filter(AB >=75) %>%
  mutate(BA = (H/AB) * 1000)

batting_appear %>%
  filter(G_p == 0) %>%
  mutate(GS = as.numeric(as.character(GS))) %>%
  ggplot() +
  geom_point(aes(x=GS, y=BA), colour='dodgerblue') +
  #scale_x_continuous(breaks=seq(from=1997, to=2016, by=5))+
  theme(
    axis.text.x=element_text(angle=45, hjust=1)
  ) +
  geom_text(aes(x=GS, y=BA, label=playerID))

View(batting_appear[batting_appear$playerID == 'cedence01',])

View(batting[batting$playerID == 'cedence01',])
model <- lm(GS ~ BA, data = batting_appear)
summary(model)
View(players)
