library(dplyr)
library(tidyverse)
library(dslabs)
data("movielens")

# my attempt
#cdata <- ddply(movielens, c("movieId", "title","year"), summarise, N=length(rating), mean=mean(rating), sd = sd(rating), se=sd/sqrt(N))
#cdata2 <- ddply(cdata, c("year"), summarise, N=sum(N))
#ggplot(cdata2,aes(year,N, group=year)) + geom_boxplot()
#max(cdata2$N)
#cdata2$year[cdata2$N==6635]

# answer
#movielens %>% group_by(movieId) %>%
#  summarize(n = n(), year = as.character(first(year))) %>%
#  qplot(year, n, data = ., geom = "boxplot") +
#  coord_trans(y = "sqrt") +
#  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# my answer
#after1993 <- movielens %>% group_by(movieId) %>% filter(year >=1993) %>% 
#  summarize(n = n(), year = as.character(first(year)), title = as.character(first(title)), avg_rating=mean(rating)) %>% top_n(25,n)
#after1993[with(after1993,order(-n)),]

#grader answer
a <- movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(25, rate) %>%
  arrange(desc(rate)) 


movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2017 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  ggplot(aes(rate, rating)) +
  geom_point() +
  geom_smooth()

library(lubridate)
#movielens %>% mutate(date = round_date(date, unit = "week")) %>%
#  group_by(date) %>%
#  summarize(rating = mean(rating)) %>%
#  ggplot(aes(date, rating)) +
#  geom_point() +
#  geom_smooth()

b <- movielens %>% 
  group_by(genres) %>%
  summarize(n = n(),
            genre = genres[1],
            rating = mean(rating)) %>%
  filter(n>=1000) %>%
  arrange(rating) 

