library(dslabs)
library(dplyr)
library(lubridate)
library(lattice)
library(ggplot2)
library(caret)
library(purrr)

data("reported_heights")

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

y_hat <- ifelse(x %in% "inclass", "Male","Female")
factor(y_hat)
mean(y_hat == dat$sex)

y_hat <- ifelse(x %in% "online", "Male","Female")
factor(y_hat)
mean(y_hat == dat$sex)

y_hat <- ifelse(x %in% "inclass", "Female","Male")
factor(y_hat)
mean(y_hat == dat$sex)

y_hat <- ifelse(x %in% "online", "Female","Male")

mean(y_hat == dat$sex)
# confusion matrix
table(predicted=y_hat, actual=dat$sex)

y_hat <- factor(y_hat)
confusionMatrix(data=y_hat, reference=y)

# new question
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

set.seed(2)
test_index<-createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

y_hat <-ifelse(train$Sepal.Length > XXX, "virginica", "versicolor") %>% factor(levels=levels(train$Species))

# find max/min for each parameter and Species for max
min(train$Sepal.Length)
#5, versicolor
max(train$Sepal.Length)
#7.9 viginica

# generate sequence
cutoff <- seq(50,79)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Length > x/10, "virginica", "versicolor") %>%
    factor(levels=levels(train$Species))
  mean(y_hat == train$Species)
})

# find max/min for each parameter and Species for max
min(train$Sepal.Width)
#2, versicolor
max(train$Sepal.Width)
#3.8 viginica

# generate sequence
cutoff <- seq(20,38)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Width > x/10, "virginica", "versicolor") %>%
    factor(levels=levels(train$Species))
  mean(y_hat == train$Species)
})

# find max/min for each parameter and Species for max
min(train$Petal.Length)
#3, versicolor
max(train$Petal.Length)
#6.9 viginica

# generate sequence
cutoff <- seq(30,69)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x/10, "virginica", "versicolor") %>%
    factor(levels=levels(train$Species))
  mean(y_hat == train$Species)
})

# find max/min for each parameter and Species for max
min(train$Petal.Width)
#1, versicolor
max(train$Petal.Width)
#2.5 viginica

# generate sequence
cutoff <- seq(10,25)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Width > x/10, "virginica", "versicolor") %>%
    factor(levels=levels(train$Species))
  mean(y_hat == train$Species)
})

# test data
cutoff <- seq(47,48)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(test$Petal.Length > x/10, "virginica", "versicolor") %>%
    factor(levels=levels(test$Species))
  mean(y_hat == test$Species)
})

# optimize test data
cutoff <- seq(49,77)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(test$Sepal.Length > x/10, "virginica", "versicolor") %>%
    factor(levels=levels(test$Species))
  mean(y_hat == test$Species)
})

cutoff <- seq(23,38)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(test$Sepal.Width > x/10, "virginica", "versicolor") %>%
    factor(levels=levels(test$Species))
  mean(y_hat == test$Species)
})

cutoff <- seq(33,67)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(test$Petal.Length > x/10, "virginica", "versicolor") %>%
    factor(levels=levels(test$Species))
  mean(y_hat == test$Species)
})

cutoff <- seq(10,25)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(test$Petal.Width > x/10, "virginica", "versicolor") %>%
    factor(levels=levels(test$Species))
  mean(y_hat == test$Species)
})

#optimize for both petal.length/width in train data

# find max/min for each parameter and Species for max
min(train$Petal.Length)
#3, versicolor
max(train$Petal.Length)
#6.9 viginica

# generate sequence
cutoff <- seq(30,69)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x/10, "virginica", "versicolor") %>%
    factor(levels=levels(train$Species))
  mean(y_hat == train$Species)
})

# 4.7 is optimal (or 4.8) for Length
# 1.5/6/7 is optimal for Width

# find max/min for each parameter and Species for max
min(train$Petal.Width)
#1, versicolor
max(train$Petal.Width)
#2.5 viginica

# generate sequence
cutoff <- seq(10,25)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Width > x/10, "virginica", "versicolor") %>%
    factor(levels=levels(train$Species))
  mean(y_hat == train$Species)
})

y_hat <- ifelse(train$Petal.Length > 4.8 | train$Petal.Width > 1.7, "virginica", "versicolor") %>% factor(levels=levels(train$Species))
mean(y_hat == train$Species)

y_hat <- ifelse(test$Petal.Length > 4.8 | test$Petal.Width > 1.7, "virginica", "versicolor") %>% factor(levels=levels(test$Species))
mean(y_hat == test$Species)


#
# Answer
#
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

plot(iris,pch=21,bg=iris$Species)

set.seed(2)
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

petalLengthRange <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
petalWidthRange <- seq(range(train[,4])[1],range(train[,4])[2],by=0.1)
cutoffs <- expand.grid(petalLengthRange,petalWidthRange)

id <- sapply(seq(nrow(cutoffs)),function(i){
  y_hat <- ifelse(train[,3]>cutoffs[i,1] | train[,4]>cutoffs[i,2],'virginica','versicolor')
  mean(y_hat==train$Species)
}) %>% which.max

optimalCutoff <- cutoffs[id,] %>% as.numeric
y_hat <- ifelse(test[,3]>optimalCutoff[1] & test[,4]>optimalCutoff[2],'virginica','versicolor')
mean(y_hat==test$Species)

