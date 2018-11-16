library(tidyverse)
library(dslabs)
library(caret)

#set.seed(1995)
#indexes <- createResample(mnist_27$train$y, 10)
# my answer
#ind1 <- indexes$Resample01
#ind1_3 <- ind1 == 3
#ind1_4 <- ind1 == 4
#ind1_7 <- ind1 == 7
# question answer
#sum(indexes[[1]] == 3)
#sum(indexes[[1]] == 4)
#sum(indexes[[1]] == 7)

# my attempt
#sum(indexes[[1]] == 3)
#sum(indexes[[2]] == 3)
#sum(indexes[[3]] == 3)
#sum(indexes[[4]] == 3)
#sum(indexes[[5]] == 3)
#sum(indexes[[6]] == 3)
#sum(indexes[[7]] == 3)
#sum(indexes[[8]] == 3)
#sum(indexes[[9]] == 3)
#sum(indexes[[10]] == 3)

#answer
#x<-sapply(indexes, function(ind){
#  sum(ind == 3)
#})
#sum(x)

# my addition
#answer <- sum(sapply(indexes, function(ind){
#  sum(ind == 3)
#}))


#B <- 10000
#set.seed(1)
#Q_stars <- replicate(B, {
#  y_star <- rnorm(100, 0, 1)
#  Q_star <- quantile(y_star, 0.75)
#})
#mean(Q_stars)
#sd(Q_stars)

#set.seed(1)
#y <- rnorm(100, 0, 1)
#indexes <- createResample(y, 10000)

#answer_Q <- sapply(indexes, function(ind){
#   quantile(ind,0.75)
#})
#answer_sd <- sapply(indexes, function(ind){
#    0.75-quantile(ind,0.75)
#})

#a_m <- mean(answer_Q)
#a_e <- sd(answer_Q)

# answer
set.seed(1)
y <- rnorm(100, 0, 1)
set.seed(1)
indexes <- createResample(y, 10000)
q_75_star <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)