library(dslabs)
library(dplyr)
library(HistData)
library(lattice)
library(ggplot2)
library(caret)

#set.seed(1)
#galton_heights <- GaltonFamilies %>%
#  filter(childNum == 1 & gender == "male") %>%
#  select(father, childHeight) %>%
#  rename(son = childHeight)


#y <- galton_heights$son
#test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

#train_set <- galton_heights %>% slice(-test_index)
#test_set <- galton_heights %>% slice(test_index)

#avg <- mean(train_set$son)
#avg
#squared_loss <- mean((avg-test_set$son)^2)

# regression
#fit <- lm(son ~ father, data = train_set)
#fit$coef

# we get f(x)=42+0.39x

#better?
#y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
#mean((y_hat - test_set$son)^2)

# predict function
#y_hat <- predict(fit,test_set)
# get same result as above
#mean((y_hat - test_set$son)^2)

# exercises

#myRMSE <- function (size) {
#  set.seed(1)
#  Sigma <- 9*matrix(c(1.0,0.5,0.5,1.0),2,2)
#  dat <- MASS::mvrnorm(size,c(69,69),Sigma) %>% data.frame() %>% setNames(c("x","y"))

#  RMSEs<-replicate(100, {
#    test_index<-createDataPartition(dat$y, times=1, p=0.5, list=FALSE)
#    train_set <-dat %>% slice(-test_index)
#    test_set <- dat %>% slice(test_index)
#    fit <- lm(y ~ x, data = train_set)
#    y_hat<-predict(fit,test_set)
#    sqrt(mean((y_hat-test_set$y)^2))
#  })
#  list(mean(RMSEs), sd(RMSEs))
#}

#n <-c(100,500,100,5000,10000)
#answer <- sapply(n, myRMSE)

#set.seed(1)
#RMSEfunc <- function (size) {
#  Sigma <- 9*matrix(c(1.0,0.5,0.5,1.0),2,2)
#  dat <- MASS::mvrnorm(n=size,c(69,69),Sigma) %>% data.frame() %>% setNames(c("x","y"))

#  rmse <- replicate(100, {
#    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
#    train_set <- dat %>% slice(-test_index)
#    test_set <- dat %>% slice(test_index)
#    fit <- lm(y ~ x, data = train_set)
#    y_hat <- predict(fit, newdata = test_set)
#    sqrt(mean((y_hat-test_set$y)^2))
#  })
#  list(mean(rmse),sd(rmse))
#}

#n<-c(100,500,1000,5000,10000)
#sapply(n,RMSEfunc)

#set.seed(1) 
#n <- c(100, 500, 1000, 5000, 10000,50000, 100000) 
#res <- sapply(n, function(n){ 
#  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2) 
#  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>% data.frame() %>% setNames(c("x", "y")) 
#  rmse <- replicate(100, { 
#    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE) 
#    train_set <- dat %>% slice(-test_index) 
#    test_set <- dat %>% slice(test_index) 
#    fit <- lm(y ~ x, data = train_set) 
#    y_hat <- predict(fit, newdata = test_set) 
#    sqrt(mean((y_hat-test_set$y)^2)) }) 
#  c(avg = mean(rmse), sd = sd(rmse)) }) 
#res

set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
set.seed(1)
rmse <- replicate(100, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat-test_set$y)^2))
})

mean(rmse)
sd(rmse)

