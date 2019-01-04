library(tidyverse)
library(dplyr)
library(lattice)
library(caret)
library(randomForest)
library(Rborist)
library(dslabs)

#n <- 1000
#sigma <- 0.25
#x <- rnorm(n, 0, 1)
#y <- 0.75 * x + rnorm(n, 0, sigma)
#dat <- data.frame(x = x, y = y)
#fit <- rpart(y ~ ., data = dat)
#plot(fit)
#text(fit)

#dat %>% 
#  mutate(y_hat = predict(fit)) %>% 
#  ggplot() +
#  geom_point(aes(x, y)) +
#  geom_step(aes(x,y_hat), col=2)


#fit <- randomForest(y ~ x, data=dat) 
#dat %>% 
#  mutate(y_hat = predict(fit)) %>% 
#  ggplot() +
#  geom_point(aes(x, y)) +
#  geom_step(aes(x, y_hat), col = 2)
#plot(fit)

#fit <- randomForest(y ~ x, data=dat, nodesize=50, maxnodes=25)
#dat %>% 
#  mutate(y_hat = predict(fit)) %>% 
#  ggplot() +
#  geom_point(aes(x, y)) +
#  geom_step(aes(x, y_hat), col = 2)

#fit <- train(y ~ x, data=dat, tuneGrid = data.frame(minNode = seq(25,100,25),predFixed = c(1,1,1,1)), method="Rborist")
#ggplot(fit,highlight=TRUE)

#set.seed(1991)  
#data("tissue_gene_expression")
#fit <- with(tissue_gene_expression, 
#            train(x, y, method = "rpart",
#                  tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))

#ggplot(fit, highlight=TRUE)
#plot(fit)
#text(fit)

#set.seed(1991)  
#data("tissue_gene_expression")
#fit <- with(tissue_gene_expression, 
#            train(x, y, method = "rpart",
#                  tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)),
#                  control = rpart.control(minsplit = 0)))

#plot(fit$finalModel)
#text(fit$finalModel)


set.seed(1991)  
data("tissue_gene_expression")
fit <- with(tissue_gene_expression, 
            train(x, y, method = "rf", nodesize=1,
                  tuneGrid = data.frame(mtry = seq(50, 200, 25))))

fit$results
ggplot(fit,highlight=TRUE)

imp <- fit$varImp
