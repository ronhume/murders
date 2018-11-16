#library(dslabs)
#library(caret)
#library(tidyverse)
#library(dplyr)
#library(ggplot2)
#library(purrr)
#data("tissue_gene_expression")

#table(tissue_gene_expression$y)
#d<-dist(tissue_gene_expression$x)

#ind <- c(1,2,39,40,73,74)
#as.matrix(d)[ind,ind]

# my try
#library(dslabs)
#library(tidyverse)
#library(caret)
#data(heights)
#set.seed(1)
#y<-heights$sex
#x<-heights$height
#test_index<-createDataPartition(y,times=1,p=0.5,list=FALSE)
#train_set<-heights[test_index,]
#test_set<-heights[-test_index,]
#for (k_inc in c(seq(2,150))){
#  knn_fit <- knn3(sex~height, data = train_set, k=k_inc)
#  y_hat <- predict(knn_fit, test_set, type = "class") %>% factor(levels = levels(train_set$sex))
#  out <- F_meas(data = y_hat, reference = test_set$sex)
#  print(paste(k_inc,out))
#}

#set.seed(1)
#data("heights")
#library(caret)
#ks <- seq(1, 101, 3)
#F_1 <- sapply(ks, function(k){
#  test_index <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)
#  test_set <- heights[test_index, ]
#  train_set <- heights[-test_index, ]
#  fit <- knn3(sex ~ height, data = train_set, k = k)
#  y_hat <- predict(fit, test_set, type = "class") %>% 
#    factor(levels = levels(train_set$sex))
#  F_meas(data = y_hat, reference = test_set$sex)
#})
#plot(ks, F_1)
#max(F_1)
#print(paste(ks,F_1))

library(tidyverse)
library(dslabs)
library(caret)
data("tissue_gene_expression")
ks <- seq(1,11,2)
set.seed(1)
train_index  <- createDataPartition(tissue_gene_expression$y, times=1, p=0.5, list=FALSE)

x <- tissue_gene_expression$x
y <- tissue_gene_expression$y

train_set_x <- x[train_index,]
test_set_x  <- x[-train_index,]

train_set_y <- y[train_index]
test_set_y  <- y[-train_index]


F_1 <- sapply(ks,function(k){
  fit <- knn3(train_set_x, train_set_y, k = k)
  y_hat <- predict(fit, test_set_x, type = "class") %>%
    factor(levels=levels(train_set_y))
  confusionMatrix(data=y_hat,reference=test_set_y)$overall["Accuracy"]
})
#plot(ks, F_1)
#F_1

set.seed(1)
fit <- train(x,y,method="knn", trControl = trainControl(method = "boot"), preProcess = c("center", "scale"), tuneLength=10)

data("tissue_gene_expression")
fit <- with(tissue_gene_expression, train(x, y, method = "knn", tuneGrid = data.frame( k = seq(1, 7, 2))))
ggplot(fit)
fit$results
