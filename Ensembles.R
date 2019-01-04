library(caret)
library(dslabs)
set.seed(1)
data("mnist_27")

#models <- c("glm", "lda",  "naive_bayes",  "svmLinear", 
#            "gamboost",  "gamLoess", "qda", 
#            "knn", "kknn", "loclda", "gam",
#            "rf", "ranger",  "wsrf", "Rborist", 
#            "avNNet", "mlp", "monmlp",
#            "adaboost", "gbm",
#            "svmRadial", "svmRadialCost", "svmRadialSigma")

#fits <- lapply(models, function(model){ 
#  print(model)
#  train(y ~ ., method = model, data = mnist_27$train)
#}) 

#names(fits) <- models

#pred <- sapply(fits, function(object) 
#  predict(object, newdata = mnist_27$test))
#dim(pred)

#acc <- colMeans(pred == mnist_27$test$y)
#acc
#mean(acc)

# my solution
#ens <- sapply(c(seq(1,200,1)), function(index) {
#  uniqv <- unique(pred[index,])
#  uniqv[which.max(tabulate(match(pred[index,], uniqv)))]
#})

#ens_acc <- mean(ens == mnist_27$test$y)

#his solution
#votes <- rowMeans(pred == "7")
#y_hat <- ifelse(votes > 0.5, "7", "2")
#mean(y_hat == mnist_27$test$y)

# accuracy estimates from cross validation
#acc_hat <- sapply(fits, function(fit) min(fit$results$Accuracy))
#mean(acc_hat)

models <- c("glm", "naive_bayes", 
            "gamboost",  "gamLoess", "qda", 
            "knn", "loclda", "gam",
            "ranger",  "wsrf", "Rborist", 
            "adaboost", "gbm",
            "svmRadial", "svmRadialCost", "svmRadialSigma")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models

pred <- sapply(fits, function(object) 
  predict(object, newdata = mnist_27$test))
dim(pred)

acc <- colMeans(pred == mnist_27$test$y)
acc
mean(acc)
votes <- rowMeans(pred == "7")
y_hat <- ifelse(votes > 0.5, "7", "2")
mean(y_hat == mnist_27$test$y)
