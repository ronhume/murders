library(tidyverse)
library(dslabs)
library(caret)
library(ggplot2)

set.seed(1993)
data("tissue_gene_expression")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]


#params < data$train %>% group_by(y) %>%
#  summarize(avg_1 = mean(x_1), avg_2 = mean(x_2), sd_1 = sd(x_1), sd_2 = sd(x_2, r = cor(x_1,x_2)))

#params <- params %>% mutate(sd_1 = mean(sd_1), sd_2 = mean(sd_2), r=mean(r))
#params

#train_lda <- train(x,y,
#                   method = "lda", data=tissue_gene_expression)
#y_hat <- predict(train_lda, x)
#cm <- confusionMatrix(data=y_hat, reference = tissue_gene_expression$y)$overall["Accuracy"]
# answer Q1
fit_lda <- train(x, y, method = "qda", preprocess="center")
print(fit_lda$results["Accuracy"])
df <- t(fit_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.))

ggplot(df, aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()

d <- apply(fit_lda$finalModel$means, 2, diff)
ind <- order(abs(d), decreasing = TRUE)[1:2]
plot(x[, ind], col = y)


set.seed(1993)
data("tissue_gene_expression")
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]

train_lda <- train(x,y,
                   method = "lda", data=tissue_gene_expression)
y_hat <- predict(train_lda, x)
cm <- confusionMatrix(data=y_hat, reference = tissue_gene_expression$y)$overall["Accuracy"]
