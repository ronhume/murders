library(tidyverse)
library(dslabs)
library(caret)

set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

fit <-train(x_subset,y, method="glm")
fit$results

#library(devtools)
#devtools::install_bioc("genefilter")
#BiocManager::install("genefilter", version="3.8")
library(genefilter)
tt <- colttests(x, y)

pvals <- tt$p.value
# answer - gives wrong value
#ind <- which((abs(pvals) <= 0.01))
#l <- length(ind)
# I used
ron_ind <- tt$p.value <= 0.01
rl <-sum(ron_ind)
ind <- which(ron_ind)
l <- length(ind)
x_subset_fit <- x[ ,ind]
fit_2 <-train(x_subset_fit,y, method="glm")
fit_2$results

