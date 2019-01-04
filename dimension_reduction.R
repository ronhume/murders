library(dslabs)
library(caret)
library(dplyr)

data("tissue_gene_expression")
dim(tissue_gene_expression$x)

pc <- prcomp(tissue_gene_expression$x)
avgs <- rowMeans(tissue_gene_expression$x)
data.frame(pc_1 = pc$x[,1], avgs = avg, 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(avgs, pc_1, color = tissue)) +
  geom_point()
cor(avgs,pc$x[,1])

for(i in 1:10){
  boxplot(pc$x[,i] ~ tissue_gene_expression$y, main = paste("PC", i))
}

plot(summary(pc)$importance[3,])