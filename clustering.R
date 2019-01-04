library(dplyr)
library(dslabs)
library(lattice)
library(caret)

data("tissue_gene_expression")

d<- dist(tissue_gene_expression$x - rowMeans(tissue_gene_expression$x))

h <- hclust(d)
plot(h)

cl <- kmeans(tissue_gene_expression$x, centers = 7)
table(cl$cluster, tissue_gene_expression$y)

library(RColorBrewer)
sds <- matrixStats::colSds(tissue_gene_expression$x)
ind <- order(sds, decreasing = TRUE)[1:50]
colors <- brewer.pal(7, "Dark2")[as.numeric(tissue_gene_expression$y)]