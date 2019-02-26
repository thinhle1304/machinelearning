#generate data set
set.seed(0)
if(!exists("mnist")) mnist <- read_mnist()
ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)
x <- mnist$train$images[ind,]
y <- mnist$train$labels[ind]  

### Test
library(dslabs)
data("tissue_gene_expression")

dim(tissue_gene_expression$x)

table(tissue_gene_expression$y)

d <- dist(tissue_gene_expression$x)

ind <- c(1, 2, 39, 40, 73, 74)
as.matrix(d)[ind,ind]