#Q1
set.seed(1)
data("heights")
library(caret)
ks <- seq(1, 101, 3)
F_1 <- sapply(ks, function(k){
  test_index <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)
  test_set <- heights[test_index, ]
  train_set <- heights[-test_index, ]
  fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class") %>% 
    factor(levels = levels(train_set$sex))
  F_meas(data = y_hat, reference = test_set$sex)
})
plot(ks, F_1)
max(F_1)

#Q2
library(dslabs)
data("tissue_gene_expression")

set.seed(1)
library(caret)
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
train_index <- createDataPartition(y, list = FALSE)
sapply(seq(1, 11, 2), function(k){
  fit <- knn3(x[train_index,], y[train_index], k = k)
  y_hat <- predict(fit, newdata = data.frame(x=x[-train_index,]),
                   type = "class")
  mean(y_hat == y[-train_index])
})