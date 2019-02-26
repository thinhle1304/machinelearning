library(dslabs)
library(tidyverse)
library(caret)

mnist <- read_mnist()
class(mnist$train$images)

x <- mnist$train$images[1:1000,]
y <- mnist$train$labels[1:1000]

x_1 <- 1:5
x_2 <- 6:10
cbind(x_1, x_2)
dim(x)

# convert vector to matrix
my_vector <- 1:15
mat <- matrix(my_vector, 5, 3, byrow = TRUE)
mat

########################
install.packages("matrixStats")


########## Test
mnist <- read_mnist()
y <- rowMeans(mnist$train$images>50 & mnist$train$images<205)
qplot(as.factor(mnist$train$labels), y, geom = "boxplot")
mean(y)