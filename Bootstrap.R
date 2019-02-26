#Q1
set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)

sum(indexes[[1]] == 3)
sum(indexes[[1]] == 4)
sum(indexes[[1]] == 7)

#Q2
x=sapply(indexes, function(ind){
  sum(ind == 3)
})
sum(x)

#Q3
set.seed(1)
B <- 10000
q_75 <- replicate(B, {
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
})

mean(q_75)
sd(q_75)

#Q4
set.seed(1)
indexes <- createResample(y, 10)
q_75_star <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)

#Q5
set.seed(1)
indexes <- createResample(y, 10000)
q_75_star <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)