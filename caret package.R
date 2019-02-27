library(caret)
train_glm <- train(y ~ ., method = "glm", data = mnist_27$train)
train_knn <- train(y~ ., method = "knn", data = mnist_27$train)

y_hat_glm <- predict(train_glm, mnist_27$test, type = "raw")
y_hat_knn <- predict(train_knn, mnist_27$test, type = "raw")

confusionMatrix(y_hat_glm, mnist_27$test$y)$overall["Accuracy"]
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall["Accuracy"]

## getModelInfo("knn)

library(tidyverse)
ggplot(train_knn, highlight = TRUE) 

## Practice
train_knn <- train(y~ ., method = "knn",
                   data = mnist_27$train,
                   tuneGrid = data.frame(k = seq(9, 7, 2)))
ggplot(train_knn, highlight = TRUE) 

train_knn$bestTune
train_knn$finalModel

#Control by 10-fold-cross-validation
control <- trainControl(method = "cv", number = 10, p = 9)
train_knn_cv <- train(y~ ., method = "knn",
                      data = mnist_27$train,
                      tuneGrid = data.frame(k = seq(9, 71, 2)))

install.packages("gam")
library(gam)

# Improve
grid <- expand.grid(span = seq(0.15, 0.65, len = 10), degree = 1)
train_loess <- train(y ~ ., 
                     method = "gamLoess",
                     tuneGrid=grid,
                     data = mnist_27$train)


