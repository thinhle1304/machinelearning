mnist <- read_mnist()
names(mnist)
dim(mnist$train$images)
class(mnist$train$labels)
table(mnist$train$labels)

set.seed(123)
index <- sample(nrow(mnist$train$images), 1000)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])

index <- sample(nrow(mnist$train$images), 1000)
x_test <- mnist$train$images[index,]
y_test <- factor(mnist$train$labels[index])


#Pre-processing
#Compute sd for each column
library(matrixStats)
sds <- colSds(x)
qplot(sds, bin = 256, color = I("black"))

#Remove due to near zero variance
library(caret)
nzv <- nearZeroVar(x)


#keeping column
col_index <- setdiff(1:ncol(x), nzv)
length(col_index)

#Model Fitting for MNIST Data
#add column name
colnames(x) <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(mnist$train$images)

#knn
control <- trainControl(method = "cv", number = 10, p = .9)
train_knn <- train(x[,col_index], y,
                   method = "knn",
                   tuneGrid = data.frame(k = c(1,3,5,7)),
                   trControl = control)

#test how long for data set
n <- 1000
b <- 2
index <- sample(nrow(x), n)
control <- trainControl(method = "cv", number = b, p = .9)
train_knn <- train(x[index, col_index], y[index],
                   method = "knn",
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)
#fit entire dataset
fit_knn <- knn3(x[ ,col_index], y, k=3)
#Check accuracy
y_hat_knn <- predict(fit_knn,
                     x_test[, col_index],
                     type="class")
cm <- confusionMatrix(y_hat_knn, factor(y_test))
cm$overall["Accuracy"]
#check specificity and sensitivity
cm$byClass[,1:2]
 
#Using nSamp argument in the Rborist function
library(Rborist)
control <- trainControl(method = "cv", number = 5, p = 0.8)
grid <- expand.grid(minNode = c(1,5), predFixed = c(10, 15, 25, 35, 50))

train_rf <- train(x[ , col_index],
                  y,
                  method = "Rborist",
                  nTree = 50,
                  trControl = control,
                  tuneGrid = grid,
                  nSamp = 5000)
ggplot(train_rf)
train_rf$bestTune

#Set the final trees to large number
fit_rf <- Rborist(x[, col_index], y,
                  nTree = 1000,
                  minNode = train_rf$bestTune$minNode,
                  predFixed = train_rf$bestTune$predFixed)
#Check Accuracy
y_hat_rf <- factor(levels(y)[predict(fit_rf, x_test[ ,col_index])$yPred])
cm <- confusionMatrix(y_hat_rf, y_test)
cm$overall["Accuracy"]


#Variable Importance - R not support
library(randomForest)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])
rf <- randomForest(x, y, ntree = 50)
imp <- importance(rf)

#image plot
image(matrix(imp, 28, 28))

#Compare with k-nearest neighbors
p_max <- predict(fit_knn, x_test[,col_index])
p_max <- apply(p_max, 1, max)
ind <- which(y_hat_knn != y_test)
ind <- ind[order(p_max[ind], decreasing = TRUE)]

#probabilities by taking the average of the class probabilities provided by random forest
# vs k-nearest neighbors
p_rf <- predict(fit_rf, x_test[,col_index])$census
p_rf <- p_rf / rowSums(p_rf)
p_knn <- predict(fit_knn, x_test[,col_index])
p <- (p_rf + p_knn) / 2
y_pred <- factor(apply(p, 1, which.max)-1)
confusionMatrix(y_pred, y_test)




