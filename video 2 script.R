library(dslabs)
library(tidyverse)
library(caret)
data(heights)

y <- heights$sex
x <- heights$height

set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set <- heights[-test_index, ]
test_set <- heights[test_index, ]

# Simple prediction
y_hat <- sample(c("Male", "Female"), length(test_index), replace =TRUE) %>%
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)

# Check male higher than women
heights %>% group_by(sex) %>% summarize(mean(height), sd(height))

# predict male if height is within 2 Sd from the average male -> make it better
y_hat <- ifelse(x > 62, "Male", "Female") %>% factor(levels = levels(test_set$sex))
mean(y == y_hat)

#Make it even better
cutoff <- seq(61,70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})
max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

# TEst accuracy is not overly optimistic
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)

#function table
table(predicted = y_hat, actual = test_set$sex)

#Accuracy seperately for each sex
test_set %>%
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>%
  summarize(accuracy = mean(y_hat == sex))


#maxtrix
confusionMatrix(data = y_hat, reference = test_set$sex)

#Maximizing the F score
cutoff <- seq(61,70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})
max(F_1)

#ROC and precision
p <- 0.9
y_hat <- sample(c("Male" , "Female"), length(test_index), replace = TRUE, prob=c(p, 1-p)) %>%
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)

#sencond method
cutoffs <- c(50, seq(60, 75), 80)
height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>%
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       FDR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})

