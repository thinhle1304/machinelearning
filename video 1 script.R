<<<<<<< HEAD
#instrall package
install.packages('caret', dependencies = TRUE)
=======
install.packages("caret")
>>>>>>> 320c9017496ce2b6e1c2069778d78d253461399e
install.packages("dslabs")

library(caret)
library(dslabs)
library(tidyverse)
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




