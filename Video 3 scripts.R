library(HistData)

galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

library(caret)
y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p =0.5, list = FALSE)

train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

# predicts
avg <- mean(train_set$son)
avg

fit <- lm(son ~ father, data = train_set)
fit$coef

#Predict funtion
y_hat <- predict(fit, test_set)
mean((y_hat - test_set$son)^2)

#Regression for a Categorical Outcome
library(HistData)
train_set %>% 
  filter(round(height)==66) %>%
  summarise(mean(sex=="Female"))
