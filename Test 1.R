library(dslabs)
library(dplyr)
library(lubridate)

data("reported_heights")

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

#Q1: What is the propotion of females in class and online? (That is, calculate
#the proportion of the in class students who are female and the proportion of the online students who are female.)

dat %>% group_by(type) %>%
        summarise(p_female = mean(sex == "Female"))

#Q2: If you used the type variable to predict sex, what would the prediction accuracy be?

y_hat <- ifelse( x == "online", "Male", "Female") %>%
  factor(levels = levels(y))
mean(y_hat == y)

#Q3: Write a line of code using the table function to show
#the confusion matrix, assuming the prediction is y_hat and the truth is y.

table(predicted = y_hat, actual = y)

#Q4: What is the sensitivity of this prediction?
library(caret)
sensitivity(y_hat, y)

#Q5: What is the specificity of this prediction?
library(caret)
specificity(y_hat, y)

#Q6: What is the prevalence (% of females) in the dat dataset defined above?
mean(y == "Female")

