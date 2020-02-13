#predict sex based on height using knn algo
#test k values of seq(1, 101, 3) 
#calculate F1 scores
library(dslabs)
library(caret)
library(tidyverse)
#library(purrr) for the map_df function
data(heights)
x <- heights$height
y <- heights$sex
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_data <- heights[-test_index,]
test_data <- heights[test_index,]
y_train <- train_data$sex
y_test <- test_data$sex

k <- seq(1, 101, 3)
#using map_df
F_1s <- map_df(k, function(k) {
  alg_knn <- knn3(sex~height, data = train_data, k = k)
  y_hat <- predict(alg_knn, newdata = test_data, type = "class") %>%
    factor(levels = levels(train_data$sex))
  f1 <- F_meas(data = y_hat, reference = factor(test_data$sex))
  tibble(k = k, F_1 = f1)
})
max(F_1s$F_1)
F_1s$k[which.max(F_1s$F_1)]

#using sapply
F_1s <- sapply(k, function(k) {
  alg_knn <- knn3(sex~height, data = train_data, k = k)
  y_hat <- predict(alg_knn, newdata = test_data, type = "class")
  y_hat <- y_hat %>% factor(levels = levels(y_train))
  F_meas(data = y_hat, reference = factor(y_test))
})

#what is the max value of the f1 score?
max(F_1s)
#what value of k does the max occur at?
k[which.max(F_1s)]