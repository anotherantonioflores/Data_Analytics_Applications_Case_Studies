library(tidyverse)
library(here)
library(lubridate)
library(tree)
library(rpart)
library(rpart.plot)
library(ggplotify)
library(ggplot2)
library(caret)
library(e1071)

data = read.delim("C:/Users/Client/OneDrive/UTSA MSDA/FALL 2024/DA Applications/case study 3/dow_jones_index.txt", sep = ",")

summary(data)
colSums(is.na(data))
table(data$stock)

# Initial cleaning of the complete data set
clean_data = data %>% 
  mutate(stock = as.factor(stock)) %>% 
  mutate(date = mdy(date), across(c(open, high, low, close, next_weeks_open, next_weeks_close), 
                                      ~ as.numeric(str_remove_all(.x, "\\$|,")))) %>%
  select(!next_weeks_open) %>% 
  select(!next_weeks_close) %>% 
  select(!percent_return_next_dividend) %>% 
  select(!days_to_next_dividend) %>% 
  arrange(date) %>% na.omit(clean_data) 

summary(clean_data)
colSums(is.na(clean_data))
summary(clean_data)
str(clean_data)

# Splitting Data
set.seed(123)
Train_data = clean_data %>% 
  filter(quarter == 1)
Test_data = clean_data %>% 
  filter(quarter ==2)

# Removing unnecessary variables and creating "week" variable to use in lieu of date.
Train_data_clean = Train_data %>% group_by(stock) %>% mutate(week = row_number()) %>%
  ungroup() %>%
  select(-quarter)

Test_data_clean = Test_data %>% group_by(stock) %>% mutate(week = row_number()) %>%
  ungroup() %>%
  select(-quarter)

# Plot to check volume of shares traded by date.
ggplot(data = Train_data, aes(x =date, y = volume, group = stock, color = stock)) +
  geom_line()

# Creating initial decision tree.
tree.dj = tree(percent_change_next_weeks_price~. -date, data = Train_data_clean)
summary(tree.dj)

levels(Train_data_clean$stock)

plot(tree.dj)
text(tree.dj)

# Perform cost complexity pruning by CV
cv.dj = cv.tree(tree.dj)
cv.dj
which.min(cv.dj$size)
best_size = cv.dj$size[which.min(cv.dj$dev)]
best_size

# Plot the estimated test error rate 
par(mfrow = c(1, 2))
plot(cv.dj$size, cv.dj$dev, type= "b")
plot(cv.dj$k, cv.dj$dev, type= "b")
# Note: Best size = 5

# Pruned the tree utilizing the best size (5).
par(mfrow = c(1, 1))
prune.dj = prune.tree(tree.dj, best = 5)
plot(prune.dj)
text(prune.dj, pretty = 0)

# Utilized alternate method to create a tree plot due to issues with stock variable
# combining levels in tree plot.
par(mfrow = c(1, 1))
tree.dj2 <- rpart(percent_change_next_weeks_price~. -date, data = Train_data_clean)
optimal_cp <- tree.dj2$cptable[which.min(tree.dj2$cptable[, "xerror"]), "CP"]
optimal_cp
pruned_tree <- prune(tree.dj2, cp = 0.03)
rpart.plot(pruned_tree, main = "rpart method")

# Get predictions on the test data 
preds_dj_pruned = predict(prune.dj, newdata=Test_data_clean)
preds_dj = predict(tree.dj, newdata=Test_data_clean)

# Computed the MSE. 
# The MSE for the pruned tree: 8.42
# The MSE for the initial tree: 9.23
mean((preds_dj_pruned - Test_data_clean$percent_change_next_weeks_price)^2)
mean((preds_dj - Test_data_clean$percent_change_next_weeks_price)^2)

##########################################
# Code for looping though each stock level
library(Metrics)

stock_levels <- unique(clean_data$stock)
# Created a blank df to paste the MSE's into.
mse_results <- data.frame(stock = character(), MSE = numeric(), stringsAsFactors = FALSE)

# For loop that iterates through each stock level returning the tree plot and MSE.
formula = percent_change_next_weeks_price ~ .

for (level in stock_levels) {
  train_subset <- Train_data_clean[Train_data_clean$stock == level, ]
  test_subset <- Test_data_clean[Test_data_clean$stock == level, ]
  
  train_subset = train_subset %>% 
    select(!stock)
  
  test_subset = test_subset %>% 
    select(!stock)
  model1 = lm(formula, data=train_subset)
  
  model2 <- tree(formula, data = train_subset, method = "anova")
  
  model3 = svm(formula, data = train_subset, kernel = "linear")
  model4 = svm(formula, data = train_subset, kernel = "radial")
  model5 = svm(formula, data = train_subset, kernel = "poly")
  
  #plot(model)
  #text(model, pretty = 0)
  #title(main = paste("Decision Tree for Stock:", level))
  predictions1 <- predict(model1, test_subset)
  predictions2 = predict(model2, test_subset)
  predictions3 = predict(model3, test_subset)
  predictions4 = predict(model4, test_subset)
  predictions5 = predict(model5, test_subset)
  
  mse_value1 <- mse(test_subset$percent_change_next_weeks_price, predictions1)
  mse_value2 = mse(test_subset$percent_change_next_weeks_price, predictions2)
  mse_value3 = mse(test_subset$percent_change_next_weeks_price, predictions3)
  mse_value4 = mse(test_subset$percent_change_next_weeks_price, predictions4)
  mse_value5 = mse(test_subset$percent_change_next_weeks_price, predictions5)
  
  mse_results <- rbind(mse_results, data.frame(stock = level, MSE_LM = mse_value1, MSE_tree = mse_value2, MSE_SVM_linear = mse_value3, MSE_SVM_radial = 
                                                 mse_value4, MSE_SVM_poly = mse_value5))
}


head(mse_results)
mse_results <- rbind(mse_results, data.frame(stock = "Average MSE", MSE_LM = mean(mse_results$MSE_LM), MSE_tree = mean(mse_results$MSE_tree),
                                             MSE_SVM_linear = mean(mse_results$MSE_SVM_linear), MSE_SVM_radial = mean(mse_value4),
                                                                   MSE_SVM_poly = mean(mse_results$MSE_SVM_poly)))
print(mse_results)


