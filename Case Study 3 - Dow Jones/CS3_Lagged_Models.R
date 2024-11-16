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

data = read.delim("dow_jones_index.data.txt", sep = ",")

summary(data)
colSums(is.na(data))
table(data$stock)



# Initial cleaning of the complete data set. We removed variables whose values
# were from a future point in time

clean <-data %>% 
  mutate(stock = as.factor(stock)) %>% 
  mutate(date = mdy(date), across(c(open, high, low, close, next_weeks_open, next_weeks_close), 
                                  ~ as.numeric(str_remove_all(.x, "\\$|,")))) %>% 
  select(!next_weeks_open) %>% 
  select(!next_weeks_close) 

clean_lags <- clean %>% arrange(date) %>% group_by(quarter,stock) %>%
  mutate(week = row_number()) %>% mutate(last_wks_open = lag(open, 1), 
                                         last_wks_close = lag(close, 1), last_wks_high = lag(high, 1), 
                                         last_wks_low = lag(low, 1),
                                         last_wks_perc_price_change = lag(percent_change_price, 1),
                                         current_dividend_perc_return = lag(percent_return_next_dividend, 1)) %>%
  ungroup() %>% 
  select(!percent_return_next_dividend)


clean_no_lag = clean %>% arrange(date) %>% group_by(quarter,stock) %>% mutate(week = row_number()) %>%
  ungroup() %>% 
  select(!percent_return_next_dividend)

# Correlation plot

library(corrplot)
corr_matrix <- cor(clean_lags[, 4:20], use = "complete.obs")
plot.new()
dev.off()
png("correlation_plot.png", width = 1200, height = 1200)
corrplot(corr_matrix, method = "circle", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)
dev.off()

# Plot of some of the highly correlated variables to test assumption of no linearity between predictors.

plot(clean_lags$open, clean_lags$high)
plot(clean_lags$open, clean_lags$high)
plot(clean_lags$previous_weeks_volume, clean_lags$volume)

summary(clean_lags)
colSums(is.na(clean_lags))
summary(clean_lags)
str(clean_lags)

# Splitting Data
set.seed(123)
Train_data = clean_lags %>% 
  filter(quarter == 1)
Test_data = clean_lags %>% 
  filter(quarter ==2)

# Removing unnecessary variables and creating "week" variable to use in lieu of date.
Train_data_clean = Train_data %>% group_by(stock) %>% mutate(week = row_number()) %>%
  ungroup() %>%
  select(-quarter, -date)

Test_data_clean = Test_data %>% group_by(stock) %>% mutate(week = row_number()) %>%
  ungroup() %>%
  select(-quarter, -date)

# Plot to check volume of shares traded by date.
ggplot(data = Train_data, aes(x =date, y = volume, group = stock, color = stock)) +
  geom_line()

ggplot(data = Test_data, aes(x =date, y = volume, group = stock, color = stock)) +
  geom_line()

# Plot to check current weeks percent change in price of stock traded by date.
ggplot(data = Train_data, aes(x =date, y = percent_change_price, group = stock, color = stock)) +
  geom_line()

clean %>% filter(quarter == 2) %>% ggplot(aes(x =date, y = percent_change_next_weeks_price, group = stock, color = stock)) +
  geom_line() + geom_vline(xintercept = as.Date("2011-05-27"), linetype = "dashed", color = "red") +
  annotate("text", x = as.Date("2011-05-27"), y = -15, label = "2011-05-27", color = "red", angle = 0, vjust = -0.5)

# Creating initial decision tree.
lag_tree.dj = tree(percent_change_next_weeks_price~., data = Train_data_clean)
summary(lag_tree.dj)

levels(Train_data_clean$stock)

plot(lag_tree.dj)
text(lag_tree.dj, pretty = 0)

# Perform cost complexity pruning by CV
cv.dj = cv.tree(lag_tree.dj)
cv.dj
which.min(cv.dj$size)
best_size = cv.dj$size[which.min(cv.dj$dev)]
best_size

# Plot the estimated test error rate 
par(mfrow = c(1, 2))
plot(cv.dj$size, cv.dj$dev, type= "b")
plot(cv.dj$k, cv.dj$dev, type= "b")
# Note: Best size = 4

# Pruned the tree utilizing the best size (4).
par(mfrow = c(1, 1))
prune.dj = prune.tree(lag_tree.dj, best = 4)
plot(prune.dj)
text(prune.dj, pretty = 0)

# Utilized alternate method to create a tree plot due to issues with stock variable
# combining levels in tree plot.
tree.dj2 <- rpart(percent_change_next_weeks_price~., data = Train_data_clean)
optimal_cp <- tree.dj2$cptable[which.min(tree.dj2$cptable[, "xerror"]), "CP"]
optimal_cp
pruned_tree <- prune(tree.dj2, cp = 0.0335)
rpart.plot(pruned_tree, main = "rpart method")

# Get predictions on the test data 
preds_dj_pruned = predict(prune.dj, newdata=Test_data_clean)
preds_dj = predict(lag_tree.dj, newdata=Test_data_clean)

# Computed the MSE. 
# The MSE for the pruned tree: 8.05
# The MSE for the initial tree: 9.8
mean((preds_dj_pruned - Test_data_clean$percent_change_next_weeks_price)^2)
mean((preds_dj - Test_data_clean$percent_change_next_weeks_price)^2)

##########################################
# Code for looping though each stock level
library(Metrics)

stock_levels <- unique(clean_no_lag$stock)
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
    select(!stock) %>% na.omit()
  
  model1 = lm(formula, data=train_subset, na.action = na.omit)
  #par(mfrow = c(2, 2))
  #plot(model1)
  
  model2 <- tree(formula, data = train_subset, method = "anova")
  
  model3 = svm(formula, data = na.omit(train_subset), kernel = "linear")
  model4 = svm(formula, data = na.omit(train_subset), kernel = "radial")
  model5 = svm(formula, data = na.omit(train_subset), kernel = "poly")
  
  #par(mfrow = c(1, 1))
  #plot(model2)
  #text(model2, pretty = 0)
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
  
  mse_results <- rbind(mse_results, data.frame(stock = level, MSE_LM = round(mse_value1), MSE_tree = mse_value2, MSE_SVM_linear = mse_value3, MSE_SVM_radial = 
                                                 mse_value4, MSE_SVM_poly = mse_value5))
}


head(mse_results)
mse_results <- rbind(mse_results, data.frame(stock = "Average MSE", MSE_LM = mean(mse_results$MSE_LM,na.rm =TRUE), MSE_tree = mean(mse_results$MSE_tree),
                                             MSE_SVM_linear = mean(mse_results$MSE_SVM_linear), MSE_SVM_radial = mean(mse_value4),
                                             MSE_SVM_poly = mean(mse_results$MSE_SVM_poly)))
print(mse_results)


# Grid for individual tree plots

stock_levels <- unique(Train_data_clean$stock)
Count_Most_Important_Node <- c()
par(mfrow = c(2, 4))
# For loop that iterates through each stock level and generates a decision tree plot

for (level in stock_levels) {
  train_subset <- Train_data_clean[Train_data_clean$stock == level, ]
  model <- tree(percent_change_next_weeks_price ~ ., data = train_subset)
  plot(model)
  text(model, pretty = 0)
  title(main = paste(level))
  splits <- model$frame[model$frame$var != "<leaf>", "var"]
  feature_names <- names(train_subset)[-1]
  splits <- feature_names[splits]
  Count_Most_Important_Node <- c(Count_Most_Important_Node, splits)
}
feature_counts <- table(Count_Most_Important_Node)

sorted_feature_counts <- sort(feature_counts, decreasing = TRUE)

print(sorted_feature_counts)

