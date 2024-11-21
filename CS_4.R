# install.packages('SMCRM')
library(SMCRM)
library(randomForest)
library(gbm)
library(tree)
library(randomForestSRC)
library(dplyr)

###### SPARK NOTES FOR BELOW CODE ######
# TreeBagB predicts acquisition best with an overall prediction accuracy of 79.3%. Optimal mtry: 1, ntrees: 3000 (line 72).
# When predicting acquisition, employees and industry were the most important predictors (lines 43 & 44).
# Best model for predicting duration using the predicted acquisitions from TreeBagB is the BoostB model with a MAPE of 21.85 using full dataset (line 180).
# The most important variables when predicting duration were employees and revenue (line 121).
# PDP plots on line 124.
# Theres a plot on line 73 that shows number of trees vs error that is good.



data("acquisitionRetention")
?acquisitionRetention
# Set seed for reproducibility
set.seed(1)

# Establishing data split
index <- sample(1:nrow(acquisitionRetention), 0.7 * nrow(acquisitionRetention))
train <- acquisitionRetention[index, ]
test <- acquisitionRetention[-index, ]

# Initial cleaning
train = train %>% select(-ret_exp, -ret_exp_sq, -freq, -freq_sq, -sow, -crossbuy,-profit)
plot(train)
test = test %>% select(-ret_exp,-ret_exp_sq, -freq, -freq_sq, -sow, -crossbuy, -profit)

# Initializing randomforest model for predicting acquisition
TreeBagA <- randomForest(as.factor(acquisition) ~ acq_exp + acq_exp_sq + industry + revenue + employees,
                         data = train, ntree = 1000, importance = TRUE)

# Evaluating model and identifying the 2 most significant predictors (employees and industry)
print(TreeBagA)
plot(TreeBagA, main="OOB Error vs Number of Trees", xlab="Number of Trees", ylab="Error Rate")
legend("topright", legend = c("OOB Error", "Class 1 Error", "Class 2 Error"), 
       col = c(1, 2, 3), lty = 1, cex = 0.8)
importance(TreeBagA)
varImpPlot(TreeBagA)

# Testing prediction accuracy on the test data (MSE: 0.207)
bag.probs.test <- predict(TreeBagA, newdata = test)
matrix_test <- table(test$acquisition, bag.probs.test)
Test.errTree <- 1 - sum(diag(matrix_test)) / sum(matrix_test)
print(Test.errTree)

# Hyperparameter tuning loop
mtry.values <- seq(1, 5, 1)
ntree.values <- seq(1e3, 8e3, 2e3)
hyper_grid <- expand.grid(mtry = mtry.values, ntree = ntree.values)

# Empty vector for storing OOB error
oob_err <- c()

# Loop over hyperparameter grid

for (i in 1:nrow(hyper_grid)) {
  set.seed(1)
  model <- randomForest(as.factor(acquisition) ~ acq_exp + acq_exp_sq + industry + revenue + employees, data = train, importance = TRUE, mtry = hyper_grid$mtry[i], ntree = hyper_grid$ntree[i])
  oob_err[i] <- model$err.rate[length(model$err.rate)]
}

# Identify optimal hyperparameters (mtry: 1 & ntree: 3000)
opt_i <- which.min(oob_err)
print(hyper_grid[opt_i, ])

# Final Random Forest model with optimal hyperparameters
TreeBagB <- randomForest(as.factor(acquisition) ~ acq_exp + acq_exp_sq + industry + revenue + employees, data = train, mtry = 1, ntree = 3000, importance = TRUE)
print(TreeBagB)
plot(TreeBagB, main="OOB Error vs Number of Trees", xlab="Number of Trees", ylab="Error Rate")
legend("topright", legend = c("OOB Error", "Class 1(0) Error", "Class 2(1) Error"), 
       col = c(1, 2, 3), lty = 1, cex = 0.8)
importance(TreeBagB)
varImpPlot(TreeBagB)


# Prediction and error on test data (MSE: 0.207)
bag.probs.test <- predict(TreeBagB, newdata = test)
matrix_test <- table(test$acquisition, bag.probs.test)
Test.errTree <- 1 - sum(diag(matrix_test)) / sum(matrix_test)
print(Test.errTree)

# Gradient boosting model (MSE not as good as random forest [0.24])
boostA <- gbm(acquisition ~ acq_exp + acq_exp_sq + industry + revenue + employees, data = train, distribution = 'bernoulli', n.trees = 1000, interaction.depth = 1, shrinkage = 0.001, verbose = F)
boost.probs <- predict(boostA, n.trees = 1000, newdata = test, type = "response")
boost.pred <- ifelse(boost.probs > 0.5, 1, 0)
matrix_test <- table(test$acquisition, boost.pred)
Test.errBoost <- 1 - sum(diag(matrix_test)) / sum(matrix_test)
print(Test.errBoost)

# Filter train data for predicted positives
subset =  filter(train, TreeBagA$predicted == 1)

# Train regression model to predict duration
TreeBagD <- randomForest(duration ~ . - acquisition, data = subset, ntree = 1000, importance = TRUE)
print(TreeBagD)

# Filter test set for acquisition == 1
test <- test %>% filter(acquisition == 1)

# Predict on the filtered test set
yhat.bag_rf <- predict(TreeBagD, newdata = test)
pred.test <- test$duration

# Calculate Mean Squared Error (MSE) when predicting duration (171238.6)
mse_rf <- mean((yhat.bag_rf - pred.test)^2)
print(mse_rf)

# MAPE when predicting duration (26.55)
abs_percent_error <- abs((yhat.bag_rf - pred.test) / pred.test) * 100
mape_rf <- mean(abs_percent_error)
print(mape_rf)

# Boosted Tree for Predicting duration
boostB <- gbm(duration~. - acquisition, data = subset, n.trees = 1000, interaction.depth = 2, shrinkage = 0.001, verbose = F)
summary(boostB)
boost.probs <- predict(boostB, n.trees = 1000, newdata = test)

#Partial dependence plot
par(mfrow = c(1, 2))
plot(boostB, i = "employees")
plot(boostB, i = "revenue")


# Calculate Mean Squared Error (MSE)
yhat.boost <- predict(boostB, newdata = test)
pred.test <- test$duration
mse_boost <- mean((yhat.boost - pred.test)^2)
print(mse_boost)

# MAPE when predicting duration (22.38)
abs_percent_error <- abs((yhat.boost - pred.test) / pred.test) * 100
mape_Boost <- mean(abs_percent_error)
print(mape_Boost)

# Hyper Parameter Tuning for predicting duration.
mtry.values <- seq(1, 5, 1)
ntree.values <- seq(1e3, 8e3, 2e3)
hyper_grid <- expand.grid(mtry = mtry.values, ntree = ntree.values)

# Empty vector for storing OOB error
oob_err <- numeric(nrow(hyper_grid))

# Loop over hyperparameter grid

for (i in 1:nrow(hyper_grid)) {
  set.seed(1)
  model <- randomForest(duration ~. -acquisition, data = subset, importance = TRUE, mtry = hyper_grid$mtry[i], ntree = hyper_grid$ntree[i])
  oob_err[i] <- model$mse[length(model$mse)]
}

# Identify optimal hyperparameters
opt_i <- which.min(oob_err)
print(hyper_grid[opt_i, ])

TreeBagE <- randomForest(duration ~ . - acquisition, data = subset, ntree = 5000, mtry=4, importance = TRUE)
print(TreeBagE)

# Filter test set for acquisition == 1
test <- test %>% filter(acquisition == 1)

# Predict on the filtered test set
yhat.bag_rf <- predict(TreeBagE, newdata = test)
pred.test <- test$duration

# Calculate Mean Squared Error (MSE)
mse_rf <- mean((yhat.bag_rf - pred.test)^2)
print(mse_rf)

# MAPE when predicting duration (28.96)
abs_percent_error <- abs((yhat.bag_rf - pred.test) / pred.test) * 100
mape_rf <- mean(abs_percent_error)
print(mape_rf)

# Strongest model for predicting acquisition is TreeBagB
# Strongest model for predicting duration is BoostB
# Using the BoostB model to predict full dataset
Full.data = acquisitionRetention %>% filter(acquisition == 1)
yhat.boost <- predict(boostB, newdata = Full.data)
pred.full <- acquisitionRetention %>% filter(acquisition == 1) %>% select(duration) %>% pull(duration)

# Calculate Mean Squared Error (MSE)
mse_boost <- mean((yhat.boost - pred.full)^2)
print(mse_boost)

# MAPE
abs_percent_error <- abs((yhat.boost - pred.full) / pred.full) * 100
mape_Boost <- mean(abs_percent_error)
print(mape_Boost)

