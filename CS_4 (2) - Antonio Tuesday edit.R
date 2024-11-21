# install.packages('SMCRM')
library(SMCRM)
library(randomForest)
library(gbm)
library(tree)
library(randomForestSRC)
library(dplyr)
library(MASS)
library(e1071)
library(tree)
library(rpart)

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
train = train %>% dplyr::select(-ret_exp, -ret_exp_sq, -freq, -freq_sq, -sow, -crossbuy,-profit)
plot(train)
test = test %>% dplyr::select(-ret_exp,-ret_exp_sq, -freq, -freq_sq, -sow, -crossbuy, -profit)


# Initializing randomforest model for predicting acquisition
TreeBagA <- randomForest(as.factor(acquisition) ~ acq_exp + acq_exp_sq + industry + revenue + employees,
                         data = train, ntree = 1000, importance = TRUE)


### Other Models
log_model = glm(as.factor(acquisition) ~ acq_exp + acq_exp_sq + industry + revenue + employees, data = train, family = "binomial")

lda_model = lda(as.factor(acquisition) ~ acq_exp + acq_exp_sq + industry + revenue + employees, data = train)

svm_linear = svm(as.factor(acquisition) ~ acq_exp + acq_exp_sq + industry + revenue + employees, data = train, type = 'C-classification', kernel = "linear")
svm_radial = svm(as.factor(acquisition) ~ acq_exp + acq_exp_sq + industry + revenue + employees, data = train, type = 'C-classification', kernel = "radial")
svm_poly = svm(as.factor(acquisition) ~ acq_exp + acq_exp_sq + industry + revenue + employees, data = train, type = 'C-classification', kernel = "poly")

tree_model <- tree(as.factor(acquisition) ~ acq_exp + acq_exp_sq + industry + revenue + employees, data = train, method = "class")



# Other model predictions

log_preds <- predict(log_model, newdata = test, type = "response")
logpred1 = ifelse(log_preds>0.5, 1, 0)

lda_preds <- predict(lda_model, newdata = test)
svm_lin_preds <- predict(svm_linear, newdata = test)
svm_rad_preds <- predict(svm_radial, newdata = test)
svm_poly_preds <- predict(svm_poly, newdata = test)

tree_preds <- predict(tree_model, newdata = test)
treepred1 = ifelse(tree_preds[, 1] >0.5, 1, 0)



# Testing prediction accuracy on the test data (MSE: 0.207)
bag.probs.test <- predict(TreeBagA, newdata = test)
matrix_testRf <- table(test$acquisition, bag.probs.test)
Test.errRf <- 1 - sum(diag(matrix_testRf)) / sum(matrix_testRf)
print(Test.errRf)


matrix_test1 <- table(test$acquisition, logpred1)
Test.err_log <- 1 - sum(diag(matrix_test1)) / sum(matrix_test1)

matrix_test2 <- table(test$acquisition, lda_preds$class)
Test.err_lda <- 1 - sum(diag(matrix_test2)) / sum(matrix_test2)

matrix_test3 <- table(test$acquisition, svm_lin_preds)
Test.err_svm_lin <- 1 - sum(diag(matrix_test3)) / sum(matrix_test3)

matrix_test4 <- table(test$acquisition, svm_rad_preds)
Test.err_svm_rad <- 1 - sum(diag(matrix_test4)) / sum(matrix_test4)

matrix_test5 <- table(test$acquisition, svm_poly_preds)
Test.err_svm_poly <- 1 - sum(diag(matrix_test5)) / sum(matrix_test5)

matrix_test6 <- table(test$acquisition, treepred1)
Test.err_tree <- 1 - sum(diag(matrix_test6)) / sum(matrix_test6)




rbind(Test.err_log, Test.err_lda, Test.err_svm_lin, Test.err_svm_poly, Test.err_svm_rad, Test.err_tree, Test.errRf)


# Evaluating model and identifying the 2 most significant predictors (employees and industry)
print(TreeBagA)
plot(TreeBagA, main="OOB Error vs Number of Trees")
legend("topright", legend = c("OOB Error", "Class 1 Error", "Class 2 Error"), 
       col = c(1, 2, 3), lty = 1, cex = 0.8)
importance(TreeBagA)
varImpPlot(TreeBagA)

length(TreeBagA$predicted)

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
TreeBagB <- randomForest(as.factor(acquisition) ~ acq_exp + acq_exp_sq + industry + revenue + employees, 
                         data = train, mtry = 1, ntree = 3000, importance = TRUE)
print(TreeBagB)
plot(TreeBagB, main="OOB Error vs Number of Trees")
legend("topright", legend = c("OOB Error", "Class 1(0) Error", "Class 2(1) Error"), 
       col = c(1, 2, 3), lty = 1, cex = 0.8)
importance(TreeBagB)
varImpPlot(TreeBagB)


# Prediction and error on test data (MSE: 0.207)
bag.probs.test <- predict(TreeBagB, newdata = test)
matrix_test7 <- table(test$acquisition, bag.probs.test)
Test.errRF2 <- 1 - sum(diag(matrix_test7)) / sum(matrix_test7)
print(Test.errRF2)

# Gradient boosting model (MSE not as good as random forest [0.24])
boostA <- gbm(acquisition ~ acq_exp + acq_exp_sq + industry + revenue + employees, 
              data = train, distribution = 'bernoulli', n.trees = 1000, interaction.depth = 1, shrinkage = 0.001, verbose = F)
boost.probs <- predict(boostA, n.trees = 1000, newdata = test, type = "response")
boost.pred <- ifelse(boost.probs > 0.5, 1, 0)
matrix_test <- table(test$acquisition, boost.pred)
Test.errBoost <- 1 - sum(diag(matrix_test)) / sum(matrix_test)
print(Test.errBoost)


#### Getting Actual Predictions

combined = rbind(train, test)
set.seed(1)
actual_predictions <- predict(TreeBagA, newdata = combined)
summary(actual_predictions)
combined = cbind(combined, actual_predictions)

subset = filter(combined, actual_predictions ==1)


################################################################3
# Filter train data for predicted positives
#subset =  filter(train, TreeBagA$predicted == 1)
subset = subset %>% 
  dplyr::select(!actual_predictions)


index2 <- sample(1:nrow(subset), 0.7 * nrow(subset))
new_train <- subset[index2, ]
new_test <- subset[-index2, ]



# Train regression model to predict duration
TreeBagD <- randomForest(duration ~ . - acquisition, data = new_train, ntree = 1000, importance = TRUE)
print(TreeBagD)

# Filter test set for acquisition == 1
#test <- test %>% filter(acquisition == 1)

# Predict on the filtered test set
yhat.bag_rf <- predict(TreeBagD, newdata = new_test)
pred.test <- new_test$duration

# Calculate Mean Squared Error (MSE) when predicting duration (171238.6)
mse_rf <- mean((yhat.bag_rf - pred.test)^2)
print(mse_rf)

# MAPE when predicting duration (26.55)
abs_percent_error <- abs((yhat.bag_rf - pred.test) / pred.test) * 100
mape_rf <- mean(abs_percent_error)
print(mape_rf)

# Boosted Tree for Predicting duration
boostB <- gbm(duration~. - acquisition, data = new_train, n.trees = 1000, interaction.depth = 2, shrinkage = 0.001, verbose = F)
summary(boostB)
boost.probs <- predict(boostB, n.trees = 1000, newdata = new_test)

#Partial dependence plot
par(mfrow = c(1, 2))
plot(boostB, i = "employees")
plot(boostB, i = "revenue")


# Calculate Mean Squared Error (MSE)
yhat.boost <- predict(boostB, newdata = new_test)
#pred.test <- test$duration
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
  model <- randomForest(duration ~. -acquisition, data = new_train, importance = TRUE, mtry = hyper_grid$mtry[i], ntree = hyper_grid$ntree[i])
  oob_err[i] <- model$mse[length(model$mse)]
}

# Identify optimal hyperparameters
opt_i <- which.min(oob_err)
print(hyper_grid[opt_i, ])

TreeBagE <- randomForest(duration ~ . - acquisition, data = new_train, ntree = 5000, mtry=4, importance = TRUE)
print(TreeBagE)

# Filter test set for acquisition == 1
#test <- test %>% filter(acquisition == 1)

# Predict on the filtered test set
yhat.bag_rf <- predict(TreeBagE, newdata = new_test)
#pred.test <- test$duration

# Calculate Mean Squared Error (MSE)
mse_rf <- mean((yhat.bag_rf - pred.test)^2)
print(mse_rf)

# MAPE when predicting duration (28.96)
abs_percent_error <- abs((yhat.bag_rf - pred.test) / pred.test) * 100
mape_rf <- mean(abs_percent_error)
print(mape_rf)

# Strongest model for predicting acquisition is TreeBagB
# Strongest model for predicting duration is BoostB


######################################
##Do we need this part?
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

