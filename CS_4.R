# install.packages('SMCRM')
library(SMCRM)
library(randomForest)
library(gbm)
library(tree)
library(randomForestSRC)
library(dplyr)

data("acquisitionRetention")

# Set seed for reproducibility
set.seed(1)

# Establishing data split
index <- sample(1:nrow(acquisitionRetention), 0.7 * nrow(acquisitionRetention))
train <- acquisitionRetention[index, ]
test <- acquisitionRetention[-index, ]

# Convert 'acquisition' to factor for classification
train <- train %>% mutate(acquisition = as.factor(acquisition))
test <- test %>% mutate(acquisition = as.factor(acquisition))

# Initializing randomforest model for predicting acquisition
TreeBagA <- randomForest(as.factor(acquisition) ~ acq_exp + acq_exp_sq + industry + revenue + employees,
                         data = train, ntree = 1000, importance = TRUE)
print(TreeBagA)

# Prediction and error on test data
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
  model <- randomForest(as.factor(acquisition) ~ acq_exp + acq_exp_sq + industry + revenue + employees, data = train, importance = TRUE, mtry = hyper_grid$mtry[i], ntree = hyper_grid$ntree[i])
  oob_err[i] <- model$err.rate[length(model$err.rate)]
}

# Identify optimal hyperparameters
opt_i <- which.min(oob_err)
print(hyper_grid[opt_i, ])

# Final Random Forest model with optimal hyperparameters
TreeBagB <- randomForest(as.factor(acquisition) ~ acq_exp + acq_exp_sq + industry + revenue + employees, data = train, mtry = 1, ntree = 1000, importance = TRUE)
print(TreeBagB)

# Prediction and error on test data
bag.probs.test <- predict(TreeBagB, newdata = test)
matrix_test <- table(test$acquisition, bag.probs.test)
Test.errTree <- 1 - sum(diag(matrix_test)) / sum(matrix_test)
print(Test.errTree)

# Gradient boosting model
train$acquisition <- as.numeric(train$acquisition) - 1
test$acquisition <- as.numeric(test$acquisition) - 1
boostA <- gbm(acquisition ~ acq_exp + acq_exp_sq + industry + revenue + employees, data = train, distribution = 'bernoulli', n.trees = 1000, interaction.depth = 1, shrinkage = 0.001, verbose = F)
boost.probs <- predict(boostA, n.trees = 1000, newdata = test, type = "response")
boost.pred <- ifelse(boost.probs > 0.5, 1, 0)
matrix_test <- table(test$acquisition, boost.pred)
Test.errBoost <- 1 - sum(diag(matrix_test)) / sum(matrix_test)
print(Test.errBoost)

# Filter train data for predicted positives
predicted_Acq <- predict(TreeBagA, train)
pred_positives <- train[predicted_Acq == 1, ]

# Train regression model to predict duration
TreeBagD <- randomForest(duration ~ . - acquisition, data = pred_positives, ntree = 1000, importance = TRUE)
print(TreeBagD)

# Filter test set for acquisition == 1
test <- test %>% filter(acquisition == 1)

# Predict on the filtered test set
yhat.bag_rf <- predict(TreeBagD, newdata = test)
pred.test <- test$duration

# Calculate Mean Squared Error (MSE)
mse_rf <- mean((yhat.bag_rf - pred.test)^2)
print(mse_rf)
