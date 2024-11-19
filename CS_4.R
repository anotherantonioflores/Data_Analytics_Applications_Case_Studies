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

# Establishing data split.
index<- sample(1:nrow(acquisitionRetention), 0.7 * nrow(acquisitionRetention))
train <- acquisitionRetention[index, ]
test <- acquisitionRetention[-index, ]

# Initializing randomforest model for predicting acquisition.
TreeBagA <- randomForest(as.factor(acquisition)~.-duration, data = train, ntree = 1000, importance = TRUE)
print(TreeBagA)

# Checking misclassification.

bag.probs.test = predict(TreeBagA, newdata = test)
matrix_test = table(test$acquisition, bag.probs.test)
Test.errTree = 1-sum(diag(matrix_test))/sum(matrix_test)
print(Test.errTree)

# Specifying observations where the predicted acquisition is 1 to be used
# in model to predict duration.
predicted_Acq <- predict(TreeBagA, train)
pred_positives <- train[predicted_Acq == 1, ]

# Train a random forest model to predict duration.
TreeBagD <- randomForest(duration~.-acquisition, data = pred_positives, ntree = 1000, importance = TRUE)
print(TreeBagD)

# Plot tree structure and importance
plot(TreeBagD)

test <- test %>% filter(acquisition == 1)
# Predict on the OOB data and calculate the mean squared error (MSE)
yhat.bag_rf <- predict(TreeBagD, newdata = test)
pred.test <- acquisitionRetention[-index, "duration"]
mse_rf <- mean((yhat.bag_rf - pred.test)^2)
cat("MSE for Random Forest model: ", mse_rf, "\n")

# Plot predictions vs actual values (Random Forest)
plot(yhat.bag_rf, pred.test)
abline(0, 1)

# Feature importance
varImpPlot(TreeBagD)

# Get Boosted
set.seed(1)
TreeBoost <- gbm(duration~. - acquisition, data = pred_positives, distribution = "gaussian",n.trees = 5000, interaction.depth = 4,
                 shrinkage = 0.05, verbose = F)

yhat.boost <- predict(TreeBoost, newdata = test, ntrees = 5000)
pred.test.boost <- test$duration
mse_boost <- mean((yhat.boost - pred.test.boost)^2)
cat("MSE for Random Forest model: ", mse_boost, "\n")

# Hyperparameter tuning with rfsrc
mtry.values <- seq(4, 6, 1)
nodesize.values <- seq(4, 8, 2)
ntree.values <- seq(4e3, 6e3, 1e3)

# Create a data frame containing all combinations 
hyper_grid <- expand.grid(mtry = mtry.values, nodesize = nodesize.values, ntree = ntree.values)

# Create an empty vector to store OOB error values
oob_err <- numeric(nrow(hyper_grid))

# Loop over the hyperparameter grid
set.seed(1)
for (i in 1:nrow(hyper_grid)) {
  
  # Train a Random Forest model with rfsrc
  model <- rfsrc(duration~. -acquisitionRetention, data = pred_positives,
                 mtry = hyper_grid$mtry[i],
                 nodesize = hyper_grid$nodesize[i],
                 ntree = hyper_grid$ntree[i])  
  
  # Store OOB error for the model                      
  oob_err[i] <- model$err.rate[length(model$err.rate)]
}

# Identify optimal set of hyperparameters based on OOB error
opt_i <- which.min(oob_err)
cat("Optimal Hyperparameters: \n")
print(hyper_grid[opt_i,])

# Train the best model using rfsrc with optimal hyperparameters
BestTree <- rfsrc(duration ~.-acquisition, 
                  data = pred_positives,
                  mtry = 6,
                  nodesize = 4,
                  ntree = 5000)

# Predict on the OOB data and calculate MSE
yhat.bag_rfsrc <- predict(BestTree, newdata = test)
actual.test <- test$duration
mse_rfsrc <- mean((yhat.bag_rfsrc$predicted - actual.test)^2)
cat("MSE for rfsrc model: ", mse_rfsrc, "\n")

