install.packages("caret")  # Install if not already installed
library(caret)

control <- rfeControl(functions = rfFuncs,  # Use Random Forest for selection
                      method = "cv",       # Cross-validation
                      number = 10)         # 10-fold CV

set.seed(123)  # Ensure reproducibility

df_numeric <- final %>% 
  select()

# Define predictor variables and target variable
predictors <- df_numeric %>%
  select(-points.x)


# Perform median imputation on the predictors
preprocess <- preProcess(predictors, method = "medianImpute")
predictors <- predict(preprocess, newdata = predictors)

# Exclude target variable
target <- df_numeric$points.x  # Target variable

# Perform RFE
results <- rfe(x = predictors, 
               y = target, 
               sizes = c(5, 10, 15, 20),  # Number of features to test
               rfeControl = control)


## RFE for Assists


# Exclude target variable
target <- df_numeric$assists.x  # Target variable

# Perform RFE
results_1 <- rfe(x = predictors, 
               y = target, 
               sizes = c(5, 10, 15, 20),  # Number of features to test
               rfeControl = control)


print(results)
predictors(results)  # Get the best subset of features



df_model <- df_numeric[, c("field_goals_made.x", "field_goals_attempted.x", "three_pointers_made.x", 
                           "free_throws_made.x", "true_shooting_percentage.x", "free_throws_attempted.x",
                           "three_pointers_percentage.x", "possessions.x", "effective_field_goal_percentage.x",
                           "field_goals_percentage.x", "pace.x", "pace_per40.x", "net_rating_opp_team", 
                           "usage_percentage.x", "pie.x", "points.x"), drop = FALSE]



set.seed(123)  # For reproducibility

trainIndex <- createDataPartition(df_model$points.x, p = 0.8, list = FALSE)
train_data <- df_model[trainIndex, ]
test_data <- df_model[-trainIndex, ]


## Random Forest

library(randomForest)

set.seed(123)
rf_model <- randomForest(points.x ~ ., data = train_data, importance = TRUE, ntree = 500)

# Predictions
rf_predictions <- predict(rf_model, test_data)

# Evaluate Performance
rf_rmse <- sqrt(mean((rf_predictions - test_data$points.x)^2))
print(paste("Random Forest RMSE:", rf_rmse))





# Call the function with your actual data frame (final)
get_player_averages(final)

# Display the result
print(player_averages)

predictions <- predict(rf_model, newdata = player_averages)

averages <- c("field_goals_made.x" = 6.3, 
              "field_goals_attempted.x" = 12, 
              "three_pointers_made.x" = 3, 
              "free_throws_made.x" = 1.1, 
              "true_shooting_percentage.x" = 0.639, 
              "free_throws_attempted.x" = 1.3,
              "three_pointers_percentage.x" = 0.437, 
              "possessions.x" = 67, 
              "effective_field_goal_percentage.x" = 0.631, 
              "field_goals_percentage.x" = 0.510, 
              "pace.x" = 100.073, 
              "pace_per40.x" = 83.397, 
              "net_rating_opp_team" = -6.01, 
              "usage_percentage.x" = 0.178, 
              "pie.x" = 0.09)
averages_df <- as.data.frame(t(averages))

predicted_points <- predict(rf_model, newdata = averages_df)

print(predicted_points)


importance(rf_model)
varImpPlot(rf_model)

