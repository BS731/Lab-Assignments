# Load libraries
library(dplyr)
library(MASS)
library(ggplot2)
library(tidyr)
library(caTools)  # Load caTools library

# Read data
data <- read.csv("C:/Users/srava/Downloads/oulad-students.csv")
assessment_data <- read.csv("C:/Users/srava/Downloads/oulad-assessments.csv")

# Data preprocessing
factor_columns <- c("code_module", "code_presentation", "gender", "region", 
                    "highest_education", "imd_band", "age_band", "num_of_prev_attempts", 
                    "disability", "final_result")
data[factor_columns] <- lapply(data[factor_columns], as.factor)

# Drop NA values
data <- na.omit(data)

# Remove the "Distinction" level from final_result
data <- subset(data, final_result != "Distinction")

# Convert response variable to numeric
data$num_of_prev_attempts <- as.numeric(as.character(data$num_of_prev_attempts))

# Split data
set.seed(123) 
split <- sample.split(data$final_result, SplitRatio = 0.8)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

# Classification Model (LDA)
classification_model <- lda(final_result ~ ., data = train_data)
classification_predictions <- predict(classification_model, newdata = test_data)
classification_accuracy <- mean(classification_predictions$class == test_data$final_result)

# Regression Model (Linear Regression)
# Ensure 'final_result' factor levels are consistent between train and test data
train_data$final_result <- factor(train_data$final_result, levels = levels(test_data$final_result))

# For simplicity, we'll predict 'num_of_prev_attempts' as a regression task
regression_model <- lm(num_of_prev_attempts ~ ., data = train_data)
regression_predictions <- predict(regression_model, newdata = test_data)

# Remove missing values from test_data$num_of_prev_attempts and regression_predictions
test_data <- test_data[!is.na(test_data$num_of_prev_attempts), ]
regression_predictions <- regression_predictions[!is.na(test_data$num_of_prev_attempts)]

# Calculate RMSE only if there are no missing values
if (!any(is.na(regression_predictions))) {
  regression_rmse <- sqrt(mean((regression_predictions - test_data$num_of_prev_attempts)^2))
  cat("Regression RMSE:", round(regression_rmse, 2), "\n")
} else {
  cat("Regression RMSE: NA (due to missing values)\n")
}

# Classification Model Interpretation
cat("\nClassification Accuracy:", round(classification_accuracy * 100, 2), "%\n")

# Regression Model Interpretation
cat("\nRegression Coefficients:\n")
print(summary(regression_model)$coefficients)
cat("\n")

# Feature Importance (For LDA, we can interpret the discriminant functions)
cat("\nFeature Importance for Classification Model (LDA):\n")
print(coef(classification_model))

# Visualize distribution of num_of_prev_attempts
ggplot(data, aes(x = num_of_prev_attempts)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Distribution of num_of_prev_attempts",
       x = "num_of_prev_attempts",
       y = "Frequency") +
  theme_minimal()

# Visualize residuals
residuals <- test_data$num_of_prev_attempts - regression_predictions
ggplot() +
  geom_histogram(aes(x = residuals), binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Residual Analysis for Regression Model",
       x = "Residuals",
       y = "Frequency") +
  theme_minimal()

# Visualize classification predictions
classification_df <- data.frame(actual = test_data$final_result, predicted = classification_predictions$class)

ggplot(classification_df, aes(x = actual, y = predicted)) +
  geom_point(position = position_jitter(width = 0.1, height = 0), alpha = 0.5) +
  labs(title = "Classification Predictions vs. Actual",
       x = "Actual Class",
       y = "Predicted Class") +
  theme_minimal()

# Visualize regression predictions
regression_df <- data.frame(actual = test_data$num_of_prev_attempts, predicted = regression_predictions)

ggplot(regression_df, aes(x = actual, y = predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Regression Predictions vs. Actual",
       x = "Actual num_of_prev_attempts",
       y = "Predicted num_of_prev_attempts") +
  theme_minimal()
