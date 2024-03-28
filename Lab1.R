library(dplyr)
library(caTools)  
library(MASS)     
library(ggplot2)  
data <- read.csv("C:/Users/srava/Downloads/oulad-students.csv")

factor_columns <- c("code_module", "code_presentation", "gender", "region", 
                    "highest_education", "imd_band", "age_band", "num_of_prev_attempts", 
                    "disability", "final_result")

data[factor_columns] <- lapply(data[factor_columns], as.factor)

data <- na.omit(data)

set.seed(123) 
split <- sample.split(data$final_result, SplitRatio = 0.8)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

model <- lda(final_result ~ ., data = train_data)

predictions <- predict(model, newdata = test_data)

predicted_classes <- as.factor(predictions$class)

conf_matrix <- table(predicted_classes, test_data$final_result)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

conf_matrix_df <- as.data.frame(conf_matrix)
conf_matrix_df$predicted_classes <- rownames(conf_matrix_df)
conf_matrix_df <- tidyr::gather(conf_matrix_df, actual_classes, value, -predicted_classes)

ggplot(conf_matrix_df, aes(x = predicted_classes, y = actual_classes, fill = factor(value))) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("lightblue", "lightcoral", "lightgreen", "lightpink", "lightsalmon", "lightseagreen", "lightsteelblue", "lightskyblue")) +
  labs(title = "Confusion Matrix",
       x = "Predicted Classes",
       y = "Actual Classes",
       fill = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cat("Accuracy:", round(accuracy * 100, 2), "%\n")
