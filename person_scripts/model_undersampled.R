library(ranger)
library(caret)
library(dplyr)

set.seed(123)

#  Stratified undersampling (50/50)
majority <- model_data %>% filter(INJ_SEVERE_BINARY == 0)
minority <- model_data %>% filter(INJ_SEVERE_BINARY == 1)
n <- min(nrow(majority), nrow(minority))

undersampled_data <- bind_rows(
  majority %>% sample_n(n),
  minority %>% sample_n(n)
)

cat("After undersampling - class balance:\n")
print(table(undersampled_data$INJ_SEVERE_BINARY))

#  Split into train/test 
train_idx <- sample(seq_len(nrow(undersampled_data)), 0.8 * nrow(undersampled_data))
train_data <- undersampled_data[train_idx, ]
test_data  <- undersampled_data[-train_idx, ]

#  Ensure target is a factor 
train_data$INJ_SEVERE_BINARY <- as.factor(train_data$INJ_SEVERE_BINARY)
test_data$INJ_SEVERE_BINARY  <- as.factor(test_data$INJ_SEVERE_BINARY)

#  Train Random Forest 
rf_under <- ranger(
  dependent.variable.name = "INJ_SEVERE_BINARY",
  data = train_data,
  num.trees = 500,
  importance = "impurity",
  classification = TRUE,
  num.threads = parallel::detectCores() - 1,
  seed = 123
)

#  Evaluate on test set
pred <- predict(rf_under, data = test_data)$predictions

cm <- confusionMatrix(as.factor(pred), as.factor(test_data$INJ_SEVERE_BINARY), positive = "1")

cat("\n Confusion Matrix:\n")
print(cm$table)

cat("\n Accuracy:", round(cm$overall["Accuracy"], 3))
cat("\n Precision:", round(cm$byClass["Precision"], 3))
cat("\n Recall:", round(cm$byClass["Recall"], 3))
cat("\n F1 Score:", round(cm$byClass["F1"], 3), "\n")
