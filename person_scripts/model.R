library(ranger)
library(caret)
library(dplyr)

set.seed(123)

# --- Split into train/test ---
train_idx <- sample(seq_len(nrow(model_data)), 0.8 * nrow(model_data))
train_data <- model_data[train_idx, ]
test_data  <- model_data[-train_idx, ]

# --- Define target and features ---
target <- "INJ_SEVERE_BINARY"
features <- setdiff(names(model_data), target)

# --- Convert target to factor for classification ---
train_data[[target]] <- as.factor(train_data[[target]])
test_data[[target]]  <- as.factor(test_data[[target]])

# --- Class weights (based on training data counts) ---
tbl <- table(train_data[[target]])
class_weights <- as.numeric(sum(tbl) / (length(tbl) * tbl))
names(class_weights) <- names(tbl)

cat("Class weights used:\n")
print(class_weights)

# --- Train weighted random forest ---
rf_weighted <- ranger(
  dependent.variable.name = target,
  data = train_data,
  num.trees = 500,
  importance = "impurity",
  classification = TRUE,
  class.weights = class_weights,
  num.threads = parallel::detectCores() - 1,
  seed = 123
)

# --- Predictions ---
pred <- predict(rf_weighted, data = test_data)$predictions

# --- Confusion Matrix ---
cm <- confusionMatrix(as.factor(pred), as.factor(test_data[[target]]), positive = "1")
cat("\n Confusion Matrix:\n")
print(cm$table)

# --- Metrics ---
cat("\n Accuracy:", round(cm$overall["Accuracy"], 3))
cat("\n Precision:", round(cm$byClass["Precision"], 3))
cat("\n Recall:", round(cm$byClass["Recall"], 3))
cat("\n F1 Score:", round(cm$byClass["F1"], 3), "\n")
