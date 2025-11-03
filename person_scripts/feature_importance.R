library(ranger)
library(caret)
library(dplyr)

set.seed(123)

#  Split into train/test 
train_idx <- sample(seq_len(nrow(model_data)), 0.8 * nrow(model_data))
train_data <- model_data[train_idx, ]
test_data  <- model_data[-train_idx, ]

#  Ensure target is a factor 
train_data$INJ_SEVERE_BINARY <- as.factor(train_data$INJ_SEVERE_BINARY)
test_data$INJ_SEVERE_BINARY  <- as.factor(test_data$INJ_SEVERE_BINARY)

#  Train Random Forest 
rf_under <- ranger(
  dependent.variable.name = "INJ_SEVERE_BINARY",
  data = train_data,
  num.trees = 500,
  importance = "permutation",
  classification = TRUE,
  num.threads = parallel::detectCores() - 1,
  seed = 123
)

imp_perm <- tibble(
  Variable = names(rf_under$variable.importance),
  Importance = rf_under$variable.importance
) %>% arrange(desc(Importance))
