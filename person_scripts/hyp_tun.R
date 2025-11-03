library(lightgbm)
library(dplyr)
library(tictoc)


# ============================================================
# 1. Prepare data
# ============================================================
set.seed(123)

#  Split into train/test 
train_idx <- sample(seq_len(nrow(model_data)), 0.8 * nrow(model_data))
train_data <- model_data[train_idx, ]
test_data  <- model_data[-train_idx, ]


#  Ensure target is a factor 
train_data$INJ_SEVERE_BINARY <- as.factor(train_data$INJ_SEVERE_BINARY)
test_data$INJ_SEVERE_BINARY  <- as.factor(test_data$INJ_SEVERE_BINARY)

top40_vars <- imp_perm %>%
  slice(1:40) %>%
  pull(Variable)


train_top40 <- train_data %>%
  select(all_of(c("INJ_SEVERE_BINARY", top40_vars)))

test_top40 <- test_data %>%
  select(all_of(c("INJ_SEVERE_BINARY", top40_vars)))

train_matrix <- as.matrix(train_top40 %>% select(-INJ_SEVERE_BINARY))
train_label  <- train_top40$INJ_SEVERE_BINARY
train_label <- as.numeric(as.character(train_top40$INJ_SEVERE_BINARY))

dtrain <- lgb.Dataset(data = train_matrix, label = train_label)

# ============================================================
# 2. Custom F1 evaluation metric
# ============================================================
f1_eval <- local({
  call_count <- 0L  # create a counter in a local environment
  
  function(preds, dtrain) {
    call_count <<- call_count + 1L
    
    # Extract labels
    labels <- lightgbm::get_field(dtrain, "label")


    
    # For debugging, only print the first 3 calls
    if (call_count <= 3L) {
      cat("\n----- [F1 EVAL DEBUG - Call", call_count, "] -----\n")
      cat("Length(preds):", length(preds), "\n")
      cat("Range(preds):", paste(round(range(preds), 4), collapse = " - "), "\n")
      cat("Labels table:\n")
      print(table(labels))
      cat("Sample preds (first 10):", paste(round(head(preds, 10), 3), collapse = ", "), "\n")
      cat("--------------------------------------------\n")
    }
    
    # Threshold predictions
    preds_binary <- ifelse(preds > 0.5, 1, 0)
    
    # Compute metrics
    tp <- sum(preds_binary == 1 & labels == 1)
    fp <- sum(preds_binary == 1 & labels == 0)
    fn <- sum(preds_binary == 0 & labels == 1)
    
    precision <- ifelse(tp + fp == 0, 0, tp / (tp + fp))
    recall <- ifelse(tp + fn == 0, 0, tp / (tp + fn))
    f1 <- ifelse((precision + recall) == 0, 0, 2 * precision * recall / (precision + recall))
    
    # Return LightGBM-compliant output
    return(list(
      name = "f1",
      value = f1,
      higher_better = TRUE
    ))
  }
})



# ============================================================
# 3. Random parameter grid (500 combos)
# ============================================================

#  Stratified undersampling (50/50)
majority <- train_data %>% filter(INJ_SEVERE_BINARY == 0)
minority <- train_data %>% filter(INJ_SEVERE_BINARY == 1)
ratio <- nrow(majority) / nrow(minority)
cat("Class ratio (neg:pos) =", round(ratio, 2), "\n")


tune_grid <- data.frame(
  learning_rate = runif(500, 0.01, 0.3),
  nrounds = sample(seq(200, 1000, by = 50), 500, replace = TRUE),
  num_leaves = sample(15:255, 500, replace = TRUE),
  min_data_in_leaf = sample(10:100, 500, replace = TRUE),
  min_gain_to_split = runif(500, 0, 2),
  bagging_fraction = runif(500, 0.6, 1.0),
  bagging_freq = sample(1:10, 500, replace = TRUE),
  feature_fraction = runif(500, 0.6, 1.0),
  lambda_l2 = runif(500, 0, 1),
  scale_pos_weight = runif(500, 0.5 * ratio, 2 * ratio)
)

cat("🔧 Created", nrow(tune_grid), "parameter combinations\n\n")

# ============================================================
# 4. Base parameters
# ============================================================
base_params <- list(
  objective = "binary",
  boosting = "gbdt",
  metric = "binary_logloss",
  verbose = -1
)

# ============================================================
# 5. Random search with F1 CV
# ============================================================
results <- data.frame()
best_score <- -Inf
best_params <- NULL

tic("Total tuning time")

for (i in 1:nrow(tune_grid)) {
  p <- tune_grid[i, ]
  params <- modifyList(base_params, as.list(p))
  params$feature_pre_filter <- FALSE

  cv <- lgb.cv(
    params = params,
    data = dtrain,
    nrounds = p$nrounds,
    nfold = 5,
    eval = f1_eval,
    stratified = TRUE,
    early_stopping_rounds = 50,
    verbose = -1
  )
  
  f1_best <- max(unlist(cv$record_evals$valid$f1$eval))
  
  best_iter <- cv$best_iter
  
  results <- rbind(
    results,
    cbind(
      tune_grid[i, ],
      F1 = f1_best,
      BestIter = best_iter
    )
  )
  
  if (f1_best > best_score) {
    best_score <- f1_best
    best_params <- params
  }
  
  cat(sprintf(
    paste0(
      "Combo %03d/%03d → ",
      "F1=%.4f | Best=%.4f | ",
      "LR=%.3f | Trees=%d | Leaves=%d | MinData=%d | MinGain=%.3f | ",
      "BagFrac=%.2f | BagFreq=%d | FeatFrac=%.2f | L2=%.3f | ",
      "ScalePosWeight=%.2f \n"
    ),
    i, nrow(tune_grid),
    f1_best, best_score,
    p$learning_rate, p$nrounds, p$num_leaves, p$min_data_in_leaf, p$min_gain_to_split,
    p$bagging_fraction, p$bagging_freq, p$feature_fraction, p$lambda_l2, p$scale_pos_weight
  ))
}

toc(log = TRUE)

# ============================================================
# 6. Save & view top configs
# ============================================================
results <- results %>% arrange(desc(F1))
write.csv(results, "lgbm_f1_randomsearch_withLR.csv", row.names = FALSE)

cat("\n🏆 Top 10 configurations by F1:\n")
print(head(results, 10))
cat("\n💾 Saved all results to 'lgbm_f1_randomsearch_withLR.csv'\n")

cat("\n✅ Best overall F1 =", round(best_score, 4), "\n")
print(best_params)
