library(caret)
set.seed(1)

# Train a dummy model that just guesses by class frequency
dummy_pred <- factor(sample(levels(train_top40$INJ_SEVERE_BINARY),
                            nrow(test_top40),
                            replace = TRUE,
                            prob = prop.table(table(train_top40$INJ_SEVERE_BINARY))))

confusionMatrix(dummy_pred, test_top40$INJ_SEVERE_BINARY, positive = "1")
