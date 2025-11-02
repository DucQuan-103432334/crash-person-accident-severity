library(ggplot2)
imp_df <- data.frame(
  Feature = names(rf_under$variable.importance),
  Importance = rf_under$variable.importance
)
ggplot(imp_df[order(-imp_df$Importance)[1:20],], aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Feature Importances", x = "Feature", y = "Importance")
