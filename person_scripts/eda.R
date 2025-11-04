library(tidyverse)
library(ggplot2)
library(corrplot)
library(gridExtra)

# Set plotting theme
theme_set(theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold")))

cat(paste0(rep("=", 80), collapse = ""), "\n")
cat("EXPLORATORY DATA ANALYSIS\n")
cat(paste0(rep("=", 80), collapse = ""), "\n\n")

# ============================================================================
# 1. DESCRIPTIVE STATISTICS
# ============================================================================

cat("\n### 1. DESCRIPTIVE STATISTICS ###\n\n")

# Overall dataset summary
cat("Dataset Overview:\n")
cat("  Rows:", nrow(merged_data), "\n")
cat("  Columns:", ncol(merged_data), "\n")
cat("  Target variable: INJ_SEVERE_BINARY\n\n")

# Target variable distribution
cat("Target Variable Distribution:\n")
target_table <- table(merged_data$INJ_SEVERE_BINARY)
print(target_table)
cat("\nProportions:\n")
print(round(prop.table(target_table) * 100, 2))
cat("\n")

# Precompute factor versions for safe ggplot aesthetics
merged_data <- merged_data %>%
  mutate(
    INJ_SEVERE_BINARY_F = factor(INJ_SEVERE_BINARY, levels = c(0, 1), labels = c("No", "Yes")),
    IS_DRIVER_F = factor(IS_DRIVER, levels = c(0, 1), labels = c("Non-Driver", "Driver"))
  )

# Summary statistics for numeric variables
cat("\nNumeric Variables Summary:\n")
numeric_vars <- merged_data %>% 
  select_if(is.numeric) %>% 
  select(-starts_with("ROAD_USER_TYPE_DESC_")) %>%
  names()

summary_stats <- merged_data %>%
  select(all_of(numeric_vars)) %>%
  summary()
print(summary_stats)

# Categorical variables summary
cat("\nCategorical Variables Frequencies:\n")
categorical_vars <- c("SEX", "AGE_GROUP_SIMPL", "SAFETY_USED_FLAG", 
                     "TIME_OF_DAY", "SPEED_CAT", "ROAD_TYPE_GRP", 
                     "LIGHT_CAT", "DCA_GROUP", "ACCIDENT_TYPE_GROUP")

for(var in categorical_vars) {
  if(var %in% names(merged_data)) {
    cat("\n", var, ":\n", sep = "")
    print(table(merged_data[[var]], useNA = "ifany"))
  }
}

# ============================================================================
# 2. UNIVARIATE ANALYSIS
# ============================================================================

cat("\n\n### 2. UNIVARIATE ANALYSIS ###\n\n")
cat("Generating univariate visualizations...\n")

# Create output directory for plots
if(!dir.exists("eda_plots")) dir.create("eda_plots")

# --- Target Variable Distribution ---
p1 <- ggplot(merged_data, aes(x = INJ_SEVERE_BINARY_F, fill = INJ_SEVERE_BINARY_F)) +
  geom_bar(stat = "count", alpha = 0.7) +
  scale_fill_manual(values = c("No" = "steelblue", "Yes" = "coral"), name = "Severe Injury") +
  labs(title = "Distribution of Target Variable (INJ_SEVERE_BINARY)",
       x = "Severe Injury", y = "Count") +
  scale_y_continuous(labels = scales::comma) +
  theme(legend.position = "none")
print(p1)
ggsave("eda_plots/01_target_distribution.png", p1, width = 8, height = 6, dpi = 300)

# --- Age Group Distribution ---
if("AGE_GROUP_SIMPL" %in% names(merged_data)) {
  p2 <- ggplot(merged_data, aes(x = AGE_GROUP_SIMPL, fill = AGE_GROUP_SIMPL)) +
    geom_bar(stat = "count", alpha = 0.7) +
    coord_flip() +
    labs(title = "Age Group Distribution", x = "Age Group", y = "Count") +
    scale_y_continuous(labels = scales::comma) +
    theme(legend.position = "none")
  print(p2)
  ggsave("eda_plots/02_age_group_distribution.png", p2, width = 8, height = 6, dpi = 300)
}

# --- Safety Equipment Usage ---
if("SAFETY_USED_FLAG" %in% names(merged_data)) {
  p3 <- ggplot(merged_data, aes(x = SAFETY_USED_FLAG, fill = SAFETY_USED_FLAG)) +
    geom_bar(stat = "count", alpha = 0.7) +
    labs(title = "Safety Equipment Usage Distribution", x = "Safety Equipment", y = "Count") +
    scale_y_continuous(labels = scales::comma) +
    theme(legend.position = "none")
  print(p3)
  ggsave("eda_plots/03_safety_equipment.png", p3, width = 8, height = 6, dpi = 300)
}

# --- Time of Day Distribution ---
if("TIME_OF_DAY" %in% names(merged_data)) {
  p4 <- ggplot(merged_data, aes(x = TIME_OF_DAY, fill = TIME_OF_DAY)) +
    geom_bar(stat = "count", alpha = 0.7) +
    labs(title = "Time of Day Distribution", x = "Time of Day", y = "Count") +
    scale_y_continuous(labels = scales::comma) +
    theme(legend.position = "none")
  print(p4)
  ggsave("eda_plots/04_time_of_day.png", p4, width = 8, height = 6, dpi = 300)
}

# --- Speed Category Distribution ---
if("SPEED_CAT" %in% names(merged_data)) {
  p5 <- ggplot(merged_data, aes(x = SPEED_CAT, fill = SPEED_CAT)) +
    geom_bar(stat = "count", alpha = 0.7) +
    labs(title = "Speed Zone Category Distribution", x = "Speed Category", y = "Count") +
    scale_y_continuous(labels = scales::comma) +
    theme(legend.position = "none")
  print(p5)
  ggsave("eda_plots/05_speed_category.png", p5, width = 8, height = 6, dpi = 300)
}

# --- Vehicle Age Distribution (Histogram) ---
if("VEHICLE_AGE_AVG" %in% names(merged_data)) {
  p6 <- ggplot(merged_data, aes(x = VEHICLE_AGE_AVG)) +
    geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7, color = "black") +
    labs(title = "Vehicle Age Distribution", x = "Vehicle Age (years)", y = "Frequency") +
    scale_y_continuous(labels = scales::comma) +
    geom_vline(aes(xintercept = median(VEHICLE_AGE_AVG, na.rm = TRUE)), 
               color = "red", linetype = "dashed", size = 1) +
    annotate("text", x = median(merged_data$VEHICLE_AGE_AVG, na.rm = TRUE), 
             y = Inf, label = "Median", vjust = 1.5, hjust = -0.1, color = "red")
  print(p6)
  ggsave("eda_plots/06_vehicle_age_histogram.png", p6, width = 8, height = 6, dpi = 300)
}

# --- Driver Status Distribution ---
if("IS_DRIVER" %in% names(merged_data)) {
  p7 <- ggplot(merged_data, aes(x = factor(IS_DRIVER), fill = factor(IS_DRIVER))) +
    geom_bar(stat = "count", alpha = 0.7) +
    scale_fill_manual(values = c("0" = "steelblue", "1" = "coral"), name = "Is Driver") +
    labs(title = "Driver vs Non-Driver Distribution", x = "Is Driver (0=No, 1=Yes)", y = "Count") +
    scale_y_continuous(labels = scales::comma) +
    theme(legend.position = "none")
  print(p7)
  ggsave("eda_plots/07_driver_status.png", p7, width = 8, height = 6, dpi = 300)
}

# ============================================================================
# 3. BIVARIATE ANALYSIS
# ============================================================================

cat("\n\n### 3. BIVARIATE ANALYSIS ###\n\n")
cat("Generating bivariate visualizations...\n")

# --- Target vs Categorical Variables ---

# Age Group vs Target
if("AGE_GROUP_SIMPL" %in% names(merged_data)) {
p8 <- ggplot(merged_data, aes(x = AGE_GROUP_SIMPL, fill = INJ_SEVERE_BINARY_F)) +
    geom_bar(position = "fill", alpha = 0.8) +
    scale_fill_manual(values = c("No" = "steelblue", "Yes" = "coral"), 
                     name = "Severe Injury") +
    labs(title = "Injury Severity by Age Group", 
         x = "Age Group", y = "Proportion") +
    coord_flip()
  print(p8)
  ggsave("eda_plots/08_injury_by_age.png", p8, width = 8, height = 6, dpi = 300)
}

# Safety Equipment vs Target
if("SAFETY_USED_FLAG" %in% names(merged_data)) {
p9 <- ggplot(merged_data, aes(x = SAFETY_USED_FLAG, fill = INJ_SEVERE_BINARY_F)) +
    geom_bar(position = "fill", alpha = 0.8) +
    scale_fill_manual(values = c("No" = "steelblue", "Yes" = "coral"), 
                     name = "Severe Injury") +
    labs(title = "Injury Severity by Safety Equipment Usage", 
         x = "Safety Equipment", y = "Proportion")
  print(p9)
  ggsave("eda_plots/09_injury_by_safety.png", p9, width = 8, height = 6, dpi = 300)
}

# Time of Day vs Target
if("TIME_OF_DAY" %in% names(merged_data)) {
p10 <- ggplot(merged_data, aes(x = TIME_OF_DAY, fill = INJ_SEVERE_BINARY_F)) +
    geom_bar(position = "fill", alpha = 0.8) +
    scale_fill_manual(values = c("No" = "steelblue", "Yes" = "coral"), 
                     name = "Severe Injury") +
    labs(title = "Injury Severity by Time of Day", 
         x = "Time of Day", y = "Proportion")
  print(p10)
  ggsave("eda_plots/10_injury_by_time.png", p10, width = 8, height = 6, dpi = 300)
}

# Speed Category vs Target
if("SPEED_CAT" %in% names(merged_data)) {
p11 <- ggplot(merged_data, aes(x = SPEED_CAT, fill = INJ_SEVERE_BINARY_F)) +
    geom_bar(position = "fill", alpha = 0.8) +
    scale_fill_manual(values = c("No" = "steelblue", "Yes" = "coral"), 
                     name = "Severe Injury") +
    labs(title = "Injury Severity by Speed Zone", 
         x = "Speed Category", y = "Proportion")
  print(p11)
  ggsave("eda_plots/11_injury_by_speed.png", p11, width = 8, height = 6, dpi = 300)
}

# Driver Status vs Target
if("IS_DRIVER" %in% names(merged_data)) {
p12 <- ggplot(merged_data, aes(x = IS_DRIVER_F, fill = INJ_SEVERE_BINARY_F)) +
    geom_bar(position = "fill", alpha = 0.8) +
    scale_fill_manual(values = c("No" = "steelblue", "Yes" = "coral"), 
                     name = "Severe Injury") +
    labs(title = "Injury Severity by Driver Status", 
         x = "Driver Status", y = "Proportion")
  print(p12)
  ggsave("eda_plots/12_injury_by_driver.png", p12, width = 8, height = 6, dpi = 300)
}

# Vehicle Age vs Target (Boxplot)
if("VEHICLE_AGE_AVG" %in% names(merged_data)) {
p13 <- ggplot(merged_data, aes(x = INJ_SEVERE_BINARY_F, y = VEHICLE_AGE_AVG, 
                                fill = INJ_SEVERE_BINARY_F)) +
    geom_boxplot(alpha = 0.7) +
    scale_fill_manual(values = c("No" = "steelblue", "Yes" = "coral"), name = "Severe Injury") +
    labs(title = "Vehicle Age vs Injury Severity", 
         x = "Severe Injury", y = "Vehicle Age (years)") +
    theme(legend.position = "none")
  print(p13)
  ggsave("eda_plots/13_vehicle_age_vs_injury.png", p13, width = 8, height = 6, dpi = 300)
}

# Sex vs Target
if("SEX" %in% names(merged_data)) {
p14 <- ggplot(merged_data, aes(x = SEX, fill = INJ_SEVERE_BINARY_F)) +
    geom_bar(position = "fill", alpha = 0.8) +
    scale_fill_manual(values = c("No" = "steelblue", "Yes" = "coral"), 
                     name = "Severe Injury") +
    labs(title = "Injury Severity by Sex", 
         x = "Sex", y = "Proportion")
  print(p14)
  ggsave("eda_plots/14_injury_by_sex.png", p14, width = 8, height = 6, dpi = 300)
}

# --- Lighting conditions vs Target ---
if("LIGHT_CAT" %in% names(merged_data)) {
p14b <- ggplot(merged_data, aes(x = LIGHT_CAT, fill = INJ_SEVERE_BINARY_F)) +
    geom_bar(position = "fill", alpha = 0.8) +
    scale_fill_manual(values = c("No" = "steelblue", "Yes" = "coral"), 
                     name = "Severe Injury") +
    labs(title = "Injury Severity by Lighting Condition",
         x = "Lighting", y = "Proportion")
  print(p14b)
  ggsave("eda_plots/18_injury_by_light.png", p14b, width = 8, height = 6, dpi = 300)
}

# --- Road type vs Target ---
if("ROAD_TYPE_GRP" %in% names(merged_data)) {
p14c <- ggplot(merged_data, aes(x = ROAD_TYPE_GRP, fill = INJ_SEVERE_BINARY_F)) +
    geom_bar(position = "fill", alpha = 0.8) +
    scale_fill_manual(values = c("No" = "steelblue", "Yes" = "coral"), 
                     name = "Severe Injury") +
    labs(title = "Injury Severity by Road Type",
         x = "Road Type", y = "Proportion")
  print(p14c)
  ggsave("eda_plots/19_injury_by_roadtype.png", p14c, width = 8, height = 6, dpi = 300)
}

# --- DCA group vs Target ---
if("DCA_GROUP" %in% names(merged_data)) {
p14d <- ggplot(merged_data, aes(x = DCA_GROUP, fill = INJ_SEVERE_BINARY_F)) +
    geom_bar(position = "fill", alpha = 0.8) +
    scale_fill_manual(values = c("No" = "steelblue", "Yes" = "coral"), 
                     name = "Severe Injury") +
    labs(title = "Injury Severity by DCA Group",
         x = "DCA Group", y = "Proportion") +
    coord_flip()
  print(p14d)
  ggsave("eda_plots/20_injury_by_dca.png", p14d, width = 8, height = 6, dpi = 300)
}

# --- Accident type vs Target ---
if("ACCIDENT_TYPE_GROUP" %in% names(merged_data)) {
p14e <- ggplot(merged_data, aes(x = ACCIDENT_TYPE_GROUP, fill = INJ_SEVERE_BINARY_F)) +
    geom_bar(position = "fill", alpha = 0.8) +
    scale_fill_manual(values = c("No" = "steelblue", "Yes" = "coral"), 
                     name = "Severe Injury") +
    labs(title = "Injury Severity by Accident Type",
         x = "Accident Type", y = "Proportion")
  print(p14e)
  ggsave("eda_plots/21_injury_by_accident_type.png", p14e, width = 8, height = 6, dpi = 300)
}

# --- Atmospheric conditions vs Target (from one-hot ATMOSPH_* columns) ---
atm_cols <- names(merged_data)[grepl("^ATMOSPH_", names(merged_data))]
if(length(atm_cols) > 0) {
  atmos_long <- merged_data %>%
    select(all_of(c("INJ_SEVERE_BINARY", atm_cols))) %>%
    tidyr::pivot_longer(cols = all_of(atm_cols), names_to = "ATMOSPH", values_to = "flag") %>%
    filter(flag == 1L) %>%
    mutate(ATMOSPH = gsub("^ATMOSPH_", "", ATMOSPH))
p14f <- ggplot(atmos_long %>% mutate(INJ_SEVERE_BINARY_F = factor(INJ_SEVERE_BINARY, levels=c(0,1), labels=c("No","Yes"))),
               aes(x = ATMOSPH, fill = INJ_SEVERE_BINARY_F)) +
    geom_bar(position = "fill", alpha = 0.8) +
    scale_fill_manual(values = c("No" = "steelblue", "Yes" = "coral"), 
                     name = "Severe Injury") +
    labs(title = "Injury Severity by Atmospheric Condition",
         x = "Atmospheric Condition", y = "Proportion") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p14f)
  ggsave("eda_plots/22_injury_by_atmospheric.png", p14f, width = 9, height = 6, dpi = 300)
}

# --- Road surface conditions vs Target (from one-hot SURFACE_* columns) ---
surf_cols <- names(merged_data)[grepl("^SURFACE_", names(merged_data))]
if(length(surf_cols) > 0) {
  surface_long <- merged_data %>%
    select(all_of(c("INJ_SEVERE_BINARY", surf_cols))) %>%
    tidyr::pivot_longer(cols = all_of(surf_cols), names_to = "SURFACE", values_to = "flag") %>%
    filter(flag == 1L) %>%
    mutate(SURFACE = gsub("^SURFACE_", "", SURFACE))
p14g <- ggplot(surface_long %>% mutate(INJ_SEVERE_BINARY_F = factor(INJ_SEVERE_BINARY, levels=c(0,1), labels=c("No","Yes"))),
               aes(x = SURFACE, fill = INJ_SEVERE_BINARY_F)) +
    geom_bar(position = "fill", alpha = 0.8) +
    scale_fill_manual(values = c("No" = "steelblue", "Yes" = "coral"), 
                     name = "Severe Injury") +
    labs(title = "Injury Severity by Road Surface Condition",
         x = "Road Surface", y = "Proportion")
  print(p14g)
  ggsave("eda_plots/23_injury_by_surface.png", p14g, width = 8, height = 6, dpi = 300)
}

# ============================================================================
# 4. STATISTICAL TESTS
# ============================================================================

cat("\n\n### 4. STATISTICAL TESTS ###\n\n")

# --- Chi-Square Tests (Categorical vs Target) ---
cat("Chi-Square Tests for Independence:\n\n")

chi_square_results <- data.frame(
  Variable = character(),
  Chi_Square = numeric(),
  DF = numeric(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)

for(var in categorical_vars) {
  if(var %in% names(merged_data)) {
    # Create contingency table
    contingency_table <- table(merged_data[[var]], merged_data$INJ_SEVERE_BINARY)
    
    # Perform chi-square test
    chi_test <- chisq.test(contingency_table)
    
    # Store results
    chi_square_results <- rbind(chi_square_results, data.frame(
      Variable = var,
      Chi_Square = chi_test$statistic,
      DF = chi_test$parameter,
      P_Value = chi_test$p.value,
      stringsAsFactors = FALSE
    ))
    
    cat("  ", var, ":\n", sep = "")
    cat("    Chi-square =", round(chi_test$statistic, 4), 
        ", df =", chi_test$parameter,
        ", p-value =", format(chi_test$p.value, scientific = TRUE), "\n")
    if(chi_test$p.value < 0.05) {
      cat("    *** Significant association (p < 0.05) ***\n")
    }
    cat("\n")
  }
}

# Print summary table
cat("\nSummary of Chi-Square Tests:\n")
print(chi_square_results)

# --- Correlation Analysis (Numeric Variables) ---
cat("\n\nCorrelation Analysis (Numeric Variables):\n\n")

# Select numeric variables for correlation
numeric_for_corr <- merged_data %>%
  select_if(is.numeric) %>%
  select(-starts_with("ROAD_USER_TYPE_DESC_")) %>%
  select_if(function(x) length(unique(x)) > 2)  # Remove binary variables with only 0/1

# Calculate correlation matrix
cor_matrix <- cor(numeric_for_corr, use = "complete.obs")
cat("Correlation Matrix (first 10 variables):\n")
print(round(cor_matrix[1:min(10, nrow(cor_matrix)), 1:min(10, ncol(cor_matrix))], 3))

# Correlation with target variable
if("INJ_SEVERE_BINARY" %in% names(numeric_for_corr)) {
  cat("\n\nCorrelation with Target Variable (INJ_SEVERE_BINARY):\n")
  # Calculate correlation matrix and extract correlations with target
  target_idx <- which(names(numeric_for_corr) == "INJ_SEVERE_BINARY")
  target_cor <- cor_matrix[, target_idx]
  
  target_cor_df <- data.frame(
    Variable = names(target_cor),
    Correlation = as.numeric(target_cor),
    Abs_Correlation = abs(as.numeric(target_cor))
  ) %>%
    filter(Variable != "INJ_SEVERE_BINARY") %>%
    arrange(desc(Abs_Correlation))
  
  print(target_cor_df)
  cat("\nTop 15 variables by absolute correlation:\n")
  print(target_cor_df %>% head(15))
}

# Correlation plot (only if we have multiple variables)
if(ncol(cor_matrix) > 1 && nrow(cor_matrix) > 1) {
  png("eda_plots/15_correlation_matrix.png", width = 1200, height = 1200, res = 150)
  corrplot(cor_matrix, method = "color", type = "upper", 
           order = "hclust", tl.cex = 0.7, tl.col = "black")
  dev.off()
  cat("\nCorrelation plot saved to eda_plots/15_correlation_matrix.png\n")
} else {
  cat("\nSkipping correlation plot (insufficient variables)\n")
}

# ============================================================================
# 5. MULTIVARIATE ANALYSIS
# ============================================================================

cat("\n\n### 5. MULTIVARIATE ANALYSIS ###\n\n")

# --- Cross-tabulation Analysis ---
cat("Key Cross-Tabulations:\n\n")

# Age Group × Safety Equipment × Target
if(all(c("AGE_GROUP_SIMPL", "SAFETY_USED_FLAG") %in% names(merged_data))) {
  cat("Age Group × Safety Equipment × Injury Severity:\n")
  cross_tab1 <- merged_data %>%
    group_by(AGE_GROUP_SIMPL, SAFETY_USED_FLAG) %>%
    summarise(
      Total = n(),
      Severe_Injury = sum(INJ_SEVERE_BINARY == 1),
      Severe_Rate = round(100 * mean(INJ_SEVERE_BINARY == 1), 2),
      .groups = "drop"
    ) %>%
    arrange(desc(Severe_Rate))
  print(cross_tab1)
  cat("\n")
}

# Time of Day × Speed Category × Target
if(all(c("TIME_OF_DAY", "SPEED_CAT") %in% names(merged_data))) {
  cat("Time of Day × Speed Category × Injury Severity:\n")
  cross_tab2 <- merged_data %>%
    group_by(TIME_OF_DAY, SPEED_CAT) %>%
    summarise(
      Total = n(),
      Severe_Injury = sum(INJ_SEVERE_BINARY == 1),
      Severe_Rate = round(100 * mean(INJ_SEVERE_BINARY == 1), 2),
      .groups = "drop"
    ) %>%
    arrange(desc(Severe_Rate))
  print(cross_tab2)
  cat("\n")
}

# Driver Status × Safety Equipment × Target
if(all(c("IS_DRIVER", "SAFETY_USED_FLAG") %in% names(merged_data))) {
  cat("Driver Status × Safety Equipment × Injury Severity:\n")
  cross_tab3 <- merged_data %>%
    group_by(factor(IS_DRIVER), SAFETY_USED_FLAG) %>%
    summarise(
      Total = n(),
      Severe_Injury = sum(INJ_SEVERE_BINARY == 1),
      Severe_Rate = round(100 * mean(INJ_SEVERE_BINARY == 1), 2),
      .groups = "drop"
    ) %>%
    arrange(desc(Severe_Rate))
  names(cross_tab3)[1] <- "IS_DRIVER"
  print(cross_tab3)
  cat("\n")
}

# --- Heatmap of Severe Injury Rates ---
cat("Generating multivariate visualizations...\n")

# Age × Safety Equipment heatmap
if(all(c("AGE_GROUP_SIMPL", "SAFETY_USED_FLAG") %in% names(merged_data))) {
  heatmap_data <- merged_data %>%
    group_by(AGE_GROUP_SIMPL, SAFETY_USED_FLAG) %>%
    summarise(Severe_Rate = mean(INJ_SEVERE_BINARY == 1), .groups = "drop")
  
  p15 <- ggplot(heatmap_data, aes(x = SAFETY_USED_FLAG, y = AGE_GROUP_SIMPL, fill = Severe_Rate)) +
    geom_tile(color = "white", size = 0.5) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.5) +
    labs(title = "Severe Injury Rate Heatmap\n(Age Group × Safety Equipment)",
         x = "Safety Equipment", y = "Age Group", fill = "Severe Rate") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p15)
  ggsave("eda_plots/16_age_safety_heatmap.png", p15, width = 8, height = 6, dpi = 300)
}

# Time × Speed heatmap
if(all(c("TIME_OF_DAY", "SPEED_CAT") %in% names(merged_data))) {
  heatmap_data2 <- merged_data %>%
    group_by(TIME_OF_DAY, SPEED_CAT) %>%
    summarise(Severe_Rate = mean(INJ_SEVERE_BINARY == 1), .groups = "drop")
  
  p16 <- ggplot(heatmap_data2, aes(x = SPEED_CAT, y = TIME_OF_DAY, fill = Severe_Rate)) +
    geom_tile(color = "white", size = 0.5) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.5) +
    labs(title = "Severe Injury Rate Heatmap\n(Time of Day × Speed Category)",
         x = "Speed Category", y = "Time of Day", fill = "Severe Rate")
  print(p16)
  ggsave("eda_plots/17_time_speed_heatmap.png", p16, width = 8, height = 6, dpi = 300)
}

# Light × Road Type heatmap
if(all(c("LIGHT_CAT", "ROAD_TYPE_GRP") %in% names(merged_data))) {
  heatmap_data3 <- merged_data %>%
    group_by(LIGHT_CAT, ROAD_TYPE_GRP) %>%
    summarise(Severe_Rate = mean(INJ_SEVERE_BINARY == 1), .groups = "drop")
  p16b <- ggplot(heatmap_data3, aes(x = ROAD_TYPE_GRP, y = LIGHT_CAT, fill = Severe_Rate)) +
    geom_tile(color = "white", size = 0.5) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.5) +
    labs(title = "Severe Injury Rate Heatmap\n(Lighting × Road Type)",
         x = "Road Type", y = "Lighting", fill = "Severe Rate")
  print(p16b)
  ggsave("eda_plots/24_light_roadtype_heatmap.png", p16b, width = 9, height = 6, dpi = 300)
}

# Surface × Speed Category heatmap
if(length(surf_cols) > 0 && "SPEED_CAT" %in% names(merged_data)) {
  surface_long2 <- merged_data %>%
    select(all_of(c("SPEED_CAT", "INJ_SEVERE_BINARY", surf_cols))) %>%
    tidyr::pivot_longer(cols = all_of(surf_cols), names_to = "SURFACE", values_to = "flag") %>%
    filter(flag == 1L) %>% mutate(SURFACE = gsub("^SURFACE_", "", SURFACE))
  heatmap_data4 <- surface_long2 %>%
    group_by(SURFACE, SPEED_CAT) %>%
    summarise(Severe_Rate = mean(INJ_SEVERE_BINARY == 1), .groups = "drop")
  p16c <- ggplot(heatmap_data4, aes(x = SPEED_CAT, y = SURFACE, fill = Severe_Rate)) +
    geom_tile(color = "white", size = 0.5) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.5) +
    labs(title = "Severe Injury Rate Heatmap\n(Road Surface × Speed Category)",
         x = "Speed Category", y = "Road Surface", fill = "Severe Rate")
  print(p16c)
  ggsave("eda_plots/25_surface_speed_heatmap.png", p16c, width = 9, height = 6, dpi = 300)
}

# ============================================================================
# 6. KEY PATTERNS SUMMARY
# ============================================================================

cat("\n\n### 6. KEY PATTERNS SUMMARY ###\n\n")

# Calculate severe injury rates by key variables
cat("Severe Injury Rates by Key Variables:\n\n")

patterns <- list()

# By Age Group
if("AGE_GROUP_SIMPL" %in% names(merged_data)) {
  age_pattern <- merged_data %>%
    group_by(AGE_GROUP_SIMPL) %>%
    summarise(
      Count = n(),
      Severe_Rate = round(100 * mean(INJ_SEVERE_BINARY == 1), 2),
      .groups = "drop"
    ) %>%
    arrange(desc(Severe_Rate))
  cat("By Age Group:\n")
  print(age_pattern)
  cat("\n")
}

# By Safety Equipment
if("SAFETY_USED_FLAG" %in% names(merged_data)) {
  safety_pattern <- merged_data %>%
    group_by(SAFETY_USED_FLAG) %>%
    summarise(
      Count = n(),
      Severe_Rate = round(100 * mean(INJ_SEVERE_BINARY == 1), 2),
      .groups = "drop"
    ) %>%
    arrange(desc(Severe_Rate))
  cat("By Safety Equipment:\n")
  print(safety_pattern)
  cat("\n")
}

# By Driver Status
if("IS_DRIVER" %in% names(merged_data)) {
  driver_pattern <- merged_data %>%
    group_by(factor(IS_DRIVER)) %>%
    summarise(
      Count = n(),
      Severe_Rate = round(100 * mean(INJ_SEVERE_BINARY == 1), 2),
      .groups = "drop"
    )
  names(driver_pattern)[1] <- "IS_DRIVER"
  cat("By Driver Status:\n")
  print(driver_pattern)
  cat("\n")
}

# By Speed Category
if("SPEED_CAT" %in% names(merged_data)) {
  speed_pattern <- merged_data %>%
    group_by(SPEED_CAT) %>%
    summarise(
      Count = n(),
      Severe_Rate = round(100 * mean(INJ_SEVERE_BINARY == 1), 2),
      .groups = "drop"
    ) %>%
    arrange(desc(Severe_Rate))
  cat("By Speed Category:\n")
  print(speed_pattern)
  cat("\n")
}

# By Time of Day
if("TIME_OF_DAY" %in% names(merged_data)) {
  time_pattern <- merged_data %>%
    group_by(TIME_OF_DAY) %>%
    summarise(
      Count = n(),
      Severe_Rate = round(100 * mean(INJ_SEVERE_BINARY == 1), 2),
      .groups = "drop"
    ) %>%
    arrange(desc(Severe_Rate))
  cat("By Time of Day:\n")
  print(time_pattern)
  cat("\n")
}

cat("\n", paste0(rep("=", 80), collapse = ""), "\n")
cat("Done!\n")
cat(paste0(rep("=", 80), collapse = ""), "\n")

#  Cramer's V for key categorical variables
compute_cramers_v <- function(tbl) {
  chi <- suppressWarnings(chisq.test(tbl))
  n <- sum(tbl)
  k <- min(nrow(tbl), ncol(tbl))
  v <- sqrt(as.numeric(chi$statistic) / (n * (k - 1)))
  return(v)
}

cat("\nEffect sizes (Cramer's V) for selected variables vs INJ_SEVERE_BINARY:\n")
for(v in c("LIGHT_CAT", "ROAD_TYPE_GRP", "DCA_GROUP", "ACCIDENT_TYPE_GROUP")) {
  if(v %in% names(merged_data)) {
    tbl <- table(merged_data[[v]], merged_data$INJ_SEVERE_BINARY)
    cv <- compute_cramers_v(tbl)
    cat(sprintf("  %-22s : %.3f\n", v, cv))
  }
}

if(length(atm_cols) > 0) {
  tbl_atm <- table(atmos_long$ATMOSPH, atmos_long$INJ_SEVERE_BINARY)
  cat(sprintf("  %-22s : %.3f\n", "ATMOSPH_COND", compute_cramers_v(tbl_atm)))
}
if(length(surf_cols) > 0) {
  tbl_surf <- table(surface_long$SURFACE, surface_long$INJ_SEVERE_BINARY)
  cat(sprintf("  %-22s : %.3f\n", "SURFACE_COND", compute_cramers_v(tbl_surf)))
}

