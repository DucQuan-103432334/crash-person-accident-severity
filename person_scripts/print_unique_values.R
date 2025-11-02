cat("\n=== UNIQUE VALUES PER VARIABLE (compact view) ===\n\n")

for (colname in names(person_vehicle_accident)) {
  vals <- unique(person_vehicle_accident[[colname]])
  n_unique <- length(vals)
  
  cat("•", colname, "→", n_unique, "unique values\n")
  
  if (n_unique <= 15) {
    print(vals)
  } else {
    cat("  (showing first 15)\n")
    print(head(vals, 15))
  }
  cat("\n--------------------------------------\n")
}
