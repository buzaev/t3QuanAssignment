getDescriptives = function(ds) 
{descriptive = data.frame(
  Column = character(),
  Mean = numeric(),
  Sigma = numeric(),
  N = integer(),
  Median = numeric(),
  Q1 = numeric(),
  Q3 = numeric(),
  Shapiro = character(),
  stringsAsFactors = FALSE
)
for (col_name in colnames(ds)) {
  if (is.numeric(ds[[col_name]]) && !is.factor(ds[[col_name]])) {
    col_data = ds[[col_name]]
    mean_val = mean(col_data, na.rm = TRUE)
    sigma_val = sd(col_data, na.rm = TRUE)
    n_val = sum(!is.na(col_data))
    median_val = median(col_data, na.rm = TRUE)
    q1_val = quantile(col_data, 0.25, na.rm = TRUE)
    q3_val = quantile(col_data, 0.75, na.rm = TRUE)
    shapiro_test = shapiro.test(col_data)
    shapiro_result = ifelse(shapiro_test$p.value > 0.05, "normal", "not normal")
    shapiro_result = ifelse(shapiro_test$p.value > 0.05, paste(round(shapiro_test$p.value,7), " (normal)"), paste(round(shapiro_test$p.value,3), " (not normal)"))
    descriptive = rbind(descriptive, data.frame(
      Column = col_name,
      Mean = mean_val,
      Sigma = sigma_val,
      N = n_val,
      Median = median_val,
      Q1 = q1_val,
      Q3 = q3_val,
      Shapiro = shapiro_result,
      stringsAsFactors = FALSE
    ))
  }
}
descriptive <- descriptive[descriptive$Column != "id", ]
descriptive$Mean = round(descriptive$Mean,2)
descriptive$Sigma = round(descriptive$Sigma,2)
descriptive$Median = round(descriptive$Median,2)
descriptive$Q1=round(descriptive$Q1)
descriptive$Q3=round(descriptive$Q3)
descriptive$MedQ1Q3=paste( descriptive$Med, " [" , descriptive$Q1, "; ", descriptive$Q3,"]", sep="")
descriptive[sapply(descriptive, is.numeric)] <- lapply(descriptive[sapply(descriptive, is.numeric)], format, scientific = FALSE)
descriptive
}