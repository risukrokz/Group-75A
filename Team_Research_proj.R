# Remove unnecessary column
Cancer_Data$X <- NULL

# Split dataset (optional)
cancer_B <- subset(Cancer_Data, diagnosis == "B")
cancer_M <- subset(Cancer_Data, diagnosis == "M")

# Normality check function (kept as-is)
check_for_normality <- function(colname, dataset){
  if(!is.numeric(dataset[[colname]])){
    print(paste("Column:", colname, "is not numeric. Skipping."))
    return(NULL)
  }
  if(length(unique(dataset[[colname]])) == 1){
    print(paste("Column:", colname, "has identical values. Skipping Shapiro-Wilk test."))
    return(NULL)
  }
  
  mean_val <- mean(dataset[[colname]], na.rm = TRUE)
  median_val <- median(dataset[[colname]], na.rm = TRUE)
  sd_val <- sd(dataset[[colname]], na.rm = TRUE)
  
  print(paste("Column:", colname))
  print(paste("Mean of column", colname, "is", mean_val))
  print(paste("Median of column", colname, "is", median_val))
  print(paste("Standard deviation of column", colname, "is", sd_val))
  
  hist(dataset[[colname]],
       main='Histogram Analysis',
       xlab=colname,
       ylab='frequency',
       col='lightblue',
       border='black',
       breaks=20
  )
  
  shapiro_test <- shapiro.test(dataset[[colname]])
  
  if(shapiro_test$p.value > 0.05){
    print(paste("Conclusion for", colname, ": Data appears roughly normal."))
  } else {
    print(paste("Conclusion for", colname, ": Data IS SKEWED."))
  }
}

# Check normality for cancer_M
for (colname in colnames(cancer_M)) {
  if (colname %in% c('id', 'diagnosis')) next  
  check_for_normality(colname, cancer_M)
}

# -----------------------------
# Step 1: Correlation Analysis (replace Cliff's Delta)
# Convert diagnosis to numeric: B=0, M=1
Cancer_Data$diagnosis_num <- ifelse(Cancer_Data$diagnosis == "M", 1, 0)

# Select numeric features
features <- Cancer_Data[, !(names(Cancer_Data) %in% c('id', 'diagnosis', 'diagnosis_num'))]

# Compute Pearson correlation with target
cor_values <- sapply(features, function(x) cor(x, Cancer_Data$diagnosis_num, method = "pearson"))

# Rank by absolute correlation
cor_ranked <- sort(abs(cor_values), decreasing = TRUE)
top_n <- 10
important_features <- names(cor_ranked[1:top_n])  # can limit top N if desired
print(cor_ranked[important_features])
# -----------------------------
# Step 2: Wilcoxon test on top correlated features
wilcox_results <- list()

for (col in important_features){
  if (!is.numeric(Cancer_Data[[col]])) next
  wtest <- wilcox.test(Cancer_Data[[col]] ~ Cancer_Data$diagnosis,
                       exact = FALSE, correct = FALSE)
  wilcox_results[[col]] <- c(statistic = wtest$statistic,
                             p_value = wtest$p.value)
}

wilcox_df <- do.call(rbind, wilcox_results)
wilcox_df <- as.data.frame(wilcox_df)
wilcox_df$feature <- rownames(wilcox_df)
rownames(wilcox_df) <- NULL

# Order Wilcoxon results by p-value
ordered_p <- wilcox_df[order(wilcox_df$p_value), ]

# -----------------------------
# Plot Wilcoxon p-values
barplot(
  ordered_p$p_value,
  names.arg = ordered_p$feature,
  las = 2,
  col = "lightblue",
  border = "black",
  main = "Wilcoxon Test p-values",
  ylab = "p-value",
  cex.names = 0.7
)

# Plot Wilcoxon statistics
ordered_w <- wilcox_df[order(ordered_p$statistic), ]
barplot(
  ordered_w$statistic,
  names.arg = ordered_w$feature,
  las = 2,
  col = "lightgreen",
  border = "black",
  main = "Wilcoxon Test Statistics",
  ylab = "W Statistic",
  cex.names = 0.7
)
