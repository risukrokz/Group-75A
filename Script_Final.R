# --- Function to check normality (simple version) ---

##Is there a significant difference in the mean radius_mean between malignant and benign tumors?
check_for_normality <- function(colname, dataset) {
  # Check if the column is numeric
  if(!is.numeric(dataset[[colname]])) {
    print(paste("Column:", colname, "is not numeric. Skipping."))
    return(NULL)
  }
  
  
  mean_val <- mean(dataset[[colname]], na.rm = TRUE)
  median_val <- median(dataset[[colname]], na.rm = TRUE)
  sd_val <- sd(dataset[[colname]], na.rm = TRUE)
  
  
  cat("Column:", colname, "\n")
  cat("Mean:", mean_val, "\n")
  cat("Median:", median_val, "\n")
  cat("Standard Deviation:", sd_val, "\n")
  
  
  hist(dataset[[colname]],
       main = paste('Histogram of', colname),
       xlab = colname,
       ylab = 'Frequency',
       col = 'lightblue',
       border = 'black',
       breaks = 20
  )
}





sum(is.na(Cancer_Data_11))
colSums(is.na(Cancer_Data_11))

Cancer_Data_11$X <- NULL
Cancer_Data_11$diagnosis <- trimws(Cancer_Data_11$diagnosis)
Cancer_Data_11$diagnosis <- factor(Cancer_Data_11$diagnosis)
table(Cancer_Data_11$diagnosis)

Cancer_Data_11$radius_mean <- as.numeric(Cancer_Data_11$radius_mean)
sum(is.na(Cancer_Data_11$radius_mean))
Cancer_Data_11 <- Cancer_Data_11[!is.na(Cancer_Data_11$radius_mean), ]


check_for_normality("radius_mean", Cancer_Data_11)


mean_malignant <- mean(Cancer_Data_11$radius_mean[Cancer_Data_11$diagnosis == "M"])
mean_benign <- mean(Cancer_Data_11$radius_mean[Cancer_Data_11$diagnosis == "B"])

t_test<-t.test(radius_mean ~ diagnosis, data = Cancer_Data_11)

print(t_test)

bar_means <- c(mean_malignant, mean_benign)
names(bar_means) <- c("Malignant", "Benign")

bp <- barplot(
  bar_means,
  main = "Mean Radius (Malignant vs Benign)",
  ylab = "Mean radius_mean",
  xlab = "Diagnosis",
  col = c("red", "green"),
  ylim = c(0, max(bar_means) + 2)
)

bp <- boxplot(
  
  Cancer_Data_11$radius_mean~Cancer_Data_11$diagnosis ,
  
  main = "Mean Radius (Malignant vs Benign)",
  
  ylab = "Mean radius_mean",
  
  xlab = "Diagnosis",
  
)

text(
  x = bp,
  y = bar_means,
  labels = round(bar_means, 2),
  pos = 3,
  cex = 0.8,
  col = "black"
)
