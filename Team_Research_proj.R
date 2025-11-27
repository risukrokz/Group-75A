sum(is.na(Cancer_Data_2))
colSums(is.na(Cancer_Data_2))
Cancer_Data_2$X=NULL
cancer_B=subset(Cancer_Data_2,diagnosis=="B")
cancer_M=subset(Cancer_Data_2,diagnosis=="M")




check_for_normality=function(colname, dataset){
  
  if(!is.numeric(dataset[[colname]])){
    print(paste("Column:", colname, "is not numeric. Skipping."))
    return(NULL)
  }
  
  
  
  mean_val=mean(dataset[[colname]], na.rm = TRUE)
  
  median_val=median(dataset[[colname]], na.rm = TRUE)
  standard_deviation=sd(dataset[[colname]], na.rm = TRUE)
  print(paste("Column:", colname))
  print(paste("Mean of column", colname, "is", mean_val))
  print(paste("Median of column", colname, "is", median_val))
  print(paste("Standard deviation of column", colname, "is", standard_deviation))
  hist(dataset[[colname]],
       main='Histogram Analysis',
       xlab=colname,
       ylab='frequency',
       col='lightblue',
       border='black',
       breaks=20
    
  )
  shapiros_test=shapiro.test(dataset[[colname]])
  
  if(shapiros_test$p.value > 0.05){
    print(paste("Conclusion for", colname, ": Data appears roughly normal."))
  }
  else{
    print(paste("Conclusion for", colname, ": Data  IS SKEWED."))
  }
  

  }
 


