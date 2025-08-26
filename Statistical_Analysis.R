install.packages("corrplot")

library(corrplot)

corrplot(cor_matrix,method = "number")

## Detecting Outliers

sales <- c(5,8,10,20,100,2,65,18,32,25,200,9,15)

sales

iqr <- IQR(sales)

first <- quantile(sales,probs = 0.25)
third <- quantile(sales,probs = 0.75)

upper_threshold <- third+ 1.5*iqr

lower_threshold <- first- 1.5*iqr

outlier_function <- function(x){
  iqr <- IQR(x)
  first <- quantile(x, probs = 0.25)
  third <- quantile(x, probs = 0.75)
  
  upper_threshold <- third+ 1.5*iqr
  lower_threshold <- first- 1.5*iqr
  outliers <- list(upper_outliers = x [x > upper_threshold], lower_outliers = x [x < lower_threshold])
  
  return(outliers)
}

outlier_function(sales)