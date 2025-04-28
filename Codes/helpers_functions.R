library('tidyverse')

factor_converter <- function(x) {
  if (!is.data.frame(x)) stop("Input must be a dataframe")
  
  for (column in seq_len(ncol(x))) {
    if (!is.factor(x[[column]]) & length(unique(x[[column]])) < 4) {
      x[[column]] <- as.factor(x[[column]])
    }
  }
  return(x)  
}



numeric_selector<- function(x){
  x %>% select_if(is.numeric)
}

category_selector<- function(x){
  x %>% select_if(is.factor)
}

box_plot_outlier<- function(x){
  Q1<- quantile(x, 0.25, na.rm = T)
  Q3<- quantile(x, 0.75, na.rm = T)
  
  inter<- Q3 - Q1
  
  lower_bound = Q1- (1.5 * inter)
  upper_bound = Q3 + (1.5 * inter)
  
  outlier<- which(x > upper_bound | x < lower_bound)
  
  total<- length(outlier)
  return(total)
}


scaler<- function(x){
  x<- as.numeric(as.character(x))
  m = mean(x, na.rm = TRUE)
  s = sd(x, na.rm = TRUE)
  list(scaled = (x - m)/s, mean_x = m, std_x = s)
}

robust_scaler<- function(x){
  x<- as.numeric(as.character(x))
  med = median(x, na.rm = TRUE)
  iqr<- IQR(x, na.rm = TRUE)
  list(scaled = (x - med)/iqr, median = med, iqr = iqr)
}


sqrt_normalizer<- function(x){
  sqrt_x<- sqrt(x)
  return(sqrt_x)
}

# outlier_detector<- function(x){deviation = sd(x,na.rm = T)
#   average = mean(x,na.rm = T)
#   scaler= (x - average)/deviation
#   outlier<- sum(abs(scaler) > 3)
#   return (outlier)}


# outliers<- data.frame(Variable = character(),
#                       Outlier = numeric(),
#                       class_value = character(),
#                       stringsAsFactors = FALSE)
# 
# for (column in seq(ncol(data))){
#   if(is.numeric(data[[column]])){
#     
#     cat("The Number of Outliers in", colnames(data[column]),
#         "is:", box_plot_outlier(data[[column]]))
#     
#     outliers<- rbind(outliers, data.frame(
#       Variable = colnames(data[column]),
#       Outlier = box_plot_outlier(data[[column]]),
#       stringsAsFactors = F))
#     cat("\n")
#   }
# }



