#***************************************************
#   IMPORTING NECCESSARY LIBRARIES AND DATA        # 
#***************************************************
library(dplyr)
data<- read.csv("DATASETS/diabetes_dataset.csv")

sum(duplicated(data))
data<- data[!duplicated(data), ]

#***************************************************
#             SUMMARY FOR EACH COLUMN              #  
#***************************************************

dataframe<- data.frame(
  Column = character(), 
  Min=numeric(), 
  Q1 = numeric(), 
  Median = numeric(), 
  Mean = numeric(), 
  Q3 = numeric(), 
  Max = numeric(), 
  stringsAsFactors =FALSE)

for (i in seq(ncol(data))){
  if (is.numeric(data[[i]])){
    cat("Summary for Column ", colnames(data[i]), "\n")
    
    summary_column = summary(data[[i]])
    print(summary_column)
    
    dataframe<- rbind(dataframe, data.frame(
      Column = colnames(data[i]),
      Min = as.numeric(summary_column['Min.']),
      Q1 = as.numeric(summary_column['1st Qu.']),
      Median = as.numeric(summary_column['Median']),
      Mean = as.numeric(summary_column['Mean']),
      Q3 = as.numeric(summary_column['3rd Qu.']),
      Max= as.numeric(summary_column['Max.']),
      stringsAsFactors = FALSE))
    
    cat('\n')
  }
}
#***************************************************
#             LABEL AND ONE-HOT ENCODING           #  
#***************************************************

factorized = factor(data$gender, 
                    levels= c('Female', 'Male', 'Other'))

data$gender= as.numeric(factorized) -1

encoding = distinct(data.frame( Object =factorized, 
                                encoding_value = data$gender))

extracted<- model.matrix(~smoking_history-1, data)


data$smoking_history<- NULL
data<- data %>% cbind(extracted)

data<- data %>% mutate(
  `Previously Smoking` =
    smoking_historyformer + `smoking_historynot current`, 
  `Currently Smoking` = 
    smoking_historycurrent + smoking_historyever,
  `Never Smoked` = 
    `smoking_historyNo Info` + smoking_historynever)

data<- data[,!colnames(data) %in% c('smoking_historyformer',
                             'smoking_historyever',
                             'smoking_historycurrent',
                             'smoking_historyNo Info',
                             'smoking_historynever',
                             'smoking_historynot current')]

data<- data %>% select(-diabetes, diabetes)
#***************************************************
#             SAVING THE PREPROCESSED DATA         #  
#***************************************************

write.csv(data, 'cleaned data.csv', row.names = FALSE) 
