library(tidyverse)
library(ggpubr)
data = read.csv('DATASETS/cleaned data.csv')
train_data <- read.csv("DATASETS/training_set.csv")
test_data <- read.csv("DATASETS/testing_set.csv")

data<- factor_converter(data)
numeric_column<- data %>% select_if(is.numeric)
categorical_column<- category_selector(data)

distribution_test<-  data.frame(` |D-MAX| ` = numeric(),
                                P_value = numeric(), Decison = character(),
                                stringsAsFactors = F, check.names = F)


for (numeric_i in seq(ncol(numeric_column))){
  
  total_column<- numeric_column[[numeric_i]]
  ks_test_column = ks.test(total_column, 'pnorm',
                           
                           mean = mean(total_column), sd = sd(total_column))
  
  distribution_test = rbind(distribution_test, data.frame(
    ` |D-MAX| `= as.numeric(ks_test_column$statistic),
    P_value = as.numeric(ks_test_column$p.value),
    
    Decision = ifelse(ks_test_column$p.value>0.05, "Accept", "Reject"),
    
    stringsAsFactors = F, check.names = F
    
  ))
}

View(distribution_test)
binary_columns<- colnames(categorical_column)

for(col in colnames(numeric_column)){
  if(col != 'diabetes')
  formular = as.formula(paste(col, '~diabetes'))
  anova_model<- aov(formular, data = data)
  print(paste("Anova Model for ", col))
  cat('\n')
  print(summary(anova_model) )
  cat('\n')
}


for(column in binary_columns){
  if(column!= 'diabetes'){
    categorical_table = table(categorical_column[[column]],
                              categorical_column$diabetes)
    categorical_test = chisq.test(categorical_table)
    cat('The ChiSquare Test for', column, 'is', '\n')
    print(categorical_test)
    cat('\n')
    
    
    }
}
categorical_column

