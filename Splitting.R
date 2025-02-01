library(caret)
library(caTools)
library(tidyverse)
data<- read.csv("DATASETS/cleaned data.csv")

data<- factor_converter(data)

numeric_column<- numeric_selector(data)


for (i in seq(ncol(data))){
  if (is.numeric(data[[i]])){
    data[[i]] = robust_scaler(data[[i]])
  }
}


splitting_data<- sample.split(data, SplitRatio = 0.8)
train_data<- subset(data, splitting_data == TRUE)
test_data<- subset(data, splitting_data == F)
View(test_data)

write.csv(train_data, "DATASETS/training_set.csv")
write.csv(test_data, "DATASETS/testing_Set.csv")

pivoting_data<- pivot_longer(data = data, cols = c(colnames(numeric_column)),
                             names_to = "Columns", values_to = "Values")
View(pivoting_data)

ggplot(data = pivoting_data)+
  geom_histogram(mapping = aes(x = Values),
                 bins = 8, fill = 'blue', color = 'red')+
  facet_wrap(~Columns, scales = 'free')+
  theme_minimal()
  