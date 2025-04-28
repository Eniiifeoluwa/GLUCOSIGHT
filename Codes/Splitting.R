library(caret)
library(caTools)
library(tidyverse)
data<- read.csv("DATASETS/cleaned data.csv")

data<- factor_converter(data)

numeric_column<- numeric_selector(data)

mean_list = list()
std_list = list()


for (i in seq(ncol(data))) {
  if (is.numeric(data[[i]])) {
    result <- scaler(data[[i]])
    data[[i]] <- result$scaled
    mean_list[[names(data)[i]]] <- result$mean_x
    std_list[[names(data)[i]]] <- result$std_x
  }
}


View(data)
splitting_data<- sample.split(data, SplitRatio = 0.8)
train_data<- subset(data, splitting_data == TRUE)
test_data<- subset(data, splitting_data == F)
View(test_data)

write.csv(train_data, "DATASETS/training_set.csv", row.names = T)
write.csv(test_data, "DATASETS/testing_Set.csv", row.names = T)

robust_scaler(data$hypertension)
data$hypertension

library(jsonlite)

smoter <- train_data %>% select(-diabetes)
train_data_oversampled <- SMOTE(X = smoter, target = train_data$diabetes)

train_data <- train_data_oversampled$data
colnames(train_data)[colnames(train_data) == 'class'] <- 'diabetes'

write.csv(train_data, "Smoted_Data.csv", row.names = FALSE)

write_json(mean_list, "mean_values.json")
write_json(std_list, "std_values.json")
