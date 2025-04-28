library(jsonlite)
library(pROC)
library(ROCR)
library(dplyr)
library(caret)
test_data <- read.csv("DATASETS/testing_set.csv")
weights<-fromJSON('json files/weights.json')
#Ensure test data is properly formatted
X_test <- as.matrix(test_data[, setdiff(names(test_data), "diabetes")])
X_test <- cbind(1, X_test)  # Add intercept
sigmoid <- function(x) {
  return(1 / (1 + exp(-x)))
}
# Generate predictions
pred_probs <- sigmoid(X_test %*% weights)
pred_class <- ifelse(pred_probs > 0.9, 1, 0)
y_test<- matrix(test_data$diabetes, ncol = 1)
# Calculate confusion matrix
cm <- confusionMatrix(factor(pred_class), factor(y_test))
pred<- prediction(pred_probs, factor(y_test))
perf<- performance(pred,'tpr', 'fpr')
plot(perf, colorize = TRUE, print.cutoffs.at= seq(0, 1, by = 0.1),
     text_adj =c(-0.2, 1.2))

auc <- performance(pred, "auc")
table(pred_class, y_test)
mean(pred_class == y_test) * 100

