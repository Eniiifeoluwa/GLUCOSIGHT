# Load required libraries
library(caret)
library(dplyr)
library('smotefamily')
library(jsonlite)
library(ROCR)
library(pROC)
# Load training and testing data
#train_data <- read.csv("DATASETS/training_set.csv")
test_data <- read.csv("DATASETS/testing_set.csv")


train_data<- read.csv("DATASETS/Smoted_Data.csv")
continuous_vars <- c("age", "bmi", "HbA1c_level", "blood_glucose_level")


train_data$gender <- ifelse(train_data$gender == "Male", 1, 0)
test_data$gender <- ifelse(test_data$gender == "Male", 1, 0)
X_train <- as.matrix(train_data[, setdiff(names(train_data), "diabetes")])
y_train <- as.numeric(train_data$diabetes)
X_train <- cbind(1, X_train)

# Sigmoid function
sigmoid <- function(z) {
  1 / (1 + exp(-z))
}

# Train the model (SGD)
set.seed(42)
n <- nrow(X_train)
p <- ncol(X_train)#weights <- runif(p)
weights<-rep(0, p)
alpha <- 0.00001
epochs <- 400
loss_values <- numeric(epochs)
for (epoch in 1:epochs) {
  total_loss <- 0
  for (i in sample(1:n)) {
    xi <- X_train[i, ]
    yi <- y_train[i]
    pred <- sigmoid(sum(weights * xi))
    loss <- (pred - yi)^2
    total_loss <- total_loss + loss
    gradient <- (pred - yi) * xi
    weights <- weights - alpha * gradient
  }
  avg_loss <- total_loss / n
  loss_values[epoch] <- avg_loss
  cat("Epoch:", epoch, "Loss:", avg_loss, "\n")
}


library(ggplot2)

# Create a data frame for plotting
loss_df <- data.frame(
  epoch = 1:epochs,
  loss = loss_values
)

# Plot
ggplot(loss_df, aes(x = epoch, y = loss)) +
  geom_line(color = "blue", size = 1) +  # Fine blue line
  geom_smooth(se = FALSE, color = "red", method = "loess", span = 0.3) + # Smooth red curve
  labs(title = "Training Loss Over Epochs",
       x = "Epoch",
       y = "Average Loss") +
  theme_minimal(base_size = 15)

