library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ROCR)
library(jsonlite)
library(caret)
library(here)

#library(smotefamily)
# Load and preprocess data (use the same preprocessing as before)
data<- read.csv(here("DATASETS/cleaned data.csv"))
train_data <- read.csv(here("DATASETS/training_set.csv"))
test_data <- read.csv(here("DATASETS/testing_set.csv"))

# Sigmoid function
sigmoid <- function(x) {
  return(1 / (1 + exp(-x)))
}
continuous <- function(x) {
  if (length(unique(x)) >= 5) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


weights<-fromJSON('json files/weights.json')
mean_weights<- fromJSON('json files/mean_values.json')
std_weights<- fromJSON('json files/std_values.json')
# Predict function
predict_diabetes <- function(age, hypertension, heart_disease, bmi, hbA1c, blood_glucose, 
                             previously_smoking, currently_smoking, never_smoked, gender) {

  
  scaled_age <- (age - mean_weights$age) / std_weights$age
  scaled_bmi <- (bmi - mean_weights$bmi) / std_weights$bmi
  scaled_hbA1c <- (hbA1c - mean_weights$HbA1c_level) / std_weights$HbA1c_level
  scaled_blood_glucose <- (blood_glucose - mean_weights$blood_glucose_level) / std_weights$blood_glucose_level
  
  # Create input vector
  X_input <- matrix(
    c(
      1, 
      gender, 
      scaled_age, 
      hypertension, 
      heart_disease, 
      scaled_bmi, 
      scaled_hbA1c, 
      scaled_blood_glucose, 
      previously_smoking, 
      currently_smoking, 
      never_smoked
    ), 
    nrow = 1
  )
  
  
  
  pred_prob <- sigmoid(X_input %*% weights)
  if (pred_prob > 0.5) return("Diabetic") else return("Not Diabetic")
}
set.seed(42)
# UI
ui <- dashboardPage(
  dashboardHeader(title = "Diabetes Prediction App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Data Pipeline", tabName = "data_pipeline", icon = icon("home")),
      menuItem("Prediction", tabName = "prediction", icon = icon("heartbeat")),
      menuItem("Data Analysis", tabName = "analysis", icon = icon("chart-line")),
      menuItem("Model Performance", tabName = "performance", icon = icon("cogs"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(tabName = "home",
              h2("Welcome to the Diabetes Prediction App"),
              p("This app uses Logistic Regression with Stochastic Gradient Descent (SGD) to predict diabetes based on input features."),
             
              img(src = "diabetes_image.jpg", height = "800px", width = "1200px")
      ),
      tabItem(tabName = "data_pipeline",
              h2("Explore the Dataset"),
              fluidRow(
                column(12,
                       box(title = "Overview of Dataset",
                           status = "info",
                           solidHeader = TRUE,
                           width = 12,
                           DTOutput("data_table")
                         
                       ))
              )
              
        
      ),
      
      tabItem(tabName = "prediction",
              h2("Make a Prediction"),
              fluidRow(
                column(4,
                       box(
                         title = "Input Parameters",
                         status = "primary",
                         solidHeader = TRUE,
                         width = 12,
                         numericInput("age", "Age:", min = 0, max = 100, value = 30),
                         selectInput("hypertension", "Hypertension:", 
                                     choices = list("No" = 0, "Yes" = 1), selected = 0),
                         selectInput("heart_disease", "Heart Disease:", 
                                     choices = list("No" = 0, "Yes" = 1), selected = 0),
                         numericInput("bmi", "BMI:", min = 0, max = 100, value = 25),
                         numericInput("hbA1c", "HbA1c Level:", min = 0, max = 20, value = 5),
                         numericInput("blood_glucose", "Blood Glucose Level:", min = 0, max = 300, value = 90),
                         selectInput("gender", "Gender:", 
                                     choices = list("Male" = 1, "Female" = 0), selected = 1),
                         radioButtons("smoking_history", "Smoking History:", 
                                      choices = list("Never Smoked" = "never", 
                                                     "Previously Smoked" = "previous", 
                                                     "Currently Smoking" = "current")),
                         actionButton("predict", "Predict", icon = icon("play"), 
                                      class = "btn-primary")
                       )
                ),
                column(8,
                       box(
                         title = "Prediction Result",
                         status = "success",
                         solidHeader = TRUE,
                         width = 12,
                         height = "300px",
                         div(
                           id = "prediction_output",
                           style = "font-size: 18px; padding: 20px; text-align: center;",
                           uiOutput("result")
                         ),
                         hr(),
                         h4("Risk Factors Explained:"),
                         p("This prediction is based on your provided health metrics compared against our model trained on diabetes data."),
                         p("Key risk factors include high blood glucose levels, elevated HbA1c, BMI, age, and presence of cardiovascular conditions.")
                       )
                )
              )
      ),
      tabItem(tabName = "analysis",
              h2("Data Analysis"),
              fluidRow(
                column(6,
                       box(
                         title = "Feature Correlation",
                         status = "warning",
                         solidHeader = TRUE,
                         width = 12,
                         plotlyOutput("correlation_plot")
                       )),
                column(6,
                       box(
                         title = 'Age Distribution',
                         status = 'primary',
                         solidHeader = TRUE,
                         width = 12,
                         plotOutput("age_distribution")
                       ))

              ),
              
              fluidRow(
                column(6,
                       box(
                         title = "Feature Distributions",
                         status = "primary",
                         solidHeader = TRUE,
                         width = 12,
                         plotOutput("feature_distribution")
                       )
                ),
                column(6,
                       box(
                         title = "Diabetes by Smoking History",
                         status = "primary",
                         solidHeader = TRUE,
                         width = 12,
                         plotOutput("smoking_plot")
                       )
                )
              )
      ),
      tabItem(tabName = "performance",
              h2("Model Performance"),
              fluidRow(
                column(6,
                       box(
                         title = "Confusion Matrix",
                         status = "danger",
                         solidHeader = TRUE,
                         width = 12,
                         verbatimTextOutput("confusion_matrix")
                       )
                ),
                column(6,
                       box(
                         title = "Model Accuracy",
                         status = "success",
                         solidHeader = TRUE,
                         width = 12,
                         verbatimTextOutput("accuracy"),
                         h4("Additional Metrics:"),
                         verbatimTextOutput("other_metrics")
                       )
                )
              ),
              fluidRow(
                column(6,
                       box(
                         title = "ROC Curve",
                         status = "info",
                         solidHeader = TRUE,
                         width = 12,
                         plotOutput("roc_curve")
                       )
                ),
                column(6,
                       box(
                         title = "Feature Importance",
                         status = "info",
                         solidHeader = TRUE,
                         width = 12,
                         plotOutput("feature_importance")
                       )
                )
              )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Show dataset (limit rows for performance)
  output$data_table <- renderDT({
    datatable(head(data, 1000), options = list(pageLength = 10, scrollX = TRUE) 
              
    )
  })
  
  # Correlation matrix heatmap
  output$correlation_plot <- renderPlotly({
    # Select only numeric columns for correlation
    numeric_cols <- sapply(data, function(x) is.numeric(x) && continuous(x))
    
    numeric_data <- data[, numeric_cols]
    
    # Remove the target variable if it's included
    if("diabetes" %in% colnames(numeric_data)) {
      numeric_data <- numeric_data[, colnames(numeric_data) != "diabetes"]
    }
    
    # Calculate correlation matrix
    correlation_matrix <- cor(numeric_data)
    
    # Create heatmap
    plot_ly(
      x = colnames(correlation_matrix),
      y = colnames(correlation_matrix),
      z = correlation_matrix,
      type = "heatmap",
      colors = colorRamp(c("#4575b4", "#ffffbf", "#d73027"), alpha = T)
    ) %>%
      layout(title = "Feature Correlations" ) %>%
      add_annotations(
        text = round(c(correlation_matrix), 2),
        x = rep(colnames(correlation_matrix), each = nrow(correlation_matrix)),
        y = rep(colnames(correlation_matrix), times = ncol(correlation_matrix)),
        showarrow = FALSE,
        font = list(size = 10)
      )
  })
  # 
  output$age_distribution <- renderPlot({
    data_copy = data
    data_copy$diabetes <- factor(data_copy$diabetes, 
                                 levels = c(0, 1), 
                                 labels = c("No", "Yes"))
    data_copy$age_group <- cut(data_copy$age,
                               breaks = c(-Inf, 17, 29, 39, 49, 59, Inf),
                               labels = c("<18", "18–29", "30–39", "40–49", "50–59", "60+"))
    
    age_diabetes_summary <- data_copy %>%
      group_by(age_group, diabetes) %>%
      summarise(count = n(), .groups = "drop")
    
    
    ggplot(age_diabetes_summary, aes(x = age_group, y = count, fill = diabetes)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Distribution of Diabetes Status Across Age Groups",
           x = "Age Group",
           y = "Number of Individuals",
           fill = "Diabetes") +
      scale_fill_manual(values = c("No" = "#A8DADC", "Yes" = "#E63946")) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12)
      )
  })
    
  # Feature distributions
  output$feature_distribution <- renderPlot({
    num_features <- c("age", "bmi", "HbA1c_level", "blood_glucose_level")
    
    # Get only features that exist in the data
    valid_features <- num_features[num_features %in% colnames(train_data)]
    
    melted_data <- data %>%
      select(all_of(valid_features), diabetes) %>%
      pivot_longer(cols = valid_features, names_to = "Feature", values_to = "Value")
    
    ggplot(melted_data, aes(x = Value, fill = factor(diabetes))) +
      geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
      facet_wrap(~Feature, scales = "free", ncol = 2) +
      scale_fill_manual(values = c("0" = "#4575b4", "1" = "#d73027"), 
                        labels = c("0" = "No Diabetes", "1" = "Diabetes")) +
      labs(fill = "Diabetes Status", y = "Count") +
      theme_minimal()
  })
  
  # Diabetes by smoking history
  output$smoking_plot <- renderPlot({
    # Check which smoking columns exist
    smoking_cols <- c("Previously.Smoking", "Currently.Smoking", "Never.Smoked")
    available_cols <- smoking_cols[smoking_cols %in% colnames(train_data)]
    
    if(length(available_cols) > 0) {
      # Create smoking category based on available columns
      smoking_data <- data %>%
        mutate(
          smoking_history = case_when(
            `Never.Smoked` == 1 ~ "Never Smoked",
            `Previously.Smoking` == 1 ~ "Previously Smoked",
            `Currently.Smoking` == 1 ~ "Currently Smoking",
            TRUE ~ "Unknown"
          )
        ) %>%
        group_by(smoking_history, diabetes) %>%
        summarise(count = n(), .groups = 'drop')
      
      # Plot
      ggplot(smoking_data, aes(x = smoking_history, y = count, fill = factor(diabetes))) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Diabetes by Smoking History", 
             x = "Smoking History", 
             y = "Count",
             fill = "Diabetes Status") +
        scale_fill_manual(values = c("0" = "#4575b4", "1" = "#d73027"), 
                          labels = c("0" = "No Diabetes", "1" = "Diabetes")) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      # Create empty plot with message
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "Smoking history data not available") +
        theme_void()
    }
  })
  
  # Process prediction inputs and provide result
  observeEvent(input$predict, {
    # Process smoking history input into one-hot encoding
    previously_smoking <- as.numeric(input$smoking_history == "previous")
    currently_smoking <- as.numeric(input$smoking_history == "current")
    never_smoked <- as.numeric(input$smoking_history == "never")
    
    # Convert inputs to appropriate numeric types
    hypertension <- as.numeric(input$hypertension)
    heart_disease <- as.numeric(input$heart_disease)
    gender <- as.numeric(input$gender)
    
    # Make prediction
    result <- tryCatch({
      predict_diabetes(
        age = input$age,
        hypertension = hypertension,
        heart_disease = heart_disease,
        bmi = input$bmi,
        hbA1c = input$hbA1c,
        blood_glucose = input$blood_glucose,
        previously_smoking = previously_smoking,
        currently_smoking = currently_smoking,
        never_smoked = never_smoked,
        gender = gender
      )
    }, error = function(e) {
      paste("Error in prediction:", e$message)
    })
    
    # Format output based on result
    if(result == "Diabetic") {
      color <- "#d73027" # Red
      shinyjs::runjs("$('#prediction_output').css('background-color', '#ffebee');")
    } else if(result == "Not Diabetic") {
      color <- "#4575b4" # Blue
      shinyjs::runjs("$('#prediction_output').css('background-color', '#e3f2fd');")
    } else {
      color <- "#000000" # Black for errors
    }
    
    # Display result
    output$result <- renderUI({
      HTML(paste0("<span style='color:", color, "; font-size: 28px; font-weight: bold;'>", result,"</span>"))
    })
    
    # Update model performance metrics on tab
    updateModel()
  })
  
  # Update model performance metrics
  updateModel <- function() {
    # Ensure test data is properly formatted
    X_test <- as.matrix(test_data[, setdiff(names(test_data), "diabetes")])
    X_test <- cbind(1, X_test)  # Add intercept
    
    # Generate predictions
    pred_probs <- sigmoid(X_test %*% weights)
    pred_class <- ifelse(pred_probs > 0.9, 1, 0)
    
    # Calculate confusion matrix
    cm <- confusionMatrix(factor(pred_class), factor(test_data$diabetes))
    
    # Display confusion matrix
    output$confusion_matrix <- renderText({
      paste(capture.output(print(cm$table)), collapse = "\n")
    })
    
    # Display accuracy
    output$accuracy <- renderText({
      paste("Accuracy:", round(cm$overall['Accuracy'], 4))
    })
    
    # Display other metrics
    output$other_metrics <- renderText({
      paste("Sensitivity:", round(cm$byClass['Sensitivity'], 4),
            "\nSpecificity:", round(cm$byClass['Specificity'], 4),
            "\nPrecision:", round(cm$byClass['Pos Pred Value'], 4),
            "\nF1 Score:", round(cm$byClass['F1'], 4))
    })
    
    # ROC Curve
    output$roc_curve <- renderPlot({
      pred <- prediction(pred_probs, test_data$diabetes)
      perf <- performance(pred, "tpr", "fpr")
      auc <- performance(pred, "auc")@y.values[[1]]
      
      plot(perf, 
           col = "darkorange", 
           lwd = 2, 
           main = paste("ROC Curve (AUC =", round(auc, 3), ")"))
      abline(a = 0, b = 1, lty = 2, col = "gray")
    })
    
    # Feature Importance
    output$feature_importance <- renderPlot({
      feature_weights <- weights[-1]  # Remove intercept
      feature_names <- colnames(X_test)[-1]  # Remove intercept column name
      
      importance_df <- data.frame(
        Feature = feature_names,
        Importance = abs(feature_weights)
      )
      
      # Sort by importance
      importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]
      
      # Plot
      ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
        geom_col(fill = "purple") +
        coord_flip() +
        labs(title = "Feature Importance", x = "Feature", y = "Absolute Weight") +
        theme_minimal()
    })
    
  }
}

# Run the app
shinyApp(ui = ui, server = server)

