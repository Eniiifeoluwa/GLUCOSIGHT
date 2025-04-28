library(tidyverse)
library(ggpubr)
data = read.csv('DATASETS/cleaned data.csv')

data<- factor_converter(data)

numeric_column<- numeric_selector(data)


correlation_matrix = cor(numeric_column, use = 'complete.obs')
#***************************************************
#             CORRELATION MATRIX                   #  
#***************************************************


pheatmap::pheatmap(correlation_matrix,
                   cluster_rows = F, 
                   cluster_cols = F, annotation_legend = TRUE,
                   legend = T, show_rownames = T, show_colnames = T,
                   display_numbers = T, border_color = 'black', 
                   main = "CORRELATION MATRIX FOR CONTINOUS FEATURES", lwd = 3,
                    number_color = 'black' )


#***************************************************
#             CREATING THE BOXPLOTS                #  
#***************************************************

extended_data<- pivot_longer(data,
                             cols = c(bmi, age, HbA1c_level, blood_glucose_level),
                             names_to = "Column_names", values_to = "Values")

ggplot(data = extended_data,
       mapping = aes(x = diabetes, y = Values, fill = diabetes))+
  geom_boxplot()+
  facet_wrap(~Column_names, scales = 'free')+
  
  labs(x= "Diabetes Outcome", y = "Value", title = "BOX PLOT OF DIABETES FEATURES")+
  
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))


#***************************************************
#             WORKING ON THE OUTLIERS          #  
#***************************************************



outliers <- data.frame(Variable = character(),
                       Outlier = numeric(),
                       class_value = character(),
                       stringsAsFactors = FALSE)

category <- 'diabetes'
for (values in unique(data[[category]])){
  subset = data[data[[category]] == values, ]
  
  for (column in seq(ncol(subset))){
    
    if (is.numeric(subset[[column]])){
      outlier = box_plot_outlier(subset[[column]])
      
      cat("The Outliers in column", colnames(subset[column]), 
          "Class", values, "is:", outlier)
      cat("\n")
      
      outliers = rbind(outliers, data.frame(
        Variable = colnames(subset[column]),
        Outlier = outlier,
        class_value = values,
        stringsAsFactors = F))
    }
  }
}

outliers$class_value <- as.factor(outliers$class_value)
outliers$Variable<- as.factor(outliers$Variable)

ggplot(data = outliers)+
  
  geom_bar(mapping = aes(x = Variable,
                               y = Outlier, fill = class_value),
           stat = 'identity', position = 'dodge')+
  geom_text(mapping= aes(x = Variable, y = Outlier, label = Outlier),
            vjust= -0.9, size = 3.4 )+
  
  labs(x = "Features", y = "Counts of Outliers", title = "OUTLIERS' CHARTS")+
  
  theme_minimal()+
  
  theme(plot.title = element_text(hjust = 0.5, face= 'bold'))

ggplot(data = extended_data, mapping=aes(x=Values))+
  geom_histogram(bins = 30, fill = 'blue', colour = 'black')+
  facet_wrap(~Column_names, scales = 'free')+
  theme_minimal()+
  labs(x = "VALUES OF FEATURES", y = 'COUNTS', 
       title = 'DISTRIBUTION OF FEATURES')+
  theme(plot.title = element_text(hjust = 0.5,
                                  face = 'bold', colour = 'red'))

ggplot(data  = extended_data, mapping = aes(x = Values))+
  
  stat_ecdf(geom = 'step', colour = 'blue', )+
  stat_function(fun = pnorm, args = list(mean = mean(extended_data$Values),
              sd = sd(extended_data$Values)), linetype= 'dashed',
                color = 'red')+
  
  facet_wrap(~Column_names, scales = 'free')+
  theme_minimal()+
  
  labs(x = "VALUES", y = 'EMPIRICAL PROBABILITY', 
       title = 'EMPIRICAL VS THEORETICAL CDF')+
  
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))+
  annotate(geom = 'text', x = max(extended_data$Values) * 0.3, y = 0.9,
           label = 'EMP.', colour = 'blue', size = 3)+
  
  annotate(geom = 'text', x = max(extended_data$Values) * 0.3, y = 0.7,
           label = 'THEO.', colour = 'red', size = 3)
data_copy = data
data_copy
data_copy$gender<- case_when(data_copy$gender == 0 ~ 'Female',
          data_copy$gender == 1 ~ 'Male',
          data_copy$gender == 2 ~'Male',
          .default = as.factor(data_copy$gender))
ggplot(data = data_copy)+
  geom_bar(mapping = aes(x = gender, y = diabetes, fill = diabetes), stat = 'identity')+
  theme_classic()



# Convert diabetes column to factor with labels
data_copy$diabetes <- factor(data_copy$diabetes, 
                                 levels = c(0, 1), 
                                 labels = c("No", "Yes"))


# Plotting
library(dplyr)
library(ggplot2)

# Summarize the counts
gender_diabetes_summary <- data_copy %>%
  group_by(gender, diabetes) %>%
  summarise(count = n(), .groups = "drop")

# Plotting
ggplot(gender_diabetes_summary, aes(x = gender, y = count, fill = diabetes)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribution of Diabetes Status by Gender",
       x = "Gender",
       y = "Number of Individuals",
       fill = "Diabetes") +
  theme_minimal() +
  scale_fill_manual(values = c("Yes" = "#E63946", "No" = "#457B9D")) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12)
  )
# Create age groups
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
correlation_matrix
