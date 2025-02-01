library(tidyverse)
library(ggpubr)
data = read.csv('cleaned data.csv')

for (column in seq(ncol(data))){
  if(length(unique(data[[column]]))<4){
    data[[column]]<- as.factor(data[[column]])
  }
}

numeric_column<- data %>% select_if(is.numeric)

#numeric_data<- data %>% select(where(is.numeric))
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
# outlier_detector<- function(x){deviation = sd(x,na.rm = T)
#   average = mean(x,na.rm = T)
#   scaler= (x - average)/deviation
#   outlier<- sum(abs(scaler) > 3)
#   return (outlier)}

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


