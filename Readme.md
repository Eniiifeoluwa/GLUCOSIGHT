

# Diabetes Prediction using Logistic Regression in R  

## Project Overview  
This project focuses on predicting diabetes using **Logistic Regression** in R. The dataset undergoes rigorous preprocessing, statistical testing, and visualization to ensure accurate and interpretable results.  

## Key Features  
- Implemented **Logistic Regression** for classification.  
- Performed **Stochastic Gradient Descent (SGD)** for optimization.  
- Conducted extensive **statistical analysis**, including:  
  - **Kolmogorov-Smirnov (KS) Test** to determine data distribution.  
  - **Chi-Square Test** for categorical variable independence.  
- Applied **Z-Score Normalization** due to the presence of outliers.  
- Visualized data distributions and relationships using **ggplot2**.  

## Dataset  
The dataset includes features such as:  
- **Numerical Variables** (e.g., glucose level, BMI, age, blood pressure)  
- **Categorical Variables** (e.g., smoking history, family diabetes history, gender)  

## Preprocessing Steps  
1. **Handling Missing Values** – Imputation where necessary.  
2. **Feature Encoding** – Used **One-Hot Encoding** and **Ordinal Encoding** for categorical variables.  
3. **Normalization** – Applied **Z-Score Normalization** to standardize numerical features.  
4. **Outlier Detection** – Identified and managed outliers for better model performance.  

## Model Implementation  
- Developed the **Logistic Regression** model in R.  
- Optimized using **Stochastic Gradient Descent (SGD)**.  
- Evaluated model performance using **Confusion Matrix, Accuracy, Precision, Recall, and AUC-ROC Curve**.  

## Statistical Analysis  
- **Kolmogorov-Smirnov (KS) Test** to check data distribution.  
- **Chi-Square Test** to assess categorical dependencies.  
- **Correlation Matrix** for numerical features.  
- **ANOVA Test** to assess the relationship between the numerical features and categorical dependent variable.
- **Feature Importance Analysis** to interpret model behavior.  

## Visualizations  
- **Boxplots** for outlier detection.  
- **Histograms** for distribution analysis.  
- **Line charts** for distribution assessment.
- **Scatter plots & Heatmaps** for feature relationships.  
- **ROC Curve** to evaluate model performance.  

## Tools & Libraries Used  
- **R Programming Language**  
- **ggplot2 & tidyverse** for data visualization  
- **caret** for model training and evaluation  
- **dplyr & tidyr** for data preprocessing  
- **MASS & stats** for statistical analysis  

## How to Run the Project  
1. Install required R packages:  
   ```r  
   install.packages(c("ggplot2", "caret", "dplyr", "tidyr", "MASS", "stats", 'ggpubr))  
   ```  
2. Load the dataset and perform preprocessing.  
3. Train the **Logistic Regression** model.  
4. Evaluate model performance using validation metrics.  
5. Visualize results for better interpretability.  

## Results & Insights  
- Identified key factors influencing diabetes risk.  
- Improved model performance through **feature selection and statistical analysis**.  
- Highlighted data distribution patterns using **Kolmogorov-Smirnov and Chi-Square tests**.  

## Future Improvements  
- Experiment with **alternative models** like Decision Trees or Random Forests.  
- Incorporate **feature engineering** to enhance predictive power.  
- Use **hyperparameter tuning** for further optimization.  

---  
**Author:** Akinola Samuel Afolabi  
**Contact:** [akinolaayomide226@gmail.com]  

