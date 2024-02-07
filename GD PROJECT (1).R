#IMPORT DATA
data <- read_excel("cancer patient data sets.xlsx", 
                                       sheet = "DATA")
View(data)

#EXPLANATORY DATA ANALYSIS

dim(data)
ncol(data)
is.na(data)
summary(data)

typeof(data$Age)
typeof(data$Gender)
typeof(data$Air.Pollution)
typeof(data$Alcohol.use)
typeof(data$OccuPational.Hazards)
typeof(data$Genetic.Risk)
typeof(data$chronic.Lung.Disease)
typeof(data$Smoking)
typeof(data$Passive.Smoker)
typeof(data$Level)


#DATA CLEANING / PRE-PREPARATION
data$Level = as.numeric(factor(data$Level))
data$Patient.Id <- NULL
View(data)
c=cor(data)
c

#CHECKING FOR FACTORS HAVING HIGH CORRELATION WITH STATE OF CANCER
high_corr = which(c > 0.5 & upper.tri(c, diag = FALSE), arr.ind = TRUE)
var_names = rownames(c)[high_corr[, 1]]
var_names = unique(var_names)
print(var_names)


target_variable <- "Level"
high_corr_vars <- c[target_variable, ] < 0
high_corr_var_names <- names(high_corr_vars)[high_corr_vars]
print(high_corr_var_names)      

#SPLITTING DATA INTO TRAINING AND TESTING
sample = sample(c(TRUE,FALSE), nrow(data), 
                 replace=TRUE, prob=c(0.7,0.3))

# creating training dataset
train_dataset  <- data[sample, ]

# creating testing dataset
test_dataset  <- data[!sample, ]

y_test=test_dataset[,10]

#MODEL FITTING
library(nnet)
model=multinom(train_dataset, data=data)


# Make sure "Level" is a factor with appropriate levels in both datasets (test_dataset and predictions)
test_dataset$Level = factor(test_dataset$Level)
predictions = factor(predictions, levels = levels(test_dataset$Level))

# Calculate sensitivity and specificity for each class
sensitivity <- diag(conf_matrix) / rowSums(conf_matrix)
specificity <- colSums(conf_matrix) - diag(conf_matrix) / (colSums(conf_matrix) - diag(conf_matrix))

# Print sensitivity and specificity for each class
print("Sensitivity (True Positive Rate):")
print(sensitivity)

print("Specificity (True Negative Rate):")
print(specificity)

install.packages(caret)
library(caret)

# Convert "Level" back to a factor if it was converted to numeric earlier
test_dataset$Level <- factor(test_dataset$Level)

# Compute the confusion matrix for overall performance metrics
conf_matrix_overall <- confusionMatrix(predictions, test_dataset$Level)

# Print overall accuracy, precision, recall, and F1-score
print("Overall Accuracy:")
print(conf_matrix_overall$overall['Accuracy'])

print("Overall Precision:")
print(conf_matrix_overall$byClass['Precision'])

print("Overall Recall (Sensitivity):")
print(conf_matrix_overall$byClass['Recall'])

print("Overall F1-score:")
print(conf_matrix_overall$byClass['F1'])