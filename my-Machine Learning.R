rm(list=ls())
gc(reset = TRUE)
#----------------- BUILDING FUNCTIONS -------  
e.g-1
Area <- function(Length, Breadth){
  Area <- Length * Breadth
  return(Area)
}
#therefore,
Area (3,7)
----------
  e.g-2  

BMI <- function(weight,height){
  BMI <- weight/height*2
  return(BMI)
}  
BMI(62,58)  
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                                     Data flow Dplyr                                                  +
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
  library(tidyverse)
library(dplyr)
data2 <- imdata %>%
  select(CaseControl, expose, age,delivage, hb, plt, parity, bmi,-id ) %>% 
  filter(hb > 10)        
print(data2)

#  #  #
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                                 HANDLING MISSING DATA                                               +
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  imdata <- read_excel("C:/Users/User/Desktop/repos/immunoData.xlsx")
print(imdata)
#Missing Data Summary-----------------
summary(data2)
# Count missing values in each column
missing_per_column <- colSums(is.na(data2))
print(missing_per_column)
# Remove rows with any missing values 
cleaned_data <- na.omit(data2)
print(cleaned_data)
# Perform mean imputation for the 'salary' column where NA values are present
mean_salary <- mean(data$Salary, na.rm = TRUE)

# Perform KNN imputation
library(VIM)  
# Perform KNN imputation
data_imputed <- kNN(cleaned_data, k = 5)  # You can adjust 'k' as needed

# Remove the 'Age' column
df <- df %>% select(-Age)
#Remove multiple columns
df <- df %>% select(-c(Age, Gender))

#  #  #


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                               MACHINE LEARNING                                                        +
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
update.packages("caTools")
library(caTools)
library(readxl)
library(car)
library(dplyr)
imdata <- read_excel("C:/Users/User/Desktop/repos/immunoData.xlsx")
print(imdata)

#Split the data
split <-sample.split(imdata, SplitRatio = 0.7)
split
train <-subset(rm_data, split =="TRUE")
test <-subset(rm_data, split =="FALSE")
train 
test

#Create the model
Model <- lm(age~., data = train)
summary(Model)

#Prediction
Pred <- predict(Model,test)
Pred


---------------------------------------------
# R program to illustrate
# Graph plotting in
# Polynomial regression
# Importing required library
library(tidyverse)
library(caret)
theme_set(theme_classic())

# Load the data
data("Boston", package = "MASS")
# Split the data into training and test set
set.seed(123)
training.samples <- Boston$medv %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data <- Boston[training.samples, ]
test.data <- Boston[-training.samples, ]

# Build the model
model <- lm(medv ~ poly(lstat, 5, raw = TRUE), data = train.data)
# Make predictions
predictions <- model %>% predict(test.data)
# Model performance
data.frame(RMSE = RMSE(predictions, test.data$medv),
           R2 = R2(predictions, test.data$medv))

ggplot(train.data, aes(lstat, medv) ) + geom_point() + 
  stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE))


#   #   #
