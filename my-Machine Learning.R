rm(list=ls())
gc(reset = TRUE)
#-------------------------------------------------------------------------------
Note ---- <= or >= #to get this symbols, SHIFT < or > and click =.
#-------------------------------------------------------------------------------

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
library(readxl)
imdata <- read_excel("C:/Users/User/Desktop/repos/immunoData.xlsx")
View(imdata)

data2 <- imdata %>%
  select(CaseControl, expose, parity, systol1, diastol1, age, bmi, hb,-id ) %>% 
  filter(!is.na(c(expose, parity, systol1, diastol1, age, bmi, hb)))        
print(data2)

#  #  #


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                                 HANDLING MISSING DATA                                               +
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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
library(caTools)
library(readxl)
library(car)
library(dplyr)
doro <- read_excel("C:/Users/User/Desktop/repos/unclean-data.xlsx")
#Data structure
str(doro) 
#Data preparation
doro$id <- NULL 
str(doro)
#Identify row without missing data
cleanedoro <- na.omit(doro)
str(cleanedoro)
#Convert data into factor
cleanedoro$class <- factor(ifelse(cleanedoro$sex ==1, "Male", "Female"))
str(cleanedoro)
#Build the model
#Splicing data---------------option1
training <- cleanedoro[1:29, 1:17]
testing <- cleanedoro[30:41, 1:17]
#split the outcome into training and testing set
training1 <- cleanedoro[1:29, 18]
testing1 <- cleanedoro[30:41, 18]
#Apply KNN algorithm to training set and outcome
library(class)
?knn
knn(train, test, cl, k = 1, l = 0, prob = FALSE, use.all = TRUE)
#
# get square_root ->sqrt(training$age)
prediction <- knn(train= training, cl= training1, k= 1, test = testing)
prediction


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
