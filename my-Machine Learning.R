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


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                                MACHINE LEARNING 
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
update.packages("caTools")
library(caTools)
library(readxl)
library(car)
imdata <- read_excel("C:/Users/User/Desktop/repos/immunoData.xlsx")

split <-sample.split(imdata$age, SplitRatio = 0.7)
split
train <-subset(imdata$age, split ="TRUE")
train <-subsets(imdata$age, split ="TRUE")
test <-subset(imdata$age, split ="FALSE")
train 
test

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
