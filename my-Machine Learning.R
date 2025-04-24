rm(list=ls())
gc(reset = TRUE)
#-------------------------------------------------------------------------------
Note ---- <= or >= #to get this symbols, SHIFT < or > and click =.
remotes::install_github("cran/DMwR")
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
+                        MACHINE LEARNING                                                        +
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  

  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                            Linear Regression

-------------------------------1st option -----------------------------------

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
#split data
training <- cleanedoro[1:29, 1:17]
testing <- cleanedoro[30:41, 1:17]
#Create the model
Model <- lm(age ~., data=training)
summary(Model)
#Prediction
Pred <- predict(Model, testing)
Pred
plot(Pred,type = "l", lty = 1.8, col = "blue")
#Finding Accuracy
actual_pred <- data.frame(cbind(actuals=testing$age, predicted=Pred))

head(actual_pred)
plot(actual_pred)
#Evaluating accuracy
accuracy <- cor(actual_pred)
accuracy
#Model performance-------------------------------------------1.option
RMSE #values closer to 0 indicate better model performance.
MSE#lower value indicates a better fit
R²#values closer to 1 indicate a Model better fit 
library(DMwR)
DMwR::regr.eval(actual_pred$actuals, actual_pred$predicted)

#  #  #
------------------------------ 2nd option -----------------------------------
library(tidyverse)
library(caret)
theme_set(theme_classic())

# Load the data
doro <- read_excel("C:/Users/User/Desktop/repos/unclean-data.xlsx")
#Data structure
str(doro) 
#Data preparation
doro$id <- NULL 
str(doro)
#Identify row without missing data
cleanedoro <- na.omit(doro)
str(cleanedoro)
# Split the data into training and test set
training.samples <- cleanedoro$age %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data <- cleanedoro[training.samples, ]
test.data <- cleanedoro[-training.samples, ]
# Build the model
model <- lm(age ~., data = train.data)
# Make predictions
predictions <- model %>% predict(test.data)
# Model performance-----------------------------------2.option
data.frame(RMSE = RMSE(predictions, test.data$age),
           R2 = R2(predictions, test.data$age))
 
#   #   #

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                         Logistic Regression
-------------------------                   ------------------------------------
library(caTools)
library(readxl)
library(car)
library(dplyr)
library(class)
library(caret)
doro1 <- read_excel("C:/Users/User/Desktop/repos/unclean-data.xlsx")
#Data structure
str(doro1)
#Data preparation-1
doro1$id <- NULL 
str(doro1)
#Data prep----2
data2 <- doro1 %>%
  select(sex, age, bmi, hb,-id )

str(data2)
#Identify row without missing data
cleanedoro1 <- na.omit(data2)
str(cleanedoro1)
#Convert data into factor
cleanedoro1$sex <-as.factor(cleanedoro1$sex)
str(cleanedoro1)

#Splicing data---------------option1
train1 <- cleanedoro1[1:35, 1:4]
test1 <- cleanedoro1[36:50, 1:4]
#Build the model
model2 <-glm(sex ~ hb, data = train1, family = "binomial")
summary(model2)

exp(coef(model2))
#Predict
Pred2 <-predict(model2, test1, type = "response")
summary(Pred2)
head(Pred2)
#Checking Performance
#convert predicted values to True/False
class_predictions <- ifelse(Pred2 > 0.5, "Yes", "No")
head(class_predictions)
#Evaluating model
library(caret)
conf_matrix <- confusionMatrix(factor(class_predictions), factor(test_data$sex))
print(conf_matrix)
))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

#Check for Missing Values
sum(is.na(data))
#Handle Categorical Variables
data$Category <- factor(data$Category)
#Normalize Numerical Features (If Needed)
data$Variable <- scale(data$Variable)
#Splitting the Data
library(caTools)
library(caret)
set.seed(123) # For reproducibility
split <- sample.split(data$Outcome, SplitRatio = 0.7) # 70% training, 30% testing
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)
#Fit the Logistic Regression Model
model <- glm(Outcome ~ Predictor1 + Predictor2, data = train_data, family = binomial)
summary(model)
#Making Predictions
predictions <- predict(model, newdata = test_data, type = "response")
head(predictions)
#If you need class predictions (like Yes/No):
class_predictions <- ifelse(predictions > 0.5, "Yes", "No")
head(class_predictions)
#Evaluating Model Performance
library(caret)
conf_matrix <- confusionMatrix(factor(class_predictions), factor(test_data$Outcome))
print(conf_matrix)
#Plotting the ROC Curve
library(pROC)
roc_curve <- roc(test_data$Outcome, predictions)
plot(roc_curve, col = "blue", main = "ROC Curve")
auc(roc_curve)


#  #  #









