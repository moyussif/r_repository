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
setwd("C:/Users/User/OneDrive - University of Ghana/myComputer@space/repos")
imdata <- read_excel("immunoData.xlsx")
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


++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                            MACHINE LEARNING                                                    +
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                            Linear Regression

-------------------------------1st option -----------------------------------

library(caTools)
library(readxl)
library(car)
library(dplyr)
setwd("C:/Users/User/OneDrive - University of Ghana/myComputer@space/repos")
doro <- read_excel("unclean-data.xlsx")
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
setwd("C:/Users/User/OneDrive - University of Ghana/myComputer@space/repos")
doro1 <- read_excel("unclean-data.xlsx")
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                         Logistic Regression
-------------------------                   ------------------------------------
rm(list = ls())    # Reset workspace
setwd("C:/Users/User/OneDrive - University of Ghana/myComputer@space/repos")
doro1 <- read_excel("unclean-data.xlsx")
nrow(doro1)
library(caTools)
library(readxl)
library(car)
library(dplyr)
library(class)
library(caret)

######### 1. LOAD DATASET #############


######### 2. CLEAN DATASET ##############
#rows with na values
sum(apply(is.na(doro1), 1, any))

# Remove rows with NA values
doro1 <- na.omit(doro1)

#check one more time for na values
sum(apply(is.na(doro1), 1, any))
nrow(doro1)

#convert sex to be a factor
is.factor(doro1$sex)
doro1$sex <- as.factor(doro1$sex)
# recheck
is.factor(doro1$sex)

########## 3. SPLIT INTO TESTING AND TRAINING SETS ##########
# Set a seed for reproducibility
set.seed(123)

# Generate random indices for the training and testing sets
train_indices <- sample(nrow(doro1), 0.7 * nrow(doro1))  # 70% for training
test_indices <- setdiff(1:nrow(doro1), train_indices) # 30% testing
head(train_indices)

# Create training and testing sets using indices
train_data <- doro1[train_indices, ]
test_data <- doro1[test_indices, ]

# check size of testing and training datasets
nrow(train_data)
nrow(test_data)
str(doro1)

########### 4. TRAIN MODEL TO PREDICT LITERACY ############
model.sex <- glm(sex ~ edu + age + cat + bmi, data = train_data, family = "binomial")
summary(model.sex)

########### 5. PREDICT LITERACY USING MODEL ##############
model.sex.predict <- predict(model.sex, test_data, type = 'response')
model.sex.predict
summary(model.sex.predict)
#print first few results
head(model.sex.predict,3)

########### 6. CHECK MODEL PERFORMANCE ###############
#convert predicted values back to True/False
test_data$predict.sex <- ifelse(model.sex.predict >= .5, "True","False")

#convert string "true" and "false" values to booleans
test_data$predict.sex <- as.logical(test_data$predict.sex)

head(test_data$predict.sex,5)
head(test_data$sex,5)

#explore
table(test_data$predict.sex,test_data$sex)
#Determine accuracy of model
accuracy <- mean(test_data$predict.sex == test_data$sex)
print(accuracy)
#

#  #  #

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                             KNN----(K Nearest Neighbour)
--------------------------------------------------------------------------------
rm(list=ls())
gc(reset = TRUE)
#
library(readr)
library(tidyverse)
setwd("C:/Users/User/OneDrive - University of Ghana/myComputer@space/repos")
ovary1 <- read.csv("ovarian.csv")
head(ovary1)
str(ovary1)
ova <-ovary1 %>% select(rx,futime,fustat, age, resid.ds,ecog.ps)
ova
#Normalize
normalize <-function(x){
  return(x-min(x)) / (max(x)-min(x))}
#
ova1 <-as.data.frame(lapply(ova[,2:5], normalize))
ova1
#Set seed
set.seed(123)
#random  selection of  70% of data
ova_data <- sample(1:nrow(ova1),size = nrow(ova1)*0.7,replace = FALSE)

# 70% train and 30% test
traindata <-ova[ova_data,]  
testdata <- ova[-ova_data,]  

#create separate dataframe for rx
train_df <-ova[ova_data,1]  
test_df <- ova[-ova_data,1] 

library(class) # it carries KNN function

NROW(train_df) # number of observations

# get square root of the total observations
sqrt(18) #square root of 18 is 4.24 or 5.0-----model for better K value
knn.4 <- knn(train=traindata,test=testdata, cl=train_df, k=4)
knn.3 <- knn(train=traindata,test=testdata, cl=train_df, k=3)
#accuracy check
Acc.3 <- 100*sum(test_df==knn.3)/NROW(test_df)
Acc.4 <- 100*sum(test_df==knn.4)/NROW(test_df)
Acc.4 # accuracy is 75% better than  K=3 (50%)

# To check prediction against actual value in tabular form
table(knn.4,test_df)
knn.4

# Model evaluation 
library(caret)
library(e1071)

confusionMatrix(table(knn.4,test_df))

i=1
k.optm=1
for(i in 1:15){
  knn.mod <-knn(train=traindata,test=testdata, cl=train_df, k=i)
  k.optm[i] <- 100*sum(test_df==knn.mod)/NROW(test_df)
  k=i
  cat(k,'=', k.optm[i],'\n')
}

plot(k.optm, type ="b", xlab = "k-value", ylab = "Accuracy")


#  #  #

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                              K-Means Clustering
--------------------------------------------------------------------------------option 1
rm(list=ls())
gc(reset = TRUE)
library(readxl)
library(readr)
library(tidyverse)
library(stats)
library(ggfortify)

setwd("C:/Users/User/OneDrive - University of Ghana/myComputer@space/repos")
VMdata <- read_excel("VMdata.xlsx")
#Select columns
datacolumns <- select(VMdata,c(1,2,3,4,5,7))

#wss plot function

wssplot <- function(VMdata, nc=15, seed=123456)
{
  wss <- (nrow(VMdata)-1)*sum(apply(VMdata,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(VMdata, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of clusters", 
       ylab="Within groups sum of squares")
}
# wss plot to choose maximum number of clusters----spotting the elbow point
wssplot(datacolumns)
# K-Means Cluster 
KM = kmeans(datacolumns,4)
#Evaluate Cluster Analysis_________ Cluster plot
autoplot(KM,datacolumns,frame=TRUE)
#Cluster centres
KM$centers

KM
#  #  #

********************************************************************************
--------------------------------------------------------------------------------option 2
rm(list=ls())
gc(reset = TRUE)
library(readxl)
library(readr)
library(tidyverse)
library(factoextra)
setwd("C:/Users/User/OneDrive - University of Ghana/myComputer@space/repos")
VMdata <- read_excel("VMdata.xlsx")
print(VMdata)
#Select columns
datacolumns <- select(VMdata,c(1,2,3,4,5,7))
print(datacolumns)
datacolumns <- na.omit(datacolumns)
# scale data
datascaled <- scale(datacolumns)
#distance
datadistn <- dist(datascaled)
#calculate how many clusters you need ----withinss (wss)
fviz_nbclust(datascaled, kmeans, method = "wss")+
  labs(subtitle = "Elbow method")
#                                 No of clusters identified = 5
--------
# Kmeans
km.clust <- kmeans(datascaled, centers = 3, nstart = 100)
print(km.clust)
km.clust$cluster

#Visualize the clustering algorithm results
km.clusters <- km.clust$cluster
rownames(datascaled) <- paste(VMdata$Discharge,1:dim(VMdata)[1],sep = "-")
#
fviz_cluster(list(data=datascaled, cluster = km.clusters))
#table
table(km.clusters,VMdata$Discharge)


#  #  #

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                            DecisionTree algorithm
--------------------------------------------------------------------------------
rm(list=ls())
gc(reset = TRUE)
library(readxl)
library(readr)
library(tidyverse)
library(rpart)

setwd("C:/Users/User/OneDrive - University of Ghana/myComputer@space/repos")
lungdata <- read_csv("lung.csv")
str(lungdata)
#Divide into Train and Test dataset  
set.seed(3)
id <- sample(2,nrow(lungdata),prob = c(0.7,0.3),replace = TRUE)
lungtrain <-lungdata[id==1,]
lungtest <- lungdata[id==2,]
nrow(lungdata)
nrow(lungtrain)
nrow(lungtest)
#Build Decision Tree  
library(rpart) 
lungModel <-rpart(sex~., data = lungtrain)  
lungModel  
#plot
plot(lungModel, margin = 0.1)
#adding text and labels
text(lungModel,use.n = TRUE,pretty = TRUE,cex=0.8)
# Prediction
predd<-predict(lungModel, newdata = lungtrain)
pred_lung <-predict(lungModel, newdata = lungtest)

#____________________________ Model evaluation ______________________________ 1
library(caret)
library(e1071)
#---------Train
predd<-predict(lungModel, newdata = lungtrain)
pred0 <-ifelse(predd>0.5,1,0)
tab0 <- table(Predicted = pred0, Actual = lungtrain$sex)
confusionMatrix(as.factor(pred0),as.factor(lungtrain$sex))
#---------Test
pred_lung <-predict(lungModel, newdata = lungtest)
pred_lung
pred1 <- ifelse(pred_lung>0.5,1,0)
tab1 <- table(Predicted = pred1, Actual = lungtest$sex)
tab1
confusionMatrix(as.factor(pred1),as.factor(lungtest$sex))

#___________________________ Model evaluation _______________________________ 2
library(gmodels)
# Create a confusion matrix using CrossTable
confusion_matrix <- CrossTable(pred_lung,lungtest$sex, prop.chisq = FALSE, 
                               prop.t = FALSE, prop.r = FALSE)
# Print the confusion matrix
print(confusion_matrix)
  

  
#   #   #  
  
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                         RandomForest algorithm
--------------------------------------------------------------------------------
rm(list=ls())
gc(reset = TRUE)  
library(readxl)
library(readr)
library(tidyverse)
library(randomForest)

setwd("C:/Users/User/OneDrive - University of Ghana/myComputer@space/repos")
lugdata <- read_csv("lung.csv")  
View(lugdata)
#Divide into Train and Test dataset  
set.seed(3)
id <- sample(2,nrow(lugdata),prob = c(0.7,0.3),replace = TRUE)
lugtrain <-lugdata[id==1,]
lugtest <- lugdata[id==2,]  
#  
lugdata$sex <-as.factor(lugdata$sex)
lugtrain$sex <-as.factor(lugtrain$sex)
# RandomForest
bestmtrc <-tuneRF(lugtrain,lugtrain$sex, stepFactor = 1.2, improve = 0.01, trace = T, plot = T)
lug_forest <- randomForest(sex~., data = lugtrain)
lug_forest
plot(lug_forest)

# importance
importance(lug_forest)
#visualize
varImpPlot(lug_forest)
#Validate Model
predF <- predict(lug_forest, newdata = lugtest, type = "class")
predF
#Evaluation___
library(caret)
library(e1071)

confusionMatrix(table(predF,lugtest$sex))


#  # #

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                      Support Vector Machine - (SVM)
--------------------------------------------------------------------------------
rm(list=ls())
gc(reset = TRUE)  
library(readxl)
library(readr)
library(tidyverse)
library(caret)
library(e1071)
library(caTools)
setwd("C:/Users/User/OneDrive - University of Ghana/myComputer@space/repos")
lundata <- read_csv("lun.csv")
# Extract columns of interest
lundatv =lundata %>% select(status, wt.loss, age)
View(lundatv)
lundatv= na.omit(lundatv)
#
split = sample.split(lundatv$status,SplitRatio = 0.75)  
train_lg = subset(lundatv,split ==TRUE)
test_lg = subset(lundatv,split == FALSE)
# scaling 
train_lg[-1] = scale(train_lg[-1]) 
test_lg[-1] = scale(test_lg[-1])
#Create Model
classifier = svm(formula = status ~.,
                  data = train_lg,
                  type = "C-classification",
                  kernel = "linear")
classifier
#Predict
y_pred = predict(classifier, newdata = test_lg[-1])
y_pred  
# Evaluate
cfm = table(test_lg[,1],y_pred)
plot(classifier,test_lg[,1])
--------------------------------
#Error in xtfrm.data.frame(x) : cannot xtfrm data frames
test_lg <-as.data.frame(test_lg) # solution
#
cfm = table(test_lg[,1],y_pred)
cfm
#
diag = diag(cfm)
rowsums =apply(cfm,1, sum) 
colsums =apply(cfm,2, sum)
accuracy = sum(diag)/sum(cfm)
precision = diag/colsums
recall = diag/rowsums
f1 = 2*precision*recall/ (precision+recall)
data.frame(accuracy,precision,recall,f1)
###

-------------------------------------------------------------------------------
rm(list=ls())
gc(reset = TRUE)  
library(readxl)
library(readr)
library(tidyverse)
library(caret)
library(e1071)
setwd("C:/Users/User/OneDrive - University of Ghana/myComputer@space/repos")
lundata <- read_csv("lun.csv")  
str(lundata)
#Divide into Train and Test dataset  
set.seed(1)
x = matrix(rnorm(20*2), ncol=2)
y=c(rep(-1,10),rep(1,10))
# First 10 elements of y are -1 and the rest are 1 
x
y
x[y==1,] = x[y==1,] + 1
#Lets check if classes are linear separable
plot(x,col= (3-y))
#
ludat <- data.frame(x=x,y=as.factor(y))
ludat
#
svmfit = svm(y~., data = ludat,kernel = "linear", cost = 0.7, scale = FALSE)
plot(svmfit,ludat)
#our support vectors
summary(svmfit)

#svmPred <-predict(svmfit,newdata = )




