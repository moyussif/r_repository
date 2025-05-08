rm(list=ls())
gc(reset = TRUE)
#-------------------------------------------------------------------------------
Note ---- <= or >= #to get this symbols, SHIFT < or > and click =.
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
setwd("C:/Users/User/OneDrive - University of Ghana/myComputer@space/repos")
imdata <- read_excel("immunoData.xlsx")
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

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
+                               Regression Analysis                                                  +
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#       Regression analysis study the relationship between variables:

1-By identifying (Linear, Curvilinear, or Quadratic).
2-By estimating parameters of the relationships(intercept(B0) and slope(B1)).
3-By validating the relationship (The  assumptions for regression).

#The process of finding the line equation (Best fit the data values)------------regression line(linear)
#Linear equation-------------------------------------------------------------- y= B0 + B1x
#B0(y intercept)--------what is y when x = 0 ?
#B1(slope line)---------how much is y changes, for each unit increase in x, 
Note,
1,The equation gives pedicted value y-hat(y) at a given x.
2,Each value will not fall exactly on the line of best fit: the difference------residual/error. y(observed)-y-hat(predicted)
3,Least ordinary square(OLS)------------------is used to determine the slope and the intercept.
Thus, it tries to minimise the difference between the observed and the fitted values.
# B0 and B1 are the Least Square Estimates (LSE)

# B0 = y - B1x  ---------------- where x(mean),y(mean). -------------------     B1 = Sy
----
  # R squared = 0 - 1,---------R squared = SSE,        Rsquared = 1 - SSR.             Sx
  ----                       ----
  #                                        SSR                        SST                 
  
  
  R squared (coefficient of determination)---------------useful for model building___(fit model for multiple regression).  
thus it tells us the amount of variables explain in the reponse(y) after fitting the model.

#R squared = explain variation of model-y
----------------------------
  #              Total variation of model                                         df =n-2
  
  
  #---------------------- Checking assumptions of the model ------------------------------------
Assumptions
#----------y is a random variable and normally distributed with mean(u) and variance(Q)squared.
#----------The unknown standard error (residual) are independent normally distributed. mean = 0, variance squared.

hist(residuals(model), col="darkgray")

plot(fitted(model), residuals(model))
----------
  A plot of residuals vs. predicted values. #The residuals should be unbiased and homoscedastic.

#    #    #  

#Note,
R squared = good for simple regression. #Adjusted R squared =for multiple regression (model building)

model = lm(Species ~ Latitude, data = Data)
# shows parameter estimates, p-value for model, r-square
summary(model)                    

library(car)
# shows p-value for effects in model
Anova(model, type="II")              

#Plot linear regression
int = model$coefficient["(Intercept)"]
slope = model$coefficient["Latitude"]
plot(Species ~ Latitude, data = Data, pch=16, xlab = "Latitude", ylab = "Species")
abline(int, slope, lty=1, lwd=2, col="blue")     #  style and color of line 

#    #    #   

#------------------------ Curvilinear Regression -------------------------------
How to fit models to curvilinear data using three methods:
  
  1) Polynomial regression;  
2) B-spline regression with polynomial splines;  
3) Nonlinear regression with the nls function.  

Each of these three will find essentially the same best-fit curve with very similar p-values and R-squared values. 

# create sample data 
sample_data <- data.frame(x=1:10, 
                          y=c(25, 22, 13, 10, 5, 9, 12, 16, 34, 44)) 
View(sample_data)

#------------ fit linear -----------------
# create a basic scatterplot 
plot(sample_data$x, sample_data$y)
# define x-axis values 
x_axis <- seq(1, 10, length=10)
#fit model
linear_model1 <- lm(y~x, data=sample_data)

lines(x_axis, predict(linear_model1, data.frame(x=x_axis)), col='green')

summary(linear_model1)



#---- fit polynomial regression models up to degree 4 -------
-------------------------------------------------------------
  #----------- Curvilinear ------------------------------------

linear_model2 <- lm(y~poly(x,2,raw=TRUE), data=sample_data)
# create a basic scatterplot 
plot(sample_data$x, sample_data$y) 
# define x-axis values 
x_axis <- seq(1, 10, length=10) 
lines(x_axis, predict(linear_model2, data.frame(x=x_axis)), col='red')

# add curve of each model to plot----------------------------
linear_model3 <- lm(y~poly(x,3,raw=TRUE), data=sample_data)
# create a basic scatterplot 
plot(sample_data$x, sample_data$y) 
# define x-axis values 
x_axis <- seq(1, 10, length=10) 
lines(x_axis, predict(linear_model3, data.frame(x=x_axis)), col='purple')

# multiple curve to plot-------------------------------------
linear_model4 <- lm(y~poly(x,4,raw=TRUE), data=sample_data)
linear_model5 <- lm(y~poly(x,5,raw=TRUE), data=sample_data)
# create a basic scatterplot 
plot(sample_data$x, sample_data$y) 
# define x-axis values 
x_axis <- seq(1, 10, length=10) 
lines(x_axis, predict(linear_model4, data.frame(x=x_axis)), col='blue') 
lines(x_axis, predict(linear_model5, data.frame(x=x_axis)), col='orange')

================================================================================
  
  #---------------------- Polynomial regression -----------------------------
Polynomial regression is really just a special case of multiple regression, 

#Simple plot of model
library(ggplot2)
ggplot(imdata,aes(x =age, y = bmi))+geom_point(size = 4, shape = 20,colour = "Black")+
  stat_smooth(method = lm, se = FALSE, formula = y~poly(x,3), colour = "Green")+
  stat_smooth(method = lm, se = FALSE, formula = y~poly(x,2), colour = "Red")

#    #    #

===================================
  View(imdata)  
Data = imdata
plot(imdata$age,imdata$bmi)

model <- lm(bmi~poly(age,1,raw=TRUE), data=imdata)
plot(model)

lines(x_axis, predict(model, data.frame(age=x_axis)), col='blue')

plot(bwgt ~ age, data = imdata, pch=16, xlab = "Age", ylab = "Babywt") 
i = seq(min(imdata$age), max(imdata$age), len=100)          #  x-values for line
predy = predict(model, data.frame(age=i))                   #  fitted values
lines(i, predy, lty=1, lwd=2, col="blue")                   #  style and color

#---------------------------- Multiple Regression ------------------------------ 
library(psych)
corr.test(Data.num, use = "pairwise", method="pearson", adjust="none", alpha=.05)

pairs(data=Data, ~ Longnose + Acerage + DO2 + Maxdepth + NO3 + SO4 + Temp)

library(PerformanceAnalytics)
chart.Correlation(Data.num, method="pearson", histogram=TRUE, pch=16)
-------------------------------------------------------------------------------
  #Multiple regression
  Model selection using the step function-(options)
1-To add terms to a model (direction="forward"),
2-To remove terms from a model (direction="backward"),
3-To use a process that both adds and removes terms (direction="both").

It uses AIC (Akaike information criterion) as a selection criterion.  

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+ ------------------------------ Model Building -------------------------------------------- + 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
  # methods for evaluating subset regression models:
  1-choose one  with the largest Adjusted R squared.
2-choose one with the smallest MSE.
3-choose one with the smallest AIC.
4-choose one with the smallest predicted sum of square (SS)
5-choose one with the number of the parammetrs used in a model equal to CP value.
Example of Mallow CP ------------------- 4 parameters, 1 intercept = 5CP

#
install.packages("olsrr")        
library(olsrr)        

g <- lm(y~.,data = dataset)       # where . is the x variables
summary(g)
g <- lm(bmi~age+parity+hb,data = imdata)
#--------model building
forward <- ols_step_forward_p(g, penter = 0.05)
forward                                               # forward
forward <- ols_step_forward_aic(g, details = TRUE)
forward

Backward <- ols_step_backward_p(g, prem = 0.05)
Backward                                             # Backward
Backward <- ols_step_backward_aic(g, details = TRUE)
Backward

Both <- ols_step_both_p(g, pent = 0.05, prem = 0.05)
Both                                                 # Stepwise
Both.aic <- ols_step_both_aic(g, details = TRUE)
Both.aic

#Options for all possible subset models
all <- ols_step_all_possible(g)
all
as.data.frame(all)
plot(all)

#option for best subset regression
best <- ols_step_best_subset(g)
best

# in conclusion, our final model 
pred <-lm(bmi~age+parity+hb,data = imdata)     #  where . is the list of x variables selected for modelling. 
summary(pred)

par(mfrom = c(2,2))
plot(pred)


dev.off()

#   #   #


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                             LOGISTIC REGRESSION                                                 +
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#A binary variable is a categorical outcome that has two categories or levels. 
#The logistic model (or logit model) is used to model the probability of a particular 
#class/event such as pass or fail, win or lose, alive or dead or healthy or sick. 
Note----the function is glm, y is categorical, x can be (categorical or continuous).
Report logistic regression outcome with Oddratio by taking exponentiation of Estimate. 
Probability is 0  - 1
OddRatio------OR<1,LESS likely to occur / OR > 1 MORE likely to occur

#---------------- Simple Logistic Regression 
library(readxl)
library(readr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(dplyr)
setwd("C:/Users/User/OneDrive - University of Ghana/myComputer@space/repos")
imdata <- read_excel("immunoData.xlsx")
View(imdata)
str(imdata)
#Convert categorical variable to factors
imdata$CaseControl <-as.factor(imdata$CaseControl)
imdata$parity <-as.factor(imdata$parity)
str(imdata)
#Create contingency table of categorical outcome and predictors we want to make sure no 0 cells.
table(imdata$CaseControl,imdata$parity)
#regression model
#------when x is continuous====
logistic <- glm(CaseControl ~ age, data = imdata, family = "binomial" )
summary(logistic)
#log-odds=-1.92264+0.05512*age
exp(0.05512)
# oddratio only
exp(coef(logistic))
#interpret
-----a unit increase in age,the odds of having case is increase by factor 1.06. 
holding other factors constant(e.g multiple logistic regression).

#------when x is categorical====
logist1 <- glm(CaseControl ~ parity, data = imdata, family = "binomial" )
logist1
summary(logist1)
#log-odds=-0.7673+0.6719*(parity=1)+2.1535*(parity=2)+1.8659*(parity=3)-15.7988*(parity=4)
parity0 is used as reference

#odd ratio only
exp(coef(logist1))
------parity-1 is 1.96 more likely to have case compared to parity-0
parity-2 is 8.62 more likely to have case compared to parity-0
parity-3 is 6.46 more likely to have case compared to parity-0
parity-4 is 1.37 more likely to have case compared to parity-0

#----------------------- Multiple Logistics regression 
multi_logist <- glm(CaseControl ~ age + bmi + parity, data = imdata, family = "binomial" )
summary(multi_logist)

#CI using profiled log-likelihood-------------as part of reporting OR & p value.       
confint(multi_logist)

#odd ratio only
exp(coef(multi_logist))

#odd ratio and 95% CI
exp(cbind(OR = coef(multi_logist), confint(multi_logist)))


#Note_______in case we want to reorder /change the reference group, Use relevel
change_ref <-relevel(imdata$parity, ref = "1")
logist2 <- glm(CaseControl ~ parity, data = imdata, family = "binomial" )
logist2


#   #   #
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                               Poisson Regression                             #(Log-linear model) 
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Assumptions ---- y
# should not be negative
# Should be discrete
# should involve time
# Mean = Variance------Variance > Mean or > 1 (Over-dispersion)----quasi poisson
# ---------------------Variance < Mean or < 1 (Under-dispersion)----other models
library(readxl)
library(readr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(Hmisc)
#------
setwd("C:/Users/User/OneDrive - University of Ghana/myComputer@space/repos")
imdata <- read_excel("immunoData.xlsx")
str(imdata)
#histogram
hist(imdata$age, breaks = 10, xlab = "Age count", main = "Age Distribution", Prob=TRUE)
#boxplot
boxplot(imdata$age, main = "Age Distribution")
#fit model
poisson1 <- glm(age ~ hb+bmi+expose, data = imdata, family = poisson())
summary(poisson1)

#fitting only variables with Significant p value
fit.onlySignificant <- glm(age ~ bmi, data = imdata, family = poisson())
summary(fit.onlySignificant)

#--------------------------------
The reg paramter for bmi is 0.0124      
#In poisson reg, y is modeled as the log of the conditional mean(l).

#interpret
The parameter indicates that one unit increase in the bmi is associated with a 0.0124 
increase in the log mean number of age    ---- Holding other variables constant


#    #    #


++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                               Survival Analysis                               #(Time to event) +
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#-------------Is a test to investigate (HOW LONG IT TAKES FOR AN EVENT TO OCCUR) 
  
# Kaplan-Meier(KM)__________________(Nonparametric method)
  Is decriptives
# Cox proportional Hazard Model_____(semi parametric method)
Distributive assumptions
#-----------------------------------
library(readr)
library(readxl)
library(survival)
library(survminer)
#-----------------------------------
setwd("C:/Users/User/OneDrive - University of Ghana/myComputer@space/repos")
#-----------------------------------
Survdata <- read_csv("lung.csv")
View(Survdata)
#create censoring variable (right censoring)
Survdata$censored[Survdata$status == 1] <- 1
Survdata$censored[Survdata$status == 0 | Survdata$status == 2 ] <- 0 
View (Survdata)

#inspect time spent
hist(Survdata$time)
# inspect distribution just those status=0(Not censored)
hist(subset(Survdata, status == 0)$time)
# inspect distribution just those status=1(Censored)
hist(subset(Survdata, status == 1)$time)

#+++++++++++++++++++++ Kaplan-Meier analysis (model) ++++++++++++++++++++++++++++++++++++++++ (KM)
+---------------------------------------------------------------------------------------------- +
km_fit1 <- survfit(Surv(time, censored) ~ 1, data=Survdata ) #------------------STEP 1
print(km_fit1)
summary(km_fit1)
#summarize by pre-specified Time interval ---------> repeat90 days for 30 times 90*(1:30)
summary(km_fit1, times = c(30, 60, 90*(1:30)))

#plot 
plot(km_fit1)
#plot cumulative survival rates (probabilities)
ggsurvplot(km_fit1, data = Survdata, risk.table = TRUE, conf.int = TRUE, ggtheme = theme_minimal())

#--------------- Km analysis model with categorical covariate ------------------STEP 2
km_fit2 <- survfit(Surv(time, censored) ~ ph.ecog, data=Survdata )
print(km_fit2)
summary(km_fit2)
summary(km_fit2, times = c(30, 60, 90*(1:30)))
#plot
ggsurvplot(km_fit2, data = Survdata, risk.table = TRUE, conf.int = TRUE, pval= TRUE, pval.method=TRUE, 
           ggtheme = theme_minimal())

#  #  #

#+++++++++++++++++++++++++ cox proportional Hazard (PH) Model +++++++++++++++++++++++++++++++++ (PH)
+------------------------------------------------------------------------------------------------ +
#---- Estimate cox Regression Model---------------------------------------------STEP1
cox_reg1 <- coxph(Surv(time, censored) ~ ph.ecog, data =Survdata)
summary(cox_reg1)

#---- Estimate cox Regression Model with Categorical and Continues predictors----STEP2
cox_reg2 <- coxph(Surv(time, censored) ~ ph.ecog + wt.loss + sex + age + ph.karno, data =Survdata)
summary(cox_reg2)


#NB---------------------------------- can change the reference group and perform (Log_overallrisk)

#    #    # 

 
  
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                              Time Series Analysis                                                   +
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(tidyverse)
library(ggplot2)
library(tseries)
library(forecast)
library(readxl)
#-------------------------------------------------------------------------------

Converting date to time series
#STEP1====tdata$Date = as.Date(tdata$Date, format = "%Y/%m/%d") 
#STEP2====hhdata = ts(tdata$attendance,start = min(tdata$Date), end = max(tdata$Date),frequency = 1)
#STEP3====class(hhdata)  
#-------------------------------------------------------------------------------

Monthly data
#1-month_data=ts(tdata$attendance, start = min(tdata$Date),end = max(tdata$Date),frequency = 12)
#2-monthly <- ts(tdata$attendance, start = 2015, frequency = 12)
#3-monthly = ts(tdata$attendance, start = c(2015,3),end = c(2022, 12),frequency = 12)
Quarterly data 
#1-qtr_data=ts(tdata$attendance, start = min(tdata$Date),end = max(tdata$Date),frequency = 4)
#2-quarterly <- ts(ttdata$registrants, start = 2015, frequency = 4)
#3-qtrly = ts(tdata$attendance, start = c(2015,3),end = c(2022, 12),frequency = 4)
Yearly data 
#1-yr_data=ts(tdata$attendance, start = min(tdata$Date),end = max(tdata$Date),frequency = 1)
#2-yearly <- ts(ttdata$registrants, start = 2015, frequency = 1)
#3-yrly = ts(tdata$attendance, start = c(2015,3),end = c(2022, 12),frequency = 1)
--------------------------------------------------------------------------------
setwd("C:/Users/User/OneDrive - University of Ghana/myComputer@space/repos")
tdata <- read_excel("CTrends.xlsx")
View(tdata)
class(tdata)
boxplot(attendance~Date, data = tdata)
--------------------------------------------------------------------------------
#To control//make the variance Equal
log(tdata$attendance)
plot(log(tdata$attendance))  
#To control//make the mean Equal
plot(diff(log(tdata$attendance)))

#convert data to time series
tsdata=ts(tdata$attendance, start = min(tdata$Date),end = max(tdata$Date),frequency = 1)
class(tsdata)
view(tsdata)
plot(tsdata)

#check to determine stationarity of data 
acf(tsdata)      #step1----autocorrelation
pacf(tsdata)     #step2----partial autocorrelation
adf.test(tsdata) #step3----augmented Dickey-fuller test

#Convert Non_stationary to Stationary---------(seasonal arima model)
tsdata_model=auto.arima(tsdata, ic = "aic", trace = TRUE)
tsdata_model
tsdisplay(residuals(tsdata_model), lag.max = 45, main = "(0,0,0) Model residuals" )
#Check for stationary again
acf(ts(tsdata_model$residuals))
pacf(ts(tsdata_model$residuals))

#Now perform forecast for stationary data-----(seasonal arima model)
mydataforecast=forecast(tsdata_model, level = c(95),h=5*4)
mydataforecast
plot(mydataforecast)
autoplot(mydataforecast)

#Non seasonal ARIMA-------------------------------------------------
nsdata_model=auto.arima(tsdata, seasonal = FALSE)
nsdata_model
tsdisplay(residuals(nsdata_model), lag.max = 45, main = "(0,0,0) Model residuals" )
non_seasonal = forecast(nsdata_model)
plot(non_seasonal)

#Now perform forecast for stationary data-----(seasonal arima model)
mysecondforecast=forecast(nsdata_model, level = c(95),h=5*4)
mysecondforecast
plot(mysecondforecast)


#Evaluate model (seasonal model)
Box.test(tsdata_model$residuals, lag = 5,type = "Ljung-Box")
Box.test(tsdata_model$residuals, lag = 15,type = "Ljung-Box")
Box.test(tsdata_model$residuals, lag = 30,type = "Ljung-Box")
#alternate the lag values until the P.values is > 0.05  ---- indicate No further autocorrelation

#   #   #

=========================== Time series Using ggplot2 ==========================
#------------------------------------------------------------------------------
library(readxl)
library(scales)
library(ggplot2)
library(ggpmisc)
#------------------------------------------------------------------------------
setwd("C:/Users/User/OneDrive - University of Ghana/myComputer@space/repos")
tdata <- read_excel("CTrends.xlsx")
View(tdata)
#convert date to time series
tdata$Date = as.Date(tdata$Date, format = "%Y/%m/%d")  

# line and Points
ggplot(tdata, aes(x = Date, y = attendance)) +
  geom_line()

#peaks
ggplot(tdata, aes(x = Date, y = attendance)) +
  geom_line()+
  stat_peaks(geom = "point", span = 15, color = "steelblue3", size = 2) +  
  stat_peaks(geom = "label", span = 15, color = "steelblue3", angle = 0,
             hjust = -0.1, x.label.fmt = "%Y-%m-%d") +
  stat_peaks(geom = "rug", span = 15, color = "blue", sides = "b")

#Valleys  
ggplot(tdata, aes(x = Date, y = attendance)) +
  geom_line()+
  stat_valleys(geom = "point", span = 11, color = "red", size = 2)+   
  stat_valleys(geom = "label", span = 11, color = "red", angle = 0,
               hjust = -0.1, x.label.fmt = "%Y-%m-%d")+
  stat_valleys(geom = "rug", span = 11, color = "red", sides = "b")

#break midpoint
ggplot(tdata, aes(x = Date, y = attendance)) +  
  geom_point()+
  geom_vline(xintercept = as.Date(tdata$Date, format = "%Y/%m/%d"),
             linetype = 2, color = 2, linewidth = 1)

# Facet wrap for multiple lines  
fw <-ggplot(tdata, aes(x = Date, y = attendance)) +  
  geom_line() +
  facet_wrap(~Date)
fw

# Facet grid for multiple lines
fg <-ggplot(tdata, aes(x = Date, y = attendance)) +  
  geom_line() +
  facet_grid(attendance~Date)
fg


#    #    #



++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                                          PCA                                                       +
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Descriptives  -------------#Variances ie equality and standard deviations
KMO & Barlet test----------#Sample adequacy(>0.07)  &  Non-correlated(0.05)
Correlation matrix---------0 - 1#correlation close to 1----highly correlated
Communality ---------------# Must be close 1 (ind//extracted)
Total Variances------------
Rotated component----------# Must be > 1
Plot-----------------------# Scree plot , Loading plot
  
--------------------------------------------------------------------------------
rm(list=ls())
gc(reset = TRUE)
#Load library
library(readr)
library(readxl)
library(rio)
setwd("C:/Users/User/OneDrive - University of Ghana/myComputer@space/repos")
#import data

pcadata <- import("pca_immdat1.csv", na.rm = TRUE)
View(pcadata)
pcadata
#Scale the data--------------------
scdat <-scale(pcadata[,-1:-5], center = T)
scdat
#No of print----
options(max.print = 10000)
options(scipen = 100)
getwd()
#Visualize Package
library(factoextra)
install.packages("pca3d")
# Built pca analysis------------
library(FactoMineR)
fpca <- PCA(scdat, ncp = 8)
fpca
fpca$eig #eigen values
fpca$ind$coord
fpca$ind$cos2
fpca$var$contrib #contribution of individual variable

#biplot
fviz_pca(fpca)
fviz_pca_biplot(fpca, repel = TRUE)
fviz_pca_biplot(fpca, repel = TRUE, col.ind = "cos2")
#individual scatter plot
fviz_pca_ind(fpca)
fviz_pca_ind(fpca, repel = TRUE, col.ind = "cos2")

# Adding Ellipses
fviz_pca_ind(fpca, geom.ind = "point", col.ind = pcadata$SCGen, palette = c("green","pink","purple", "blue", "yellow","red"), 
             addEllipses = TRUE,legend.title = "individual component" )


fviz_pca_ind(fpca, geom.ind = "point", col.ind = pcadata$NICUadmin, palette = c("purple", "blue", "red"), 
             addEllipses = TRUE,legend.title = "individual component" )


#Variable
fviz_pca_var(fpca)
fviz_pca_var(fpca, repel = TRUE, col.var = "contrib")

#scree plot
fviz_screeplot(fpca)
fviz_screeplot(fpca, ncp = 8)
fviz_screeplot(fpca, ncp = 8, geom="line")
fviz_screeplot(fpca, ncp = 8, geom="bar")
fviz_screeplot(fpca, ncp = 8, geom="bar", barfill="red")
fviz_screeplot(fpca, choice="eigenvalue")
fviz_screeplot(fpca, choice="eigenvalue", ncp= 8)
#
fpca$eig
#to export the data generated with eigenvalues
table1 <- fpca$eig
class(table1)
table1 <- as.data.frame(table1)
class(table1)

#import data
library(writexl)
write_xlsx(table1, "eigentable.xlsx")

#----------------elbow method-----------------
plot(table1$`cumulative percentage of variance`)

#----------------Rotated components-----------
library(psych)
rpca <- principal(scdat, nfactors = 3, rotate = "varimax", scores = TRUE)
rpca
rpca$communality
rpca$loadings
print(rpca$loadings, digits = 3, cutoff = 0) #save in the working directorate---->convert image to excel 
# rotated component matrix Barplot
barplot(rpca$loadings)
barplot(rpca$loadings, beside = TRUE)
barplot(rpca$loadings, beside = TRUE, col = "blue", main = "Rotated Component matrix")

# add R color palette
library(pals)
barplot(rpca$loadings, beside = TRUE, col = brewer.accent(18), main = "Rotated Component matrix")  
barplot(rpca$loadings, beside = TRUE, col = brewer.greens(18), main = "Rotated Component matrix")  
barplot(rpca$loadings, beside = TRUE, col = alphabet(18), main = "Rotated Component matrix")  

#import scores
rpca$scores
scores <- rpca$scores
class(rpca$scores)
scores <- as.data.frame(rpca$scores)
class(scores)
#
write_xlsx(scores, "scores.xlsx")

#   #    #  


++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                        Modelling infectious diseases                                                   +
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
library(readxl)
library(readr)
library(rio)
library(tidyverse)
library(skimr)
library(deSolve)
library(reshape2)

#Part one: SIR Mode----------- (Example one) --------------------------

sir<-function(time,state,parameters)
{with(as.list(c(state,parameters)),
      {dS<--beta*S*I
      dI<-beta*S*I-gamma*I
      dR<-gamma*I
      return(list(c(dS,dI,dR)))})
}

# state=(SIR),   parameters=(beta, gamma) 


#Provide the initial values and some parameters as below:
init<-c(S=1-1e-6,I=1e-6,0.0)
parameters<-c(beta=1.4247,gamma=0.14286)
times<-seq(0,70,by=1)

#Put the results in dataframe    Ordinary differential equation (ode)
ode<-as.data.frame(ode(y,times,func,parms))
ode<-as.data.frame(ode(y=init,times=times,func=sir,parms=parameters))
summary(ode)

out<-as.data.frame(ode)
out$time<-NULL

#Ploting the results
matplot(times,out,type="l",xlab="Time",ylab="Susceptible and Recovered",main = "SIR Model",
        lwd = 1, lty = 1, bty = "l", col = 2:4)
legend(40,0.7,c("Susceptible","Infected","Recovered"),pch=1,col=2:4)


infections <- out$I
peak <- max(infections)
match(peak, infections) #this will show the day where peak is high

................................................................................


#--------------Step 1: writing the differential equations with() function below:

sir_2 <- function(time, variables, parameters) {
  with(as.list(c(variables, parameters)), {
    dS <- -beta * I * S
    dI <- beta * I * S - gamma * I
    dR <- gamma * I
    return(list(c(dS, dI, dR)))
  })
}

#with() works on lists only, not on vectors.........

#-------------Step 2: defining some values for the parameters in a named vector:

parameters_values <- c(
  beta = 0.004, # infectious contact rate (/person/day)
  gamma = 0.5 # recovery rate (/day)
)

#-------------Step 3: defining initial values for the variables
#The initial values of the variables need to be defined in a named vector:
initial_values <- c(
  S = 999, # number of susceptible at time = 0
  I = 1, # number of infectious at time = 0
  R = 0 # number of recovered (and immune) at time = 0
)

#-------------Step 4: the points in time where we need to calculate variables values
#We want to know the values of our SIR model variables at these time points:
time_values <- seq(0, 10) # days

#-------------Step 5: numerically solving the SIR model We have defined all the needed ingredients:
ls()

## [1] "infections" "init" "initial_values"
## [4] "ode" "out" "parameters"
## [7] "parameters_values" "peak" "sir"
## [10] "sir_2" "time_values" "times"


sir_2

## function(time, variables, parameters) {
## with(as.list(c(variables, parameters)), {
## dS <- -beta * I * S
## dI <- beta * I * S - gamma * I
## dR <- gamma * I
## return(list(c(dS, dI, dR)))
## })
## }
parameters_values
## beta gamma
## 0.004 0.500
initial_values
## S I R
## 999 1 0
time_values
## [1] 0 1 2 3 4 5 6 7 8 9 10
#-----------------------------                        
#so now we can use the ode() function of the deSolve package to numericallysolve our model:

sir_values_1 <- ode(
  y = initial_values,
  times = time_values,
  func = sir_2,
  parms = parameters_values
)
sir_values_1

#you can use these values for further analytical steps,  
sir_values_1 <- as.data.frame(sir_values_1)  
sir_values_1

with(sir_values_1, {
  # plotting the time series of susceptibles:
  plot(time, S, type = "l", col = "blue",
       xlab = "time (days)", ylab = "number of people")
  # adding the time series of infectious:
  lines(time, I, col = "red")
  # adding the time series of recovered:
  lines(time, R, col = "green")
})

# adding a legend:
legend("right", c("susceptibles", "infectious", "recovered"),
       col = c("blue", "red", "green"), lty = 1, bty = "n")
---------------------------------------------------------------------
  
  #The value of the R0 is
  (999 + 1) * parameters_values["beta"] / parameters_values["gamma"]
# Or __________________assigning N <- (999 + 1)
N <- (999 + 1)
N * parameters_values["beta"] / parameters_values["gamma"]

# beta 8

sir_try <- function(beta, gamma, S0, I0, R0, times) {
  require(deSolve) # for the "ode" function
  
  # the differential equations:
  sir_equations <- function(time, variables, parameters) {
    with(as.list(c(variables, parameters)), {
      dS <- -beta * I * S
      dI <- beta * I * S - gamma * I
      dR <- gamma * I
      return(list(c(dS, dI, dR)))
      9
    })
  }
  
  # the parameters values:
  parameters_values <- c(beta = beta, gamma = gamma)
  # the initial values of variables:
  initial_values <- c(S = S0, I = I0, R = R0)
  # solving
  out <- ode(initial_values, times, sir_equations, parameters_values)
  # returning the output:
  as.data.frame(out)
}

sir_try(beta = 0.004, gamma = 0.5, S0 = 999, I0 = 1, R0 = 0, times = seq(0, 10))

## time S I R
## 1 0 999.0000000 1.00000 0.000000
## 2 1 963.7055761 31.79830 4.496125
## 3 2 461.5687749 441.91575 96.515480
## 4 3 46.1563480 569.50418 384.339476
## 5 4 7.0358807 373.49831 619.465807
## 6 5 2.1489407 230.12934 767.721720
## 7 6 1.0390927 140.41085 858.550058
## 8 7 0.6674074 85.44479 913.887801
## 9 8 0.5098627 51.94498 947.545162
## 10 9 0.4328913 31.56515 968.001960
## 11 10 0.3919173 19.17668 980.431400

------------- Comparing a model’s predictions with data -----------------------  
  
  flu <- read.table("https://bit.ly/2vDqAYN", header = TRUE)
# The above link may be broken in the future so is a good practice to save
#the data on your computer after first download from the internet
load("flu.RData")
flu

## day cases
## 1 0 1
## 2 1 6
## 3 2 26
## 4 3 73
## 5 4 222
## 6 5 293
## 7 6 258
## 8 7 236
## 9 8 191
## 10 9 124
## 11 10 69
## 12 11 26
## 13 12 11
## 14 13 4

#-------- Plot the points of the flu data set and use the sir_try() function to visually compare 
#                                                   the model’s predictions and the data points:
with(flu, plot(day, cases, pch = 19, col = "red", ylim = c(0, 600)))
predictions <- sir_try(beta = 0.004, gamma = 0.5, S0 = 999, I0 = 1, R0 = 0, times = flu$day)
with(predictions, lines(time, I, col = "red"))

#The above model did not fit the observed data well so we need to train the model on the data by 
#changing beta and gamma parameters.-------In this case, we will change only the beta parameter:
with(flu, plot(day, cases, pch = 19, col = "red", ylim = c(0, 600)))
predictions <- sir_try(beta = 0.0025, gamma = 0.5, S0 = 999, I0 = 1, R0 = 0, times = flu$day)
with(predictions, lines(time, I, col = "red"))

### The above model is better than the first model but not good yet
-----------------------------------                          
  #Write a function that takes parameters values as inputs and draws the figure as an output. 
  #Play with that function to see how changing the values of parameters can bring the model’s predictions closer to the data points.
  
  model_fit <- function(beta, gamma, data, N = 763, ...) {
    I0 <- data$cases[1] # initial number of infected (from data)
    times <- data$day # time points (from data)
    # model's predictions:
    predictions <- sir_try(beta = beta, gamma = gamma, # parameters
                           S0 = N - I0, I0 = I0, R0 = 0, # variables' intial values
                           times = times) # time points
    # plotting the observed prevalences:
    with(data, plot(day, cases, ...))
    # adding the model-predicted prevalence:
    with(predictions, lines(time, I, col = "red"))
  } 

model_fit(beta = 0.004, gamma = 0.5, flu, pch = 19, col = "red", ylim = c(0, 600))
-------------------------------------
  
  #The above model did not fit the data well so let us change the beta parameter to train the data
  
  model_fit(beta = 0.0025, gamma = 0.5, flu, pch = 19, col = "red", ylim = c(0, 600))  

#The above model reasonably fits the data well. Let us get some model predictions based on the above model:
beta=0.0025
gamma=0.5
s0=762
I0=1
R0=0
time=flu$day
predictions <- sir_try(beta = beta, gamma =gamma, S0 = s0, I0 = I0, R0 = R0, times = time)
predictions

#----And we want to compare these model’s predictions with real prevalence data:
#One simple way to do so is to compute the “sum of squares” as below:

sum((predictions$I - flu$cases)ˆ2)

# Which is the squared sum of the lengths of vertical black segments of the figure below:
# the observed prevalences:

with(flu, plot(day, cases, pch = 19, col = "red", ylim = c(0, 600)))
# the model-predicted prevalences:
with(predictions, lines(time, I, col = "red", type = "o"))
# the "errors":
segments(flu$day, flu$cases, predictions$time, predictions$I)

#And we want to predict beyond the observed time period (i.e., forecast) based on the best parameters from
the fitted model above
newtime=seq(0,20)
model_forecast <- sir_try(beta = beta, gamma =gamma, S0 = s0, I0 = I0, R0 = R0, times = newtime)
summary(model_forecast)

matplot(model_forecast, type="l", lty=1, main="SIRS model", xlab="Time",ylab="Number of people")
legend <- colnames(model_forecast)[2:4]
legend(15000,900000, legend=legend, col=2:4, lty = 1)


#What are the effects of increasing or decreasing the values of the transmission 
#contact rate (β) and the recovery rate (γ) on the shape of the epi curve?

#Estimating model’s parameters Sums of squares

#This is our model’s predictions:
predictions <- sir_try(beta = 0.004, gamma = 0.5, S0 = 999, I0 = 1, R0 = 0, times = flu$day)
predictions

#One simple way to do so is to compute the “sum of squares” as below:
sum((predictions$I - flu$cases)ˆ2)
#Which is the squared sum of the lengths of vertical black segments of the figure below

#the observed prevalences:
with(flu, plot(day, cases, pch = 19, col = "red", ylim = c(0, 600)))
# the model-predicted prevalences:
with(predictions, lines(time, I, col = "red", type = "o"))
# the "errors":
segments(flu$day, flu$cases, predictions$time, predictions$I)

#=================-----------------------------------------=====================

Shiny
Note the syntax follows that used by the popular ODE solver deSolve. require(shinySIR)
#required package: shinySIR
## Loading required package: dplyr  ## Attaching package: ’dplyr’
The following objects are masked from ’package:stats’:
  ## filter, lag
  The following objects are masked from ’package:base’:
  ## intersect, setdiff, setequal, union
  ## Loading required package: shiny,tidyr,ggplot2
  
  run_shiny(model = "SIR")
# Warning in run_shiny(model = "SIR"):The length of the manual colour scale vector (’values’) must equal 
#the number of model variables. Using default ggplot colours instead.

mySIRS <- function(t, y, parms) {
  with(as.list(c(y, parms)),{
    # Change in Susceptibles
    dS <- - beta * S * I + delta * R
    # Change in Infecteds
    dI <- beta * S * I - gamma * I
    # Change in Recovereds
    dR <- gamma * I - delta * R
    return(list(c(dS, dI, dR)))
  })
}

#The interactive plot can then be created by calling this function with neweqns, specifying initial conditions
# for all model variables (ics), and specifying vectors for the parameter attributes, including parameter starting
# values (parm0), names to be displayed in the interactive menu (parm_names), and minimum and maximum
# values for the interactive menu (parm_min and parm_max, respectivel

run_shiny(model = "SIRS (w/out demography)",
          neweqns = mySIRS,
          ics = c(S = 9999, I = 1, R = 0),
          parm0 = c(beta = 5e-5, gamma = 1/7, delta = 0.1),
          parm_names = c("Transmission rate", "Recovery rate", "Loss of immunity"),
          parm_min = c(beta = 1e-5, gamma = 1/21, delta = 1/365),
          parm_max = c(beta = 9e-5, gamma = 1 , delta = 1))

## Warning in run_shiny(model = "SIRS (w/out demography)", neweqns = mySIRS, : 
#The length of the manual colour scale vector (’values’) must equal the number of model variables.Using default ggplot colours instead.


============================================================================== 
  #---------------------- Part two: SIRS models in R ---------------------------

----------------------- Modelling Waning Immunity ----------------------------
  #It is possible that people gain immunity after recovering from the infection but the immunity doesn’t last forever. 
  #So, these individuals become susceptible again. Here, σ is the waning rate.The SIRS model is an extension of SIR model. 
  
  #For SIRS, additional compartment called “waning immunity”is added to the SIR model. Thus, 
  
  #The basic SIRS model has three compartments with three parameters 
  
  ∂S
∂t
= -(β x S) + (σ x R) . . . . . . . . . . . . (6)
∂I
∂t
= (β x S) - (γ x I) . . . . (7)
∂R
∂t
= (γ x I) - (σ x R) . . . . . . . . . . . . . . . . . . (8)
Waning immunity

Assume σ =0.2 per day,β =0.4 per day and waning rate,*σ =1/10 per year if average immunity period is taken as 10 years. 

The model is run for a time period of 50 years in daily intervals.
require(ggplot2)
require(deSolve)
require(reshape2)
## Loading required package: reshape2
## Attaching package: ’reshape2’
## The following object is masked from ’package:tidyr’:
## smiths
# Model input
initial_values=c(S=999999,I=1,R=0)
parameters=c(gamma=0.2*365,beta=0.4*365,sigma=1/(10))
# Time points
time=seq(from=1,to=50,by=1/365)
#SIR model function
sirs_model <- function(time,state,parameters){
  with(as.list(c(state,parameters)),{
    N=S+I+R
    lambda=beta*(I/N)
    dS=-lambda*S+sigma*R
    dI=lambda*S-gamma*I
    dR=gamma*I-sigma*R
    return(list(c(dS,dI,dR)))
  })
}

# Solving the differential equations:
model_sirs<-as.data.frame(ode(y=initial_values,func = sirs_model,parms=parameters,times = time))
names(model_sirs)
## [1] "time" "S" "I" "R"
matplot(model_sirs, type="l", lty=1, main="SIRS model", xlab="Time",ylab="Number of people")
legend <- colnames(model_sirs)[2:4]
legend(15000,900000, legend=legend, col=2:4, lty = 1)

# Note that We can plot the prevalence instead of the number of people by dividing the values by 1,000,000

#Alternatively, plot the prevalence with ggplot2
model_sirs_long=melt(model_sirs,id="time")
names(model_sirs_long)
## [1] "time" "variable" "value"
#Prevalence plot
ggplot(data = model_sirs_long,
       aes(x = time, y = value/1000000, colour = variable, group = variable)) +
  geom_line() +
  xlab("Time (years)")+
  ylab("Prevalence") +scale_color_discrete(name="State")




==============================================================================
  #--------------------------- Part three: SEIR model --------------------------
------------------------------ SEIR model fitting ----------------------------
  
  #The SEIR model is an extension of SIR model. 
  #For SEIR, additional compartment called “exposed” is added to the SIR model. 
  #Thus, The basic SEIR model has four compartments represented as:
  
  1. S - “Susceptible” – individuals who have not been exposed to the virus
2. E - “Exposed” – individuals exposed to the virus, but not yet infectious
3. I - “Infectious” – exposed individuals who go on to become infectious
4. R - “Recovered” – infectious individuals who recover and become immune to the virus
5. Population size N is the sum of the individuals in the 4 compartments.

#---------Parameters of the SEIR model
#  The flow of individuals between compartments is characterised by a number of parameters.
#  β (beta):is the transmission coefficient.Is the average number of infectious contacts an infectious individual 
#  in the population makes at each time period. A high value of β means the virus has more opportunity to spread.

#  σ (sigma): is the rate at which exposed individuals become infectious. Is the reciprocal of the average time 
#  it takes to become infectious. 
#  That is, if an individual becomes infectious after 4 days on average, σ will be 1/4 (or 0.25).

#  γ (gamma): is the rate at which infectious individuals recover. As before, think of it as the reciprocal of the
#  average time it takes to recover. That is, if it takes 10 days on average to recover, γ will be 1/10 (or 0.1).

#  μ (mu): is an optional parameter to describe the mortality rate of infectious individuals. The higher μ is,
#  the more deadly the virus. From these parameters, you can construct a set of differential equations. 
#  These describe the rate at which each compartment changes size.

Setting-up SEIR Equations----------------------------------------
  #Equation (9) - Susceptible
  #The first thing to see from the model is that there is no way S can increase over time. 
  #There are no flows back into the compartment. Equation (6) must be negative, as S can only ever decrease.
  
  In what ways can an individual leave compartment S? 
  #Well, they can become infected by an infectious individual in the population.
  
  #At any stage, the proportion of infectious individuals in the population = I/N.
  #And the proportion of susceptible individuals will be S/N.
  Under the assumption of perfect mixing 
#(that is, individuals are equally likely to come into contact with any other in the population), 
#the probability of any given contact being between an infectious and susceptible individual is (I / N) * (S / N).
#This is multiplied by the number of contacts in the population. 
#This is found by multiplying the transmission coefficient β, by the population size N. 
#Combining that all together and simplifying gives equation (9): 
∂S
∂t
= - (β x S x I) / N . . . . . . . . . . (9)

Equation (10) - Exposed
Next, let’s consider the “exposed” compartment, E. Individuals can flow into and out of this compartment.
The flow into E will be matched by the flow out of S. So the first part of the next equation will simply be
the opposite of the previous term.
Individuals can leave E by moving into the infectious compartment. This happens at a rate determined by two variables 
– the rate σ and the current number of individuals in E. So overall equation (10) is:
  
  ∂E
∂t
= (β x S x I) - (σ x E) . . . . (10)
Equation (11) - Infectious
he next compartment to consider is the “infectious” compartment, I. There is one way into this compartment, 
which is from the “exposed” compartment.

There are two ways an individual can leave the “infectious” compartment.
Some will move to “recovered”. This happens at a rate γ.

Others will not survive the infection. They can be modeled using the mortality rate μ. So equation (11) looks like:
  ∂I
∂t
= (σ x E) - (γ x I) - (μ x I) . . . . (11)
Equation (12) - Recovered
Now let’s look at the “recovered” compartment, R.
This time, individuals can flow into the compartment (determined by the rate γ).And no individuals can flow out of the compartment 
(although in some models, it is assumed possible to move back into the “susceptible” compartment 
  
  - especially infectious diseases where re-infection is possible - COVID-19?). So the overall equation (12) looks like this:
  ∂R
∂t
= γ x I . . . . . . . . . . . . . . . . . . (12)
Equation (13) - Mortality (optional)
Using similar reasoning, you could also construct equation (13) for the change in mortality. 
You mightconsider this a fifth compartment in the model.
∂M
∂t
= μ x I . . . . . . . . . . . . . . . . . . (13)
You may set μ to zero (0) to exclude this compartment from the model.

Thus, we have given the full set of differential equations (9-13) Solving equations 9-13 (SEIR model) in R
require(deSolve)
SEIR <- function(time, current_state, params){
  with(as.list(c(current_state, params)),{
    N <- S+E+I+R
    dS <- -(beta*S*I)/N
    dI <- sigma*E - gamma*I - mu*I
    dR <- gamma*I
    dM <- mu*I
    return(list(c(dS, dE, dI, dR, dM)))
  })
}


The above function describes and provides 3 arguments:
  (a) The current time step.
(b) A list of the current states of the system (that is, the estimates for each of S, E, I and R at the current time step).
(c) A list of parameters used in the equations (recall these are β, σ, γ, and μ).

Inside the function body, you define the system of differential equations as described above. 
These are evaluated for the given time step and are returned as a list.
The order in which they are returned must match the order in which you provide the current states.

Now take a look at the code below:
  params <- c(beta=0.5, sigma=0.25, gamma=0.2, mu=0.001)
initial_state <- c(S=999999, E=1, I=0, R=0, M=0)
times <- 0:365

The above codes initialises the parameters and initial state (starting conditions) for the model.
It also generates a vector of times from zero to 365 days.

Now, create the model:
  model <- ode(initial_state, times, SEIR, params)

This uses deSolve’s ode() function to solve the equations with respect to time.
See here for the documentation.

The arguments required are:
  1. The initial state for each of the compartments
2. The vector of times (this example solves for up to 365 days)
3. The SEIR() function, which defines the system of equations
4. A vector of parameters to pass to the SEIR() function

Running the command below will give the summary statistics of the model.

summary(model)


## S E I R M

Already, you will find some interesting insights.
1. Out of a million individuals, 108,264 did not become infected.
2. At the peak of the epidemic, 126,516 individuals were infectious simultaneously.
3. 887,300 individuals recovered by the end of the model.
4. A total of 4436 individuals died during the epidemic.

You can also visualise the evolution of the pandemic using the matplot() function. colnames(model)
## [1] "time" "S" "E" "I" "R" "M"
matplot(model, type="l", lty=1, main="SEIR model", xlab="Time")
legend <- colnames(model)[2:6]
legend("right", legend=legend, col=2:6, lty = 1)

#Assign both x and y axis labels
matplot(model, type="l", lty=1, main="SEIR model", xlab="Time",ylab="Number of people")
legend <- colnames(model)[2:6]
legend("right", legend=legend, col=2:6, lty = 1)
#Add the peak time line vertically
matplot(model, type="l", lty=1, main="SEIR model", xlab="Time",ylab="Number of people")
abline(v=112,col="blue")
legend <- colnames(model)[2:6]
legend("right", legend=legend, col=2:6, lty = 1) 

#Add the peak time line vertically
matplot(model, type="l", lty=1, main="SEIR model", xlab="Time",ylab="Number of people")
abline(v=112,col="blue")
text(112,920000,"Peak day: 112",cex = 0.7,pos=3)
legend <- colnames(model)[2:6]
legend("right", legend=legend, col=2:6, lty = 1)

#The associated plot is shown above:
#You can also coerce the model output to a dataframe type. Then, you can analyse the model further.

infections <- as.data.frame(model)$I
peak <- max(infections)
match(peak, infections)
## [1] 112
===============================================================================
  #------------------- SEIRM model with intervention methods ---------------------

The SEIR model is an interesting example of how an epidemic develops without any changes in the population’s behaviour.
You can build more sophisticated models by taking the SEIR model as a starting point and adding extra features.

This lets you model changes in behaviour (either voluntary or as a result of government intervention).
Many (but not all) countries around the world entered some form of “lockdown” during the coronavirus pandemic of 2019.

Ultimately, the intention of locking down is to alter the course of the epidemic by reducing the transmission coefficient, β.

The code below defines a model which changes the value of β between the start and end of a period of lockdown.

All the numbers used are purely illustrative. You could make an entire research career (several times over)
trying to figure out the most realistic values.
SEIR_lockdown <- function(time, current_state, params){
  with(as.list(c(current_state, params)),{
    beta = ifelse(
      (time <= start_lockdown || time >= end_lockdown),
      0.5, 0.1
    )
    N <- S+E+I+R
    dS <- -(beta*S*I)/N
    dE <- (beta*S*I)/N - sigma*E
    dI <- sigma*E - gamma*I - mu*I
    dR <- gamma*I
    dM <- mu*I
    return(list(c(dS, dE, dI, dR, dM)))
  })
}

The only change is the extra ifelse() statement to adjust the value of β to 0.1 during lockdown and β to 0.5 
before and after the lockdown.
Thus, if before or after the lockdown, give beta value to be 0.5 (i.e., increased transmission rate) 
but if within the lockdown periods, assign beta value to be 0.1 (i.e., reduced transmission
                                                                 rate)
You need to pass two new parameters to the model. These are the start and end times of the lockdown periods.
Here, the lockdown begins on day 90, and ends on day 150.

params <- c(
  sigma=0.25,
  gamma=0.2,
  mu=0.001,
  start_lockdown=90,
)

initial_state <- c(S=999999, E=1, I=0, R=0, M=0)
times <- 0:365
model2 <- ode(initial_state, times, SEIR_lockdown, params)

Now you can view the summary and graphs associated with this model.
summary(model2)

## S E I R M


This will reveal: You can see:
  1. Out of a million individuals, 156,886 did not become infected.
2. At the peak of the epidemic, 72,444 individuals were infectious simultaneously. 838,917 individuals
recovered by the end of the model.
3. A total of 4195 individuals died during the epidemic.
4. Plotting the model using matplot() reveals a strong “second wave” effect 
(as was seen across many countries in Europe towards the end of 2020).
matplot(
  model2,
  type="l",
  lty=1,
  main="SEIR model (with intervention)",
  xlab="Time"
)                        
legend <- colnames(model2)[2:6]
legend("right", legend=legend, col=2:6, lty = 1)


Put the start and end of the lockdown periods for better visualization
matplot(
  model2,
  type="l",
  lty=1,
  main="SEIR model (with intervention)",
  xlab="Time",
  ylab="Number of people"
)
abline(v=90, col="blue")
abline(v=150, col="blue")
text(112,600000,"start & end- lockdown: day 90 & 150",cex = 0.7,pos=3)
legend <- colnames(model2)[2:6]
legend("right", legend=legend, col=2:6, lty = 1)                        


Finally, you can coerce the model to a dataframe and carry out more detailed analysis from there.

infections <- as.data.frame(model2)$I
peak <- max(infections)
match(peak, infections)
## [1] 223

1. In this scenario, the number of infections peaked on day 223.
2. In other scenarios, you could model the effect of vaccination. Or, you could build in seasonal differences
in the transmission rate.

Exercise for participants (duration - one hour)
Using the last model above, fit a similar model for the scenarios below separately, and interpret your results for each scenario:
  
  (1) vary the value of β between the start and end of a period of lockdown as 0.5 and 0.2 respectively.
(2) vary the value of β between the start and end of a period of lockdown as 0.6 and 0.2 respectively.
(3) vary the value of γ to 0.4 and β between the start and end of a period of lockdown as 0.5 and 0.1 respectively.                        

#   #   #
