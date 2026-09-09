#================== Chisquare and Logistic Regression ==========================
rm(list=ls())
gc(reset = TRUE)
#
#---------------------- Load required packages
library(readxl)
library(readr)
library(tidyverse)
library(car)
library(pROC)
library(psych)
library(lessR)
library(Hmisc)
library(stats)
library(broom)
library(epitools)
library(ResourceSelection)

#------------------Import data------------
health_data <- read_excel("C:/Users/User/Downloads/health_data.xlsx")
#
str(health_data) 
print(health_data, width = Inf)
#
#Create Hypetensive variable
health_data$hypertensive <- ifelse(health_data$BP_systolic >= 140,"Yes", "No")

#Create Age_group
healthData = health_data %>% mutate(Age_group = case_when(
  Age  < 5 ~ "0-4",
  Age  < 11 ~ "5-10",
  Age  < 18 ~ "11-17",
  Age  < 35 ~ "18-34",
  Age  < 50 ~ "35-49",
  Age  < 65 ~ "50-64",
  TRUE ~ "65+"
))

str(healthData)
#
#-------------------------------------------------------------------------------
            Data Conversion _(CODING)  
#-------------------------------------------------------------------------------
#
healthData$Age_group <- as.factor(healthData$Age_group)

healthData$sex <- as.factor(healthData$sex)

healthData$marital_status <- as.factor(healthData$marital_status)

healthData$smoking  <- as.factor(healthData$smoking)

healthData$hypertensive <- as.factor(healthData$hypertensive)
#
str(healthData)


# ==============================
# # #
#...............................................................................
#--------------------------- Test of Association -------------------------------

# Is gender associated with smokingstatus ?
#  --> table_status
smoke_status <- table( Gender = Sars_3$SEX, Smokingstatus = Sars_3$smoking) 
#
smoke_status <- matrix(c( 13,35, 62, 106), ncol = 2)
#
colnames(smoke_status)<- c("Yes","No")
rownames(smoke_status)<- c("Female","Male")
#
print(smoke_status)
#...............................................................................
#------------------------ Chisquare of Independence .............................
#To add marginal totals
addmargins(smoke_status33,margin = c(1,2))

#To get proportions
prop.table(smoke_status33)

#To get percentages
prop.table(smoke_status)*100
#To round off values
round(prop.table(smoke_status33)*100, 2)
#plot
barplot(prop.table(smoke_status33)*100)
#
pie(table(data$SES), col = c("white","gray90","gray60"))#for 2x3 Table

#----------------- Perform chis-square
chisq.test(table(Sars_3$SEX, Sars_3$smoking))
chisq.test(smoke_status33)    
# --------------- Fisher's Exact Test ....................... for  cell count <2
fisher.test(table(Sars_3$SEX, Sars_3$smoking))

?t.test
# # #
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ---------------------------- Oddratio & riskratio ------------------------------ 
  #                                  ,
  #_______The right order  for using contingency table for interpretation_________for Epitab func.
  Disease                                 
Exposure       No (ref)  Yes                              
Level 1 (ref)  a         b                               
Level 2        c         d    
#________________________________________________________________________________

#------------ Oddratio
oddsratio(Sars_3$SEX, Sars_3$smoking)
# 
#----------- riskratio
riskratio(Sars_3$SEX, Sars_3$smoking)
#                      

#================  
library(epitools)
#------------ Oddratio
OR <-epitab(Sars_3$SEX, Sars_3$smoking, method = "oddsratio",conf.level = 0.95)
OR1<-epitab(Sars_3$SEX, Sars_3$smoking, method = "oddsratio", rev = "columns", conf.level = 0.95)
OR                     
#----------- riskratio                     
RR <-epitab(Sars_3$SEX, Sars_3$smoking, method = "riskratio",conf.level = 0.95)
RR                     




# # #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+                              LOGISTIC REGRESSION                             +
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#A binary variable is a categorical outcome that has two categories or levels. 
#The logistic model (or logit model) is used to model the probability of a particular 
#class/event such as pass or fail, win or lose, alive or dead or healthy or sick. 
Note----the function is glm, y is categorical, x can be (categorical or continuous).
Report logistic regression outcome with Oddratio by taking exponentiation of Estimate. 
Probability is 0  - 1
OddRatio------OR<1,LESS likely to occur / OR > 1 MORE likely to occur
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#------------------------- Simple Logistic Regression ---------------------------
#
library(readxl)
library(readr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(dplyr)

#Import data.............................................
Sars_2 <- read_excel("C:/Users/User/Downloads/Sars-2.xlsx")
str(Sars_2)
#
#--------------------------------------------------------------------------------
Data Conversion  
#--------------------------------------------------------------------------------
#
Sars_2$AgeCategory <- as.factor(Sars_2$AgeCategory)
Sars_2$SEX <- as.factor(Sars_2$SEX)
Sars_2$SarsCov_Strain <- as.factor(Sars_2$SarsCov_Strain)
Sars_2$Hospitalstatus <- as.factor(Sars_2$Hospitalstatus)
Sars_2$categoryofcases <- as.factor(Sars_2$categoryofcases)
Sars_2$TreatmentOUTCOME <- as.factor(Sars_2$TreatmentOUTCOME)
Sars_2$infection <- as.factor(Sars_2$infection)
Sars_2$Resistance <- as.factor(Sars_2$Resistance)
Sars_2$organism <- as.factor(Sars_2$organism)
Sars_2$smoking <- as.factor(Sars_2$smoking)

# revisit the data structure
str(Sars_2)
print(Sars_2)

#
countmissing <- colSums(is.na(Sars_2))
print(countmissing)

sars <- na.omit(Sars_2)
print(sars)

#---------Selecting column 
Sars_LG <- sars %>% select(-Duration_wks)


#filter for Delta (SaraCovstrain)

filter_Delta <- Sars_LG %>% filter(SarsCov_Strain== "Delta")

print(filter_Delta, n= Inf)

dim(filter_Delta)
dim(Sars_LG)

#fitting glm  for Continuous variable
mich_log <- glm(SarsCov_Strain ~ Duration_days, data = Sars_LG, family = "binomial")
mich_log

summary(mich_log)
#confidence intervals
confint(mich_log) 
#Odd ratio
exp(coef(mich_log))

exp(cbind(OR = coef(mich_log), confint(mich_log)))


# Fitting  glm for Categorical variable
michell_logist_categorical1 <- glm(SarsCov_Strain ~ categoryofcases, data = Sars_LG, family = "binomial")
michell_logist_categorical1

summary(michell_logist_categorical1)


table(Sars_LG$SarsCov_Strain, Sars_LG$Resistance)
str(sars)
#Create contingency table of categorical outcome and predictors we want to make sure no 0 cells.
table(imdata$CaseControl,imdata$parity)

#regression model
#----------------when x is continuous ==========================================
logistic <- glm(CaseControl ~ age, data = imdata, family = "binomial" )
summary(logistic)

#log-odds=-1.92264+0.05512*age
exp(0.05512)

# oddratio only
exp(coef(logistic))

#interpret
-----a unit increase in age,the odds of having case is increase by factor 1.06. 
holding other factors constant(e.g multiple logistic regression).

#--------------when x is categorical ===========================================
#
#Note_______in case we want to reorder /change the reference group, Use relevel
change_ref <-relevel(imdata$parity, ref = "1")
#
logist1 <- glm(CaseControl ~ parity, data = imdata, family = "binomial" )
logist1
summary(logist1)
#log-odds=-0.7673+0.6719*(parity=1)+2.1535*(parity=2)+1.8659*(parity=1)-15.7988*(parity=4)
parity0 is used as reference

#odd ratio only
exp(coef(logist1))
parity-1 is 1.96 more likely to have case compared to parity-0
parity-2 is 8.62 more likely to have case compared to parity-0
parity-3 is 6.46 more likely to have case compared to parity-0
parity-4 is 1.37 more likely to have case compared to parity-0
#
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ----------------------- Multiple Logistics regression --------------------------
  #  
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








# ==============================
# MULTICOLLINEARITY
# ==============================

vif(model_full)


vif
# ==============================
# ROC / AUC
# ==============================

predicted <- predict(
  model_full,
  type = "response"
)

roc_curve <- roc(
  data$outcome,
  predicted
)

auc(roc_curve)

plot(
  roc_curve,
  col = "blue",
  main = "ROC Curve"
  
