
# # #  
#-------------------------------------------------------------------------------
#+++++++++++++++++++++ ONE/TWO SAMPLE OF PROPORTION ++++++++++++++++++++++++++++
gc(reset = TRUE)  
#Load required packages..................................
library(readxl)
library(tidyverse)
library(epitools)
library(XNomial)

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

#---------Selecting column 
Sars_3 <- Sars_2 %>% select(c(SEX, SarsCov_Strain, Hospitalstatus, Resistance, organism, smoking))

str(Sars_3)
#
sars_4 <- Sars_2 %>% select(c(SEX, SarsCov_Strain, Hospitalstatus, Resistance, organism, smoking)) %>% 
  filter(SEX == "Male")
#
print(sars_4)


#
#'''''''''''''''''''' ONE SAMPLE TEST OF PROPORTION ''''''''''
    
#Is the proportion of smoker in male =5.2% ?
p1_table <- table(sars_4$smoking)
p1_table                                    
    
p1_table <- matrix(c(35, 106), ncol = 2) 
 
p1_table11 <- matrix(c(106,35), ncol = 2)

   
#Therefore
prop.test(p1_table, p = 0.052)
    
    
#''''''''''''''''''' TWO SAMPLE TEST OF PROPORTION ''''''''''''

#Get table of sex by smoking status
smoke_status <- table( Gender = Sars_3$SEX, Smokingstatus = Sars_3$smoking)     
smoke_status                                                              #E.g, where column1-13, 35. column2-> 62, 106.
    


smoke_status33 <- matrix(c( 35,13, 106, 62), ncol = 2)

colnames(smoke_status33)<- c("Yes","No")
rownames(smoke_status33)<- c("Male", "Female")
#

#Re-order the columns by list the column-2 first.
smoke_status <- matrix(c( 13,35, 62, 106), ncol = 2)
colnames(smoke_status)<- c("Yes","No")
rownames(smoke_status)<- c("Female","Male")
#
prop.test(smoke_status)
    
#..................... check counts in each cell. 
chisq.test(table_status)$expected
#fisher.test(table_status)
  
  
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
Sars_2$Delta <- as.factor(Sars_2$Delta)
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
 str(Sars_LG)
#---------Selecting column 
Sars_LG <- sars %>% select(-Duration_wks)

#fitting glm  for Continuous variable
mich_log1 <- glm(Delta ~ TreatmentOUTCOME , data = Sars_LG, family = "binomial")
summary(mich_log1)


tryy <-glm(formula = Delta ~ Duration_days, family = "binomial", data = Sars_LG)
 summary(tryy)

summary(mich_log)
#confidence intervals
confint(mich_log) 
#Odd ratio
exp(coef(mich_log))

exp(cbind(OR = coef(mich_log), confint(mich_log)))
     



#===================multiple logistic regression================================

multiple.Logistic <-glm(Delta ~ Duration_days +TreatmentOUTCOME, family = "binomial", data = Sars_LG)


summary(multiple.Logistic)

summary(multiple.Logistic)
 
 exp(coef(tryy))
 
 
 
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


#================================================================================
#===============================================================================

#-------------------------------------------------------------------------------
---------------------------- Model Building ------------------------------------  
  
  # methods for evaluating subset regression models:
1-choose one  with the largest Adjusted R squared.
2-choose one with the smallest MSE.
3-choose one with the smallest AIC.
4-choose one with the smallest predicted sum of square (SS)
5-choose one with the number of the parammetrs used in a model equal to CP value.
Example of Mallow CP ------------------- 4 parameters, 1 intercept = 5CP


str(Sars_LG)
#
install.packages("olsrr")        
library(olsrr)        

ggg <-lm(systolic1 ~ Age+AgeCategory+cholesterol+sugar_level+smoking+exercise_score_hrs, data = Sars_LG)
 summary(ggg)


g <- lm(y~.,data = dataset)       # where . is the x variables
summary(g)
g <- lm(bmi~age+parity+hb,data = imdata)

#--------model building
forward <- ols_step_forward_p(ggg, penter = 0.05)
forward                                               # forward
forward <- ols_step_forward_aic(ggg, details = TRUE)
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
all <- ols_step_all_possible(ggg)
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


# # #
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  +                              LOGISTIC REGRESSION                             +
  ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #A binary variable is a categorical outcome that has two categories or levels. 
  #The logistic model (or logit model) is used to model the probability of a particular 
  #class/event such as pass or fail, win or lose, alive or dead or healthy or sick. 
  Note----the function is glm, y is categorical, x can be (categorical or continuous).
Report logistic regression outcome with Oddratio by taking exponentiation of Estimate. 
Probability is 0  - 1
OddRatio------OR<1,LESS likely to occur / OR > 1 MORE likely to occur

#--------------------------- Simple Logistic Regression 
library(readxl)
library(readr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(dplyr)
setwd("C:/Users/User/OneDrive - University of Ghana/myComputer@space/repos")
ARMdata <- read_csv("Untitled2.csv")
print(ARMdata)
str(ARMdata)
#Convert categorical variable to factors
ARMdata$ParasitePresence <-as.factor(ARMdata$ParasitePresence)
ARMdata$AgeGroup <-as.factor(ARMdata$AgeGroup)
ARMdata$Gender <-as.factor(ARMdata$Gender)
ARMdata$study_site <-as.factor(ARMdata$study_site)
str(ARMdata)

multi_logist <- glm(ParasitePresence ~ Gender + AgeGroup + study_site, data = ARMdata, family = "binomial" )
summary(multi_logist)
confint(multi_logist)
exp(coef(multi_logist))
exp(cbind(OR = coef(multi_logist), confint(multi_logist)))

#Create contingency table of categorical outcome and predictors we want to make sure no 0 cells.
table(imdata$CaseControl,imdata$parity)

#regression model
#------when x is continuous===========================================
logistic <- glm(CaseControl ~ age, data = imdata, family = "binomial" )
summary(logistic)

#log-odds=-1.92264+0.05512*age
exp(0.05512)

# oddratio only
exp(coef(logistic))

#interpret
-----a unit increase in age,the odds of having case is increase by factor 1.06. 
holding other factors constant(e.g multiple logistic regression).

#------when x is categorical============================================
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
















