rm(list=ls())
gc(reset = TRUE)
#--------------------------- required package
install.packages("AICcmodavg")
install.packages("DescTools")
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                          Inferential Statistics                              +
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(tidyverse)
library(psych)
library(car)
library(lessR)
library(FSA)
library(Hmisc)
library(stats)
library(epitools)
library(RColorBrewer)
library(ggfortify)
library(ggpubr)
library(plotrix)
library(XNomial)
library(pwr)
library(BSDA)
library(stats)
library(AICcmodavg)
library(DescTools)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Import data
  SarsCoV <- read_excel("C:/Users/User/Desktop/SarsCoV.xlsx")
  View(SarsCoV)
  print(SarsCoV)
  str(SarsCoV)
  
  --------------------------------------------------------------------------------
                  Data Conversion _(CODING)  
 -------------------------------------------------------------------------------- 
  SarsCoV$AgeCategory <- as.factor(SarsCoV$AgeCategory)
  SarsCoV$SEX <- as.factor(SarsCoV$SEX)
  SarsCoV$SarsCov_Strain <- as.factor(SarsCoV$SarsCov_Strain)
  SarsCoV$Hospitalstatus <- as.factor(SarsCoV$Hospitalstatus)
  SarsCoV$categoryofcases <- as.factor(SarsCoV$categoryofcases)
  SarsCoV$TreatmentOUTCOME <- as.factor(SarsCoV$TreatmentOUTCOME)
  SarsCoV$infection <- as.factor(SarsCoV$infection)
  SarsCoV$Resistance <- as.factor(SarsCoV$Resistance)
  SarsCoV$organism <- as.factor(SarsCoV$organism)
  SarsCoV$smoking <- as.factor(SarsCoV$smoking)
  
  # revisit the data structure
  str(SarsCoV)

  # Explore the data by using descriptive statistics----------------------------

  summary(SarsCoV)
  #further insight into data normality
  describeBy(SarsCoV) 
  # use normality test
  shapiro.test(SarsCoV$Age)
  # Basic plots
  hist(SarsCoV$Age)

  #Transform the data
  SarsCoV$Agee <-log(SarsCoV$Age) 
  
  str(SarsCoV)
  
  hist(SarsCoV$Agee)
  
  shapiro.test(SarsCoV$Agee)
  #
  #Barplot
  plot(SarsCoV$SEX)
  #boxplot
  plot(x= SarsCoV$SEX,y = SarsCoV$Age)
  #scatterplot
  plot(x= SarsCoV$systolic1, y= SarsCoV$sugar_level)
  
  #pie / donut -----depending the input at the hole
  PieChart(categoryofcases, data = SarsCoV, hole = 0.5, main = NULL)
 #
  
+++++++++++++++++ One sample test of Mean ++++++++++++++++++++++++++++++++++++++
#One sample t.test
  
+++++++++++++++++ Two sample test of Mean ++++++++++++++++++++++++++++++++++++++
#Independent t.test
#Paried t.test
    
+++++++++++++++++ More than Two sample (ANOVA) +++++++++++++++++++++++++++++++++    
#One_Way ANOVA (Between group )
#Factorial ANOVA
#Repeated measure ANOVA(Within group)
    
+++++++++++++++++ One sample test of proportions +++++++++++++++++++++++++++++++  
#Chisqure Goodness of Fit
    
+++++++++++++++++ Two sample test of Asociation ++++++++++++++++++++++++++++++++ 
#Chisquare of independence
  
  
  