rm(list=ls())
gc(reset = TRUE)
#--------------------------- required packages
library(readxl)
library(readr)
library(tidyverse)
library(RColorBrewer)
library(ggpubr)
library(psych)
library(car)
library(lessR)
library(plotrix)
library(FSA)
library(Hmisc)


#---------------------------- Import functions 
#set directorate 
setwd("C:/Users/User/Desktop")
ghsdata <- read_excel("yussif.xlsx")


bosco <- read_excel("C:/Users/User/Desktop/yussif.xlsx")

str(bosco)

View(bosco)

print(bosco)

#  #  #--------------------- Working with dplyr
# Remove the 'Apgar1min' column

judith <- bosco %>% select(-Apgar1min)
print(ghsdata1)
#Remove multiple columns
ghsdata1 <- ghsdata1 %>% select(-c(Age, Gender))
#  #  #
#--------------------------- Data Conversion 

str(judith)

judith$Age <-as.integer(judith$Age)

judith$Gravidity <- as.integer(judith$Gravidity)
judith$Parity <- as.integer(judith$Parity)

judith$Status <-factor(judith$Status,
                                  levels = c(1,2),
                                  labels = c("control", "case"))

judith$Babysex <-factor(judith$Babysex,
                          levels = c(1,2),
                          labels = c("Male", "Female"))

judith$ModeOfDelivery <-factor(judith$ModeOfDelivery,
                        levels = c(1,2),
                        labels = c("Vaginal", "Caesarean"))

judith$bld_grp <-factor(judith$bld_grp,
                            levels = c(1,2,3,4),
                            labels = c("A positive", "B positive", "AB positive", "O positive"))
str(judith)
print(judith)
#   #   #


#---------------------------- Explore data                                   
#Missing Data Summary
summary(judith)

# Count missing values in each column
missing_per_column <- colSums(is.na(judith))
print(missing_per_column)

# Remove rows with any missing values 
judith <- na.omit(judith)
print(judith)

judith <- colSums(is.na(judith))
print(judith)

#  #  # 
#--------------------- Data Transformation 

library(psych)
library(readxl)

skew(ghsdata1$Age, na.rm = TRUE)
kurtosi(ghsdata1$Age, na.rm = TRUE)


lg_data <-log(imdata$systol1)
hist(lg_data)

#-----------------Confidence interval of the mean
t.test(ghsdata1$Age,
       conf.level=0.95)         

#   #   #
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                            Descriptive statistics                            

library(psych)
summary(judith)
describe(judith)
describe(judith$Age)

summary(imdata$bwgt)
describe(imdata$bwgt)

Histogram
hist(ghsdata1$Age, col="gray", main="NCD for pregnant women", xlab="Birthweight")

#   #   #

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                        TEST OF ONE / TWO SAMPLES                                                
  
library(psych)
library(readxl) 
library(lessR)

describe(judith$Age)

amdata <- read_excel("ARMdata.xlsx")
print(amdata)  
describe(amdata)  
#-----------------One sample t-test
observed    = judith$Age
theoretical = 29
hist(judith$Age, col="gray", main="Histogram of Age", xlab="Age")

t.test(observed, mu = theoretical, conf.int=0.95) 

#------------------Two Samples Student’s t–test 
#paired t.test  
t.test(Age ~ Status, data=judith, var.equal=TRUE, conf.level=0.95)
#independent t.test
t.test(Gravidity ~ Babysex, data=judith, var.equal=FALSE, conf.level=0.95)

# Histogram
hs<-histogram(~ age | CaseControl, col="gray",data = imdata)
hs
# Boxplot
boxplot(age ~ CaseControl, data = imdata, names=c("Case","Control"), ylab="age")

#------------- Mann–Whitney Test
wilcox.test(Value ~ Group, data=Data, exact = FALSE)
wilcox.test(Value ~ Group, data=Data)
#Box plots
boxplot(Value ~ Group, data = Data, names=c("2 pm","5 pm"), ylab="Value")

boxplot(estimated_parasitemia ~ age, data=imdata, names=c("Control","Case"), ylab="Value")

#  #  #
wilcox.test(Data$August, Data$November, paired=TRUE)
#Simple 1-to-1 plot of values
plot(Data$August, Data$November, pch = 16, xlab="August", ylab="November")
abline(0,1, col="blue", lwd=2)  

#   #   #

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#------------------------- One-way Anova using LessR -------------------option.1
#visualise statistical assumptions
library(lessR)
library(readxl)
imdata <- read_excel("ARMdata.xlsx")
print(imdata)
#normality assumption
tapply(imdata$estimated_parasitemia, imdata$age, shapiro.test)
library(car)
leveneTest(estimated_parasitemia ~ age, data=imdata)
#One-way ANOVA
ANOVA(estimated_parasitemia ~ site_name, data=imdata)
#effect size(for groups with significant)
library(effsize)
cohen.d(age ~ expose, data=subset(imdata, expose!= "non exposed"), paired=FALSE)        
cohen.d(age ~ expose, data=subset(imdata, expose!= "singleexposed"),paired=FALSE)        
#Bar charts 
age_means <- tapply(imdata$estimated_parasitemia, imdata$age,mean)

BarChart(age_means)
BarChart(age_means, values="off", bxlab = "Malaria_exposed", ylab = "Women Age")

#   #    #

  
#------------------------------- Correlation -------------------------------
  
#Pearson correlation (Parametric)
cor.test( ~ age + bmi, data=imdata, method = "pearson", conf.level = 0.95)

cor.test( ~ age + bmi, data=imdata, method = "spearman", continuity = FALSE, conf.level = 0.95)
  
  #   #   #
  
  
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                           Regression analysis                                +

  model = lm(Species ~ Latitude, data = Data)
  summary(model)                    
  
  #    #    #   
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                           LOGISTIC REGRESSION                                +

#--------------------------- Simple Logistic Regression 
logistic <- glm(CaseControl ~ age, data = imdata, family = "binomial" )
summary(logistic)
exp(0.05512)
exp(coef(logistic))

#----------------------- Multiple Logistics regression 
multi_logist <- glm(CaseControl ~ age + bmi + parity, data = imdata, family = "binomial" )
summary(multi_logist)


#   #   #
  
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                          Chisquare                                           +
  
imdata <- read_excel("immunoData.xlsx")
#chisquare
table(imdata$CaseControl,imdata$sex)
chisq.test(imdata$CaseControl,imdata$sex)

  