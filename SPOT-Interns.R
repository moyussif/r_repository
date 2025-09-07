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
library(stats)
library(epitools)
library(VGAM)
library(nnet)

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

=======================================================================================
                                 Ayoola data
______________________________________________________________________________________

cngTB <- read_excel("C:/Users/User/Desktop/MTB only_ Lineage.xlsx")
str(cngTB)

table(cngTB$Lineage,cngTB$AgeCategory)
table(cngTB$Lineage,cngTB$Gender)
table(cngTB$Lineage,cngTB$B5Marital_Status)
# Perform Fisher's Exact Test
fisher.test(cngTB$Lineage,cngTB$Gender, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$AgeCategory, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$B5Marital_Status, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$B6Education, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$occupationClass, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$B10ResidenceClassification, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$B13MonthlyIncome, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$Coughgt2wk, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$Coughphlegm, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$phlegmDuration, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$bloodysputum, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$bloodysputumDuration, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$Fever, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$DurationFever, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$weightloss, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$DurationWeightloss, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$Night_Sweats, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$DurationNightSweat, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$SwollenGlnds, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$DurantionSwollenGlnds, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$Shortnessofbreath, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$DurationShortnessBreath, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$Chestpain, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$DurationChestPain, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$weakness, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$DurationWeakness, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$Recurringchills, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$DuratiionChills, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$Lossofappetite, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$DurationLossAppetite, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$receivedinjections, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$Completetreatment, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$outcomeoftreatment, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$familymembercoughing, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$samehousewithTBpatient, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$Share_Room, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$neighborhood, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$Church, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$Socialize, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$Workplace, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$close_Friend, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$Friend_Similar, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$E1_Haveyoueverworkedinalabthat, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$aresidentinajailpris, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$employeeorvolunteer, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$workedinahospital, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$workedinanursinghome, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$livedinrefugeecamp, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$workinanoffice_Open, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$workinvolvesand, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$workinvolvesmoke, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$manyinthesameroom, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$workwithcattle, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$DiabetesMellitus, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$BodyWeight, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$HIV, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$AIDS, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$Treatmentforrheumatoidarthritis, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$G7_Medicaltreatment_corticosteroids, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$Cancer, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$Kidneydisease, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$Silicosis, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$Leukemia, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$Substanceabuse, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$Smokingcigarette, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$BCG, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$Scanty, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$countSmear, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$Socialize, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$Socialize, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$Socialize, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$Socialize, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$Socialize, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$Socialize, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$Socialize, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$Socialize, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$Socialize, simulate.p.value = TRUE)
# Perform Chi-square Test
chisq.test(cngTB$Lineage,cngTB$AgeCategory, simulate.p.value = TRUE)

==================================================================================
+                            Multinomial regression                              +
==================================================================================  
library(VGAM)

data(cngTB)

# Original factor
lineag <- factor(c("Bovis", "Caprae", "L1", "L2", "L3", "L4","L5","L6"))
# Releveling the factor
lineage <- relevel(lineag, ref = "Bovis")
# Displaying the levels of the releveled factor
levels(lineage)
#fit model
model <- vglm(lineage ~ Gender + AgeCategory + B5Marital_Status + B6Education,B13MonthlyIncome,
              family = multinomial,
              data = cngTB)
summary(model)

all <- exp(cbind(OR = coef(model), confint(model)))
summary(all)

#_____________future predictions____________________
predicted_probs <- predict(model, type = "response")
predicted_classes <- colnames(predicted_probs)[apply(predicted_probs, 1, which.max)]

new_sample <- data.frame(Sepal.Length = 5.1, Sepal.Width = 3.5,
                         Petal.Length = 1.4, Petal.Width = 0.2)

new_pred <- predict(model, newdata = new_sample, type = "response")
print(new_pred)
#______________________________________________________________________ Option 2
library(nnet)
data(cngTB)
# Original factor
lineag <- factor(c("Bovis", "Caprae", "L1", "L2", "L3", "L4","L5","L6"))
# Releveling the factor
lineage <- relevel(lineag, ref = "Bovis")
# Displaying the levels of the releveled factor
levels(lineage)
#fit model
model <- multinom(Lineage ~ Gender
                  + AgeCategory,
                  + B5Marital_Status,
                  + B6Education,
                  data = cngTB)

all <- exp(cbind(OR = coef(model), confint(model)))
summary(all)

# # #

#__________future predictions____________________
new_data <- data.frame(Petal.Length = 1.5,
                       Petal.Width = 0.3, 
                       Sepal.Length = 4.5, 
                       Sepal.Width = 3.1)
predict(model, newdata = new_data, type = "class")

data$outcome_var <- relevel(data$outcome_var, ref = "BaselineCategory")







