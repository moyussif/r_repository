#
rm(list=ls())
gc(reset = TRUE)
#--------------------------- required package ----------------------------------
#Install packages (run once)
#install.packages(c("readxl", "tidyverse", "lubridate","psych", "car", "lessR", 
#                   "epitools","stats", "Hmisc", "FSA"))
#
#---------------------- Load required packages
library(readxl)
library(readr)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(ggpubr)
library(psych)
library(car)
library(lessR)
library(plotrix)
library(ggfortify)
library(FSA)
library(Hmisc)
library(stats)
library(epitools)
#
#------------------Import data------------
covid_19 <- read_excel("C:/Users/User/Downloads/covid-19.xlsx")
 
View(covid_19) 
print(covid_19, width = Inf)
print(covid_19, width = Inf, n= Inf)

#----data structures

str(covid_19)

#
#-------------------------------------------------------------------------------
                          Create Age group
#-------------------------------------------------------------------------------
#
covdata = covid_19 %>% mutate(age_group = case_when(
  Age  < 5 ~ "0-4",
  Age  < 11 ~ "5-10",
  Age  < 18 ~ "11-17",
  Age  < 35 ~ "18-34",
  Age  < 50 ~ "35-49",
  Age  < 65 ~ "50-64",
    TRUE ~ "65+"
  ))

str(covdata)
print(covdata)
print(covdata, width = Inf, n = Inf)

describe(covdata) #categorical

describeBy(covdata)#Continuous

str(covdata)
#-------------------------------------------------------------------------------
                    Data Conversion _(CODING)  
#-------------------------------------------------------------------------------
#
covdata$age_group <- as.factor(covdata$age_group)

covdata$SEX <- as.factor(covdata$SEX)

covdata$SarsCovStrain <- as.factor(covdata$SarsCovStrain)

covdata$Hospitalstatus <- as.factor(covdata$Hospitalstatus)

covdata$categoryofcases <- as.factor(covdata$categoryofcases)

covdata$Treatment_Outcome <- as.factor(covdata$Treatment_Outcome)

covdata$Asymptomatic <- as.factor(covdata$Asymptomatic)

covdata$Diabetes  <- as.factor(covdata$Diabetes)

covdata$HIV <- as.factor(covdata$HIV)

covdata$malaria <- as.factor(covdata$malaria)

covdata$Hypertension  <- as.factor(covdata$Hypertension)

covdata$Coinfection <- as.factor(covdata$Coinfection)

covdata$Numberoforganism <- as.factor(covdata$Numberoforganism)

covdata$NoofResistance  <- as.factor(covdata$NoofResistance)

covdata$CovidStatus <- as.factor(covdata$CovidStatus)
#
str(covdata)

# # #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 #             Transform and Manipulate Data using Dplyr 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(dplyr)                                                 
#---------Selecting column 
columns_needed <- covdata %>% select(c(Age, age_group, SEX, CovidStatus, SarsCovStrain,Hospitalstatus,Duration_days,categoryofcases,
                                       Treatment_Outcome,Asymptomatic, Noofsymptoms,Diabetes,HIV, malaria, Hypertension, Coinfection,
                                       NoofResistance))
str(columns_needed)
#----------Remove a column
covdata1<- columns_needed %>% select(-Asymptomatic)
str(covdata1)

print(covdata1)

#---------filter rows ?filter()
filterData <- covdata1 %>%
  filter(Age > 16) 
#                  or   filter(hhdata2, age <= 30)

print(filterData)
 
str(filterData)
#---------rename a column
names(filterData)[2]<- "Agecategory"
names(filterData)[3]<- "Gender"
names(filterData)[16]<- "Resistance"


str(filterData)

#---------summarise()
filterData %>% count(categoryofcases)

michelle <- filterData %>% summarise(mean_Age=mean(Age))
print(michelle)

michelle2 <- filterData %>% summarise(median_Age=median(Age), mean_Age=mean(Age), IQR_Age=IQR(Age))
print(michelle2)

#---------group_by()
AverageAgebyGender <-filterData %>% group_by(Gender) %>% summarise(mean = mean(Age))
print(AverageWeightbyGender)

#--------Arrange()
filterData %>% 
  arrange(Duration_days)

#--------Sorting descending order
filterData %>% 
  arrange(desc( Duration_days)) 

# # #
--------------------------------------------------------------------------------
                          HANDLING MISSING DATA
--------------------------------------------------------------------------------
# Count missing values in each column
countmissing <- colSums(is.na(filterData))
print(countmissing)

# Remove rows with any missing values 
cleanData<- na.omit(filterData)
print(cleanData)

# Verify for missings
Check_Clean_data <- colSums(is.na(cleanData))
print(Check_Clean_data)

# # #
#--------------------------------------------------------------------------------
 #                  Descriptive statistics- (Explore data)            
#--------------------------------------------------------------------------------
library(gtsummary)
library(flextable)
library(officer)

Descriptive1 <- cleanData %>%
  select(Age, Agecategory, Gender, CovidStatus, SarsCovStrain,Hospitalstatus,Duration_days,categoryofcases,
         Treatment_Outcome,Diabetes,HIV, malaria, Hypertension, Coinfection,
         Resistance) %>%
  tbl_summary(by = Hospitalstatus,
              statistic = list(all_continuous() ~ "{median} ({p25}, {p75})",
                               all_categorical() ~ "{n} ({p}%)"),
              digits = all_continuous() ~ 1,missing = "ifany") %>%
  add_overall() %>% 
  bold_labels()

Descriptive1 
#
gtsummary::as_flex_table(Descriptive1) %>%
  flextable::save_as_docx(path = "Descriptive1.docx")

# Basic plots
hist(cleanData$Age)

#Normality by Descriptive statistic
describeBy(cleanData$Age)
describeBy(cleanData$Duration_days)
describeBy(cleanData$Noofsymptoms)

# use statistical test for normality
shapiro.test(cleanData$Age)

--------------------------------------------------------------------------------
Transform the data
Square root Transformation#<-sqrt(SarsCoV$Age)------------------moderately skewed (-1 to -0.5//0.5 to 1)
Cube root Transformation#  <-sign(Hanisah$Baby_Weight)*abs(Hanisah$Baby_Weight)^(1/3)  moderately Right skewed(Negative/Zero)
Log Transformation#        <-log(SarsCoV$Age)--------------Highly skewed(above1),But not (Negative//Zero)
--------------------------------------------------------------------------------
cleanData$Agee <-log(cleanData$Age) 
str(cleanData)
hist(cleanData$Agee)
shapiro.test(cleanData$Agee)

--------------------------------------------------------------------------------
                    Data Visualization
--------------------------------------------------------------------------------
#
#--------------------------Basic Plots------------------------------------------Option.1
#Barplot
plot(cleanData$Gender)
#pie
pie(table(cleanData$Resistance))
pie(table(cleanData$Resistance), col = c("white","gray90","gray60"))
#pie / donut -----depending the input at the hole
PieChart(categoryofcases, data = cleanData, hole = 0.5, main = NULL)
#boxplot
plot(x= cleanData$Gender,y = cleanData$Age)
#scatterplot
plot(x= cleanData$Duration_days, y= cleanData$Noofsymptoms)
hist(cleanData$Agee)

# # #
#-------------------------- charts with LessR ----------------------------------Option.2
library(lessR)
#
# Piechart
PieChart(categoryofcases, data = cleanData, hole = 0.5, main = NULL)

# Donut chart
PieChart(Resistance, data = cleanData, fill = "blues", hole_fill = "#B7E3E0", main = NULL)

# Barchart
BarChart(Agecategory, data = cleanData, fill = "blues", main = NULL)

#colorviridis
BarChart(Agecategory, data = cleanData, fill = "viridis", main = NULL, color = "black",lwd = 1.5,
         values_color = c(rep("white", 4), 1), values_size = 0.85)

# slant x labels (45 angle)
BarChart(Agecategory, data = cleanData, fill = "viridis", main = NULL, color = "black",lwd = 1.5,
         rotate_x=45, values_color = c(rep("white", 4), 1), values_size = 0.85)

#Histogram
hist(cleanData$Agee, col="gray", main="covid data", xlab="age")
hist(cleanData$Duration_days, col= "turquoise", main="Covid data", xlab="Days")


# Boxplot
boxplot(Age ~ Gender, data = cleanData, col= "turquoise")


# # #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+                          Inferential Statistics                              +
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

#---------------------------One sample test of Mean
#One sample t.test
  
theorical = 35
observed = cleanData$Age

t.test(observed,mu=theorical, conf.int=95.0 )# Option1 
#
t.test(cleanData$Age,mu=36, conf.int=95.0 )  #Option 2
hist(cleanData$Age)  

# # #
#++++++++++++++++++++++++++ Two sample test of Mean ++++++++++++++++++++++++++++
#
#Independent t.test'''''''''''''''''''''(PARAMETRIC)
t.test(Age ~ Gender, data = cleanData, Var.equal =FALSE, conf.level = 0.95)  

#Mann–Whitney Test''''''''''''''''''''''(NON-PARAMETRIC)
wilcox.test(Age ~ Gender, data = cleanData, exact = FALSE)
#
#
hist(Agee ~ Gender, data = cleanData, names=c("Female","Male"), ylab="Age")  
#
boxplot(Age ~ Gender, data = cleanData, names=c("Female","Male"), ylab="Age")
#_____________________________________________________________________________

# # #
#Paried t.test''''''''''''''''''''''''''(PARAMETRIC)
t.test(Sars_2$systolic1,Sars_2$systolic2,paired = TRUE,conf.level = 0.95) 
#
#Wilcoxon Signed-rank Test''''''''''''''(NON-PARAMETRIC)
wilcox.test(Data$August, Data$November, paired=TRUE)

# # #
#++++++++++++++++++++++ More than Two sample (ANOVA) +++++++++++++++++++++++++++    
library(AICcmodavg) 
library(DescTools)  
#
#-------------------- One_Way ANOVA (Between group )----------------------------# LessR
#
Plot(Age, data=cleanData, facet1 = Resistance)
#normality 
tapply(cleanData$Age, cleanData$Resistance,shapiro.test)

#equality of variance
leveneTest(Age ~ Resistance, data=cleanData)

#One-way ANOVA
ANOVA(Age ~ Resistance, data=cleanData)

#Bar charts 
Resist_mean <- tapply(cleanData$Age, cleanData$Resistance,mean)
BarChart(Resist_mean)
    
#

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





#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#--------------------------------- Correlation ----------------------------------
#
#Pearson correlation (Parametric--residuals are normally distributed)
#
cor.test( ~ age + bmi, data=imdata, method = "pearson", conf.level = 0.95)

##
library(ggpubr)
#CorrelationPlot----------ggscatter() 
Hanisah90 <- read_excel("C:/Users/User/Desktop/covid02.xlsx")
str(Hanisah90)
    
ggscatter(Hanisah90, x = "Noofsymptoms", y = "Durationdays",
          add = "reg.line",                                 # Add regression line
          conf.int = TRUE,                                  # Add confidence interval
          add.params = list(color = "blue",
                            fill = "lightgray"))+
  stat_cor(method = "pearson", label.x = 3, label.y = 30)   # Add correlation coefficient   
    
#Spearman correlation (Non-parametric / ordinals)
#
cor.test( ~ age + bmi, data=imdata, method = "spearman", continuity = FALSE, conf.level = 0.95)
    
# # #