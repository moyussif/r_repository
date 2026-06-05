rm(list=ls())
gc(reset = TRUE)
#--------------------------- required package
install.packages("readxl")
install.packages("readr")
install.packages("tidyverse")
install.packages("RColorBrewer")
install.packages("ggpubr")
install.packages("psych")
install.packages("car")
install.packages("lessR")
install.packages("plotrix")
install.packages("ggfortify")
install.packages("epitools")
install.packages("FSA")
install.packages("PMCMRplus")

#-------------------------- Load packages
library(readxl)
library(readr)
library(writexl)
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
library(PMCMRplus)

#--------------------------- set directorate -----------------------------------Option.1
setwd("C:/Users/User/Desktop")
mpData <- read_excel("mparasite.xlsx")

covid01 <- read_csv("covid01.csv") 

#-------------------------- Import functions                                    Option.2
mparasite <- read_excel("C:/Users/User/Desktop/mparasite.xlsx") #.xlsx format

str(mparasite)

covid02 <- read.csv("C:/Users/User/Desktop/MMS/covid01.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE) #.csv format

str(covid02)

#--------------------------- EXport function  
library(writexl)
write_xlsx(mparasite, "falciparum.xlsx")


--------------------------------------------------------------------------------
                        Understand Data Structures                            
--------------------------------------------------------------------------------  
  str(mparasite)   #data structure
print(mparasite) #view data in console


-------------------------------------------------------------------------------
                       DATA MANIPULATION (Dplyr) 
-------------------------------------------------------------------------------
  library(dplyr)
#---------Selecting column
columns_needed <- covid02 %>% select(c(Age,AgeCategory,SEX,SarsCovStrain,Hospitalstatus,Durationdays,Durationweeks,
                                       categoryofcases,TreatmentOUTCOME,Noofsymptoms,Coinfection,NoofResistance,Numberoforganism))
str(columns_needed)
#----------Remove a column
remove_one <- columns_needed %>% select(-Numberoforganism)
str(remove_one)

#--Remove multiple columns
remove_more <- remove_one %>% select(-c(Coinfection,NoofResistance))
str(remove_more)

#---------filter rows ?filter()
Hanisah <- remove_more %>% select(c(Age,AgeCategory,SEX,SarsCovStrain,Hospitalstatus,Durationdays,Durationweeks,
                                    categoryofcases,TreatmentOUTCOME,Noofsymptoms)) %>% 
  filter(Age < 30)

#---------Create New column (mutate)
mutate_colmn <- rename_colmns %>% mutate(bmi=weight/height*2)

#---------show only New column (transmute)
transmutate_col <- mutate_colmn %>% transmute(bmi)

print(Hanisah)
str(Hanisah)

#---------Change character type(rename_with)
Upper_caps <- rename_colmns %>% rename_with(Mother_group,toupper)
Lower_caps <- rename_colmns %>% rename_with(DeliveryMode,tolower)

#---------rename a column
#Data %>% rename(NewColumn=OldColumn)

names(Hanisah)[1]<- "mum_Age"
names(Hanisah)[4]<- "Delivery_mode"
names(Hanisah)[6]<- "Baby_Age"
names(Hanisah)[7]<- "Baby_Gest_Age"
str(Hanisah)

#---------summarise()
Hanisah1 <- Hanisah %>% summarise(sum_MotherAge=sum(Motherage))
print(Hanisah1)
Hanisah2 <- Hanisah %>% summarise(mean_Age=mean(Motherage))
print(Hanisah2)
Hanisah3 <- Hanisah %>% summarise(sum_BabyWt=sum(Baby_age_days),Mean_BabyWt=mean(Baby_age_days))
print(Hanisah3)

#---------group_by()
AverageWeightbyGender <-Hanisah %>% group_by(Baby_sex) %>% summarise(mean = mean(Baby_Weight))
print(AverageWeightbyGender)

# # #
--------------------------------------------------------------------------------
                  Reshaping Data -(LONG.data / WIDE.data) 
--------------------------------------------------------------------------------
library(tidyr) 
print(HTdata)
# use gather()function to make data longer
long <- HTdata %>%  
  gather(fullName, Frequency, 
         first_trimester, second_trimester, third_trimester)
print(long)
# use separate()function 
separate_data <- long %>%  
  separate(fullName, c("firstName","secondName"))
separate_data
# use unite() function 
unite_data <- separate_data %>%  
  unite(fullName, firstName, secondName, sep = " ") 
unite_data

# # #
--------------------------------------------------------------------------------
                      HANDLING  DUPLICATE DATA
--------------------------------------------------------------------------------
BF_data = data.frame#Data
#
duplicated(BF_data)#base functions
sum(duplicated(BF_data))  
#
unique(BF_data) #base functions
#
distinct(BF_data)#Dplyr package  

# # #
--------------------------------------------------------------------------------
                      HANDLING MISSING DATA
--------------------------------------------------------------------------------
# Count missing values in each column
missing_per_column <- colSums(is.na(Hanisah))
print(missing_per_column)
# Remove rows with any missing values 
Cleaned_Hanisah <- na.omit(Hanisah)
print(Cleaned_Hanisah)
#
Check_Clean_Hanisah <- colSums(is.na(Cleaned_Hanisah))
print(Check_Clean_Hanisah)

# # #
--------------------------------------------------------------------------------
                     Data Conversion _(CODING)                          Option 1
--------------------------------------------------------------------------------
str(Hanisah)
#Date to time series
tdata$Date = as.Date(tdata$Date, format = "%Y/%m/%d")
hhdata = ts(tdata$attendance,start = min(tdata$Date), end = max(tdata$Date),frequency = 1)

#-------Continuous data
Hanisah$mum_Age <-as.integer(Hanisah$mum_Age)
Hanisah$Baby_Age <-as.integer(Hanisah$Baby_Age)
Hanisah$Baby_Weight <-as.integer(Hanisah$Baby_Weight)

salarylevel <-if else(data$salary < 5000,c("Low"),c("High"))
SES <- as.factor(data$salary)

#-------Categorical data
Hanisah$Baby_sex <-factor(Hanisah$Baby_sex,
                          levels = c(0,1),
                          labels = c("Female", "Male"))

Hanisah$Baby_bloodgroup <-factor(Hanisah$Baby_bloodgroup,
                                 levels = c(0,1,2,3,4,5,6,7),
                                 labels = c("Notdone", "A-","A+", "B-","B+","AB+", "AB-","O+"))

Hanisah$Baby_G6PD <-factor(Hanisah$Baby_G6PD,
                           levels = c(0,1,2,3),
                           labels = c("No defect","Full defect","Partial defect","Not done"))


Hanisah$Delivery_mode <-factor(Hanisah$Delivery_mode,
                               levels = c(0,1,2),
                               labels = c("Vaginal", "Caesarean", "Assisted"))

Hanisah$Baby_Gest_Age <-factor(Hanisah$Baby_Gest_Age,
                               levels = c(0,1),
                               labels = c("Pretern", "Fullterm"))


str(Hanisah)
print(Hanisah)
# # #
--------------------------------------------------------------------------------
                         Reverse Coding                                 Option 2
--------------------------------------------------------------------------------
  
  Hanisah77$SarsCovStrain <-factor(Hanisah77$SarsCovStrain,
                                   levels = c("delta", "omicron"),
                                   labels = c(1, 2))

Hanisah77$categoryofcases <-factor(Hanisah77$SarsCovStrain,
                                   levels = c("mild", "moderate", "severe"),
                                   labels = c(1,2,3))

# # #
--------------------------------------------------------------------------------
                     CHECKING FOR DATA NORMALITY           
--------------------------------------------------------------------------------
#POS_skewed = Mean > Median/Mode         NEG_skewed = Mean < Median/Mode        LOW  = Q1-1.5(IQR)        
#                                                                               HIGH = Q3+1.5(IQR)
library(psych)
skew(Hanisah30$mum_Age)
kurtosi(Hanisah30$Baby_Age, na.rm = TRUE)
mardia(Hanisah30$Baby_Age)
describeBy(Hanisah30)

#  #  #
--------------------------------------------------------------------------------
                     Transforming skewed data
--------------------------------------------------------------------------------
#Square root Transformation---------------------------------------moderately skewed (-1 to -0.5//0.5 to 1)
sq_data <-sqrt(Hanisah$Baby_Weight)
hist(sq_data)
#Cube root Transformation-----------------------------------------moderately Right skewed(Negative//Zero)
cb_data <-sign(Hanisah$Baby_Weight)*abs(Hanisah$Baby_Weight)^(1/3)
hist(cb_data)
#Log Transformation---------------------------------------Highly skewed(above1),But not (Negative//Zero)
Hanisah30$lg_mum_age <-log(Hanisah30$mum_Age)
hist(lg_data)

# # #
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                        
                            Descriptive statistics
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(psych)
describeBy(Hanisah30)
library(skimr)
str(Hanisah30)
skim_without_charts(Hanisah30)

#=========================== CONFIDENCE INTERVALS ==============================

#-----------------Confidence interval of the mean
t.test(Hanisah$Baby_Weight,
       conf.level=0.95)       

#-----------------confidence intervals for groups defined by a variable
install.packages("Rmisc")
library(Rmisc)
summarySE(data=covid01, measurevar="Age", groupvars="AgeCategory", conf.interval = 0.95)

#   #   #

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                            Data Visualizations
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#-------------------------- #charts with LessR --------------------------Option.1
library(lessR)

covid01 <- read_excel("C:/Users/User/Desktop/covid01.xlsx")

# Piechart
PieChart(AgeCategory, data = covid01, hole = 0, main = NULL)
# Donut chart
PieChart(AgeCategory, data = covid01, fill = "blues", hole_fill = "#B7E3E0", main = NULL)

# Barchart
BarChart(AgeCategory, data = covid01, fill = "blues", main = NULL)

#colorviridis
BarChart(AgeCategory, data = covid01, fill = "viridis", main = NULL, color = "black",lwd = 1.5,
         values_color = c(rep("white", 4), 1), values_size = 0.85)
# slant x labels (45 angle)
BarChart(AgeCategory, data = covid01, fill = "viridis", main = NULL, color = "black",lwd = 1.5,
         rotate_x=45, values_color = c(rep("white", 4), 1), values_size = 0.85)

#Histogram
hist(covid01$Age, col="gray", main="covid data", xlab="age")
hist(covid01$Age, col= "turquoise", main="Covid data", xlab="Age")
summary(covid01$Age)

# Boxplot
boxplot(Age ~ SEX, data = covid01)

# # #
#------------------------- Using GGPLOT2 ---------------------------------Option.2
library(ggplot2)

#============================= Barchart  =======================================
ht <-ggplot(data = covid01,
            mapping = aes(x = AgeCategory))+
  geom_bar()+theme_classic()
ht
                                                                                #covid01$SEX <- factor(covid01$SEX)
#Stacked Barchart
ht2 <-ggplot(data = covid01,
             mapping = aes(x = AgeCategory, colour = SEX, fill = SEX))+
  geom_bar()+
  theme_classic()
ht2

#Clustered Barchart
ht3 <-ggplot(covid01,
             mapping = aes(x = AgeCategory, y = Age,  fill = SEX)) +
  geom_col(position = "dodge") +
  theme_minimal()
ht3

#flip axis
ht4 <-ggplot(covid01,
             mapping = aes(x = AgeCategory, y = Age,  fill = SEX)) +
  geom_col(position = "dodge") +
  coord_flip()+
  theme_minimal()
ht4

#=========================== Histogram =========================================

ht5 <-ggplot(data = covid01,
             mapping = aes(x = Age))+
  geom_histogram()

ht5

#Histogram with stacked
ht6 <-ggplot(data = covid01,
            mapping = aes(x = Age, fill = SEX))+
  geom_histogram(bins = 10, position = "stack")

ht6

#Histogram with dodge
ht7 <-ggplot(data = covid01,
            mapping = aes(x = Age, fill = SEX))+
  geom_histogram(bins = 10, position = "dodge")

ht7
#flip axis
ht8 <-ggplot(data = covid01,
             mapping = aes(x = Age, fill = SEX))+
  geom_histogram(bins = 10, position = "dodge")+
  coord_flip()

ht8

#============================= Boxplot =========================================
imm <- ggplot(data = covid01,
               mapping = aes(x = SEX, y = Age))+
  geom_boxplot()
imm
# Boxplot by category ##
immu <- ggplot(data = covid01,
               mapping = aes(x = SEX, y = Age, fill = SEX))+
  geom_boxplot()
immu

#Boxplot with coord_flip
immp <- ggplot(data = covid01,
               mapping = aes(x = SEX, y = Age, fill = SEX))+
  geom_boxplot()+coord_flip()
immp 

#plotting by factor
imu <- ggplot(data = covid01,
               mapping = aes(x = SEX, y = Age))+
  geom_boxplot(aes(fill = factor(SarsCovStrain)))

imu

#Adding labels
imml <- ggplot(data = covid01,
              mapping = aes(x = SEX, y = Age))+
  geom_boxplot(aes(fill = factor(SarsCovStrain)))+
  coord_flip()+
  labs(title ="Immunoassay Data", subtitle = "ImmunoAnalysis",caption = "Data collection by Mohammed")+
  annotate( "text",x = 2,y = 70, label ="covid-19 patients", color ="purple",
            fontface ="bold", size =4.0,angle = 30)
imml

                                                                               # covid01$categoryofcases <-factor(covid01$categoryofcases)
# Facet wrap 
imwrp <- ggplot(data = covid01,
                 mapping = aes(x = SEX, y = Age, fill = categoryofcases))+
  geom_violin()+
  facet_wrap(~categoryofcases)
imwrp

# Facet grid
imwgg <- ggplot(data = covid01,
                mapping = aes(x = SEX, y = Age, fill = categoryofcases))+
  geom_violin()+
  facet_grid(~categoryofcases)+
  labs(title ="Covid Data", subtitle = "ImmunoAnalysis",caption = "Data collection by Mohammed")+
  annotate( "text",x = 2,y = 70, label ="Covid patients", color ="purple",
            fontface ="bold", size =4.0,angle = 30)
imwgg

#============================ Combo plots ======================================
library(RColorBrewer)
library(ggpubr)
par()


p <-imu <- ggplot(data = covid01,
                  mapping = aes(x = SEX, y = Age))+
  geom_boxplot(aes(fill = factor(SarsCovStrain)))+
  coord_flip()+
  labs(title ="Immunoassay Data", subtitle = "ImmunoAnalysis",caption = "Data collection by Mohammed")+
  annotate( "text",x = 2,y = 70, label ="covid-19 patients", color ="purple",
            fontface ="bold", size =4.0,angle = 30)
p


p1 <-ggplot(data = covid01,
             mapping = aes(x = AgeCategory, colour = SEX, fill = SEX))+
  geom_bar()+
  theme_classic()
p1


p2 <-ggplot(data = covid01,
             mapping = aes(x = Age, fill = SEX))+
  geom_histogram(bins = 10, position = "dodge")+
  coord_flip()
p2

#to arrange plot for publication
ggarrange(p, p1, p2 + rremove("x.text"), labels = c("A", "B", "c"), ncol = 1, nrow = 3,
          common.legend = TRUE, legend = "bottom")

# Save plot 
ggsave("p.png")


# # #
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                        TEST OF ONE / TWO SAMPLES                             +
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
library(psych)
library(readxl) 
library(lessR)

--------------------------------------------------------------------------------
                    TEST OF NOMINAL/CATEGORICAL VARIABLE 
--------------------------------------------------------------------------------
  
Follow up with post hoc tests (optional for Chi-Square)if there are more than two groups in 
either of the variables with p<0.05, a post hoc test is conducted.
using bonferroni correction----/------post hoc chi square of independence.

#----------------------------------------------------------------------------
1----Exact Goodness-of-fit--------------#whether is difference to hypothised value
2----Chi-Square Goodness-of -fit--------#Differecnes between observed and expected value
3----Chi-Square test of independence----#Test of association
4----Fisher exact test------------- N < 100
5----McNemars test.---------------------#TO compare before and after observations
6----G test-----------------------------#Can accommodate experimental design
  
  
#  #  #  
  
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                         Chisquare / Fisher Exact                             +
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
library(readxl)
library(tidyverse)
library(stats)

table(cngTB$Lineage,cngTB$AgeCategory)
# Perform Fisher's Exact Test
fisher.test(cngTB$Lineage,cngTB$AgeCategory, simulate.p.value = TRUE)
# Perform Chi-square Test
chisq.test(cngTB$Lineage,cngTB$AgeCategory, simulate.p.value = TRUE)    

================================================================================
+                         ODDRATIO - RISKRATIO                                  +
================================================================================
library(epitools)
setwd("C:/Users/User/OneDrive - University of Ghana/myComputer@space/repos")
imdata <- read_excel("immunoData.xlsx")
#chisquare
table(imdata$CaseControl,imdata$sex)
chisq.test(imdata$CaseControl,imdata$sex)

odds ratio // Risk ratio
# method = c("oddsratio", "riskratio", "rateratio"),
#rev = c("neither", "rows", "columns", "both"),           
#oddsratio = c("wald", "fisher", "midp", "small"),
#riskratio = c("wald", "boot", "small"),
#rateratio = c("wald", "midp"),
#pvalue = c("fisher.exact", "midp.exact", "chi2"),
#correction = FALSE, verbose = FALSE)
---------------------------------------
oddsratio(imdata$CaseControl,imdata$sex)       #or

OR <-epitab(imdata$CaseControl,imdata$sex,
            method = "oddsratio",
            conf.level = 0.95)
OR
--------------------------------------
riskratio(imdata$CaseControl,imdata$sex)  #or

RR <-epitab(imdata$CaseControl,imdata$sex,
            method = "riskratio",
            conf.level = 0.95)

RR

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-------------------- Exact Test of Goodness-of-Fit ----------------------------
#The exact test goodness-of-fit can be performed with the binom.test function in the native stats package.
#The probability can be entered as a decimal or a fraction.  
#Other options include the confidence level for the confidence interval about the proportion
#In most circumstances, the two-sided test is used.
if(!require(XNomial)){install.packages("XNomial")}
if(!require(pwr)){install.packages("pwr")}
if(!require(BSDA)){install.packages("BSDA")}

#Binomial test--------------------proportions-----------------------------------
used to determine if the proportion of successes in a binary experiment differs from a hypothesized probability.


-------------------------------------------------------------------------------
                      Exact binomial test
--------------------------------------------------------------------------------
#In this example:
# 2 is the number of successes
# 10 is the number of trials
# 0.5 is the hypothesized probability of success
# Probability of single event only! Not binomial test!
dbinom(2, 10, 0.5)   

binom.test(2, 10, 0.5, alternative="less", conf.level=0.95) 

binom.test(2, 10, 0.5, alternative="two.sided", conf.level=0.95)

#   #   #      
#Probability density plot
#-------------------------------------------------------------------------------
          Probability density plot, binomial distribution
--------------------------------------------------------------------------------
  # In this example:
  # You can change the values for trials and prob
  # You can change the values for xlab and ylab
trials = 10
prob = 0.5

x = seq(0, trials)                   # x is a sequence, 1 to trials
y = dbinom(x, size=trials, p=prob)   # y is the vector of heights

barplot (height=y, names.arg=x,
         xlab="Number of uses of right paw",
         ylab="Probability under null hypothesis")

#   #   #      


#Comparing doubling a one-sided test and using a two-sided test
#-------------------------------------------------------------------------------
Exact binomial test, Compares performing a one-sided test and 
doubling the probability, and performing a two-sided test
--------------------------------------------------------------------------------
  binom.test(7, 12, 3/4, alternative="less", conf.level=0.95) 

# Create an object called Test with the test results      
Test = binom.test(7, 12, 3/4, alternative="less", conf.level=0.95)      

2 * Test$ p.value 
# This extracts the p-value from the test result, we called Test and multiplies it by 2
binom.test(7, 12, 3/4, alternative="two.sided", conf.level=0.95)

#     #     #


#Post-hoc test with manual pairwise tests
#A multinomial test can be conducted with the xmulti function in the package XNomial. 
This can be followed with the individual binomial tests for each proportion, as post-hoc tests.
#-------------------------------------------------------------------------------
Post-hoc example, multinomial and binomial test
-------------------------------------------------------------------------------
  
observed = c(72, 38, 20, 18)
expected = c(9, 3, 3, 1)

library(XNomial)
xmulti(observed, expected, detail = 2)         

# Reports three types of p-value
P value  (LLR)  =  0.003404  # log-likelihood ratio
P value (Prob)  =  0.002255  # exact probability
P value (Chisq) =  0.001608  # Chi-square probability


# Note last p-value below agrees with Handbook
successes   = 72
total       = 148
numerator   = 9
denominator = 16

binom.test(successes, total, numerator/denominator, alternative="two.sided", conf.level=0.95) 

----------------------------------------------------------      
successes   = 38
total       = 148
numerator   = 3
denominator = 16

binom.test(successes, total, numerator/denominator, alternative="two.sided", conf.level=0.95) 

----------------------------------------------------------      
successes   = 20
total       = 148
numerator   = 3
denominator = 16

binom.test(successes, total, numerator/denominator, alternative="two.sided", conf.level=0.95) 

----------------------------------------------------------      
successes   = 18
total       = 148
numerator   = 1
denominator = 16

binom.test(successes, total, numerator/denominator, alternative="two.sided", conf.level=0.95) 


#     #     #


#Post-hoc test alternate method with custom function When you need to do multiple similar tests, however, 
#it is often possible to use the programming capabilities in R to do the tests more efficiently.
#The following example may be somewhat difficult to follow for a beginner.  
#It creates a data frame and then adds a column called p.Value that contains the p-value from the binom.test 
#performed on each row of the data frame.
#------------------------------------------------------------------------------
Post-hoc example, multinomial and binomial test
# Alternate method for multiple tests
----------------------------------------------------------------------- 
  Input =("
Successes Total Numerator Denominator
 72        148   9         16
 38        148   3         16
 20        148   3         16
 18        148   1         16
")

D1 = read.table(textConnection(Input),header=TRUE)

Fun = function (x){
  binom.test(x["Successes"],x["Total"],
             x["Numerator"]/x["Denominator"])$ p.value
}

D1$ p.Value = apply(D1, 1, Fun)
D1 


#Binomial test examples
#-----------------------------------------------------------------------
Parasitoid examples, exact binomial test
------------------------------------------------------------------------
  binom.test(10, (17+10), 0.5, alternative="two.sided", conf.level=0.95)

binom.test(36, (7+36), 0.5, alternative="two.sided", conf.level=0.95)

#     #     #

#-----------------------------------------------------------------------
Drosophila example, exact binomial test
------------------------------------------------------------------------
  binom.test(140, (106+140), 0.5, alternative="two.sided", conf.level=0.95)

#     #     #

#Sign test example
The following is an example of the two-sample dependent-samples sign test.
The data are arranged as a data frame in which each row contains the values for both measurements being compared 
for each experimental unit.
This is sometimes called “wide format” data. The SIGN.test function in the BSDA package is used.  
The option md=0 indicates that the expected difference in the medians is 0 (null hypothesis).

#This function can also perform a one-sample sign test.
#------------------------------------------------------------------------
Tree beetle example, two-sample sign test
-------------------------------------------------------------------------
  Input =("
Row  Angiosperm.feeding  A.count  Gymonsperm.feeding   G.count
1    Corthylina           458     Pityophthorus           200
2    Scolytinae          5200     Hylastini_Tomacini      180
3    Acanthotomicus_P     123     Orhotomicus              11
4    Xyleborini_D        1500     Ipini                   195
5    Apion               1500     Antliarhininae           12
6    Belinae              150     Allocoryninae_Oxycorinae 30
7    H_Curculionidae    44002     Nemonychidae             85
8    H_Cerambycidae     25000     Aseminae_Spondylinae     78
9    Megalopodinae        400     Palophaginae              3
10   H_Chrysomelidae    33400     Aulocoscelinae_Orsod     26
")

Data = read.table(textConnection(Input),header=TRUE)

library(BSDA)

SIGN.test(x = Data$ A.count, y = Data$ B.count, md = 0, alternative = "two.sided", conf.level = 0.95)


#Binomial test examples
#---------------------------------------------------------------------
First Mendel example, exact binomial test
----------------------------------------------------------------------
  binom.test(428, (428+152), 0.75, alternative="two.sided", conf.level=0.95)

#     #     #
#---------------------------------------------------------------------
First Mendel example, exact binomial test
##  Alternate method with XNomial package
--------------------------------------------------------------------------------
observed = c(428, 152)
expected = c(3, 1)

library(XNomial)
xmulti(observed, expected, detail = 2)             

# reports three types of p-value      
P value  (LLR)  =  0.5331   # log-likelihood ratio
P value (Prob)  =  0.5022   # exact probability
P value (Chisq) =  0.5331   # Chi-square probability

# # #
================================================================================      
                 Multinomial test example
#-------------------------------------------------------------------------------
Second Mendel example, multinomial exact test
------------------------------------------------------------------------
  observed = c(315, 108, 101, 32)
expected = c(9, 3, 3, 1)

library(XNomial)
xmulti(observed, expected, detail = 2)              
# reports three types of p-value      
P value  (LLR)  =  0.9261    # log-likelihood ratio
P value (Prob)  =  0.9382    # exact probability
P value (Chisq) =  0.9272    # Chi-square probability

# # # 


==============================================================================     
  #---------------- Chi-square Test of Goodness-of-Fit  ------------------------

if(!require(dplyr)){install.packages("dplyr")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(grid)){install.packages("grid")}
if(!require(pwr)){install.packages("pwr")}

#Chi-square goodness-of-fit example
#---------------------------------------------------------------------
Drosophila example, Chi-square goodness-of-fit
----------------------------------------------------------------------
  observed = c(770, 230)        # observed frequencies
expected = c(0.75, 0.25)      # expected proportions

chisq.test(x = observed, p = expected)

#     #     #


#Examples: extrinsic hypothesis
#---------------------------------------------------------------------
Crossbill example, Chi-square goodness-of-fit
----------------------------------------------------------------------
  observed = c(1752, 1895)    # observed frequencies
expected = c(0.5, 0.5)      # expected proportions

chisq.test(x = observed, p = expected)

#     #     #


#--------------------------------------------------------------------
Rice example, Chi-square goodness-of-fit
---------------------------------------------------------------------
  observed = c(772, 1611, 737)
expected = c(0.25, 0.50, 0.25)

chisq.test(x = observed, p = expected)

#     #     #

# -------------------------------------------------------------------
Bird foraging example, Chi-square goodness-of-fit
---------------------------------------------------------------------
  observed = c(70, 79, 3, 4)
expected = c(0.54, 0.40, 0.05, 0.01)

chisq.test(x = observed, p = expected)

#     #     #

#Example: intrinsic hypothesis  
# -------------------------------------------------------------------
Intrinsic example, Chi-square goodness-of-fit
---------------------------------------------------------------------
  observed       = c(1203,  2919,  1678)
expected.prop  = c(0.211, 0.497, 0.293)
expected.count = sum(observed)*expected.prop

chi2 = sum((observed- expected.count)^2/ expected.count)
chi2

#     #     #

#------------------ Chi-square goodness-of-fit -------------------------
Pea color example, Chi-square goodness-of-fit
------------------------------------------------------------------------
  observed = c(423, 133)  
expected = c(0.75, 0.25)

chisq.test(x = observed, p = expected)

#     #     #

#Simple bar plot with barplot
#--------------------------------------------------------------------
Simple bar plot of proportions
# Uses data in a matrix format
---------------------------------------------------------------------
  observed = c(70, 79, 3, 4)
expected = c(0.54, 0.40, 0.05, 0.01)
total = sum(observed)

observed.prop = observed / total
observed.prop

#    #    #

#--------------- G–test of Goodness-of-Fit------------------------
Examples: extrinsic hypothesis
#G-test goodness-of-fit test with DescTools and RVAideMemoire
# --------------------------------------------------------------
Crossbill example, G-test goodness-of-fit
--------------------------------------------------------------
observed = c(1752, 1895)    # observed frequencies
expected = c(0.5, 0.5)      # expected proportions

library(RVAideMemoire)
G.test(x=observed, p=expected)

# # #


# --------------------------------------------------------------
Crossbill example, G-test goodness-of-fit
#   Manual calculation
----------------------------------------------------------------
  observed      = c(1752, 1895)     # observed frequencies
expected.prop = c(0.5, 0.5)       # expected proportions

degrees = 1                       # degrees of freedom
expected.count = sum(observed)*expected.prop

G = 2 * sum(observed * log(observed / expected.count))
G                          

pchisq(G, df=degrees, lower.tail=FALSE) 

#     #     #


#Examples of G-test goodness-of-fit test with DescTools and RVAideMemoire
# ----------------------------------------------------------------------
Rice example, G-test goodness-of-fit
-----------------------------------------------------------------------
  observed = c(772, 1611, 737)
expected = c(0.25, 0.50, 0.25)

#  G-test for given probabilities
library(RVAideMemoire)
G.test(x=observed, p=expected)



# #  #

#------------------------------------------------------------------
Foraging example, G-test goodness-of-fit
-------------------------------------------------------------------
  observed = c(70, 79, 3, 4)
expected = c(0.54, 0.40, 0.05, 0.01)

library(RVAideMemoire)
G.test(x=observed,
       p=expected)

#     #     #


#Example: intrinsic hypothesis
#---------------------------------------------------------------------
Intrinsic example, G-test goodness-of-fit, amphipod
----------------------------------------------------------------------
  observed       = c(1203,  2919,  1678)
expected.prop  = c(.21073, 0.49665, 0.29262)

### Note: These are recalculated for more precision
###       In this case, low precision probabilities
###         change the results
expected.count = sum(observed)*expected.prop

G = 2 * sum(observed * log(observed / expected.count))
G                         

pchisq(G, df=1, lower.tail=FALSE)  

#     #     #


===============================================================================      
  #------------------------ Chi-square Test of Independence ---------------------

The Chi-square test of independence can be performed with the chisq.test function in the native stats package in R.
For this test, the function requires the contingency table to be in the form of matrix.

Depending on the form of the data to begin with,this can require an extra step, 
either combing vectors into a matrix or cross-tabulating the counts among factors in a data frame.  
None of this is too difficult, 
but it requires following the correct example depending on the initial form of the data. 

When using read.table and as.matrix to read a table directly as a matrix, be careful of extra spaces at the end of lines 
or extraneous characters in the table, as these can cause errors.

if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(grid)){install.packages("grid")}
if(!require(pwr)){install.packages("pwr")}

#When to use it
#Example of chi-square test with matrix created with read.table
#---------------------------------------------------------------------
Vaccination example, Chi-square independence
###      Example directly reading a table as a matrix
----------------------------------------------------------------------
  Input =("
Injection.area  No.severe  Severe
Thigh           4788       30
Arm             8916       76
")

Matriz = as.matrix(read.table(textConnection(Input),
                              header=TRUE,
                              row.names=1))
Matriz  

# Continuity correction for 2 x 2 table      
chisq.test(Matriz,
           correct=TRUE)
# No continuity correction for 2 x 2 table
chisq.test(Matriz,
           correct=FALSE)      

#-Example of chi-square test with matrix created by combining vectors
#---------------------------------------------------------------------
Vaccination example, Chi-square independence
###  Example creating a matrix from vectors
----------------------------------------------------------------------
R1 = c(4788, 30)
R2 = c(8916, 76)
rows   = 2
Matriz = matrix(c(R1, R2),
                nrow=rows,
                byrow=TRUE)

rownames(Matriz) = c("Thigh", "Arm")          # Naming the rows and
colnames(Matriz) = c("No.severe", "Severe")   #  columns is optional.
Matriz

# Continuity correction for 2 x 2 table      
chisq.test(Matriz,
           correct=TRUE)      
# No continuity correction for 2 x 2 table
chisq.test(Matriz,
           correct=FALSE)      
=====================================================================
  # Post-hoc tests
  For the example of post-hoc pairwise testing,the pairwiseNominalIndependence function from the package rcompanion 
to make the task easier.Then we’ll use pairwise.table in the native stats package as an alternative.

#Post-hoc pairwise chi-square tests with rcompanion
#---------------------------------------------------------------------
Post-hoc example, Chi-square independence, pp. 60–61
----------------------------------------------------------------------
  Input =("
Supplement     No.cancer  Cancer
'Selenium'     8177       575
'Vitamin E'    8117       620
'Selenium+E'   8147       555
'Placebo'      8167       529
")

Matriz = as.matrix(read.table(textConnection(Input),
                              header=TRUE,
                              row.names=1))
Matriz

chisq.test(Matriz) 

library(rcompanion)
pairwiseNominalIndependence(Matriz,
                            fisher = FALSE,
                            gtest  = FALSE,
                            chisq  = TRUE,
                            method = "fdr")


#Post-hoc pairwise chi-square tests with pairwise.table
#----------------------------------------------------------------------
Post-hoc example, Chi-square independence
# As is, this code works on a matrix with two columns and compares rows
-----------------------------------------------------------------------
  Input =("
Supplement     No.cancer  Cancer
'Selenium'     8177       575
'Vitamin E'    8117       620
'Selenium+E'   8147       555
'Placebo'      8167       529
")

Matriz = as.matrix(read.table(textConnection(Input),
                              header=TRUE,
                              row.names=1))
Matriz
chisq.test(Matriz)

FUN = function(i,j){    
  chisq.test(matrix(c(Matriz[i,1], Matriz[i,2],
                      Matriz[j,1], Matriz[j,2]),
                    nrow=2,
                    byrow=TRUE))$ p.value
}

pairwise.table(FUN,
               rownames(Matriz),
               p.adjust.method="none")

======================
  
  
  
  #Chi-square test of independence with continuity correction and without correction
  #---------------------------------------------------------------------------------
Helmet example, Chi-square independence
----------------------------------------------------------------------------------
  Input =("
PSE        Head.injury  Other.injury
Helemt     372          4715
No.helmet  267          1391
")

Matriz = as.matrix(read.table(textConnection(Input),
                              header=TRUE,
                              row.names=1))
Matriz
# Continuity correction for 2 x 2 table      
chisq.test(Matriz,
           correct=TRUE)       
# No continuity correction for 2 x 2 table      
chisq.test(Matriz,
           correct=FALSE)       

#     #     #



#Chi-square test of independence
# --------------------------------------------------------------
Gardemann apolipoprotein example, Chi-square independence
----------------------------------------------------------------
  Input =("
Genotype  No.disease Coronary.disease
'ins/ins'   268        807
'ins/del'   199        759
'del/del'    42        184
")

Matriz = as.matrix(read.table(textConnection(Input),
                              header=TRUE,
                              row.names=1))
Matriz

chisq.test(Matriz)

#     #     #


#---------Simple bar plot with error bars showing confidence intervals----------
### ----------------------------------------------------------------
Plot example, herons and egrets, Chi-square test of association
----------------------------------------------------------------
  
  Input =("
Supplement     No.cancer  Cancer
'Selenium'     8177       575
'Vitamin E'    8117       620
'Selenium+E'   8147       555
'Placebo'      8167       529
")

Prostate = read.table(textConnection(Input),header=TRUE)

### Add sums and confidence intervals
library(dplyr)
Prostate =
  mutate(Prostate,
         Sum = No.cancer + Cancer)
Prostate =
  mutate(Prostate,
         Prop = Cancer / Sum,
         low.ci = apply(Prostate[c("Cancer", "Sum")], 1,
                        function(y) binom.test(y['Cancer'], y['Sum'])$ conf.int[1]),
         high.ci = apply(Prostate[c("Cancer", "Sum")], 1,
                         function(y) binom.test(y['Cancer'], y['Sum'])$ conf.int[2])
  )
Prostate

#Plot (Bar chart plot)
library(ggplot2)
ggplot(Prostate,
       aes(x=Supplement, y=Prop)) +
  geom_bar(stat="identity", fill="gray40",
           colour="black", size=0.5,
           width=0.7) +
  geom_errorbar(aes(ymax=high.ci, ymin=low.ci),
                width=0.2, size=0.5, color="black") +
  xlab("Supplement") +
  ylab("Prostate cancer proportion") +
  scale_x_discrete(labels=c("Selenium", "Vitamin E",
                            "Selenium+E","Placebo")) +
  
  ## ggtitle("Main title") +
  theme(axis.title=element_text(size=14, color="black",
                                face="bold", vjust=3)) +
  theme(axis.text = element_text(size=12, color = "gray25",
                                 face="bold")) +
  theme(axis.title.y = element_text(vjust= 1.8)) +
  theme(axis.title.x = element_text(vjust= -0.5))


#     #     #  


#Bar plot with categories and no error bars
#------------------------------------------------------------------------------
Plot example, herons and egrets, Chi-square independence
--------------------------------------------------------------
  
  Input =("
Habitat      Bird   Count
Vegetation   Heron   15
Shoreline    Heron   20
Water        Heron   14
Structures   Heron    6
Vegetation   Egret    8
Shoreline    Egret    5
Water        Egret    7
Structures   Egret    1
")

Birds = read.table(textConnection(Input),header=TRUE)

# Specify the order of factor levels
library(dplyr)
Birds=
  mutate(Birds,
         Habitat = factor(Habitat,levels=unique(Habitat)),
         Bird = factor(Bird,levels=unique(Bird))
  )

# Add sums and proportions
Birds$ Sum[Birds$ Bird == 'Heron'] =
  sum(Birds$ Count[Birds$ Bird == 'Heron'])

Birds$ Sum[Birds$ Bird == 'Egret'] =
  sum(Birds$ Count[Birds$ Bird == 'Egret'])

Birds=
  mutate(Birds,
         prop = Count / Sum
  )
Birds 


#Plot adapted from:
# shinyapps.stat.ubc.ca/r-graph-catalog/

library(ggplot2)
library(grid)
ggplot(Birds,
       aes(x = Habitat, y = prop, fill = Bird, ymax=0.40, ymin=0)) +
  geom_bar(stat="identity", position = "dodge", width = 0.7) +
  geom_bar(stat="identity", position = "dodge", colour = "black",
           width = 0.7, show_guide = FALSE) +
  scale_y_continuous(breaks = seq(0, 0.40, 0.05),
                     limits = c(0, 0.40),
                     expand = c(0, 0)) +
  scale_fill_manual(name = "Bird type" ,
                    values = c('grey80', 'grey30'),
                    labels = c("Heron (all types)",
                               "Egret (all types)")) +
  
  ## geom_errorbar(position=position_dodge(width=0.7),
  ##               width=0.0, size=0.5, color="black") +
  labs(x = "Habitat Location", y = "Landing site proportion") +
  ## ggtitle("Main title") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"),
        plot.title = element_text(size = rel(1.5),
                                  face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.key.size = unit(0.4, "cm"),
        legend.key = element_rect(fill = "black"),
        axis.title.y = element_text(vjust= 1.8),
        axis.title.x = element_text(vjust= -0.5)
  )


#     #     # 


How to do the test
Chi-square test of independence with data as a data frame
In the following example for the chi-square test of independence, the data is read in as a data frame, 
not as a matrix as in previous examples.This allows more flexibility with how data are entered. 

For example you could have counts for same genotype and health distributed among several lines,
or have a count of 1 for each row, with a separate row for each individual observation.  
The xtabs function is used to tabulate the data and convert them to a contingency table.
# --------------------------------------------------------------
Gardemann apolipoprotein example, Chi-square independence,
###      Example using cross-tabulation
--------------------------------------------------------------
  Input =("
Genotype  Health       Count
ins-ins   no_disease   268
ins-ins   disease      807
ins-del   no_disease   199
ins-del   disease      759
del-del   no_disease    42
del-del   disease      184
")

Data.frame = read.table(textConnection(Input),header=TRUE)

# Cross-tabulate the data
Data.xtabs = xtabs(Count ~ Genotype + Health,
                   data=Data.frame)
Data.xtabs 
# includes N and factors      
summary(Data.xtabs)     

## Chi-square test of independence
chisq.test(Data.xtabs)

#     #     #



#------------------------ G–test of Independence --------------------------
G-test example with functions in RVAideMemoire
---------------------------------------------------------------------------
  Vaccination example, G-test of independence
# --------------------------------------------------------------
Input =("
 Injection.area  No.severe  Severe      
 Thigh           4788       30
 Arm             8916       76
")

Matriz = as.matrix(read.table(textConnection(Input),
                              header=TRUE,
                              row.names=1))
Matriz

library(DescTools)
GTest(Matriz,
      correct="none")            # "none" "williams" "yates"  

library(RVAideMemoire)
G.test(Matriz)

#     #     #

Post-hoc tests
For the following example of post-hoc pairwise testing, 
we’ll use the pairwise.G.test function from the package RVAideMemoire to make the task easier.  
Then we’ll use pairwise.table in the native stats package as an alternative.


Post-hoc pairwise G-tests with RVAideMemoire

#--------------------------------------------------------------
Post-hoc example, G-test of independence
---------------------------------------------------------------
  
  Input =("
 Supplement     No.cancer  Cancer
 'Selenium'     8177       575
 'Vitamin E'    8117       620
 'Selenium+E'   8147       555
 'Placebo'      8167       529
")

Matriz = as.matrix(read.table(textConnection(Input),
                              header=TRUE,
                              row.names=1))
Matriz

library(RVAideMemoire)
G.test(Matriz)

library(RVAideMemoire)
pairwise.G.test(Matriz,
                p.method = "none")           # Can adjust p-values;


-------
  Post-hoc pairwise G-tests with pairwise.table
As is, this function works on a matrix with two columns, and compares rows.
### --------------------------------------------------------------
Post-hoc example, G-test of independence
--------------------------------------------------------------
  Input =("
Supplement      No.cancer  Cancer
 'Selenium'     8177       575
 'Vitamin E'    8117       620
 'Selenium+E'   8147       555
 'Placebo'      8167       529
")
Matriz = as.matrix(read.table(textConnection(Input),
                              header=TRUE,
                              row.names=1))
Matriz
-------------------------
  
  library(DescTools)   
GTest(Matriz,
      correct="none") 

FUN = function(i,j){    
  GTest(matrix(c(Matriz[i,1], Matriz[i,2],
                 Matriz[j,1], Matriz[j,2]),
               nrow=2,
               byrow=TRUE),
        correct="none")$ p.value   # "none" "williams" "yates"
}

pairwise.table(FUN,
               rownames(Matriz),
               p.adjust.method="none")       # Can adjust p-values


#     #     #


#--------------------- Fisher’s Exact Test of Independence ------------------
if(!require(rcompanion)){install.packages("rcompanion")}

#__________Post-hoc pairwise Fisher’s exact tests with RVAideMemoire

### --------------------------------------------------------------
Post-hoc example, Fisher’s exact test
### --------------------------------------------------------------
Input =("
Frequency  Damaged  Undamaged
Daily       1        24
Weekly      5        20
Monthly    14        11
Quarterly  11        14
")

Matriz = as.matrix(read.table(textConnection(Input),
                              header=TRUE,
                              row.names=1))
Matriz

fisher.test(Matriz,
            alternative="two.sided")

--------------------------------------
  library(rcompanion)
PT = pairwiseNominalIndependence(Matriz,
                                 fisher = TRUE,
                                 gtest  = FALSE,
                                 chisq  = FALSE,
                                 digits = 3)
PT
--------------------------------------
  library(rcompanion)
cldList(comparison = PT$Comparison,
        p.value    = PT$p.adj.Fisher,
        threshold  = 0.05)




#-Examples of Fisher’s exact test with data in a matrix
### --------------------------------------------------------------
Chipmunk example, Fisher’s exact test
------------------------------------------------------------------
  
  Input =("
Distance    Trill  No.trill
 10m        16     8
 100m        3    18
")

Matriz = as.matrix(read.table(textConnection(Input),
                              header=TRUE,
                              row.names=1))

Matriz

fisher.test(Matriz,
            alternative="two.sided")


#     #     #



### --------------------------------------------------------------
Drosophila example, Fisher’s exact test
--------------------------------------------------------------
  
  Input =("
Variation             Synonymous  Replacement
 'Polymorphisms'      43          2
 'Fixed differences'  17          7
")

Matriz = as.matrix(read.table(textConnection(Input),
                              header=TRUE,
                              row.names=1))

Matriz

fisher.test(Matriz,
            alternative="two.sided")



#     #     #


### --------------------------------------------------------------
King penguin example, Fisher’s exact test
--------------------------------------------------------------
  
  Input =("
 Site     Alive  Dead
 Lower    43     7
 Middle   44     6
 Upper    49     1
")

Matriz = as.matrix(read.table(textConnection(Input),
                              header=TRUE,
                              row.names=1))

Matriz

fisher.test(Matriz,
            alternative="two.sided")


#     #     #



### --------------------------------------------------------------
Moray eel example, Fisher’s exact test
--------------------------------------------------------------
  
  Input =("

 Site     G.moringa  G.vicinus      
 Grass    127        116
 Sand      99         67
 Border   264        161
")

Matriz = as.matrix(read.table(textConnection(Input),
                              header=TRUE,
                              row.names=1))

Matriz

fisher.test(Matriz,
            alternative="two.sided")



p-value = 0.04438

alternative hypothesis: two.sided

#     #     #



### --------------------------------------------------------------
Herons example, Fisher’s exact test
--------------------------------------------------------------
  
  Input =("
 Site          Heron  Egret      
 Vegetation    15     8
 Shoreline     20     5
 Water         14     7
 Structures     6     1
")

Matriz = as.matrix(read.table(textConnection(Input),
                              header=TRUE,
                              row.names=1))

Matriz

fisher.test(Matriz,
            alternative="two.sided")



p-value = 0.5491

alternative hypothesis: two.sided



#     #     #


Graphing the results
Graphing is discussed above in the “Chi-square Test of Independence” section.

Similar tests – McNemar’s test
Care is needed in setting up the data for McNemar’s test.  

For a before-and-after test, the contingency table is set-up as before and after as row and column headings,or vice-versa.  
Note that the total observations in the contingency table is equal to the number of experimental units.  
That is, in the following example there are 62 men, and the sum of the counts in the contingency table is 62.  
If you set up the table incorrectly, you might end with double this number, and this will not yield the correct results.


#----------- McNemar’s test with data in a matrix ----------------
### --------------------------------------------------------------
Dysfunction example, McNemar test
--------------------------------------------------------------
  
  Input =("
 Row          After.no  After.yes      
 Before.no    46        10
 Before.yes    0         6
")

Matriz = as.matrix(read.table(textConnection(Input),
                              header=TRUE,
                              row.names=1))

Matriz

mcnemar.test(Matriz, correct=FALSE) 

#     #     #

____________________________________________________________________      
#----- McNemar test Dysfunction ---- using cross-tabulation
--------------------------------------------------------------------
  Input =("
ED.before  ED.after  Count
 no         no       46
 no         yes      10 
 yes        no        0
 yes        yes       6
")

Data = read.table(textConnection(Input),header=TRUE)

Data.xtabs = xtabs(Count ~ ED.before + ED.after, data=Data)

Data.xtabs
mcnemar.test(Data.xtabs, correct=FALSE)

#     #     #



#------------ Fisher’s exact test with data as a data frame -------
Chipmunk example, Fisher’s exact test #  using cross-tabulation
-------------------------------------------------------------------
  
  Input =("
Distance    Sound   Count
 10m        trill   16
 10m        notrill  8
 100m       trill    3
 100m       notrill 18
")

Data = read.table(textConnection(Input), header=TRUE)
Data.xtabs = xtabs(Count ~ Distance + Sound, data=Data)
Data.xtabs

summary(Data.xtabs)



#--------- Fisher’s exact test of independence

fisher.test(Data.xtabs,
            alternative="two.sided")

#     #     #


### --------------------------------------------------------------
Bird example, Fisher’s exact test, SAS example
# Example using cross-tabulation
------------------------------------------------------------------
  Input =("

Bird    Substrate   Count
 heron  vegetation  15
 heron  shoreline   20
 heron  water       14
 heron  structures   6
 egret  vegetation   8
 egret  shoreline    5
 egret  water        7
 egret  structures   1
")

Data = read.table(textConnection(Input), header=TRUE)

Data.xtabs = xtabs(Count ~ Bird + Substrate, data=Data)

Data.xtabs


summary(Data.xtabs)

### Fisher’s exact test of independence
fisher.test(Data.xtabs,
            alternative="two.sided")
#     #     #                      


#Repeated G–tests of goodness-of-fit example
#______________________________________________________________
Arm crossing example, Repeated G–tests of goodness-of-fit
---------------------------------------------------------------
  Input =("
Ethnic.group  R    L
 Yemen        168  174
 Djerba       132  195
 Kurdistan    167  204
 Libya        162  212
 Berber       143  194
 Cochin       153  174
")

Data = read.table(textConnection(Input),header=TRUE)
----------
  
  Individual G-tests
install.packages("RVAideMemoire")

library(RVAideMemoire)
Fun.G = function (Q){                           # Functions
  G.test(x=c(Q["R"], Q["L"]),           #   to calculate
         p=c(0.5, 0.5)                  #   individual G’s,
  )$statistic                    #   df’s, and p-values
}

Fun.df = function (Q){
  G.test(x=c(Q["R"], Q["L"]),
         p=c(0.5, 0.5)
  )$parameter
}

Fun.p = function (Q){
  G.test(x=c(Q["R"], Q["L"]),
         p=c(0.5, 0.5)
  )$p.value
}

library(dplyr)
Data=
  mutate(Data,
         Prop.R = R / (R + L),                         # Calculate proportion
         #     of right arms
         G =       apply(Data[c("R", "L")], 1, Fun.G),
         df =      apply(Data[c("R", "L")], 1, Fun.df),
         p.Value = apply(Data[c("R", "L")], 1, Fun.p)
  )

Data

--------------------
  
  
  Heterogeneity G-test
# We need a data matrix      
Data.matrix = as.matrix(Data[c("D", "S")])      

# to run G-test for heterogeneity
Data.matrix                       
G.test(Data.matrix)                     
------------------------------      
  
  Pooled G-test
# Set up data for pooled for G-test     
Total.D = sum(Data$D)                           
Total.S = sum(Data$S)                           

observed = c(Total.D, Total.S) # Pooled
expected = c(0.5, 0.5)

G.test(x=observed,                              
       p=expected)
------------------------------      
  
  Total G-test
# Set up data for total for G-test     
Total.G = sum(Data$G)                           
degrees = 3

Total.G  = sum(Data$G)                          
Total.df = sum(Data$df)
Total.G     # Total                                    

Total.df

pchisq(Total.G,
       df=Total.df,
       lower.tail=FALSE)


#     #     #  


#The Cochran–Mantel–Haenszel test (CMH test)
==============================================================================
  Is used to analyze stratified categorical data, particularly when you have multiple 2x2 contingency tables 
across different groups or times, allowing you to assess the association between two variables 
while controlling for a potential confounding factor      
------------------------------------------------------------------------------
  #===== Cochran–Mantel–Haenszel Test for Repeated Tests of Independence =======

if(!require(dplyr)){install.packages("dplyr")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(grid)){install.packages("grid")}
if(!require(vcd)){install.packages("vcd")
  
  
  # Cochran–Mantel–Haenszel Test with data read by read.ftable
  # --------------------------------------------------------------
  Handedness example, Cochran–Mantel–Haenszel test
  # Example using read.ftable
  --------------------------------------------------------------
    
    # Note no spaces on lines before row names.
    #   read.ftable can be fussy about leading spaces.
    
    Input =(
      "                  Group W.Child B.adult PA.white W.men G.soldier
Whorl      Handed
Clockwise  Right            708     136      106    109      801
           Left              50      24       32     22      102
CounterCl  Right            169      73       17     16      180
           Left              13      14        4     26       25
")
    
    Tabla = as.table(read.ftable(textConnection(Input)))
    # Display a flattened table
    ftable(Tabla)     
    
    
    Cochran–Mantel–Haenszel test
    mantelhaen.test(Tabl
                    
                    
                    Woolf test
                    
                    library(vcd)
                    # Show log odds for each 2x2
                    oddsratio(Tabla, log=TRUE)            
                    
                    library(vcd)
                    # Woolf test for homogeneity of odds ratios across strata.If significant, C-M-H test is not appropriate
                    woolf_test(Tabla)                
                    
                    #Breslow-Day test
                    library(DescTools)
                    BreslowDayTest(Tabla)
                    
                    Individual Fisher exact tests
                    
                    
                    n = dim(Tabla)[3]
                    for(i in 1:n){
                      Name = dimnames(Tabla)[3]$Group[i]
                      P.value = fisher.test(Tabla[,,i])$p.value
                      cat(Name, "\n")
                      cat("Fisher test p-value: ", P.value, "\n")
                      cat("\n")
                    }
                    
                    # Note: "Group" must be the name of the stratum variable
                    
                    # Cochran–Mantel–Haenszel Test with data entered as a data frame
                    # ----------------------------------------------------------------
                    Mussel example, Cochran–Mantel–Haenszel test
                    # Example using cross-tabulation of a data frame
                    ----------------------------------------------------------------
                      
                      Input =("
 Location    Habitat     Allele     Count
  Tillamook  marine          94     56
  Tillamook  estuarine       94     69
  Tillamook  marine      non-94     40  
  Tillamook  estuarine   non-94     77
  Yaquina    marine          94     61 
  Yaquina    estuarine       94    257
  Yaquina    marine      non-94     57  
  Yaquina    estuarine   non-94    301
  Alsea      marine          94     73 
  Alsea      estuarine       94     65
  Alsea      marine      non-94     71  
  Alsea      estuarine   non-94     79
  Umpqua     marine          94     71  
  Umpqua     estuarine       94     48
  Umpqua     marine      non-94     55   
  Umpqua     estuarine   non-94     48
 ")
                    
                    Data = read.table(textConnection(Input),header=TRUE)
                    
                    # Specify the order of factor levels Otherwise, R will alphabetize them
                    
                    library(dplyr)
                    
                    Data =
                      mutate(Data,
                             Location = factor(Location, levels=unique(Location)),
                             Habitat = factor(Habitat, levels=unique(Habitat)),
                             Allele = factor(Allele, levels=unique(Allele))
                      )
                    # Cross-tabulate the data
                    
                    # Note here, Location is stratum variable (is last) Habitat x Allele are 2 x 2 tables
                    
                    Data.xtabs = xtabs(Count ~ Allele + Habitat + Location,
                                       data=Data)
                    
                    # Display a flattened table                        
                    ftable(Data.xtabs)                      
                    
                    
                    Cochran–Mantel–Haenszel test
                    mantelhaen.test(Data.xtabs)
                    ------------------------------------------------# that is fine for now
                      
                      # fisher exact test:
                      tab <- tabyl(imdata3, mode, nicu)
                    fisher.test(tab)
                    # Chi-square test::
                    tab <- tabyl(imdata3, mode, nicu)
                    chisq.test(tab)
                    chisq.test(tab)$residuals
                    
                    
                    #   #   #                      
                    














+++++++++++++++++++++++++ One sample test of Mean ++++++++++++++++++++++++++++++
#-------------------------------------------------------------------------------
observed    = covid01$Age
theoretical = 46

t.test(observed, mu = theoretical, conf.int=0.95)                      #Option.1

t.test(covid01$Age, mu = 46, conf.int=0.95)                            #Option.2


hist(covid01$Age, col="green", main="Histogram of values", xlab="Age")

#  #  #

+++++++++++++++++++++++++++ Two Samples Mean +++++++++++++++++++++++++++++++++++
#-------------------------------------------------------------------------------
Two-sample t-test, independent (unpaired) observations
bartlett.test(Value ~ Group, data=Data) #If p-value >= 0.05, use var.equal=TRUE below.

#paired t.test  
t.test(Age ~ SEX, data=covid01, var.equal=TRUE, conf.level=0.95)

#independent t.test
t.test(Age ~ SEX, data=covid01, var.equal=FALSE, conf.level=0.95)

# Boxplot
boxplot(Age ~ SEX, data = covid01, names=c("Female","Male"), ylab="Age")
#

#========================= Non parametric ======================================
#------- Mann–Whitney Test
wilcox.test(Age ~ SEX, data=covid01, exact = FALSE)
#Box plots
boxplot(Age ~ SEX, data = covid01, names=c("Female","Male"), ylab="Age")

# # #

#------ Wilcoxon Signed-rank Test
wilcox.test(Data$August, Data$November, paired=TRUE)
#Simple 1-to-1 plot of values
plot(Data$August, Data$November, pch = 16, xlab="August", ylab="November")
abline(0,1, col="blue", lwd=2)  

# # #

+++++++++++++++++++++++++++++ANOVA Between Groups ++++++++++++++++++++++++++++++
#-------------------------- One_Way ANOVA (LessR) ------------------------------

#visualise statistical assumptions
library(lessR)
library(readxl)

print(covid01)
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

#   #   # 
#==============================  Factorial Anova ==============================# 
library(psych)
library(ggplot2)
library(ggpubr)
library(readxl)

describe(amdata)

#residuals
res_aov <- aov(bmi ~ expose, data = imdata)
res_aov

#combine plots
par(mfrow = c(1,2))
#histogram
hist(res_aov$residuals)
#QQ-plot
library(car)
QQ <- qqPlot(res_aov$residuals, id = TRUE)  #id=TRUE to remove point identification
shapiro.test(res_aov$residuals) #normality

#Bartlett’s test and Levene’s test to check the homoscedasticity of groups from a one-way anova.
#Levene-test------------------------------  is less sensitive to deviation from normality
#homogeneity of variance one variable
leveneTest(bmi ~ grvdty, data = imdata)
#homogeneity of variance multiple variable
leveneTest(bmi ~ expose*CaseControl*sex*mode*grvdty, data = imdata) 

#Bartlett test----------------------------
bartlett.test(bmi ~ grvdty, data = imdata)
bartlett.test(bmi ~ interaction(grvdty,sex,expose), data = imdata)
#One_way Anova
one <- aov(estimated_parasitemia ~ age, data = imdata)
summary(one)
#Two_way Anova
two <- aov(bmi ~ expose + grvdty, data = imdata)
summary(two)
#three_way Anova
three <- aov(bmi ~ expose + grvdty + sex, data = imdata)
summary(three)
#interaction
interaction <- aov(bmi ~ expose + grvdty + sex + expose*grvdty, data = imdata)
summary(interaction)

#model fit----------------------------------
library(AICcmodavg)
model.set <- list(one,two,three,interaction)
model.names <- c("one", "two","three", "interaction")
aictab(model.set, modnames = model.names, sort = TRUE)
bictab(model.set, modnames = model.names, sort = TRUE)

#effect size--------------------------------
library(effectsize)
eta_squared(interaction, partial = TRUE)
eta_squared(interaction, partial = FALSE)
omega_squared(interaction, partial = TRUE)
epsilon_squared(interaction, partial = TRUE)

#post hoc analysis-------------------------
tukey.interaction <- TukeyHSD(interaction)
tukey.interaction

tukey.plot.aov <- aov(bmi ~ expose:CaseControl, data = imdata)
tukey.plot.test <-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test,las = 2)

#   #   #
----------------------------------------------------------
  Tukey and LSD mean separation tests (pairwise comparisons)
TukeyHSD, HSD.test, and LSD.test are not appropriate for cases where there are unequal variances
though TukeyHSD does make an adjustment for mildly unequal sample sizes.
----------------------------------------------------------
  
  
  #  #  #
  +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
  #----------------Kruskal–Wallis Test-------------------------------------------
if(!require(FSA)){install.packages("FSA")
  
  #-------------------------------------- kruskal.test(Value ~ Group, data = Data)
  library(psych)
  describe(imdata)
  
  #-Medians and descriptive statistics
  library(FSA)
  Summarize(age ~ expose, data = imdata)
  
  #-Kruskal–Wallis test
  kruskal.test(age ~ expose, data = imdata)
  
  #-Dunn test for multiple comparisons(Post Hoc)
  The Dunn test is performed with the dunnTest function in the FSA package.  
  Adjustments to the p-values could be made using the method option to control the familywise error rate or 
  to control the false discovery rate.  
  # Dunn test methods--------“bonferroni”, “holm”,“sidak”, “hs”, “hochberg”, “bh”(Benjamini-Hochberg),“none”, “by”,  
  
  library(FSA)
  PT = dunnTest(bmi ~ expose, data=imdata, method="bh")           
  PT
  
  #   #   #
  
  ++++++++++++++++++++++++++++ANOVA Within Groups +++++++++++++++++++++++++++++++
    #------------------------- Repeated Measures ANOVA --------------------------#
    # Create a data frame
    wgdata <- data.frame(subject, time, response)
  
  # Perform Repeated-Measures ANOVA
  aov_model <- aov(response ~ time + Error(subject/time), data = wgdata)
  
  # View the summary of the ANOVA
  summary(aov_model) 
  
  ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #------------------------------ Friedman test -------------------------------#
    library(PMCMRplus)
  
  result <- friedman.test(outcome ~ group, data = your_data)
  
  ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #-------------------------------- ANCOVA ------------------------------------#
    library(car)
  ancova_model <-aov(finalscore~group + initialscore, data=score)
  Anova(ancova_model,type="III")                           
  
  
  +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #------------------------------- MANOVA --------------------------------------#   
    mv_data <- data.frame(group, weight, height)
  head(mv_data)
  
  #fit Manova Model
  manova_model <- manova(cbind(weight, height) ~ group, data = mv_data) 
  
  # or   group all target dependent Variables
  y<-cbind(weight, height)
  #
  manova1_model <- manova(y ~ group, data = mv_data)
  summary(manova1_model)
  #View results
  summary(manova_model, test = "Wilks")
  or
  summary(manova_model, test = "Pillai")
  summary(manova_model, test = "Hotelling-Lawley")
  summary(manova_model, test = "Roy")
  
  #run anova 
  summary.aov(manova_model)
  
  #View plots
  par(mfrow=c(1,2))
  
  boxplot(weight ~ group,
          data=mv_data,
          col="lightblue",
          main="Weight Across Groups",
          xlab="Group",
          ylab="Weight")
  
  boxplot(height ~ group,
          data=mv_data,
          col="lightgreen",
          main="Height Across Groups",
          xlab="Group",
          ylab="Height")
  
  #----------------------- Correlation and Linear Regression ---------------------
  Correlation
  Correlation can be performed with the cor.test function in the native stats package.  
  It can perform Pearson, Kendall, and Spearman correlation procedures.  
  
  #Pearson correlation (Parametric)
  Pearson correlation is a parametric test, and assumes that the data are linearly related 
  and that the residuals are normally distributed.
  --------
    cor.test( ~ age + bmi, data=imdata, method = "pearson", conf.level = 0.95)
  
  #Kendall correlation (Non-parametric)
  Kendall rank correlation is a non-parametric test that does not assume a distribution of the data.
  It ranks the data to determine the degree of correlation.
  --------
    cor.test( ~ age + bmi, data=imdata, method = "kendall", continuity = FALSE, conf.level = 0.95)
  ======================
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
  
  ==================================================================================
    install.packages("GGally")
  
  library(psych)
  library(ggplot2)
  library(GGally)   
  library(readxl)
  EGF01 <- read_excel("C:/Users/User/Desktop/EGF01.xlsx")
  print(EGF01)
  GGally::ggcorr(ARMdata1)
  GGally::ggpairs(ARMdata1)
  psych::pairs.panels(ARMdata1)
  ================================================================================== 
    
    #Spearman correlation (Non-parametric / ordinals)
    Spearman rank correlation is a non-parametric test that does not assume a distribution of the data.
  It ranks the data to determine the degree of correlation, and is appropriate for ordinal measurements.
  --------
    cor.test( ~ age + bmi, data=imdata, method = "spearman", continuity = FALSE, conf.level = 0.95)
  
  #   #   #
  
  
  ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    +                            Regression analysis                                                 +
    ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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

#-------------------------------------------------------------------------------
---------------------------- Model Building ------------------------------------  
  
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


++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  +                              LOGISTIC REGRESSION                                               +
  ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
#log-odds=-0.7673+0.6719*(parity=1)+2.1535*(parity=2)+1.8659*(parity=1)-15.7988*(parity=4)
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


# # #

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
print(all)

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
summary(model)

all <- exp(cbind(OR = coef(model), confint(model)))
print(all)

# # #

#__________future predictions____________________
new_data <- data.frame(Petal.Length = 1.5,
                       Petal.Width = 0.3, 
                       Sepal.Length = 4.5, 
                       Sepal.Width = 3.1)
predict(model, newdata = new_data, type = "class")

data$outcome_var <- relevel(data$outcome_var, ref = "BaselineCategory")

#   #   #

+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  Poisson  Regression                                #Log-linear model
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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

#  #  #



