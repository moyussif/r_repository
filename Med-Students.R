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

=====================================
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
library(epitools)
library(FSA)

#---------------------------- Import functions ------------------------------------------------------- 
#set directorate 
Hanisah <- read_excel("C:/Users/User/Desktop/TrainingData.xlsx")                #.xlsx format
str(Hanisah)

Neonate <- read_csv("C:/Users/User/Desktop/NNJ.csv")                            #.csv format
View(Neonate)
str(Neonate)

#------------------------ Data Manipulation with Dplyr ----------------------------------------------
library(dplyr)
#---------Selecting column
columns_needed <- Neonate %>% select(c(Motherage,HBV,HIV,Syphilis,Mother_bloodgroup,Mother_G6PD,Mode_of_delivery,
                                       Baby_sex,Baby_age_days,Baby_gestational_age,Baby_Weight,Baby_bloodgroup,Baby_G6PD,
                                       Babyfood,Diagnosis2,Diagnosis3))
str(columns_needed)
#----------Remove a column
remove_one <- columns_needed %>% select(-Syphilis)
str(remove_one)

#--Remove multiple columns
remove_more <- remove_one %>% select(-c(HBV, HIV))
str(remove_more)

#---------filter rows
Hanisah <- remove_more %>% select(c(Motherage,Mother_bloodgroup,Mother_G6PD,Mode_of_delivery,Baby_sex,Baby_age_days,
                                    Baby_gestational_age,Baby_Weight,Baby_bloodgroup,Baby_G6PD,Babyfood,Diagnosis2,Diagnosis3)) %>% 
  filter(Motherage < 40)
print(Hanisah)
str(Hanisah)
#---------summarise()
Hanisah1 <- Hanisah %>% summarise(sum_MotherAge=sum(Motherage))
print(Hanisah1)
Hanisah2 <- Hanisah %>% summarise(mean_Age=mean(Motherage))
print(Hanisah2)
Hanisah3 <- Hanisah %>% summarise(sum_BabyWt=sum(Baby_Weight),Mean_BabyWt=mean(Baby_Weight))
print(Hanisah3)

#---------group_by()
AverageWeightbyGender <-Hanisah %>%
  group_by(Baby_sex) %>%
  summarise(mean = mean(Baby_Weight))

print(AverageWeightbyGender)

#---------rename a column
#Data %>% rename(NewColumn=OldColumn)


names(Hanisah)[1]<- "mum_Age"
names(Hanisah)[4]<- "Delivery_mode"
names(Hanisah)[6]<- "Baby_Age"
names(Hanisah)[7]<- "Baby_Gest_Age"
str(Hanisah)


#---------Create New column (mutate)--------------------------------------------
mutate_colmn <- rename_colmns %>% mutate(bmi=weight/height*2)
#---------show only New column (transmute)
transmutate_col <- mutate_colmn %>% transmute(bmi)
#---------Change character type(rename_with)
Upper_caps <- rename_colmns %>% rename_with(Mother_group,toupper)
Lower_caps <- rename_colmns %>% rename_with(DeliveryMode,tolower)

###

#-------------------- Reshaping Data-(LONG.data / WIDE.data) -------------------
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

#-------------------------- Data Conversion ------------------------------------ Option 1
str(Hanisah)

#-------Continuous data
Hanisah$mum_Age <-as.integer(Hanisah$mum_Age)
Hanisah$Baby_Age <-as.integer(Hanisah$Baby_Age)
Hanisah$Baby_Weight <-as.integer(Hanisah$Baby_Weight)

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

Hanisah$Mother_bloodgroup <-factor(Hanisah$Mother_bloodgroup,
                                   levels = c(0,1,2,3,4,5,6,7),
                                   labels = c("Notdone", "A-","A+", "B-","B+","AB+", "AB-","O+"))

Hanisah$Mother_G6PD <-factor(Hanisah$Mother_G6PD,
                             levels = c(0,1,2,3),
                             labels = c("No defect","Full defect","Partial defect","Not done"))


Hanisah$Delivery_mode <-factor(Hanisah$Delivery_mode,
                               levels = c(0,1,2),
                               labels = c("Vaginal", "Caesarean", "Assisted"))

Hanisah$Babyfood <-factor(Hanisah$Babyfood,
                          levels = c(0,1,2),
                          labels = c("No feeding", "Breastfeeding", "Mixedfeeding"))
Hanisah$Baby_Gest_Age <-factor(Hanisah$Baby_Gest_Age,
                               levels = c(0,1),
                               labels = c("Pretern", "Fullterm"))


Hanisah$Diagnosis2 <-factor(Hanisah$Diagnosis2,
                            levels = c(0,1),
                            labels = c("No NNJ", "NNJ"))

Hanisah$Diagnosis3 <-factor(Hanisah$Diagnosis3,
                            levels = c(0,1,2),
                            labels = c("No NNJ", "NNJ", "NNJ & Others"))
str(Hanisah)
print(Hanisah)
print(Hanisah$Baby_G6PD)
#==============================Reverse conversion ----------------------------- Option 2



Hanisah77$SarsCovStrain <-factor(Hanisah77$SarsCovStrain,
                                 levels = c("delta", "omicron"),
                                 labels = c(1, 2))

Hanisah77$categoryofcases <-factor(Hanisah77$SarsCovStrain,
                                   levels = c("mild", "moderate", "severe"),
                                   labels = c(1,2,3))


# #  #

#--------------------------- Handling Missing Data -----------------------------
# Count missing values in each column
missing_per_column <- colSums(is.na(Hanisah))
print(missing_per_column)

# Remove rows with any missing values 
Cleaned_Hanisah <- na.omit(Hanisah)
print(Cleaned_Hanisah)

Check_Clean_Hanisah <- colSums(is.na(Cleaned_Hanisah))
print(Check_Clean_Hanisah)

# # 

#---------------------------- Data Normality -----------------------------------
library(psych)
str(Hanisah)
print(Hanisah)
#
describeBy(Hanisah)

#___________________________ Outliers    
                            LOW  = Q1-1.5(IQR)      
                            HIGH = Q3+1.5(IQR)
#___________________________ Kurtosis(0-3)
            
#___________________________ Skewness(-1+1)
                            POS_skewed = Mean > Median/Mode    
                            NEG_skewed = Mean < Median/Mode                
            
#---------------------------Transforming skewed data---------------------------------------------------

#Square root Transformation------------------------------------moderately skewed (-1 to -0.5//0.5 to 1)
sq_data <-sqrt(Hanisah$Baby_Weight)
hist(sq_data)
#Cube root Transformation--------------------------------------moderately Right skewed(Negative//Zero)
cb_data <-sign(Hanisah$Baby_Weight)*abs(Hanisah$Baby_Weight)^(1/3)
hist(cb_data)
#Log Transformation--------------------------------------------Highly skewed(above1),But not (Negative//Zero)
Hanisah30$lg_mum_age <-log(Hanisah30$mum_Age)
hist(lg_data)

#     #     #

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #                            Descriptive statistics                            
library(skimr)
str(Hanisah)
skim_without_charts(Hanisah)

library(psych)
describeBy(Hanisah)

#   #   #
#------------------------- Visualize summarized data

#---------group_by()
Tablehanisah <-Hanisah %>%
  group_by(Delivery_mode) %>%
  summarise(sum = sum(Baby_Weight))
print(Tablehanisah)

#-----------------Pie Chart 
library(lessR)
slices <- c(13, 262, 597)
lbls <- c( "Assisted", "Caesarean", "Vaginal")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct)
# add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Baby's Weight by Mode of Delivery")

# 3D Exploded Pie Chart - Done by Hanisah
library(plotrix)
slices <- c(13, 262, 597)
lbls <- c( "Assisted", "Caesarean", "Vaginal")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie3D(slices,labels=lbls,explode=0.1,
      main="Baby's Weight by Mode of Delivery")

Hanisah100 <- data.frame(Hanisah)
#---------------Barchart
Hanisah100 <- as.data.frame(Hanisah)
BarChart(Hanisah100, Delivery_mode)

#----------------Histogram:
Histogram
hist(Hanisah$mum_Age, col= "turquoise", main="Maternal Age", xlab="Age")





Hanisah19 <- read_excel("C:/Users/User/Desktop/immudata.xlsx")
print(Hanisah19)


#============================= Inferential Statistics ==========================


#--------------------------------- Chi square                  
table(Hanisah6$SEX,Hanisah6$Durationweeks)
table(Hanisah6$Hospitalstatus,Hanisah6$SEX)
#Chi square
chisq.test(Hanisah6$Hospitalstatus,Hanisah6$SEX)

#Fisher's Exact Test
fisher.test(Hanisah6$SEX,Hanisah6$Hospitalstatus)
# # #   
oddsratio(Hanisah6$SEX,Hanisah6$Hospitalstatus)

riskratio(Hanisah6$SEX,Hanisah6$Hospitalstatus)  


#--------------------------------- ONE / TWO SAMPLES                                                
library(psych)
library(lessR)
##
summary(Hanisah25)
describeBy(Hanisah25)  

#-------------------- One sample t-test
observed    = Hanisah19$age
theoretical = 5

t.test(observed, mu = theoretical, conf.int=0.95)  


hist(Cleaned_Hanisah$mum_Age, col="gray", main="Histogram of Mother's Age", xlab="Age")


#--------------------- Two Samples Student’s t–test 
#=========================== PARAMETRIC 
Hanisah17 <- read_excel("C:/Users/User/Desktop/covid02.xlsx")
str(Hanisah17)
describe.by(Hanisah17)

#paired t.test  
t.test(Age ~ SEX, data=Hanisah17, var.equal=TRUE, conf.level=0.95)
#independent t.test
t.test(Age ~ SEX, data=Hanisah17, var.equal=FALSE, conf.level=0.95)

# Histogram
hs<-hist(Age ~ SEX, col="gray",data=Hanisah17, margin=FALSE)
hs
# Boxplot
boxplot(Age ~ SEX, data=Hanisah17, names=c("Female","Male"), ylab="Age")


#========================== NON-PARAMETRIC 

#------------- Mann–Whitney Test ---Independent test
wilcox.test(mum_Age ~ Baby_sex, data=Hanisah30, exact = FALSE)

#--------------Wilcoxon sign Test---Paired test
wilcox.test(lg_mum_age ~ Diagnosis2, data=Hanisah30, exact = TRUE)

#Box plots
boxplot(mum_Age ~ Baby_Gest_Age, data=Cleaned_Hanisah, names=c("preterm","Fullterm"), ylab="age")


#   #   #
#-------------------------- One-way Anova using LessR --------------------------option.1
library(readxl)
library(lessR)
library(car)
Hanisah6 <- read_excel("C:/Users/User/Desktop/covid02.xlsx")
str(Hanisah6)

#plot
Plot(Noofsymptoms, data=Hanisah6, by1=Coinfection)
#Normality assumption
tapply(Hanisah6$Noofsymptoms, Hanisah6$Coinfection, shapiro.test)
#decribe skewness/Kurtosis
describe.by(Hanisah6)
#Levene’s test Homogeneity of variance 
-----------------------------------
leveneTest(Age ~ categoryofcases, data=Hanisah17)

library(psych)
#One-way ANOVA
ANOVA(Durationdays ~ categoryofcases, data=Hanisah17)
#effect size(for groups with significant)
library(effsize)
cohen.d(Durationdays ~ categoryofcases, data=subset(Hanisah17, categoryofcases!= "mild"), paired=FALSE) 
cohen.d(Durationdays ~ categoryofcases, data=subset(Hanisah17, categoryofcases!= "moderate"), paired=FALSE) 
cohen.d(Durationdays ~ categoryofcases, data=subset(Hanisah17, categoryofcases!= "severe"),paired=FALSE)        

#Bar charts 
age_means <- tapply(Hanisah17$Durationdays, Hanisah17$categoryofcases)

BarChart(age_means)

# # #
#===================== ANOVA ANALYSIS----option.2 =============================
==========================  Factorial Anova =================================== 
library(psych)
library(car)
Hanisah17 <- read_excel("C:/Users/User/Desktop/covid02.xlsx")
str(Hanisah17)

QQ <- qqPlot(res_aov$residuals, id = TRUE)  #id=TRUE to remove point identification
shapiro.test(res_aov$residuals) #normality

# Bartlett’s test and Levene’s test 
-----------------------------------
leveneTest(mum_Age ~ Diagnosis3, data=Hanisah30)
leveneTest(mum_Age ~ Diagnosis3*Delivery_mode*Baby_sex*Baby_Gest_Age, data=Hanisah30) 

bartlett.test(mum_Age ~ Diagnosis3, data=Hanisah30)
bartlett.test(mum_Age ~interaction(Delivery_mode,Baby_sex,Baby_Gest_Age), data=Hanisah30)
#residuals
res_aov <- aov(bmi ~ expose, data = imdata)
res_aov
#combine plots
par(mfrow = c(1,2))
#histogram
hist(res_aov$residuals)
----------------------------------------------------------------------------
str(Hanisah17)
#One_way Anova
one <- aov(Noofsymptoms ~ categoryofcases, data = Hanisah17)
summary(one)
#postHoc
tukey.one <- TukeyHSD(one)
tukey.one
summary(tukey.one)
plot(tukey.one,las = 2)
-----------------------------------------------------------
#Two_way Anova
two <- aov(Noofsymptoms ~ categoryofcases + Coinfection, data = Hanisah17)
summary(two)
#three_way Anova
three <- aov(Noofsymptoms ~ categoryofcases + Coinfection+Hospitalstatus, data = Hanisah17)
summary(three)
#interaction
interaction <- aov(Noofsymptoms ~ categoryofcases + Coinfection + Hospitalstatus + Coinfection*Hospitalstatus, data = Hanisah17)
summary(interaction)
describe.by(Hanisah19$bmi)
#post hoc analysis
tukey.interaction <- TukeyHSD(interaction)
tukey.interaction

tukey.plot.aov <- aov(bmi ~ exposed:casecontrol, data = Hanisah19)
summary(tukey.plot.aov)

tukey.plot.test <-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test,las = 2)

#   #   #
-------------------------------------------------------------------------------------------------
Tukey and LSD mean separation tests (pairwise comparisons)
TukeyHSD, HSD.test, and LSD.test are not appropriate for cases where there are unequal variances
though TukeyHSD does make an adjustment for mildly unequal sample sizes.
-------------------------------------------------------------------------------------------------
  
  #   #    #
  
  ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #--------------------------- Kruskal–Wallis Test -----------------------------------------------
if(!require(FSA)){install.packages("FSA")
  #---------------------------------------------- kruskal.test(Value ~ Group, data = Data)
  library(psych)
  describe(imdata)
  #-Medians and descriptive statistics
  library(FSA)
  Summarize(Noofsymptoms ~ Coinfection, data=Hanisah6)
  
  #-Kruskal–Wallis test
  kruskal.test(Noofsymptoms ~ Coinfection, data=Hanisah6)
  
  #-Dunn test for multiple comparisons(Post Hoc)
  The Dunn test is performed with the dunnTest function in the FSA package.  
  Adjustments to the p-values could be made using the method option to control the familywise error rate or 
  to control the false discovery rate.  
  # Dunn test methods--------“bonferroni”, “holm”,“sidak”, “hs”, “hochberg”, “bh”(Benjamini-Hochberg),“none”, “by”,  
  
  library(FSA)
  PT = dunnTest(Noofsymptoms ~ Coinfection, data=Hanisah6, method="bh")           
  PT
  
  #   #   #
  
  +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #-------------------------------- Correlation -------------------------------------------------------------
  Correlation
  Correlation can be performed with the cor.test function in the native stats package.  
  It can perform Pearson, Kendall, and Spearman correlation procedures.  
  
  #Pearson correlation (Parametric)==========================================================================
  Pearson correlation is a parametric test, and assumes that the data are linearly related 
  and that the residuals are normally distributed.
  --------
    cor.test( ~ Durationdays + Noofsymptoms, data=Hanisah90, method = "pearson", conf.level = 0.95)
  
  #Kendall correlation (Non-parametric)
  Kendall rank correlation is a non-parametric test that does not assume a distribution of the data.
  It ranks the data to determine the degree of correlation.
  --------
    cor.test( ~ Durationdays + Noofsymptoms, data=Hanisah90, method = "kendall", continuity = FALSE, conf.level = 0.95)
  
  #Spearman correlation (Non-parametric / ordinals)
  Spearman rank correlation is a non-parametric test that does not assume a distribution of the data.
  It ranks the data to determine the degree of correlation, and is appropriate for ordinal measurements.
  --------
    cor.test( ~ Durationdays + Noofsymptoms, data=Hanisah90, method = "spearman", continuity = FALSE, conf.level = 0.95)
  
  #   #   #---------------------------------------------
  pearson <- cor.test(Hanisah90$Durationdays, Hanisah90$Noofsymptoms, method = "pearson")
  pearson
  
  =========================== Correlation Analysis ===============================
    cor() #compute the correlation coefficient
  cor.test() # test for association between paired samples (coefficient and p-value)
  
  cor.test(x = Hanisah15$Noofsymptoms, y = Hanisah15$AgeCategory,continuity = FALSE,
           method="spearman")
  
  
  
  library(ggpubr)
  #CorrelationPlot----------ggscatter() 
  Hanisah15 <- read_excel("C:/Users/User/Desktop/covid02.xlsx")
  str(Hanisah15)
  
  
  
  #---------------------------------------------------------------  
  ggscatter(Hanisah15, x = "Noofsymptoms", y = "Durationdays",
            add = "reg.line",conf.int = TRUE,
            cor.coef = TRUE, cor.method = "pearson",
            xlab = "Noofsymptoms", ylab = "Durationdays")
  
  
  #----------------------------------------------------- 
  Hnsh<-ggscatter(Hanisah15, x = "Noofsymptoms", y = "Durationdays",
                  add = "reg.line",                                 # Add regression line
                  conf.int = TRUE,                                  # Add confidence interval
                  add.params = list(color = "blue",
                                    fill = "lightgray"))+
    stat_cor(method = "pearson", label.x = 3, label.y = 30)   # Add correlation coefficient 
  

  #plot
  plot(Hanisah17$Noofsymptoms,Hanisah17$Durationdays,
       main = "Age by duration",
       xlab = "MOther Age",
       ylab = "Baby Age",
       pch=19,col="blue")
  # plot with ggplot2
  ggplot(Hanisah17, aes(x = Durationdays, y = Noofsymptoms)) +
    geom_point(color = "blue")
  