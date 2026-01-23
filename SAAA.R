rm(list=ls())
gc(reset = TRUE)

#----LOADING PACKAGES required
library(readxl)
library(readr)
library(tidyverse)
library(RColorBrewer)
library(ggpubr)
library(psych)
library(car)
library(lessR)
library(plotrix)
library(ggfortify)
library(epitools)
library(FSA)

#------IMPORT DATA
rawDATA <- read_excel("C:/Users/User/Desktop/rawDATA.xls")
View(rawDATA)
print(rawDATA)
str(rawDATA)
#------------------------ Data Manipulation with Dplyr ----------------------------------------------
library(dplyr)
#-------Rename

#---------Selecting column
columns_needed <- rawDATA %>% select(-c("Antibiotics given during labour or delivery", "Duration of labour"))

str(columns_needed)

#---------filter rows
alldata <- columns_needed %>% select(c("mum_Age","Marital_Status","Edu_Status","Occupation",
                                    "Condition_type","delivery_mode","Birth Wt","Microbes","BabySEX","Foodtype"))

str(alldata)

names(columns_needed)[1]<- "mum_Age"
names(columns_needed)[5]<- "Condition_type"
names(columns_needed)[7]<- "delivery_mode"
names(columns_needed)[8]<- "Birth_wt"
names(columns_needed)[9]<- "Microbes"
names(columns_needed)[12]<- "BabySEX"
names(columns_needed)[14]<- "Foodtype"

#
str(columns_needed)
#
Alan1 <- Alan %>% summarise(mum_Age=sum(mum_Age))
print(Alan1)

Alan2 <- Alan %>% summarise(mum_Age=mean(mum_Age))
print(Alan2)

#----checking for missing data
missing_per_column <- colSums(is.na(Alan))
print(missing_per_column)

# Remove rows with any missing values 
Cleaned_Hanisah <- na.omit(Alan)
print(Alan)

#----- Descriptive statistics

describe.by(Alan)

skim_without_charts(Alan)

install.packages("skimr")
library(skimr)
#------------------------- Visualize data

#---------group_by()
Anela <-Alan %>% group_by(Marital_Status) %>% summarise(sum = sum(mum_Age))
print(Anela)

#-----------------Pie Chart 
library(lessR)
slices <- c(678, 280, 1914)
lbls <- c( "single", "cohabiting", "married")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct)
# add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="marital status")

# 3D Exploded Pie Chart - Done by Hanisah
library(plotrix)
slices <- c(678, 280, 1914)
lbls <- c( "single", "cohabiting", "married")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie3D(slices,labels=lbls,explode=0.1,
      main="marital status by mothers age")

Anela2 <- data.frame(Alan)
#---------------Barchart
Anela2 <- as.data.frame(Alan)

BarChart(Anela2,Marital_Status)
#----------------Histogram:
Histogram
hist(Alan$mum_Age, col= "red", main="Maternal Age", xlab="Age")


#------------------------- Visualizing using ggplo2 ---------------------------
                    
##Simple Barchart

SAAA <-ggplot(data = Alan,
                mapping = aes(x = Marital_Status))+
  geom_bar()

SAAA

##stacked Barchart
Sylvia <-ggplot(data = Alan,
            mapping = aes(x = Marital_Status, fill = delivery_mode))+
  geom_bar()

Sylvia

#------------Histogram:

ht3 <-ggplot(data = Alan,
             mapping = aes(x = mum_Age))+
  geom_histogram()
ht3

#Histogram with stacked
ht <-ggplot(data = Alan,
            mapping = aes(x = mum_Age, fill = delivery_mode))+
  geom_histogram(bins = 10, position = "stack")

ht

#Histogram with dodge
Dodge <-ggplot(data = Alan,
            mapping = aes(x = mum_Age, fill = Marital_Status))+
  geom_histogram(bins = 2.0, position = "dodge")

Dodge

Adepa <-ggplot(data = Alan,
               mapping = aes(x = mum_Age, fill = Marital_Status))+
  geom_histogram(bins = 2.0, position = "dodge")+
  theme_classic()

Adepa
str(Alan)

#--------------------------Boxplot:
Anela2 <- ggplot(data = Alan,
               mapping = aes(x = Marital_Status, y = Birth_wt))+
  geom_boxplot()

Anela2


# Boxplot by category ##
me <- ggplot(data = Alan,
               mapping = aes(x = Marital_Status, y = Birth_wt, fill = ROMduration))+
  geom_boxplot()+
  theme_cleveland()+
  coord_flip()

me

#plotting by factor
friday <- ggplot(data = Alan,
               mapping = aes(x = Marital_Status, y = Birth_wt))+
  geom_boxplot(aes(fill = factor(Occupation)))

friday
=======================================================================================
  # Facet wrap 
  Akon <- ggplot(data = Alan,
                   mapping = aes(x = Marital_Status, y = Birth_wt, fill = Occupation))+
  geom_violin()+
  facet_wrap(~Occupation)

Akon
======================================================================================


# Facet grid
  Akon2 <- ggplot(data = Alan,
                 mapping = aes(x = Marital_Status, y = Birth_wt, fill = Occupation))+
  geom_boxplot()+
  theme_classic()+
  facet_grid(~Occupation)

  Akon

#============================= Inferential Statistics ==========================
str(alldata)

#--------------------------------- Chi square                  
table(alldata$BabySEX,alldata$Condition_type)
#Chi square
chisq.test(alldata$BabySEX,alldata$Condition_type)

#Fisher's Exact Test
fisher.test(alldata$BabySEX,alldata$Condition_type)
# # #   
oddsratio(Hanisah6$SEX,Hanisah6$Condition_type)

riskratio(Hanisah6$SEX,Hanisah6$Condition_type)  


#--------------------------------- ONE / TWO SAMPLES                                                
library(psych)
library(lessR)
##
summary(Hanisah25)
describeBy(Hanisah25)  

#-------------------- One sample t-test
observed = alldata$mum_Age
theoretical = 20

t.test(observed, mu = theoretical, conf.int=0.95)  


hist(alldata$mum_Age, col="gray", main="Histogram of Mother's Age", xlab="Age")


#--------------------- Two Samples Student’s t–test 


#=========================== PARAMETRIC 

#paired t.test  
t.test(mum_Age ~ BabySEX, data=alldata, var.equal=TRUE, conf.level=0.95)

#independent t.test
t.test(mum_Age ~ BabySEX, data=alldata, var.equal=FALSE, conf.level=0.95)

# Histogram
hs<-hist(mum_Age ~ BabySEX, col="gray",data=alldata, margin=TRUE)
hs
# Boxplot
boxplot(mum_Age ~ BabySEX, data=alldata, names=c("Female","Male"), ylab="Age")

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
  



