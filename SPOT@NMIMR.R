#
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
install.packages("PMCMRplus")
install.packages("stats")
install.packages("Hmisc")
install.packages("FSA")
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
library(PMCMRplus)
library(AICcmodavg)
#--------------------------- set directorate -----------------------------------Option.1
setwd("C:/Users/User/Downloads")

healthData <- read_csv("health_data.csv")    #.csv format 

Sars_2 <- read_excel("Sars-2.xlsx")          #.xlsx format

#-------------------------- Import functions....................................Option.2
# 
healthd <- read_csv("health_data.csv", header = TRUE, sep = ",")    #.csv format
#
Sars_2 <- read_excel("C:/Users/User/Desktop/Sars-2.xlsx")          #.xlsx format
View(Sars_2)
#---------------------- Understand Data Structures -----------------------------
str(Sars_2)
print(Sars_2)
#
#-------------------------------------------------------------------------------
                     Data Conversion _(CODING)  
#-------------------------------------------------------------------------------
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

# # #
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                Transform and Manipulate Data using Dplyr 
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(dplyr)                                                 
#---------Selecting column 
columns_needed <- hhdata %>% select(c(age, sex, marital_status, height, weight, heart_rate, smoking, cholesterol, sugar_level,
                                     BP_systolic, BP_diastolic, exercise_score_hrs, wealth_index))
str(columns_needed)
#----------Remove a column
hhdata2 <- hhdata %>% select(-wealth_index)
str(hhdata2)

#--Remove multiple columns
remove_more <- remove_one %>% select(-c(BP_systolic, BP_diastolic))
str(remove_more)

#---------filter rows ?filter()
hhdata2 <- remove_more %>% select(c(age, sex, marital_status, height, weight, heart_rate, smoking, cholesterol, sugar_level,
                                    exercise_score_hrs)) %>% 
 filter(age > 30) 
#                  or   filter(hhdata2, age <= 30)

print(hhdata2)

#---------Create New column (mutate)
hhdata2$BMI <- hhdata2 %>% mutate(bmi=weight/height*2)

str(hhdata2)

#---------rename a column
names(VDdata)[1]<- "Age"
names(VDdata)[2]<- "Gender"

str(hhdata2)

#---------summarise()
hhdata2 %>% count(marital_status)

Hanisah2 <- hhdata2 %>% summarise(mean_Age=mean(Age))
print(Hanisah2)

Hanisah3 <- hhdata2 %>% summarise(median_Age=median(Age), mean_Age=mean(Age))
print(Hanisah3)

#---------group_by()
AverageWeightbyGender <-VDdata %>% group_by(Gender) %>% summarise(mean = mean(Age))
print(AverageWeightbyGender)
#--------Arrange()
hhdata2 %>% 
  arrange(cholesterol) %>% 
  head()
#--------Sorting descending order
hhdata2 %>% 
  arrange(desc(cholesterol)) %>% 
  head()

#-------Descriptive statistics using`dplyr` 
basic_stats<-hhdata2%>%
  group_by(Gender)%>%
  summarise(mean_age=mean(Age),mean_bmi=mean(cholesterol),sd_age=sd(Age),sd_bmi=sd(cholesterol))

basic_stats

# # #
--------------------------------------------------------------------------------
                           HANDLING MISSING DATA
--------------------------------------------------------------------------------
# Count missing values in each column
majorie <- colSums(is.na(VDdata))
print(majorie)

# Remove rows with any missing values 
Julius <- na.omit(VDdata)
print(Julius)

# Verify for missings
Check_Clean_data <- colSums(is.na(Julius))
print(Check_Clean_data)

# # #
--------------------------------------------------------------------------------
              Descriptive statistics- (Explore data)            
--------------------------------------------------------------------------------
#Descriptive for Mean and Median 
summary(Sars_2)
#further insight - Skewness & Kurtosis
describeBy(Sars_2) 
# use statistical test for normality
shapiro.test(Sars_2$Age)
# Basic plots
hist(Sars_2$Age)
--------------------------------------------------------------------------------
                            Transform the data
Square root Transformation#<-sqrt(SarsCoV$Age)------------------moderately skewed (-1 to -0.5//0.5 to 1)
Cube root Transformation#  <-sign(Hanisah$Baby_Weight)*abs(Hanisah$Baby_Weight)^(1/3)  moderately Right skewed(Negative/Zero)
Log Transformation#        <-log(SarsCoV$Age)--------------Highly skewed(above1),But not (Negative//Zero)
--------------------------------------------------------------------------------
SarsCoV$Agee <-log(SarsCoV$Age) 
str(SarsCoV)
hist(SarsCoV$Agee)
shapiro.test(SarsCoV$Agee)

--------------------------------------------------------------------------------
                          Data Visualization
--------------------------------------------------------------------------------
#
#--------------------------Basic Plots------------------------------------------Option.1
#Barplot
plot(SarsCoV$SEX)
#pie
pie(table(data$SES))
pie(table(data$SES), col = c("white","gray90","gray60"))
#pie / donut -----depending the input at the hole
PieChart(categoryofcases, data = SarsCoV, hole = 0.5, main = NULL)
#boxplot
plot(x= SarsCoV$SEX,y = SarsCoV$Age)
#scatterplot
plot(x= SarsCoV$systolic1, y= SarsCoV$sugar_level)
hist(SarsCoV$Agee)

# # #
#-------------------------- charts with LessR ----------------------------------Option.2
library(lessR)
#
# Piechart
PieChart(marital_status, data = VDdata, hole = 0.5, main = NULL)
# Donut chart
PieChart(marital_status, data = VDdata, fill = "blues", hole_fill = "#B7E3E0", main = NULL)
# Barchart
BarChart(marital_status, data = VDdata, fill = "blues", main = NULL)
#colorviridis
BarChart(marital_status, data = VDdata, fill = "viridis", main = NULL, color = "black",lwd = 1.5,
         values_color = c(rep("white", 4), 1), values_size = 0.85)
# slant x labels (45 angle)
BarChart(AgeCategory, data = covid01, fill = "viridis", main = NULL, color = "black",lwd = 1.5,
         rotate_x=45, values_color = c(rep("white", 4), 1), values_size = 0.85)

#Histogram
hist(hhdata2$Age, col="gray", main="covid data", xlab="age")
hist(hhdata2$Age, col= "turquoise", main="Covid data", xlab="Age")
summary(hhdata2$Age)

# Boxplot
boxplot(Age ~ Gender, data = hhdata2)

# # #
#---------------------------- Using GGPLOT2 ------------------------------------Option.3
library(ggplot2)

#============================ Barchart  
#ggbarplot
ggbarplot(data=VDdata, x="marital Status", y="Age", fill ="sex", add = c("mean_sd"), 
          position = position_dodge(0.7), width = 0.5)+
  scale_fill_brewer(palette = "Blues")
#
ht <-ggplot(data = VDdata,
            mapping = aes(x = marital_status))+
  geom_bar()+
  geom_text(stat = "count", aes(label = after_stat(count)),
            vjust = -0.5)
theme_classic()
ht

#Stacked Barchart
ht2 <-ggplot(data = VDdata,
             mapping = aes(x = smoking, colour = Gender, fill = Gender))+
  geom_bar()+
  geom_text(stat = "count", aes(label = after_stat(count)),
            vjust = -0.5)+
  theme_classic()
ht2

#Clustered Barchart
ht3 <-ggplot(VDdata,
             mapping = aes(x = marital_status, y = Age,  fill = Gender)) +
  geom_col(position = "dodge") +
  theme_minimal()
ht3

#flip axis
ht4 <-ggplot(hhdata2,
             mapping = aes(x = marital_status, y = Age,  fill = Gender)) +
  geom_col(position = "dodge") +
  coord_flip()+
  theme_minimal()
ht4

#=========================== Histogram 

ht5 <-ggplot(data = hhdata2,
             mapping = aes(x = Age))+
  geom_histogram()

ht5

#Histogram with stacked
ht6 <-ggplot(data = VDdata,
             mapping = aes(x = Age, fill = Gender))+
  geom_histogram(bins = 10, position = "stack")

ht6

#Histogram with dodge
ht7 <-ggplot(data = VDdata,
             mapping = aes(x = Age, fill = Gender))+
  geom_histogram(bins = 10, position = "dodge")

ht7
#flip axis
ht8 <-ggplot(data = covid01,
             mapping = aes(x = Age, fill = SEX))+
  geom_histogram(bins = 10, position = "dodge")+
  coord_flip()

ht8

#============================= Boxplot 
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
imml <- ggplot(data = VDdata,
               mapping = aes(x = Gender, y = Age))+
  geom_boxplot(aes(fill = factor(smoking)))+
  labs(title ="Patient Data", subtitle = "smoking behavior",caption = "Data collection by Mohammed")+
  annotate( "text",x = 5,y = 70, label ="covid-19 patients", color ="purple",
            fontface ="bold", size =4.0,angle = 30)
imml


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

#---------------------------- Combo plots --------------------------------------Option.4
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
+                          Inferential Statistics                              +
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#--------------load required packages
library(readxl)
library(readr)
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

#----------------Import data
Sars_2 <- read_excel("C:/Users/User/Desktop/Sars-2.xlsx")
View(Sars_2)
print(Sars_2)
str(Sars_2)
-------------------------------------------------------------------------------
                     Data Conversion _(CODING)  
------------------------------------------------------------------------------- 
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

#--------- Explore the data by using descriptive statistics --------------------

summary(Sars_2)
#further insight into data normality
describeBy(Sars_2) 
# use normality test
shapiro.test(Sars_2$Age)
# Basic plots
hist(Sars_2$Age)

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
  
  theorical = 35
observed = SarsCoV$Age

t.test(observed,mu=theorical, conf.int=95.0 )# Option1 
#
t.test(SarsCoV$Age,mu=35, conf.int=95.0 )  #Option 2
hist(SarsCoV$Age)  



+++++++++++++++++ Two sample test of Mean ++++++++++++++++++++++++++++++++++++++
  #Independent t.test
  t.test(Age ~ SEX, data = SarsCoV, Var.equal =FALSE, conf.level = 0.95)  
#
hist(Age ~ SEX, data = SarsCoV, names=c("Female","Male"), ylab="Age")  
#
boxplot(Age ~ SEX, data = SarsCoV, names=c("Female","Male"), ylab="Age")

#Paried t.test
t.test(Sars_2$systolic1,Sars_2$systolic2,paired = TRUE,conf.level = 0.95) 
#

# # #
++++++++++++++++++++ More than Two sample (ANOVA) ++++++++++++++++++++++++++++++    
library(readxl)
library(lessR)
library(car)
library(psych)
library(ggplot2)
library(ggpubr)
library(AICcmodavg) 
library(DescTools)  
#
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#--------------------- One_Way ANOVA (Between group )---------------------------LessR
print(Sars_2)
#
Plot(Duration_days, data=SarsCoV, facet1 = categoryofcases)
#normality 
tapply(Sars_2$diastolic1, Sars_2$categoryofcases,shapiro.test)
#equality of variance
leveneTest(Duration_days ~ categoryofcases, data=Sars_2)
#One-way ANOVA
ANOVA(Duration_days ~ categoryofcases, data=Sars_2)
#Bar charts 
Duration_mean <- tapply(Sars_2$Duration_days, Sars_2$categoryofcases,mean)
BarChart(Duration_mean)
#
#--------------------- One_Way ANOVA (Between group )---------------------------Car
#
#Step1-----fit model for residuals
redd <- aov(Age ~ categoryofcases, data = SarsCoV) 
redd 
summary(redd)
#step2-----Check Normality
shapiro.test(redd$residuals)

#step3-----homogeneity of variance
leveneTest(Age ~ categoryofcases, data = SarsCoV)

#Step4----------plots Histogram & QQplot
par(mfrow = c(1,2))
#histogram
hist(redd$residuals)
#QQ-plot
QQ <- qqPlot(redd$residuals, id = TRUE)             

# # # 
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#----------------------- Factorial ANOVA ---------------------------------------
#
#homogeneity of variance multiple variable
leveneTest(Age ~ categoryofcases*SEX*smoking*TreatmentOUTCOME*SarsCov_Strain, data = SarsCoV) 
# #
#One_way Anova
one <- aov(sugar_level ~ marital_status, data = VDdata)
summary(one)
#Two_way Anova
two <- aov(sugar_level ~ marital_status + sex, data = VDdata)
summary(two)
#three_way Anova
three <- aov(sugar_level ~ marital_status + sex + smoking, data = VDdata)
summary(three)
#interaction
interaction <- aov(sugar_level ~ marital_status + sex + smoking*heart_rate, data = VDdata)
summary(interaction)

#model fit----------------------------------
library(AICcmodavg)
model.set <- list(one,two,three,interaction)
model.names <- c("one", "two","three", "interaction")
aictab(model.set, modnames = model.names, sort = TRUE)
bictab(model.set, modnames = model.names, sort = TRUE)

#post hoc analysis-------------------------
library(DescTools)
tukey.interaction <- TukeyHSD(interaction)
tukey.interaction
#
tukey.plot.aov <- aov(sugar_level ~ marital_status + sex + smoking, data = VDdata)
tukey.plot.test <-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test,las = 2)
# # #
------------------------------------------------------------------------------
  Tukey and LSD mean separation tests (pairwise comparisons)
TukeyHSD, HSD.test, and LSD.test are not appropriate for cases where there are unequal variances
though TukeyHSD does make an adjustment for mildly unequal sample sizes.
------------------------------------------------------------------------------
  
  
# # #
++++++++++++++++++++++ Kruskal–Wallis Test +++++++++++++++++++++++++++++++++++++
#------------------------------------------------------------------------------- Nonparametric
if(!require(FSA)){install.packages("FSA")
#-------------------------------------- kruskal.test(Value ~ Group, data = Data)
    library(psych)
    describe(imdata)
    
    #-Medians and descriptive statistics
    library(FSA)
    Summarize(age ~ expose, data = imdata)
    
    #-Kruskal–Wallis test
    kruskal.test(sugar_level ~ marital_status, data = VDdata)
    
    #-Dunn test for multiple comparisons(Post Hoc)
    The Dunn test is performed with the dunnTest function in the FSA package.  
    Adjustments to the p-values could be made using the method option to control the familywise error rate or 
    to control the false discovery rate.  
    # Dunn test methods--------“bonferroni”, “holm”,“sidak”, “hs”, “hochberg”, “bh”(Benjamini-Hochberg),“none”, “by”,  
    
    library(FSA)
    PT = dunnTest(sugar_level ~ marital_status, data = VDdata, method="bh")           
    PT
    
# #  #
    
++++++++++++++++++++++++++++ANOVA Within Groups +++++++++++++++++++++++++++++++
#------------------------- Repeated Measures ANOVA ---------------------------Parametric
      # Create a data frame
      wgdata <- data.frame(subject, time, response)
    
    # Perform Repeated-Measures ANOVA
    aov_model <- aov(response ~ time + Error(subject/time), data = wgdata)
    
    # View the summary of the ANOVA
    summary(aov_model) 
    
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#------------------------------ Friedman test --------------------------------Nonparametric
      library(PMCMRplus)
    
    result <- friedman.test(outcome ~ group, data = your_data)
    
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#-------------------------------- ANCOVA ------------------------------------#
      library(car)
    ancova_model <-aov(finalscore~group + initialscore, data=score)
    Anova(ancova_model,type="III")                           
    
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
    
    
# # #  
--------------------------------------------------------------------------------
+++++++++++++++++++++ ONE/TWO SAMPLE  PROPORTION +++++++++++++++++++++++++++++++  
#...............................................................................
#
0-Chi-square for proportions---------------------prop.test()#for one or more proportions
1-Exact Goodness-of-fit-------------------------binom.test()#whether difference to hypothised value
2-Chi-Square test of independence---------------chisq.test()#Test of association independence      
3-Fisher exact test----------------------------fisher.test()#N < 100, cell count<5 // Likelihood ratio chi-square
4-McNemars chisquare test---------------------mcnemar.test()#Paired categorical data
5-Mantel-Haenszel chisquare test-----------mantelhaen.test()#for stratified 2×2 tables (confounders)
6-Posthoc-chisquare of independence-----chisq.posthoc.test()#pairwise comparisons after Chi-square
#
#.................. Chisquare Data .........................

Proportions-#Decimal/Percentage
Count      -# whole number

#...........................................................
rm(list=ls())
gc(reset = TRUE)

#Load required packages.....................................
library(readxl)
library(tidyverse)
library(epitools)
library(XNomial)

#Import data.............................................
Sars_2 <- read_excel("C:/Users/User/Desktop/Sars-2.xlsx")
View(Sars_2)
print(Sars_2)
str(Sars_2)

-------------------------------------------------------------------------------
                        Data Conversion  
------------------------------------------------------------------------------- 
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

# # #  
----------------------- One Sample test of Proportion --------------------------
#Is the proportion of HIV in female =5.2% ?
      
prop1_table <-table(data$HIV)
prop1_table                         # E.g,where the output is row.25, column.143
prop1_table <- matrix(c(143, 18), ncol = 2)
#Therefore
prop.test(prop1_table, p = 0.052)
    
# # #
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
------------------------ Two Sample test of Proportion -------------------------
#Get table of sex by HIV status
      
table_status <- table(Gender = data$Sex, HIVstatus = data$HIV) 
table_status                        # E.g,where the output is column1-> 15, 143.column2-> 7, 23.
# Input results
table_status <- matrix(c(15, 143, 7, 23), ncol = 2)
colnames(table_status)<- c("Reactive","Non reactive")
rownames(table_status)<- c("Female","Male")
#
prop.test(table_status)
# # #    
#chisq.test(table_status)$expected

#fisher.test(table_status)
# # #
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
--------------------------- Test of Association --------------------------------
# Is gender associated with HIVstatus ?
      
#---Chisquare of Independence--------
# 
chsq_table <-table(data$sex,data$HIV)
chsq_table
#To add marginal totals
addmargins(chsq_table,margin = c(1,2))
#To get proportions
prop.table(chsq_table)
#To get percentages
prop.table(chsq_table)*100
#To round off values
round(prop.table(chsq_table)*100, 2)
#plot
barplot(prop.table(chsq_table)*100)
#
pie(table(data$SES), col = c("white","gray90","gray60"))#for 2x3 Table
# Perform chis-square
chisq.test(table(data$sex,data$HIV))
    
# --------------- Fisher's Exact Test ....................... for  cell count <2
fisher.test(table(data$sex,data$HIV))
    
# # #
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
---------------------------- Oddratio & riskratio ------------------------------                                 
library(epitools)
#------------ Oddratio
oddsratio(data$sex,data$HIV)
#           or
OR <-epitab(data$sex,data$HIV,method = "oddsratio",conf.level = 0.95)
OR
#----------- riskratio
riskratio(data$sex,data$HIV)
#           or
RR <-epitab(data$sex,data$HIV,method = "riskratio",conf.level = 0.95)
RR
    
# # #
mcnemar.test(Matriz, correct=FALSE) 

# # #
mcnemar.test(Data.xtabs, correct=FALSE)

# # #
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
----------------------- Exact Test of Goodness-of-Fit -------------------------- 
#To determine if the proportion of successes in a binary experiment differs from hypothesized probability.
binom.test()
#Where:
2 is the number of successes
10 is the number of trials
0.5 is the hypothesized probability of success
#
dbinom(2, 10, 0.5)   
#One.sided
binom.test(2, 10, 0.5, alternative="less", conf.level=0.95) 
#two.sided
binom.test(2, 10, 0.5, alternative="two.sided", conf.level=0.95)  # In most circumstances, the two-sided test is used.

#Probability Density plot-(binomial distribution)    
trials = 10                          #You can change the values for trials & prob // for xlab & ylab.
prob = 0.5                                                         

x = seq(0, trials)                                                     # x is a sequence, 1 to trials
y = dbinom(x, size=trials, p=prob)                                       # y is the vector of heights
#
barplot (height=y, names.arg=x,
         xlab="Number of uses of right paw",
         ylab="Probability under null hypothesis")

# # #     
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
------------------ Chi-square Test of Goodness-of-Fit --------------------------
#
observed = c(770, 230)        # observed frequencies
expected = c(0.75, 0.25)      # expected proportions

chisq.test(x = observed, p = expected)


# # #
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
--------------- Post-hoc with multinomial and binomial test --------------------
#post hoc tests
Follow up with post hoc tests (optional for Chi-Square)if there are more than two groups in
either of the variables with p<0.05, a post hoc test is conducted.using bonferroni correction.
#---------------
library(XNomial)
#
observed = c(72, 38, 20, 18)
expected = c(9, 3, 3, 1)
#
xmulti(observed, expected, detail = 2)         

# Reports three types of p-value
P value  (LLR)  =  0.003404  # log-likelihood ratio
P value (Prob)  =  0.002255  # exact probability
P value (Chisq) =  0.001608  # Chi-square probability

# # #
#Note last p-value below agrees with Handbook--------------------------
successes   = 72
total       = 148
numerator   = 9
denominator = 16

binom.test(successes, total, numerator/denominator, alternative="two.sided", conf.level=0.95) 

#Binomial test examples
#-----------------------------------------------------------------------
Parasitoid examples, exact binomial test
------------------------------------------------------------------------
  binom.test(10, (17+10), 0.5, alternative="two.sided", conf.level=0.95)
#-----------------------------------------------------------------------
Drosophila example, exact binomial test
------------------------------------------------------------------------
  binom.test(140, (106+140), 0.5, alternative="two.sided", conf.level=0.95)

# # #
#------------------------- multinomial exact test -----------------------------
observed = c(315, 108, 101, 32)
expected = c(9, 3, 3, 1)

library(XNomial)
xmulti(observed, expected, detail = 2)              
# reports three types of p-value      
P value  (LLR)  =  0.9261    # log-likelihood ratio
P value (Prob)  =  0.9382    # exact probability
P value (Chisq) =  0.9272    # Chi-square probability


# # #
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
--------------------------------- Correlation ----------------------------------
#-------------------------------------------------------------------------------
Correlation
Correlation can be performed with the cor.test function in the native stats package.  
It can perform Pearson, Kendall, and Spearman correlation procedures.  
  
#Pearson correlation (Parametric)
Pearson correlation is a parametric test, and assumes that the data are linearly related 
and that the residuals are normally distributed.
#
cor.test( ~ age + bmi, data=imdata, method = "pearson", conf.level = 0.95)
  
#Kendall correlation (Non-parametric)
Kendall rank correlation is a non-parametric test that does not assume a distribution of the data.
It ranks the data to determine the degree of correlation.
#
cor.test( ~ age + bmi, data=imdata, method = "kendall", continuity = FALSE, conf.level = 0.95)
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
Spearman rank correlation is a non-parametric test that does not assume a distribution of the data.
It ranks the data to determine the degree of correlation, and is appropriate for ordinal measurements.
#
cor.test( ~ age + bmi, data=imdata, method = "spearman", continuity = FALSE, conf.level = 0.95)
  
# # #
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                            Regression analysis                               +
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Regression analysis study the relationship between variables:
    
1-By identifying (Linear, Curvilinear, or Quadratic).
2-By estimating parameters of the relationships(intercept(B0) and slope(B1)).
3-By validating the relationship (The  assumptions for regression).
  
#The process of finding the line equation (Best fit the data values)------------regression line(linear)
#Linear equation-------------------------------------------------------------- y= B0 + B1x
#B0(y intercept)--------what is y when x = 0 ?
#B1(slope line)---------how much is y changes, for each unit increase in x, 
Note,
1,The equation gives pedicted value y-hat(y) at a given x.
2,Each value will not fall exactly on the line of best fit: the difference---residual/error. y(observed)-y-hat(predicted)
3,Least ordinary square(OLS)--it tries to minimise the difference between the observed and the fitted values by 
determining the slope and the intercept.
  
# B0 and B1 are the Least Square Estimates (LSE)
  
# B0 = y - B1x  ---------------- where x(mean),y(mean). -------------------     B1 = Sy
  ----
 # R squared = 0 - 1,---------R squared = SSE,        Rsquared = 1 - SSR.             Sx
    ----                       ----
    #                                        SSR                        SST                 
    
R squared (coefficient of determination)-------------useful for model building___(fit model for multiple regression).  
thus it tells us the amount of variables explain in the reponse(y) after fitting the model.
  
#R squared = explain variation of model-y
----------------------------
  #              Total variation of model                                         df =n-2
    
The **broom** package has a function called `augment()` which can calculate:
    
  1. estimated log odds 
  2. probabilities
  2. residuals
  3. hat values
  4. Cooks distance
  5. standardized residuals
  
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++      
#---------------------- Checking assumptions of the model ----------------------
Assumptions
#-y is a random variable and normally distributed with mean(u) and variance(Q)squared.
#The unknown standard error (residual) are independent normally distributed. mean = 0, variance squared.
  
 hist(residuals(model), col="darkgray")
  
 plot(fitted(model), residuals(model))
#
  A plot of residuals vs. predicted values.  #The residuals should be unbiased & homoscedastic.
  
#Note,
R squared = good for simple regression. 
Adjusted R squared =for multiple regression (model building)
#
model = lm(Species ~ Latitude, data = Data)
#shows parameter estimates, p-value for model, r-square
summary(model)                    
  
library(car)
#shows p-value for effects in model
Anova(model, type="II")              
  
#Plot linear regression
int = model$coefficient["(Intercept)"]
slope = model$coefficient["Latitude"]
plot(Species ~ Latitude, data = Data, pch=16, xlab = "Latitude", ylab = "Species")
abline(int, slope, lty=1, lwd=2, col="blue")     #  style and color of line 
  
# # #   
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
------------------------- Curvilinear Regression -------------------------------
#
How to fit models to curvilinear data using three methods:
#    
1) Polynomial regression;  
2) B-spline regression with polynomial splines;  
3) Nonlinear regression with the nls function.  

Each of these three will find essentially the same best-fit curve with very similar p-values and R-squared values. 

# create sample data 
sample_data <- data.frame(x=1:10, 
                          y=c(25, 22, 13, 10, 5, 9, 12, 16, 34, 44)) 
View(sample_data)
print(sample_data)

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
------------------------- Polynomial regression --------------------------------
#
Polynomial regression is really just a special case of multiple regression, 
#Simple plot of model
library(ggplot2)
ggplot(imdata,aes(x =age, y = bmi))+geom_point(size = 4, shape = 20,colour = "Black")+
  stat_smooth(method = lm, se = FALSE, formula = y~poly(x,3), colour = "Green")+
  stat_smooth(method = lm, se = FALSE, formula = y~poly(x,2), colour = "Red")

# # #
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

# # #
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


# # #
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                             Poisson  Regression                              + #Log-linear model 
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
