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
#-------------------------- Load packages
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
HHdata <- read_excel("hhdata.xlsx")

healthData <- read_csv("health_data.csv") 
hhdata <- read_excel("C:/Users/User/Downloads/hhdata.xlsx")
#-------------------------- Import functions                                    Option.2
VDdata <- read_excel("C:/Users/User/Downloads/VDDdata.xlsx") #.xlsx format


healthd <- read.csv("health_data.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE) #.csv format


#------------------- Understand Data Structures---------------------------------                            

str(hhdata)   #data structure
print(hhdata) #view data in console

--------------------------------------------------------------------------------
                       Data Conversion _(CODING)                          Option 1
--------------------------------------------------------------------------------
 
#-------Continuous data
hhdata$age <-as.integer(hhdata$age)

#-------Categorical data
#

hhdata$sex <- as.factor(hhdata$sex)

hhdata$marital_status <- as.factor(hhdata$marital_status)

hhdata$smoking <- as.factor(hhdata$smoking)

str(hhdata)
print(hhdata)

hhdata2 <- hhdata %>% select(-wealth_index)

#---------Create New column (mutate)
hhdata2$BMI <- hhdata2 %>% mutate(bmi=weight/height*2)

str(hhdata2)



+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                           Data Wrangling                        
#The processes of *transforming* or *manipulating* raw data into a useful format 
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(dplyr)                                                 
#---------Selecting column 
#columns_needed <- hhdata %>% select(c(age, sex, marital_status, height, weight, heart_rate, smoking, cholesterol, sugar_level,
 #                                      BP_systolic, BP_diastolic, exercise_score_hrs, wealth_index))
#str(columns_needed)
#----------Remove a column
#hhdata2 <- hhdata %>% select(-wealth_index)
#str(hhdata2)

#--Remove multiple columns
#remove_more <- remove_one %>% select(-c(BP_systolic, BP_diastolic))
#str(remove_more)

#---------filter rows ?filter()
#hhdata2 <- remove_more %>% select(c(age, sex, marital_status, height, weight, heart_rate, smoking, cholesterol, sugar_level,
#                                    exercise_score_hrs)) %>% 
 # filter(age > 30) 
                                                                                #  or   filter(hhdata2, age <= 30)
#View(hhdata2)

#
#hhdata2 %>% count(marital_status) %>% filter(marital_status == "Married")

&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
hhdata2 <- hhdata %>% select(-wealth_index)
str(hhdata2)
#---------Create New column (mutate)
hhdata2$BMI <- hhdata2 %>% mutate(bmi=weight/height*2)

str(hhdata2)
#

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
(Julius)
#
Check_Clean_data <- colSums(is.na(Julius))
print(Check_Clean_data)

# # #
--------------------------------------------------------------------------------
                     CHECKING FOR DATA NORMALITY           
--------------------------------------------------------------------------------
#POS_skewed = Mean > Median/Mode         NEG_skewed = Mean < Median/Mode        LOW  = Q1-1.5(IQR)        
#                                                                               HIGH = Q3+1.5(IQR)
library(psych)

describeBy(Julius)

hist(Julius$Age, margin = FALSE)


#  #  #
--------------------------------------------------------------------------------
                     Transforming skewed data
--------------------------------------------------------------------------------
#Square root Transformation---------------------------------------moderately skewed (-1 to -0.5//0.5 to 1)
sq_data <-sqrt(hhdata$age)
hist(sq_data)
#Cube root Transformation-----------------------------------------moderately Right skewed(Negative//Zero)
cb_data <-sign(Hanisah$Baby_Weight)*abs(Hanisah$Baby_Weight)^(1/3)
hist(cb_data)
#Log Transformation---------------------------------------Highly skewed(above1),But not (Negative//Zero)
hhdata$age <-log(hhdata$age)
hist(hhdata$age)

# # #
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                        
                            Descriptive statistics
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(psych)
describeBy(Hanisah30)
library(skimr)
str(Hanisah30)
skim_without_charts(hhdata2)
# # #
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                           Data Visualizations                                +
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
#------------------------- Basic Visualizations --------------------------------Option.1
# Histograms
hist(hhdata2$Age)
# Boxplot
boxplot(age~ sex, data = VDdata)
#Scatter plots
hhdata_plt<-hhdata2%>%filter(Age>30)

                  exercise<-hhdata2$exercise_score_hrs
                  cholestro<-hhdata2$cholesterol

plot(exercise, cholestro)

#we can use the `with` function
with(hhdata_plt, plot( exercise, cholestro))


#Confidence interval of the mean
t.test(hhdata2$heart_rate,
       conf.level=0.95)       

#-------------------------- charts with LessR ----------------------------------Option.2
library(lessR)

covid01 <- read_excel("C:/Users/User/Desktop/covid01.xlsx")

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
#---------------------------- Using GGPLOT2 ------------------------------------Option.2
library(ggplot2)

#============================= Barchart  =======================================
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
                                                                                #covid01$SEX <- factor(covid01$SEX)
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

#=========================== Histogram =========================================

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

# # #
+++++++++++++++++++++++++ One sample test of Mean ++++++++++++++++++++++++++++++
#-------------------------------------------------------------------------------
observed    = VDdata$age
theoretical = 46

t.test(observed, mu = theoretical, conf.int=0.95)                      #Option.1

t.test(VDdata$age, mu = 46, conf.int=0.95)                            #Option.2

#plots
hist(VDdata$age, col="green", main="Histogram of values", xlab="Age")
#
boxplot(VDdata$age, col="green", main="Histogram of values", xlab="Age")
#  #  #

+++++++++++++++++++++++++++ Two Samples Mean +++++++++++++++++++++++++++++++++++
#-------------------------------------------------------------------------------

#independent t.test
t.test(age ~ sex, data=VDdata, var.equal=FALSE, conf.level=0.95)

#paired t.test  
t.test(age ~ sex, data=VDdata, var.equal=TRUE, conf.level=0.95)

#Histogram
hist(age ~ sex, data=VDdata, names=c("Female","Male"), ylab="Age")
# Boxplot
boxplot(age ~ sex, data=VDdata, names=c("Female","Male"), ylab="Age")
#

#========================= Non parametric ======================================
#------- Mann–Whitney Test
wilcox.test(age ~ sex, data=VDdata, exact = FALSE)

#------ Wilcoxon Signed-rank Test
wilcox.test(Data$August, Data$November, paired=TRUE)

#Box plots
boxplot(age ~ sex, data=VDdata, names=c("Female","Male"), ylab="Age")


# # #

+++++++++++++++++++++++++++++ANOVA Between Groups ++++++++++++++++++++++++++++++
#----------------------------- One_Way ANOVA  ----------------------------------Parametric
library(lessR)
library(readxl)

#normality assumption
tapply(SarsCoV$Duration_days, SarsCoV$categoryofcases, shapiro.test)

library(car)
leveneTest(age ~ marital_status, data=VDdata)
#One-way ANOVA
ANOVA(age ~ marital_status, data=VDdata)

age_means <- tapply(VDdata$age, VDdata$marital_status,mean)
#
BarChart(age_means)

# # # 
#---------------------------  Factorial Anova ----------------------------------Parametric
library(psych)
library(ggplot2)
library(ggpubr)
library(readxl)
describeBy(VDdata)

#residuals
res_marital <- aov(age ~ marital_status, data = VDdata)
res_marital
summary(res_marital)
#combine plots
par(mfrow = c(1,2))
#histogram
hist(res_marital$residuals)
#QQ-plot
library(car)
QQ <- qqPlot(res_marital$residuals, id = TRUE)  #id=TRUE to remove point identification
#
shapiro.test(res_marital$residuals) 

#homogeneity of variance one variable
leveneTest(sugar_level ~ marital_status, data = VDdata)
#homogeneity of variance multiple variable
leveneTest(sugar_level ~ marital_status*sex*smoking, data = VDdata) 

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

tukey.plot.aov <- aov(sugar_level ~ marital_status + sex + smoking, data = VDdata)
tukey.plot.test <-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test,las = 2)

# # #
----------------------------------------------------------
Tukey and LSD mean separation tests (pairwise comparisons)
TukeyHSD, HSD.test, and LSD.test are not appropriate for cases where there are unequal variances
though TukeyHSD does make an adjustment for mildly unequal sample sizes.
----------------------------------------------------------
  
  
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
  
  #   #   #
  
  ++++++++++++++++++++++++++++ANOVA Within Groups +++++++++++++++++++++++++++++++
  #------------------------- Repeated Measures ANOVA ---------------------------Parametric
  # Create a data frame
  wgdata <- data.frame(subject, time, response)
  
  # Perform Repeated-Measures ANOVA
  aov_model <- aov(response ~ time + Error(subject/time), data = wgdata)
  
  # View the summary of the ANOVA
  summary(aov_model) 
  
  ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #------------------------------ Friedman test --------------------------------Nonparametric
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

# # #  
  
======================== ONE/TWO SAMPLE  PROPORTION ============================
    
    
#------------------ Chi-square Test of Goodness-of-Fit -------------------------
  
  
# # #
#---------------------- Chisquare of Independence ------------------------------
# 
table(VDdata$sex,VDdata$smoking)
  
chisq.test(VDdata$sex,VDdata$smoking, simulate.p.value = TRUE)
  
  #------------------------ Fisher's Exact Test ----------------------------------
  table(cngTB$Lineage,cngTB$AgeCategory)
  
  fisher.test(VDdata$sex,VDdata$smoking, simulate.p.value = TRUE)
  
  #plot
  ht2 <-ggplot(data = VDdata,
               mapping = aes(x = smoking, colour = sex, fill = sex))+
    geom_bar()+
    theme_classic()
  ht2
  
  #--------------------------- mcnemar.test --------------------------------------
  #mcnemar.test(df$before, df$after)                   
  
  mcnemar.test(VDdata$sex,VDdata$smoking)
  
  
  #-------------------------- mantelhaen.test ------------------------------------
  #exposure__ROW
  #outcome___COLUMN
  #stratum___CONFOUNDER
  
  mantelhaen.test(outcome ~ exposure + stratum, data = df)
  
  
  
  #======================== OddRatio - Riskratio =================================
  library(epitools)
  #chisquare
  table(imdata$CaseControl,imdata$sex)
  chisq.test(imdata$CaseControl,imdata$sex)
  
  ################################################################################
  odds ratio // Risk ratio                                                       #
  method = c("oddsratio", "riskratio", "rateratio"),    #
  rev = c("neither", "rows", "columns", "both"),     #         
  oddsratio = c("wald", "fisher", "midp", "small"),        #
  riskratio = c("wald", "boot", "small"),                  #
  rateratio = c("wald", "midp"),                           #
  pvalue = c("fisher.exact", "midp.exact", "chi2"),     #
  correction = FALSE, verbose = FALSE)                      #
################################################################################

#--------------------------------------- Odd ratio
oddsratio(imdata$CaseControl,imdata$sex)       
#                   or 
OR <-epitab(imdata$CaseControl,imdata$sex,
            method = "oddsratio",
            conf.level = 0.95)
OR
#--------------------------------------- risk ratio
riskratio(imdata$CaseControl,imdata$sex)  
#                  or
RR <-epitab(imdata$CaseControl,imdata$sex,
            method = "riskratio",
            conf.level = 0.95)
RR

  
  
  
    
 
   # # #
  ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  +                               Correlation                                  +
  ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
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
  
  library(ggpubr)
  #CorrelationPlot----------ggscatter() 
 
  ggscatter(Hanisah90, x = "Noofsymptoms", y = "Durationdays",
            add = "reg.line",                                 # Add regression line
            conf.int = TRUE,                                  # Add confidence interval
            add.params = list(color = "blue",
                              fill = "lightgray"))+
    stat_cor(method = "pearson", label.x = 3, label.y = 30)   # Add correlation coefficient   
 
  #Spearman correlation (Non-parametric / ordinals)
    Spearman rank correlation is a non-parametric test that does not assume a distribution of the data.
  It ranks the data to determine the degree of correlation, and is appropriate for ordinal measurements.
  --------
    cor.test( ~ age + bmi, data=imdata, method = "spearman", continuity = FALSE, conf.level = 0.95)
  
  
# # #
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                            Regression analysis                                +
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
  
# # #   
#------------------------------------------------------------------------------ 
---------------------------- Multiple Regression ------------------------------ 
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
+                           Poisson  Regression                                +#Log-linear model
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

#-------------------------------------------------------------------------------
++++++++++++++++++++++++ Multinomial regression ++++++++++++++++++++++++++++++++
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


# # #
