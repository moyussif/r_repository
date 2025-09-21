rm(list=ls())
gc(reset = TRUE)
#--------------------------- required package

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
library(FSA)
library(Hmisc)
library(stats)
library(epitools)
library(VGAM)
library(mlogit)
library(nnet)

#---------------------------- Import functions 
#set directorate 
library(dplyr)
TrainingData <- read_excel("C:/Users/User/Desktop/TrainingData.xlsx")
View(TrainingData)

print(TrainingData)

str(TrainingData)

#------------------------ Data Manipulation with Dplyr -------------------------

#---------Selecting column
Hanisah <- TrainingData %>% select(c(Age, Gender,NewReligion,OccupationCLASS,Education,
                                   SmearCount,Age_Categories))

#----------Remove a column
Hanisa <- Hanisah %>% select(-Age)
print(Hanisa)

#--Remove multiple columns
Hanis <- Hanisah %>% select(-c(Age, NewRegion))
print(Hanis)

#---------rename a column
#Data %>% rename(NewColumn=OldColumn)
Hanidd<-Hanisah %>% rename(Sex=Gender)
print(Hanidd)

#---------Create New column (mutate)
Hanisah <- TrainingData %>% mutate(bmi=height/weight)

#---------show only New column (transmute)
Hanisah <- TrainingData %>% transmute(bmi)

#---------Change character type(rename_with)
Hanisah <- TrainingData %>% rename_with(Education,toupper)
Hanisah <- TrainingData %>% rename_with(Education,tolower)

#---------filter rows
Hanisah <- TrainingData %>% select(c(Age, Gender,NewReligion,Education,
                                     filter(Age>18)))

#---------summarise()
Hanisah <- TrainingData %>% summarise(sum_Age=sum(Age))
Hanisah <- TrainingData %>% summarise(sum_Age=sum(Age,na.rm = TRUE))
Hanisah <- TrainingData %>% summarise(sum_Age=sum(Age),Mean_Age=mean(Age)

#---------group_by()
Hanisgroup <-TrainingData %>%
               group_by(Gender) %>%
               summarise(mean = mean(Age)))
###

#--------------------Reshaping Data-(LONG.data / WIDE.data)---------------------
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


#-------------------------- Data Transformation --------------------------------

#--------- Data Conversion 
str(Anita)

Hanisah$Age <-as.integer(Hanisah$Age)

Anita$Sex <-factor(Anita$Sex,
                        levels = c(1,2),
                        labels = c("Male", "Female"))
Anita$NewReligion <-factor(Anita$NewReligion,
                   levels = c(1,2,4,5,99),
                   labels = c("Afric", "Christinity","Islam", "Other","NA"))
str(Anita)


judith$Gravidity <- as.integer(judith$Gravidity)
judith$Parity <- as.integer(judith$Parity)
#----------------------------------------------------------------
judith$Status <-factor(judith$Status,
                                  levels = c(1,2),
                                  labels = c("control", "case"))


judith$ModeOfDelivery <-factor(judith$ModeOfDelivery,
                        levels = c(1,2),
                        labels = c("Vaginal", "Caesarean"))

judith$bld_grp <-factor(judith$bld_grp,
                            levels = c(1,2,3,4),
                            labels = c("A positive", "B positive", "AB positive", "O positive"))
str(judith)
print(judith)
#   #   #

#--------------------------- Handling Missing Data -----------------------------
# Count missing values in each column
missing_per_column <- colSums(is.na(judith))
print(missing_per_column)

# Remove rows with any missing values 
judith <- na.omit(judith)
print(judith)

judith <- colSums(is.na(judith))
print(judith)

# Perform mean imputation for the 'salary' column where NA values are present
mean_salary <- mean(data$Salary, na.rm = TRUE)


# # 
#--------------------------- Data Normality ----------------------------------- Skewness = Mean - Median
#                                                                                          -------------
skewness(-1+1)---- statistic + 1.96   | Kurtosis(1-3)---- statistic + 1.96      #              Sd
--------                                ---------                                       
 SE                                       SE                                                  
#_____________________________________________________________________________# Q1-1.5(IQR) ____________
#POS_skewed = Mean > Median/Mode      | NEG_skewed = Mean < Median/Mode        # Q3+1.5(IQR)        
##
library(psych)
library(readxl)

skew(imdata$age, na.rm = TRUE)
kurtosi(imdata$age, na.rm = TRUE)
mardia(imdata$age,na.rm = TRUE)
#------------------------------Transforming skewed data-------------------------

#Square root Transformation ==== moderately skew (-1 to -0.5 // 0.5 to 1 )
sq_data <-sqrt(imdata$bmi)
hist(sq_data)
#Cube root==== moderately----right skewed can apply to negative and zero values
cb_data <-sign(imdata$age)*abs(imdata$age)^(1/3)
hist(cb_data)
#Log Transformation====highly skewed (above 1),Cannot be applied negative/zero values
lg_data <-log(imdata$systol1)
hist(lg_data)

#-----------------Confidence interval of the mean
t.test(imdata$ bwgt,
       conf.level=0.95)         

#     #     #

#---------------------------- Data Visualization -------------------------------
#-----------------Pie Chart 
slices <- c(18, 35, 47, 64)
lbls <- c( "None", "Low", "Medium", "High")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct)
# add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of exposure")
# 3D Exploded Pie Chart
library(plotrix)
slices <- c(18, 35, 47, 64)
lbls <- c( "None", "Low", "Medium", "High")
pie3D(slices,labels=lbls,explode=0.1,
      main="Pie Chart of exposure ")

#----------------Donut chart
install.packages("webr")
library(webr)
# Pie-Donut chart
PieDonut(imdata, aes(CaseControl, expose ), title = "Expose status")
#Explode Pie
PieDonut(imdata, aes(CaseControl, expose ), title = "Expose status", explode = 2, explodeDonut=FALSE)
#Explode Donut for Control
PieDonut(imdata, aes(CaseControl, expose ), title = "Expose status", explode = 2, explodeDonut=TRUE)
#Explode Donut for Case
PieDonut(imdata, aes(CaseControl, expose ), title = "Expose status", explode = 1, explodeDonut=TRUE)

#-------------plotting points 
immu <- ggplot(data = imdata,
               mapping = aes(x = systol1, y = diastol1, colour = CaseControl, shape = expose))+
  geom_point()+theme_classic()
immu
#creating density plot 
immu1 <- ggplot(data = mdata,
                mapping = aes(x = diastol1, colour = CaseControl))+
  geom_density()
immu1
#point and smoothing
immu2 <- ggplot(data = imdata,
                mapping = aes(x = bmi, y = plt, colour = nicu))+
  geom_smooth()+
  geom_point()
immu2

#-----------------Barchart
ht <-ggplot(data = imdata,
            mapping = aes(x = expose))+
  geom_bar()
ht
#column Barchrt
ht1 <-ggplot(data = ARMdata,
             mapping = aes(x = Gender, colour = ParasitePresence, fill = ParasitePresence))+
  geom_bar()
ht1
#stacked Barchart
ht2 <-ggplot(data = ARMdata,
             mapping = aes(x = AgeGroup, colour = ParasitePresence,fill = ParasitePresence))+
  geom_bar()+
  theme_classic()
ht2

#----------------Histogram:
ht3 <-ggplot(data = imdata,
             mapping = aes(x = systol1))+
  geom_histogram()
ht3

#Histogram with stacked
ht <-ggplot(data = imdata,
            mapping = aes(x = systol1, fill = expose))+
  geom_histogram(bins = 10, position = "stack")
ht

#Histogram with dodge
ht <-ggplot(data = imdata,
            mapping = aes(x = systol1, fill = expose))+
  geom_histogram(bins = 10, position = "dodge")
ht
#------------------------Viridis-(Histogram)
hh <-ggplot(data = imdata,
            mapping = aes(x = systol1, fill = expose))+
  geom_histogram(bins = 10, position = "dodge")
hh + scale_fill_viridis_d()
hh + scale_fill_viridis_d(direction = -1)
hh

#--------------------------Boxplot:
immu <- ggplot(data = amdata,
               mapping = aes(x = gender, y = estimated_parasitemia))+
  geom_boxplot()
immu
# Boxplot by category ##
immu <- ggplot(data = amdata,
               mapping = aes(x = gender, y = estimated_parasitemia, colour = target))+
  geom_boxplot()
immu

#Boxplot with coord_flip
immu <- ggplot(data = imdata,
               mapping = aes(x = systol1, y = diastol1, colour = expose))+
  geom_boxplot()+ coord_flip()
immu

#plotting by factor
immu <- ggplot(data = imdata,
               mapping = aes(x = systol1, y = diastol1))+
  geom_boxplot(aes(fill = factor(nicu)))
immu

# Facet wrap 
immwrp <- ggplot(data = imdata,
                 mapping = aes(x = systol1, y = diastol1, colour = grvdty))+
  geom_violin()+
  facet_wrap(~expose)
immwrp
-------------------------------------------------------------------------------------
  
  ht2 <-ggplot(data = ARMdata,
               mapping = aes(x = Gender, colour = AgeGroup,fill = AgeGroup))+
  geom_bar()+
  labs(title ="Parasite Across 10 Site ", subtitle = "Parasitemia estimation across age and gender",caption = "By Parasitology Team")+
  annotate( "text",x = 145,y = 90, label =" Dynamic of Parasite Infection", color ="purple",
            fontface ="bold", size =4.0,angle = 30)+
  theme_bw()+scale_colour_viridis_d()+
  facet_wrap(~study_site)  
ht2

p <- ggplot(ARMdata, aes(x=study_site, color = ParasitePresence, fill=ParasitePresence,xlab="Angle"))+geom_bar(alpha = 0.8)+theme_update()+scale_color_jama()
p
p1 <- ggplot(ARMdata, aes(x=AgeGroup, color = ParasitePresence, fill=ParasitePresence))+geom_bar(alpha = 0.8)+theme_test()
p1
p2 <- ggplot(ARMdata, aes(x=Gender, color = ParasitePresence, fill = ParasitePresence))+geom_bar(alpha = 0.8)+theme_update()
p2
p2 <- PieDonut(ARMdata, aes(ParasitePresence, AgeGroup ), title = "Expose status")

#to arrange plot for publication
ggarrange(p, p1, p2 + rremove("x.text"), labels = c("A", "B", "c"), ncol = 1, nrow = 3,
          common.legend = TRUE, legend = "bottom")

-------------------------------------------------------------------------------------
  
  
  
  # Facet grid
  immgrd <- ggplot(data = amdata,
                   mapping = aes(x = gender, y =age, colour = estimated_parasitemia))+
  geom_violin()+
  facet_grid(estimated_parasitemia~age)
immgrd

#-------------------Label & Annotation
immuLAB <- ggplot(data = imdata)+
  geom_point(mapping = aes(x = systol1, y = diastol1, colour = CaseControl, shape = expose))+
  labs(title ="Immunoassay Data", subtitle = "ImmunoAnalysis",caption = "Data collection by Mohammed")+
  annotate( "text",x = 145,y = 90, label ="Malaria in pregnancy                              ", color ="purple",
            fontface ="bold", size =4.0,angle = 30)
immuLAB
#  Alternative
immuLAB <- ggplot(data = imdata)+
  geom_point(mapping = aes(x = systol1, y = diastol1, colour = CaseControl, shape = expose))+
  labs(title ="Immunoassay Data", subtitle = "ImmunoAnalysis",caption = "Data collection by Mohammed")+
  annotate( "text",x = 145,y = 90, label ="Malaria in pregnancy                              ", color ="purple",
            fontface ="bold", size =4.0,angle = 30)+
  theme_classic()
immuLAB

#----------------------Viridis ---
immuLAB <- ggplot(data = imdata)+
  geom_point(mapping = aes(x = systol1, y = diastol1, colour = expose))+
  labs(title ="Immunoassay Data", subtitle = "ImmunoAnalysis",caption = "Data collection by Mohammed")+
  annotate( "text",x = 145,y = 90, label ="Malaria in pregnancy                              ", color ="purple",
            fontface ="bold", size =4.0,angle = 30)+
  theme_classic()+scale_colour_viridis_d()
immuLAB
#--------------viridis(City\nCenter)
immuLAB <- ggplot(data = ARMdata)+
  geom_point(mapping = aes(x = systol1, y = diastol1, colour = expose))+
  labs(title ="Immunoassay Data", subtitle = "ImmunoAnalysis",caption = "Data collection by Mohammed")+
  annotate( "text",x = 145,y = 90, label ="Malaria in pregnancy", color ="purple",
            fontface ="bold", size =4.0,angle = 30)+
  theme_classic()+scale_colour_viridis_d(option = "City\nCenter")
immuLAB

#------------------- Error Bars 

install.packages(
  "DMwR",
  repos = c("http://rstudio.org/_packages",
            "http://cran.rstudio.com")
)

library(magrittr)
library(readxl)
library(tidyverse)
library(ggplot2)
library(ggsci)
library(ggpubr)
setwd("C:/Users/User/OneDrive - University of Ghana/myComputer@space/repos")
vkdata <- read_excel("VMB-1.xlsx")
print(vkdata)

# Default bar plot
p<- ggplot(vkdata, aes(x=Sex_frequency, y=AGE, fill=Abnormal_discharge)) + 
  geom_bar(stat="identity", color="grey", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=AGE-sd(AGE), ymax=AGE+sd(AGE)), width=.2,
                position=position_dodge(.9))+
  theme_light()+
  scale_colour_aaas()+
  labs(title="number of affairs ", x="Frequency of sex", y = "Age")

print(p)

# Save plot 
ggsave("p.png")




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

======================================================================================================================
+                            Multinomial regression                              +++++++++++++++++++++++++++++++++++++
======================================================================================================================  
library(VGAM)
cngTB <- read_excel("C:/Users/User/Desktop/All_lineages.xlsx")
str(cngTB)
act <-remove_missing(cngTB)
str(act)
describeBy(act)
print(act)
View(act)
# lineag <- factor(cngTB$Lineage)
#fit model
datafrme <-as.data.frame(judith)
vglm(Lineage~DiabetesMellitus+BodyWeight+HIV+AIDS+Treatmentforrheumatoidarthritis+G7_Medicaltreatment_corticosteroids+
       Cancer+Kidneydisease+Silicosis+Leukemia+Substanceabuse+Smokingcigarette+BCG+countSmear,
     family = multinomial,
     data = datafrme)

+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
cngTB <- read_excel("C:/Users/User/Desktop/All_lineages.xlsx")
str(cngTB)
act <-remove_missing(cngTB)  
#---------------------------- Explore data -------------------------------------                                  
#Missing Data Summary
summary(cngTB)

# Count missing values in each column
missing_per_column <- colSums(is.na(cngTB))
print(missing_per_column)

# Remove rows with any missing values 
judith <- na.omit(cngTB)
print(judith)

judith <- colSums(is.na(judith))
print(judith)
str(judith)

#--------------------------------- Data Conversion 
cngTB$Gender <-factor(cngTB$Gender,
                      levels = c(1,2),
                      labels = c("Male", "Female"))
##
cngTB$AgeCategory <-factor(cngTB$AgeCategory,
                           levels = c(1,2,3,4,5),
                           labels = c("Below 18", "18 - 29", "30 - 44", "45 - 60", "Above 60"))
##
cngTB$Marital_Status <-factor(cngTB$Marital_Status,
                              levels = c(1,2,3,4),
                              labels = c("single", "married","divorced","widowed"))
##
cngTB$Education <-factor(cngTB$Education,
                         levels = c(1,2,3,4,5),
                         labels = c("primary", "junior high","senior high","tertiary","none"))
##
cngTB$occupationClass <-factor(cngTB$occupationClass,
                               levels = c(1,2,3,4,5),
                               labels = c("unemployed", "student","skilled","unskilled","unknown"))
##
cngTB$ResidenceClassification <-factor(cngTB$ResidenceClassification,
                                       levels = c(1,2,3,4,5),
                                       labels = c("village", "town","residential","suburb","slum"))
##
cngTB$MonthlyIncome <-factor(cngTB$MonthlyIncome,
                             levels = c(1,2,3,4,5,6),
                             labels = c("none", "500 & below","501-1000","1001-1500","1501-2500", "Above 2500"))
##-------------------------------------------------------------------------------------------------------------

cngTB$Coughgt2wk <-factor(cngTB$Coughgt2wk,
                          levels = c(1,2),
                          labels = c("yes", "no"))
cngTB$Coughphlegm <-factor(cngTB$Coughphlegm,
                           levels = c(1,2),
                           labels = c("yes", "no"))
cngTB$bloodysputum <-factor(cngTB$bloodysputum,
                            levels = c(1,2),
                            labels = c("yes", "no"))
cngTB$Fever <-factor(cngTB$Fever,
                     levels = c(1,2),
                     labels = c("yes", "no"))
cngTB$weightloss <-factor(cngTB$weightloss,
                          levels = c(1,2),
                          labels = c("yes", "no"))
cngTB$SwollenGlnds <-factor(cngTB$SwollenGlnds,
                            levels = c(1,2),
                            labels = c("yes", "no"))
cngTB$Shortnessofbreath <-factor(cngTB$Shortnessofbreath,
                                levels = c(1,2),
                                labels = c("yes", "no"))
cngTB$Night_Sweats <-factor(cngTB$Night_Sweats,
                            levels = c(1,2),
                            labels = c("yes", "no"))
cngTB$Chestpain <-factor(cngTB$Chestpain,
                         levels = c(1,2),
                         labels = c("yes", "no"))
cngTB$weakness <-factor(cngTB$weakness,
                        levels = c(1,2),
                        labels = c("yes", "no"))

cngTB$Recurringchills <-factor(cngTB$Recurringchills,
                               levels = c(1,2),
                               labels = c("yes", "no"))
cngTB$Lossofappetite <-factor(cngTB$Lossofappetite,
                               levels = c(1,2),
                               labels = c("yes", "no"))
#----------------------------------------------------------------------------------------------------------------
cngTB$familymembercoughing <-factor(cngTB$familymembercoughing,
                                    levels = c(1,2),
                                    labels = c("yes", "no"))
cngTB$samehousewithTBpatient <-factor(cngTB$samehousewithTBpatient,
                                      levels = c(1,2),
                                      labels = c("yes", "no"))
cngTB$Share_Room <-factor(cngTB$Share_Room,
                          levels = c(1,2),
                          labels = c("yes", "no"))

cngTB$E1_Haveyoueverworkedinalabthat <-factor(cngTB$E1_Haveyoueverworkedinalabthat,
                          levels = c(1,2),
                          labels = c("yes", "no"))

cngTB$aresidentinajailpris <-factor(cngTB$aresidentinajailpris,
                          levels = c(1,2),
                          labels = c("yes", "no"))

cngTB$workedinahospital <-factor(cngTB$workedinahospital,
                          levels = c(1,2),
                          labels = c("yes", "no"))
cngTB$workediNAursinghome <-factor(cngTB$workediNAursinghome,
                          levels = c(1,2),
                          labels = c("yes", "no"))
cngTB$livedinrefugeecamp <-factor(cngTB$livedinrefugeecamp,
                          levels = c(1,2),
                          labels = c("yes", "no"))
cngTB$workinvolvesand <-factor(cngTB$workinvolvesand,
                          levels = c(1,2),
                          labels = c("yes", "no"))

cngTB$workinvolvesmoke <-factor(cngTB$workinvolvesmoke,
                               levels = c(1,2),
                               labels = c("yes", "no"))

cngTB$manyinthesameroom <-factor(cngTB$manyinthesameroom,
                                levels = c(1,2),
                                labels = c("yes", "no"))
cngTB$workwithcattle <-factor(cngTB$workwithcattle,
                                 levels = c(1,2),
                                 labels = c("yes", "no"))

cngTB$DiabetesMellitus <-factor(cngTB$DiabetesMellitus,
                              levels = c(1,2),
                              labels = c("yes", "no"))
cngTB$HIV <-factor(cngTB$HIV,
                   levels = c(1,2),
                   labels = c("yes", "no"))

cngTB$AIDS <-factor(cngTB$AIDS,
                   levels = c(1,2),
                   labels = c("yes", "no"))

cngTB$Substanceabuse <-factor(cngTB$Substanceabuse,
                             levels = c(1,2),
                             labels = c("yes", "no"))

cngTB$Smokingcigarettee <-factor(cngTB$Smokingcigarette,
                              levels = c(1,2),
                              labels = c("yes", "no"))

cngTB$BCG  <-factor(cngTB$BCG,
                    levels = c(1,2),
                    labels = c("yes", "no"))

str(cngTB)
print(cngTB)
#   #   #

#--------------------------- Simple Logistic Regression ------------------------------------------------------------- 
logistic <- glm(CaseControl ~ age, data = imdata, family = "binomial" )
summary(logistic)
exp(0.05512)
exp(coef(logistic))

#----------------------- Multiple Logistics regression 
multi_logist <- glm(CaseControl ~ age + bmi + parity, data = imdata, family = "binomial" )
summary(multi_logist)



summary(model)
class(judith)

all <- exp(cbind(OR = coef(model), confint(model)))
print(all)

