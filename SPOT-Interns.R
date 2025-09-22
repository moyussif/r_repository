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

#---------filter rows ?filter()
Hanisah <- remove_more %>% select(c(Motherage,Mother_bloodgroup,Mother_G6PD,Mode_of_delivery,Baby_age_days,Baby_sex,
                                    Baby_bloodgroup,Baby_G6PD,Baby_Weight,Baby_gestational_age,Babyfood,Diagnosis2,Diagnosis3)) %>% 
                            filter(Motherage < 40)
print(Hanisah)

#---------summarise()
Hanisah1 <- Hanisah %>% summarise(sum_Age=sum(Motherage))
print(Hanisah1)
Hanisah2 <- Hanisah %>% summarise(mean_Age=mean(Motherage,na.rm = TRUE))
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
names(Hanisah)[5]<- "Baby_Age"
names(Hanisah)[10]<- "Baby_Gest_Age"
str(Hanisah)


#---------Create New column (mutate)--------------------------------------------
mutate_colmn <- rename_colmns %>% mutate(bmi=weight/height*2)
#---------show only New column (transmute)
transmutate_col <- mutate_colmn %>% transmute(bmi)
#---------Change character type(rename_with)
Upper_caps <- rename_colmns %>% rename_with(Mother_group,toupper)
Lower_caps <- rename_colmns %>% rename_with(DeliveryMode,tolower)

###

#-------------------- Reshaping Data-(LONG.data / WIDE.data) ----------------------------------------
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


#-------------------------- Data Conversion ------------------------------------
str(Hanisah)

#-------Continuous data
Hanisah$mum_Age <-as.integer(Hanisah$mum_Age)
Hanisah$Baby_Age <-as.integer(Hanisah$Baby_Age)
Hanisah$Baby_Weight <-as.integer(Hanisah$Baby_Weight)
Hanisah$Baby_Gest_Age <-as.integer(Hanisah$Baby_Gest_Age)

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

#   #   #

#--------------------------- Handling Missing Data --------------------------------------------------
# Count missing values in each column
missing_per_column <- colSums(is.na(Hanisah))
print(missing_per_column)

# Remove rows with any missing values 
Cleaned_Hanisah <- na.omit(Hanisah)
print(Cleaned_Hanisah)

Check_Clean_Hanisah <- colSums(is.na(Cleaned_Hanisah))
print(Check_Clean_Hanisah)

# # 
#--------------------------- Data Normality ----------------------------------- Skewness=Mean - Median -----------
#                                                                                        -------------
skewness(-1+1)---- statistic + 1.96   |   Kurtosis(1-3)---- statistic + 1.96    #             Sd
--------                                  ---------                                       
 SE                                         SE                                                  
#POS_skewed = Mean > Median/Mode         NEG_skewed = Mean < Median/Mode        LOW  = Q1-1.5(IQR)        
#                                                                               HIGH = Q3+1.5(IQR)
library(psych)
library(readxl)

skew(Cleaned_Hanisah$Baby_Age, na.rm = TRUE)
kurtosi(Cleaned_Hanisah$Baby_Age, na.rm = TRUE)
mardia(Cleaned_Hanisah$mum_Age,na.rm = TRUE)

#---------------------------Transforming skewed data--------------------------------------------------------------

#Square root Transformation-------------------------moderately skewed (-1 to -0.5//0.5 to 1)
sq_data <-sqrt(Cleaned_Hanisah$Baby_Age)
hist(sq_data)
#Cube root Transformation---------------------------moderately Right skewed(Negative//Zero)
cb_data <-sign(Cleaned_Hanisah$Baby_Weight)*abs(Cleaned_Hanisah$Baby_Weight)^(1/3)
hist(cb_data)
#Log Transformation---------------------------------Highly skewed(above1),But not (Negative//Zero)
lg_data <-log(Cleaned_Hanisah$Baby_Age)
hist(lg_data)

#     #     #
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                            Descriptive statistics                            
library(skimr)
skim_without_charts(Cleaned_Hanisah)
str(Cleaned_Hanisah)
library(psych)
summary(Cleaned_Hanisah)
describeBy(Cleaned_Hanisah)

#-----------------Confidence interval of the mean
t.test(Cleaned_Hanisah$mum_Age,
       conf.level=0.95)

#   #   #

#---------------------------- Data Visualization -------------------------------
#-----------------Pie Chart 
library(lessR)
slices <- c(35, 47, 64)
lbls <- c( "No NNJ", "NNJ", "NNJ & Others")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct)
# add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Diagnostic outcomes")
# 3D Exploded Pie Chart
library(plotrix)
slices <- c(35, 47, 64)
lbls <- c( "No NNJ", "NNJ", "NNJ & Others")
pie3D(slices,labels=lbls,explode=0.1,
      main="Diagnostic outcomes")

#----------------Donut chart
install.packages("webr")

install.packages(                                                
  "webr",
  repos = c("http://rstudio.org/_packages",                      
            "http://cran.rstudio.com")                           
)

library(webr)
# Pie-Donut chart
PieDonut(Cleaned_Hanisah, aes(Baby_sex, Delivery_mode ), title = "DeliverybyGender")
#Explode Pie
PieDonut(Cleaned_Hanisah, aes(Baby_sex, Delivery_mode ), title = "DeliverybyGender", explode = 2, explodeDonut=FALSE)
#Explode Donut for Control
PieDonut(Cleaned_Hanisah, aes(Baby_sex, Delivery_mode ), title = "DeliverybyGender", explode = 2, explodeDonut=TRUE)
#Explode Donut for Case
PieDonut(Cleaned_Hanisah, aes(Baby_sex, Delivery_mode ), title = "DeliverybyGender", explode = 1, explodeDonut=TRUE)

#-------------plotting points
library(ggplot2)
immu <- ggplot(data = Cleaned_Hanisah,
               mapping = aes(x = Baby_Weight, y = Baby_Age, colour = Baby_sex))+
  geom_point()+theme_classic()
immu
#creating density plot 
immu1 <- ggplot(data = Cleaned_Hanisah,
                mapping = aes(x = Baby_Weight, colour = Baby_sex))+
  geom_density()
immu1
#point and smoothing
immu2 <- ggplot(data = Cleaned_Hanisah,
                mapping = aes(x = Baby_Weight, y = Baby_Age, colour = Delivery_mode))+
  geom_smooth()+
  geom_point()
immu2

#-----------------Barchart
ht <-ggplot(data = Cleaned_Hanisah,
            mapping = aes(x = Baby_G6PD))+
  geom_bar()
ht
#Stacked Barchart
ht1 <-ggplot(data = Cleaned_Hanisah,
             mapping = aes(x = Baby_G6PD, colour = Baby_sex, fill = Baby_sex))+
  geom_bar()
ht1
#Stacked Barchart
ht2 <-ggplot(data = Cleaned_Hanisah,
             mapping = aes(x = Baby_G6PD, colour = Baby_sex, fill = Baby_sex))+
  geom_bar()+
  theme_classic()
ht2

#----------------Histogram:
Histogram
hist(Cleaned_Hanisah$mum_Age, col="gray", main="Maternal Age", xlab="Age")


ht3 <-ggplot(data = Cleaned_Hanisah,
             mapping = aes(x = mum_Age))+
  geom_histogram()
ht3

#Histogram with stacked
ht <-ggplot(data = Cleaned_Hanisah,
            mapping = aes(x = mum_Age, fill = Delivery_mode))+
  geom_histogram(bins = 10, position = "stack")
ht

#Histogram with dodge
ht <-ggplot(data = Cleaned_Hanisah,
            mapping = aes(x = mum_Age, fill = Delivery_mode))+
  geom_histogram(bins = 10, position = "dodge")
ht
#------------------------Viridis-(Histogram)
hh <-ggplot(data = Cleaned_Hanisah,
            mapping = aes(x = mum_Age, fill = Delivery_mode))+
  geom_histogram(bins = 10, position = "dodge")
hh + scale_fill_viridis_d()
hh + scale_fill_viridis_d(direction = -1)
hh

#--------------------------Boxplot:
immu <- ggplot(data = Cleaned_Hanisah,
               mapping = aes(x = Baby_sex, y = Baby_Weight))+
  geom_boxplot()
immu
# Boxplot by category ##
immu <- ggplot(data = Cleaned_Hanisah,
               mapping = aes(x = Baby_sex, y = Baby_Weight, color = Baby_sex))+
  geom_boxplot()
immu

#Boxplot with coord_flip
immu <- ggplot(data = Cleaned_Hanisah,
               mapping = aes(x = Baby_sex, y = Baby_Weight, color = Baby_sex))+
  geom_boxplot()+coord_flip()
immu 

#plotting by factor
immu <- ggplot(data = Cleaned_Hanisah,
               mapping = aes(x = Baby_sex, y = Baby_Weight))+
  geom_boxplot(aes(fill = factor(Babyfood)))
immu

# Facet wrap 
immwrp <- ggplot(data = Cleaned_Hanisah,
               mapping = aes(x = Baby_sex, y = Baby_Weight, fill = Diagnosis3))+
  geom_violin()+
  facet_wrap(~Diagnosis3)
immwrp
# Facet grid
immwrp <- ggplot(data = Cleaned_Hanisah,
                 mapping = aes(x = Baby_sex, y = Baby_Weight, fill = Baby_G6PD))+
  geom_violin()+
  facet_grid(~Diagnosis3)
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
======================================================================================================
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                        TEST OF ONE / TWO SAMPLES                                                
  
library(readxl)
library(psych)
library(lessR)

Neonate <- read_csv("C:/Users/User/Desktop/NNJ.csv")                            
View(Neonate)
str(Neonate)
##
summary(Cleaned_Hanisah)
describeBy(Cleaned_Hanisah)  
#-----------------One sample t-test
observed    = Cleaned_Hanisah$mum_Age
theoretical = 20
hist(Cleaned_Hanisah$mum_Age, col="gray", main="Histogram of Mother's Age", xlab="Age")

t.test(observed, mu = theoretical, conf.int=0.95) 

#------------------Two Samples Student’s t–test 
#paired t.test  
t.test(mum_Age ~ Diagnosis2, data=Cleaned_Hanisah, var.equal=TRUE, conf.level=0.95)
#independent t.test
t.test(mum_Age ~ Diagnosis2, data=Cleaned_Hanisah, var.equal=FALSE, conf.level=0.95)

# Histogram
hs<-hist(mum_Age ~ Diagnosis2, col="gray",data=Cleaned_Hanisah)
hs
# Boxplot
boxplot(mum_Age ~ Diagnosis2, data=Cleaned_Hanisah, names=c("No_NNJ","NNJ"), ylab="age")

#------------- Mann–Whitney Test
wilcox.test(mum_Age ~ Diagnosis2, data=Cleaned_Hanisah, exact = FALSE)
wilcox.test(mum_Age ~ Diagnosis2, data=Cleaned_Hanisah, exact = TRUE)

#Box plots
boxplot(mum_Age ~ Baby_Gest_Age, data=Cleaned_Hanisah, names=c("preterm","Fullterm"), ylab="age")

#   #   #

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#------------------------- One-way Anova using LessR -------------------option.1
#visualise statistical assumptions

library(readxl)
library(lessR)
print(Cleaned_Hanisah)
#plot
Plot(mum_Age, data=Cleaned_Hanisah, by1=Diagnosis3)
#normality assumption
tapply(Cleaned_Hanisah$mum_Age, Cleaned_Hanisah$Diagnosis3, shapiro.test)
library(car)
leveneTest(mum_Age ~ Diagnosis3, data=Cleaned_Hanisah)

#One-way ANOVA
ANOVA(mum_Age ~ Diagnosis3, data=Cleaned_Hanisah)

#effect size(for groups with significant)
library(effsize)
cohen.d(mum_Age ~ Diagnosis3, data=subset(Cleaned_Hanisah, Diagnosis3!= "No NNJ"), paired=FALSE)        
cohen.d(mum_Age ~ Diagnosis3, data=subset(Cleaned_Hanisah, Diagnosis3!= "NNJ"),paired=FALSE)        

#Bar charts 
age_means <- tapply(Cleaned_Hanisah$mum_Age, Cleaned_Hanisah$Diagnosis3)

BarChart(age_means)
BarChart(age_means, values="off", bxlab = "Diagnostic outcome", ylab = "Women Age")

#   #    #

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                             Chi square                                        +
  
table(Cleaned_Hanisah$Baby_sex,Cleaned_Hanisah$Diagnosis2)
chisq.test(Cleaned_Hanisah$Baby_sex,Cleaned_Hanisah$Diagnosis2)
# Perform Fisher's Exact Test
fisher.test(Cleaned_Hanisah$Baby_sex,Cleaned_Hanisah$Diagnosis2)

# # # 


++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#-------------------------------- Correlation ----------------------------------
  
#Pearson correlation (Parametric)
cor.test( ~ Baby_Age + Baby_Weight, data=Cleaned_Hanisah, method = "pearson", conf.level = 0.95)
#Spearman correlation (Non-parametric)
cor.test( ~ Baby_Age + Baby_Weight, data=Cleaned_Hanisah, method = "spearman", continuity = FALSE, conf.level = 0.95)
  
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

=================================================================================
                                 Ayoola data
_________________________________________________________________________________

cngTB <- read_excel("C:/Users/User/Desktop/MTB only_ Lineage.xlsx")
str(cngTB)

table(cngTB$Lineage,cngTB$AgeCategory)
table(cngTB$Lineage,cngTB$Gender)
table(cngTB$Lineage,cngTB$B5Marital_Status)
# Perform Fisher's Exact Test
fisher.test(cngTB$Lineage,cngTB$Gender, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$AgeCategory, simulate.p.value = TRUE)
fisher.test(cngTB$Lineage,cngTB$B5Marital_Status, simulate.p.value = TRUE)

# Perform Chi-square Test
chisq.test(cngTB$Lineage,cngTB$AgeCategory, simulate.p.value = TRUE)

=================================================================================
+                            Multinomial regression                             +
================================================================================= 
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

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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

#--------------------------- Simple Logistic Regression ------------------------ 
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

