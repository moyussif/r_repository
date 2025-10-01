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
#---------------------------
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


#-------------------------- Data Conversion ------------------------------------
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
#=============================================
==============================================
# #  #
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

library(writexl)
write_xlsx(Cleaned_Hanisah, "Aziz.xlsx")

#--------------------------- Data Normality ----------------------------------- Skewness=Mean - Median -----------
library(psych)
library(readxl) 

Hanisah <- read_excel("C:/Users/User/Desktop/Aziz.xlsx") 

#  #  #


-------------
skewness(-1+1)---- statistic + 1.96   |   Kurtosis(0-3)---- statistic + 1.96    #             Sd
--------                                  ---------                                       
 SE                                         SE                                                  
#POS_skewed = Mean > Median/Mode         NEG_skewed = Mean < Median/Mode        LOW  = Q1-1.5(IQR)        
#                                                                               HIGH = Q3+1.5(IQR)

skew(Hanisah30$mum_Age)
kurtosi(Hanisah30$Baby_Age, na.rm = TRUE)
mardia(Hanisah30$Baby_Age)

#---------------------------Transforming skewed data--------------------------------------------------------------

#Square root Transformation-------------------------moderately skewed (-1 to -0.5//0.5 to 1)
sq_data <-sqrt(Hanisah$Baby_Weight)
hist(sq_data)
#Cube root Transformation---------------------------moderately Right skewed(Negative//Zero)
cb_data <-sign(Hanisah$Baby_Weight)*abs(Hanisah$Baby_Weight)^(1/3)
hist(cb_data)
#Log Transformation---------------------------------Highly skewed(above1),But not (Negative//Zero)
Hanisah30$lg_mum_age <-log(Hanisah30$mum_Age)
hist(lg_data)

#     #     #
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                            Descriptive statistics                            
library(skimr)
str(Hanisah)
skim_without_charts(Hanisah)

library(psych)
describeBy(Hanisah)


#-----------------Confidence interval of the mean
t.test(Hanisah$Baby_Weight,
       conf.level=0.95)

#   #   #

#---------------------------- Data Visualization -------------------------------

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

#----------------Donut chart
install.packages("webr")

install.packages(                                                
  "webr",
  repos = c("http://rstudio.org/_packages",                      
            "http://cran.rstudio.com")                           
)

library(webr)
# Pie-Donut chart
PieDonut(Hanisah, aes(Baby_sex, Delivery_mode ), title = "DeliverybyGender")
#Explode Pie
PieDonut(Cleaned_Hanisah, aes(Baby_sex, Delivery_mode ), title = "DeliverybyGender", explode = 2, explodeDonut=FALSE)
#Explode Donut for Control
PieDonut(Cleaned_Hanisah, aes(Baby_sex, Delivery_mode ), title = "DeliverybyGender", explode = 2, explodeDonut=TRUE)
#Explode Donut for Case
PieDonut(Cleaned_Hanisah, aes(Baby_sex, Delivery_mode ), title = "DeliverybyGender", explode = 1, explodeDonut=TRUE)

# # #

#-------------plotting points---------------------------------GGPLOT2
Hanisah25 <- read_excel("C:/Users/User/Desktop/Aziz.xlsx")


ggplot(data = DATASET,
       mapping = aes(x = COLUMN, y = COLUMN, colour, fill, alpha))+
?geom()


library(ggplot2)

Hanis1 <- ggplot(data = Hanisah25,
               mapping = aes(x = Baby_Weight, y = Baby_Age))+
  geom_point()+
  geom_smooth() +
  theme_minimal() 

Hanis1
#creating density plot 
immu1 <- ggplot(data = Hanisah100,
                mapping = aes(x = Baby_Weight, colour = Baby_sex))+
  geom_density()
immu1
#point and smoothing
immu2 <- ggplot(data = Hanisah25,
                mapping = aes(x = Baby_Weight, y = Baby_Age, colour = Delivery_mode))+
  geom_smooth()+
  geom_point()
immu2

#-----------------Barchart
ht <-ggplot(data = Hanisah25,
            mapping = aes(x = Baby_G6PD, fill = Baby_G6PD))+
  geom_bar()+theme_classic()
ht
#Stacked Barchart
ht1 <-ggplot(data = Hanisah25,
             mapping = aes(x = Baby_sex, fill = Diagnosis3))+
  geom_bar()
ht1
#Stacked Barchart
ht2 <-ggplot(data = Hanisah25,
             mapping = aes(x = Baby_G6PD, colour = Baby_sex, fill = Baby_sex))+
  geom_bar()+
  theme_classic()
ht2

#----------------Histogram:

ht3 <-ggplot(data = Hanisah25,
             mapping = aes(x = mum_Age))+
  geom_histogram()
ht3

#Histogram with stacked
ht <-ggplot(data = Hanisah25,
            mapping = aes(x = mum_Age, fill = Delivery_mode))+
  geom_histogram(bins = 10, position = "stack")
ht

#Histogram with dodge
ht <-ggplot(data = Hanisah25,
            mapping = aes(x = mum_Age, fill = Delivery_mode))+
  geom_histogram(bins = 2.5, position = "dodge")
ht
#------------------------Viridis-(Histogram)
hh <-ggplot(data = Hanisah100,
            mapping = aes(x = mum_Age, fill = Delivery_mode))+
  geom_histogram(bins = 10, position = "dodge")
hh + scale_fill_viridis_d()
hh + scale_fill_viridis_d(direction = -1)
hh

#--------------------------Boxplot:
immu <- ggplot(data = Hanisah25,
               mapping = aes(x = Baby_sex, y = Baby_Weight))+
  geom_boxplot()
immu
# Boxplot by category ##
immu <- ggplot(data = Hanisah25,
               mapping = aes(x = Baby_sex, y = Baby_Weight, fill = Baby_sex))+
  geom_boxplot()
immu

#Boxplot with coord_flip
immu <- ggplot(data = Hanisah25,
               mapping = aes(x = Baby_sex, y = Baby_Weight, color = Baby_sex))+
  geom_boxplot()+coord_flip()
immu 

#plotting by factor
immu <- ggplot(data = Hanisah25,
               mapping = aes(x = Baby_sex, y = Baby_Weight))+
  geom_boxplot(aes(fill = factor(Babyfood)))
immu
=======================================================================================
# Facet wrap 
immwrp <- ggplot(data = Hanisah25,
               mapping = aes(x = Baby_sex, y = Baby_Weight, fill = Diagnosis3))+
  geom_violin()+
  facet_wrap(~Diagnosis3)
immwrp
======================================================================================

facet

# Facet grid
immwrp <- ggplot(data = Hanisah25,
                 mapping = aes(x = Baby_sex, y = Baby_Weight, fill = Baby_G6PD))+
  geom_violin()+
  facet_grid(~Diagnosis3)
immwrp




#-------------------------------- Combos --------------------------------------
library(RColorBrewer)
par()

 
p <- ggplot(data = Hanisah25,
            mapping = aes(x = Baby_sex, y = Baby_Weight, fill = Baby_sex))+
      geom_boxplot()+
      theme_update()
p
p1 <- ggplot(data = Hanisah25,
             mapping = aes(x = Baby_sex, fill = Diagnosis3))+
        geom_bar()+
        theme_test()
p1
p2 <- ggplot(data = Hanisah25,
             mapping = aes(x = mum_Age, fill = Delivery_mode))+
        geom_histogram(bins = 5, position = "dodge")+theme_update()
p2


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
  scale_colour_aaas()+ Hanisah19 <- read_excel("C:/Users/User/Desktop/immudata.xlsx")

  labs(title="number of affairs ", x="Frequency of sex", y = "Age")

print(p)

# Save plot 
ggsave("p.png")


  #   #    #
Hanisah19 <- read_excel("C:/Users/User/Desktop/immudata.xlsx")
print(Hanisah19)
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #                             Chi square                                        +
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
table(Hanisah19$`PE-CaseControl`,Hanisah19$mode)
chisq.test(Hanisah19$`PE-CaseControl`,Hanisah19$mode)
# Perform Fisher's Exact Test
fisher.test(Hanisah19$`PE-CaseControl`,Hanisah19$mode)

# # #   
  
  
  
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                        TEST OF ONE / TWO SAMPLES                                                
 
library(psych)
library(lessR)
##
summary(Hanisah25)
describeBy(Hanisah25)  

#-----------------One sample t-test
observed    = Hanisah19$age
theoretical = 5

t.test(observed, mu = theoretical, conf.int=0.95)  


hist(Cleaned_Hanisah$mum_Age, col="gray", main="Histogram of Mother's Age", xlab="Age")



#------------------Two Samples Student’s t–test 
#==============PARAMETRIC============================================
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

#NON -PARAMETRIC=========================================================
#------------- Mann–Whitney Test ---Independent test
wilcox.test(mum_Age ~ Baby_sex, data=Hanisah30, exact = FALSE)
#--------------Wilcoxon sign Test---Paired test
wilcox.test(lg_mum_age ~ Diagnosis2, data=Hanisah30, exact = TRUE)

#Box plots
boxplot(mum_Age ~ Baby_Gest_Age, data=Cleaned_Hanisah, names=c("preterm","Fullterm"), ylab="age")


#   #   #

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#------------------------- One-way Anova using LessR -------------------option.1
#visualise statistical assumptions

library(readxl)
library(lessR)
library(car)

str(Hanisah19)



#plot
Plot(Age, data=Hanisah17, by1=categoryofcases)

#Normality assumption
library(lessR)
tapply(Hanisah17$Age, Hanisah17$categoryofcases, shapiro.test)
library(car)
QQ <- qqPlot(res_aov$residuals, id = TRUE)  #id=TRUE to remove point identification
shapiro.test(res_aov$residuals) #normality

# Bartlett’s test and Levene’s test 
-----------------------------------
leveneTest(mum_Age ~ Diagnosis3, data=Hanisah30)
leveneTest(mum_Age ~ Diagnosis3*Delivery_mode*Baby_sex*Baby_Gest_Age, data=Hanisah30) 

bartlett.test(mum_Age ~ Diagnosis3, data=Hanisah30)
bartlett.test(mum_Age ~interaction(Delivery_mode,Baby_sex,Baby_Gest_Age), data=Hanisah30) 

#One-way ANOVA
ANOVA(Durationdays ~ categoryofcases, data=Hanisah17)

#effect size(for groups with significant)
library(effsize)
cohen.d(Durationdays ~ categoryofcases, data=subset(Hanisah17, categoryofcases!= "mild"), paired=FALSE)        
cohen.d(Durationdays ~ categoryofcases, data=subset(Hanisah17, categoryofcases!= "severe"),paired=FALSE)        

#Bar charts 
age_means <- tapply(Hanisah17$Durationdays, Hanisah17$categoryofcases)

BarChart(age_means)

******************************************************************************



#===================== ANOVA ANALYSIS----option.2 =============================
==========================  Factorial Anova =================================== 
library(psych)

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
one <- aov(Durationdays ~ categoryofcases, data = Hanisah17)
summary(one)
#postHoc
tukey.one <- TukeyHSD(one)
tukey.one
summary(tukey.one)
plot(tukey.one,las = 2)

#Two_way Anova
two <- aov(bmi ~ exposed + grvdty, data = Hanisah19)
summary(two)
#three_way Anova
three <- aov(bmi ~ exposed + grvdty + sex, data = Hanisah19)
summary(three)

#interaction
interaction <- aov(bmi ~ exposed + grvdty + sex + grvdty*sex, data = Hanisah19)
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


#--------------------------- Kruskal–Wallis Test -----------------------------------------------
if(!require(FSA)){install.packages("FSA")
#---------------------------------------------- kruskal.test(Value ~ Group, data = Data)
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






=========================================================================================



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

