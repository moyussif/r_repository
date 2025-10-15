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
library(ggfortify)
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
table(Hanisah6$SEX,Hanisah6$Durationweeks)
table(Hanisah6$Hospitalstatus,Hanisah6$SEX)

chisq.test(Hanisah6$Hospitalstatus,Hanisah6$SEX)

# Perform Fisher's Exact Test
fisher.test(Hanisah6$SEX,Hanisah6$Hospitalstatus)
# # #   
oddsratio(Hanisah6$SEX,Hanisah6$Hospitalstatus)

riskratio(Hanisah6$SEX,Hanisah6$Hospitalstatus)  
  
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
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
# Bartlett’s test and Levene’s test Homogeneity of variance 
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

#==========================  
 
  
  
  
  #plot
  plot(Hanisah17$Noofsymptoms,Hanisah17$Durationdays,
       main = "Age by duration",
       xlab = "MOther Age",
       ylab = "Baby Age",
       pch=19,col="blue")
  # plot with ggplot2
  ggplot(Hanisah17, aes(x = Durationdays, y = Noofsymptoms)) +
    geom_point(color = "blue")
  
#Matrix plots
    install.packages("GGally")
  
  library(psych)
  library(ggplot2)
  library(GGally)   
  library(readxl)
  EGF01 <- read_excel("C:/Users/User/Desktop/EGF01.xlsx")
  print(EGF01)
  GGally::ggcorr(Hnsh)
  GGally::ggpairs(Hnsh)
  psych::pairs.panels(Hnsh)
  
================================================================================ 
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                           Regression analysis                                +

  model = lm(Noofsymptoms ~ Durationdays, data = Hanisah17)
  summary(model)                    
  
  
  #    #    #   

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
============================  Linear Regression ================================
library(ggfortify)
Hanisah15 <- read_excel("C:/Users/User/Desktop/covid02.xlsx")
str(Hanisah15)

#fit model--------simple Regression
model = lm(Durationdays ~  Age, data = Hanisah15)
model
summary(model)

#regression Assumption
autoplot(model)

#Modelling-------- Multiple Regression 
mode7 = lm(Durationdays ~  Age+SEX+Hospitalstatus+categoryofcases+Noofsymptoms+Coinfection+
             NoofResistance+Numberoforganism, data = Hanisah15)
mode7
summary(mode7)

library(olsrr)
#--------model building  (Backward)
Backward1 <- ols_step_backward_aic(mode7, details = TRUE)
Backward1
###
forward <- ols_step_forward_p(mode7, penter = 0.05)
forward                                               # forward
forward <- ols_step_forward_aic(mode7, details = TRUE)
forward
###
Both <- ols_step_both_p(g, pent = 0.05, prem = 0.05)
Both                                                 # Stepwise
Both.aic <- ols_step_both_aic(g, details = TRUE)
Both.aic
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                             LOGISTIC REGRESSION                              +
--------------------------------------------------------------------------------  
  Hanisah8 <- read_excel("C:/Users/User/Desktop/covid02.xlsx")
str(Hanisah8)
# Count missing values in each column
Hanisah77 <- colSums(is.na(Hanisah7))
print(Hanisah77)

# Remove rows with any missing values 
Hanisah77 <- na.omit(Hanisah8)
print(Hanisah77)

str(Hanisah77)
#c=Convert to factors
Hanisah77$SarsCovStrain <- as.factor(Hanisah77$SarsCovStrain)
Hanisah77$categoryofcases <- as.factor(Hanisah77$categoryofcases)

str(Hanisah77)

#------------------------ Simple Logistic Regression ---------------------------

#categoricals
table(Hanisah77$SarsCovStrain, Hanisah77$categoryofcases)
#continuous
logis2<- glm(SarsCovStrain~categoryofcases,data = Hanisah77,family = "binomial")
logis2
summary(logis2)
#Oddratio
exp(coef(logis2))

all <- exp(cbind(OR = coef(logis2), confint(logis2)))
print(all)

#----------------------- Multiple Logistics regression 
multi.logis1 <-glm(SarsCovStrain~Age+categoryofcases+Coinfection+Durationdays,
                   data = Hanisah77, family = "binomial")
multi.logis1
summary(multi.logis1)
#Oddratio
exp(coef(logis1))

all <- exp(cbind(OR = coef(multi.logis1), confint(multi.logis1)))
print(all)

#Note_______in case we want to reorder /change the reference group, Use relevel
change_ref <-relevel(Hanisah77$categoryofcases, ref = "moderate")

logist2 <- glm(CaseControl ~ parity, data = imdata, family = "binomial" )
logist2

#   #   #
=================================================================================
                      Chisquare - Fisher Exact
_________________________________________________________________________________

cngTB <- read_excel("C:/Users/User/Desktop/MTB only_ Lineage.xlsx")
str(cngTB)

table(cngTB$Lineage,cngTB$Gender)
# Perform Fisher's Exact Test
fisher.test(cngTB$Lineage,cngTB$Gender, simulate.p.value = TRUE)
# Perform Chi-square Test
chisq.test(cngTB$Lineage,cngTB$Gender, simulate.p.value = TRUE)



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


