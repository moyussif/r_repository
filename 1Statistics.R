
rm(list=ls())
gc(reset = TRUE)
#set directorate
setwd("C:/Users/User/OneDrive - University of Ghana/myComputer@space/repos")
#-------------------------------------------------------------------------------
Note ---- <= or >= #to get this symbols, SHIFT < or > and click =.
#---------------------------- Import functions ---------------------------------
child_data1 <- read.csv("children_data.csv", stringsAsFactors = TRUE)         # for categorical
library(readr)
all_data2 <- read_csv(file = 'children_data.csv')                               
library(readxl)
imdata <- read_excel("C:/Users/User/Desktop/repos/immunoData.xlsx")
library(rio)
import("C:/Users/User/Desktop/repos/NNJ.csv")
#--------------------------- EXport function  
library(writexl)
write_xlsx(table1, "eigentable.xlsx")

#--------------------------- Reshape data  
library(tidyr) 
setwd("C:/Users/User/OneDrive - University of Ghana/myComputer@space/repos")
HTdata <- read_excel("HTdata.xlsx")
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

#-------------------------- Data Conversion 
#Categorical data
sex <- as.factor(data$gender)
salarylevel <-if else(data$salary < 5000,c("Low"),c("High"))
SES <- as.factor(data$salary)
#Continuous data
Age <- as.integer(data$age)
weight <- as.integer(data$bwgt)
Income <- as.integer(data$salary)
#Date to time series
tdata$Date = as.Date(tdata$Date, format = "%Y/%m/%d")
hhdata = ts(tdata$attendance,start = min(tdata$Date), end = max(tdata$Date),frequency = 1)

#   #   #
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  When packages are loadings fails ----use the codes below
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  
install.packages(                                                
  "VIM",
  repos = c("http://rstudio.org/_packages",                      
            "http://cran.rstudio.com")                           
)                                                                
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#  #  #
  
  
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                                     Data flow Dplyr                                                  +
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
library(tidyverse)
library(dplyr)
data2 <- imdata %>%
  select(CaseControl, expose, age,delivage, hb, plt, parity, bmi,-id ) %>% 
  filter(hb > 10)        
print(data2)

#  #  #
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                                 HANDLING MISSING DATA                                               +
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
setwd("C:/Users/User/OneDrive - University of Ghana/myComputer@space/repos")
imdata <- read_excel("immunoData.xlsx")
print(imdata)
#Missing Data Summary-----------------
summary(data2)
# Count missing values in each column
missing_per_column <- colSums(is.na(data2))
print(missing_per_column)
# Remove rows with any missing values 
cleaned_data <- na.omit(data2)
print(cleaned_data)
# Perform mean imputation for the 'salary' column where NA values are present
mean_salary <- mean(data$Salary, na.rm = TRUE)

# Perform KNN imputation
library(VIM)  
# Perform KNN imputation
data_imputed <- kNN(cleaned_data, k = 5)  # You can adjust 'k' as needed

# Remove the 'Age' column
df <- df %>% select(-Age)
#Remove multiple columns
df <- df %>% select(-c(Age, Gender))


#  #  #
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                    Merge Data
---------------------------------------------------------------------------------------------------------
library(readxl)
library(readr)
library(writexl)
library(tidyverse)
library(dplyr)
setwd("C:/Users/User/Desktop")
df_A <- read_csv("epidata.csv")
print(df_A)
df_B <- read_csv("labdata.csv")
print(df_B)

str(df2)

combodata <- left_join(df_A,df_B, by = 'Sample_ID')
joined_df <- merge(df_A, df_B, by = "ID", all.x = TRUE)

df <- merge(df_A, df_B, by.x = "Sample_ID", by.y = "Sample_ID", all = TRUE)

final_df <-left_join(df_A, df_B, by = c('SAMPLE_ID'='Sample_ID'))

write.csv(df,"C:/Users/User/OneDrive - University of Ghana/myComputer@space/repos/kk.csv")
write_xlsx(combo, "zzdata.xlsx")
write_xlsx(df, "C:/Users/User/OneDrive - University of Ghana/myComputer@space/repos/output.xlsx")


names(df1)
names(df2)
joindf <- left_join(df1, df2, by = "SAMPLE_ID", relationship = "many-to-many")

final_df <- left_join(df_A, df_B, by = "SAMPLE_ID")


 View(joindf)
str(join_df)

write_xlsx(join_df, "wednesday.csv")

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                                  Explore data                                                        +
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                               Data Transformation 
#-------------------------------------------------------------------------------------------------------
skewness(-1+1)---- statistic + 1.96   | Kurtosis(1-3)---- statistic + 1.96     #Skewness = Mean - Median
---------- -        |                   --------- -          #            -------------
SE              |                     SE                 #                Sd
#_____________________________________|________________________________________# Q1-1.5(IQR) ___________
POS_skewed = Mean > Median/Mode      | NEG_skewed = Mean < Median/Mode        # Q3+1.5(IQR)        
#-------------------------------------------------------------------------------------------------------
library(psych)
library(readxl)
setwd("C:/Users/User/OneDrive - University of Ghana/myComputer@space/repos")
imdata <- read_excel("immunoData.xlsx")

skew(imdata$age, na.rm = TRUE)
kurtosi(imdata$age, na.rm = TRUE)
mardia(imdata$age,na.rm = TRUE)

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



+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                              Descriptive statistics                                         +
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(skimr)
skim_without_charts(imdata)
library(psych)
summary(imdata)
describe(imdata)

summary(imdata$bwgt)
describe(imdata$bwgt)

Histogram
hist(imdata$ bwgt, col="gray", main="Immuno assay data", xlab="Birthwgt")
# Will also report count of NA’
describe(imdata$bwgt, type=2)    
#-------------- aggregate data Table for report
library(janitor)
library(readxl)
setwd("C:/Users/User/OneDrive - University of Ghana/myComputer@space/repos")
imdata <- read_excel("immunoData.xlsx")
print(imdata)
imdata %>%
  tabyl(hb, sex) %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front") %>%
  adorn_title(
    row_name = "Hgb",
    col_name = "Gender",
    placement = "combined") %>% 
  flextable::flextable() %>% 
  flextable::autofit()


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                             Visualization with ggplot2                                            +
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
par()
#par(mfrow = c(2,2))                               par(mfrow = c(4,4))
#plot(data$vars)                                   plot(data$vars)
install.packages(
  "webr",
  repos = c("http://rstudio.org/_packages",
            "http://cran.rstudio.com")
)


library(ggplot2)
library(RColorBrewer)
library(plotrix)
library(webr)
library(readxl)
setwd("C:/Users/User/OneDrive - University of Ghana/myComputer@space/repos")
imdata <- read_excel("immuData.xlsx")
View(imdata)
------------------------------------------------------------------------------------------------------------------------
setwd("C:/Users/User/OneDrive - University of Ghana/myComputer@space/repos")
ARMdata <- read_csv("Untitled2.csv")
print(ARMdata)

#Convert categorical variable to factors
ARMdata$Age <-as.integer(ARMdata$Age)
ARMdata$Gender <-as.factor(ARMdata$Gender)
ARMdata$AgeGroup <-as.factor(ARMdata$AgeGroup)
ARMdata$study_site <-as.factor(ARMdata$study_site)
ARMdata$ParasitePresence <-as.factor(ARMdata$ParasitePresence)
str(ARMdata)
###
ARMdata <- ARMdata %>%
select(Dateenroll, Gender,   Age, AgeGroup, study_site, ParasitePresence, -study__1 ) 
print(ARMdata)
#Recoding Back

ARMdata$Age <-as.integer(ARMdata$Age)
ARMdata$ParasitePresence <-factor(ARMdata$ParasitePresence,
                                  levels = c(0,1),
                                  labels = c("Negative", "Positive"))
  
ARMdata$AgeGroup <-factor(ARMdata$AgeGroup,
                          levels = c(0,1,2),
                          labels = c("Below 5yrs", "5 - 14yrs", "Above 14 yrs "))
  
ARMdata$Gender <-factor(ARMdata$Gender,
                        levels = c(0,1),
                        labels = c("Female", "Male"))
  
ARMdata$study_site <-factor(ARMdata$study_site,
                            levels = c(1,2,3,4,5,6,7,8,9,10),
                            labels = c("Ada East", "Dzodze", "Ejura", "Kade", "Kenyase", "Nkoranza", "Saboba", "Walewale", "Weija", "Zebilla"))
str(ARMdata)
print(ARMdata) 
  
  
  
------------------------------------------------------------------------------------------------------------------------
#-------------------------------- Pie Chart ------------------------
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

#------------------------------- Donut chart ----------------------
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

#------------------------------ plotting points ------------------- 
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

#------------------------------ Barchart: ------------------------
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

#------------------------------ Histogram:-------------------------
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
#--------------------------Viridis-(Histogram)---------------------
hh <-ggplot(data = imdata,
            mapping = aes(x = systol1, fill = expose))+
  geom_histogram(bins = 10, position = "dodge")
hh + scale_fill_viridis_d()
hh + scale_fill_viridis_d(direction = -1)
hh

#----------------------------- Boxplot:----------------------------
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

#---------------------- Label & Annotation
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

#---------------------- Viridis -------------------
immuLAB <- ggplot(data = imdata)+
  geom_point(mapping = aes(x = systol1, y = diastol1, colour = expose))+
  labs(title ="Immunoassay Data", subtitle = "ImmunoAnalysis",caption = "Data collection by Mohammed")+
  annotate( "text",x = 145,y = 90, label ="Malaria in pregnancy                              ", color ="purple",
            fontface ="bold", size =4.0,angle = 30)+
  theme_classic()+scale_colour_viridis_d()
immuLAB
#--------------viridis(City\nCenter)--------------------------------
immuLAB <- ggplot(data = ARMdata)+
  geom_point(mapping = aes(x = systol1, y = diastol1, colour = expose))+
  labs(title ="Immunoassay Data", subtitle = "ImmunoAnalysis",caption = "Data collection by Mohammed")+
  annotate( "text",x = 145,y = 90, label ="Malaria in pregnancy", color ="purple",
            fontface ="bold", size =4.0,angle = 30)+
  theme_classic()+scale_colour_viridis_d(option = "City\nCenter")
immuLAB

#------------------- Error Bars ------------------------------------

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

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                        TEST OF ONE / TWO SAMPLES                                                 +
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
library(psych)
library(readxl) 
library(lessR)
setwd("C:/Users/User/OneDrive - University of Ghana/myComputer@space/repos")
amdata <- read_excel("ARMdata.xlsx")
print(amdata)  

describe(amdata)  
t.test(estimated_parasitemia ~ gender, data=amdata, var.equal=FALSE, conf.level=0.95)

  
  
#------------------One sample t-test with obs as VECTORS
observed    = c(0.52, 0.20, 0.59, 0.62, 0.60)
theoretical = 0

t.test(observed, mu = theoretical, conf.int = 0.95)

#-----------------One sample t-test with obs as DATA.FRAME--------------------- 1_sample
observed    = Data$Angle
theoretical = 50
hist(Data$ Angle, col="gray", main="Histogram of values", xlab="Angle")

t.test(observed, mu = theoretical, conf.int=0.95)

#  #  #

#------------------Two Samples Student’s t–test for --------------------------- 2_sample
Two-sample t-test, independent (unpaired) observations
bartlett.test(Value ~ Group, data=Data) #If p-value >= 0.05, use var.equal=TRUE below.

#paired t.test  
t.test(Value ~ Group, data=Data, var.equal=TRUE, conf.level=0.95)
#independent t.test
t.test(Value ~ Group, data=Data, var.equal=FALSE, conf.level=0.95)

t.test(age ~ CaseControl, data=imdata, var.equal=FALSE, conf.level=0.95)

# Histogram
hs<-histogram(~ age | CaseControl, col="gray",data = imdata)
hs
# Boxplot
boxplot(age ~ CaseControl, data = imdata, names=c("Case","Control"), ylab="age")

#    #    #

#------------- Mann–Whitney Test
wilcox.test(Value ~ Group, data=Data, exact = FALSE)
wilcox.test(Value ~ Group, data=Data)
#Box plots
boxplot(Value ~ Group, data = Data, names=c("2 pm","5 pm"), ylab="Value")

boxplot(estimated_parasitemia ~ age, data=imdata, names=c("Control","Case"), ylab="Value")

#  #  #

#------------- Wilcoxon Signed-rank Test
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
setwd("C:/Users/User/OneDrive - University of Ghana/myComputer@space/repos")
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

#ggplot2
library(ggplot2)

p <- ggplot(imdata, aes(x=estimated_parasitemia, color = age, fill=age))+ geom_density(alpha = 0.7)
p
p1 <- ggplot(imdata, aes(x=bmi, color = expose, fill=expose))+ geom_density(alpha = 0.4)
p1

#   #   # 


#================== ANOVA ANALYSIS----option.2 ==================
=====================  Factorial Anova ========================== 
library(psych)
library(ggplot2)
library(ggpubr)
library(readxl)
setwd("C:/Users/User/OneDrive - University of Ghana/myComputer@space/repos")
amdata <- read_excel("ARMdata.xlsx")

describe(amdata)
#plot
p <- ggplot(amdata, aes(x=site_name, color = estimated_parasitemia, fill=target,xlab="Angle"))+geom_bar(alpha = 0.8)+theme_minimal()
p
p1 <- ggplot(imdata, aes(x=age, color = estimated_parasitemia, fill=target))+geom_bar(alpha = 0.8)+theme_cleveland()
p1
p2 <- ggplot(imdata, aes(x=gender, color = imdata$estimated_parasitemia, fill = target))+geom_bar(alpha = 0.8)+theme_gray()
p2

#to arrange plot for publication
ggarrange(p, p1, p2 + rremove("x.text"), labels = c("A", "B", "c"), ncol = 1, nrow = 3,
          common.legend = TRUE, legend = "bottom")
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
interaction <- aov(bmi ~ expose + grvdty + sex + CaseControl, data = imdata)
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

#post hoc analysis
tukey.interaction <- TukeyHSD(interaction)
tukey.interaction

tukey.plot.aov <- aov(bmi ~ expose:CaseControl, data = imdata)
tukey.plot.test <-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test,las = 2)

#   #   #
-----------------------------------------------------------
  Tukey and LSD mean separation tests (pairwise comparisons)
TukeyHSD, HSD.test, and LSD.test are not appropriate for cases where there are unequal variances
though TukeyHSD does make an adjustment for mildly unequal sample sizes.
----------------------------------------------------------
  
  
  #====================== ANOVA ANALYSIS----option.3 ==================
------------------------------ One-way Anova ------------------------
  if(!require(agricolae)){install.packages("agricolae")}
---------------------------------------------------------------------
library(FSA)
Summarize(age ~ expose, data = imdata)

#Fit the linear model and conduct ANOVA 
model = lm(age ~ expose, data=imdata)

library(car)
Anova(model, type="II")                    

# Can use type="III"
Anova(model, type="III", contrasts = c("contr.sum", "contr.poly")) 

# Produces type I sum of squares
anova(model)                               

# Produces r-square, overall p-value, parameter estimates
summary(model)     

#Checking assumptions of the model
hist(residuals(model), col="darkgray")

#plot of residuals vs. predicted values.
library(car)
QQ <- qqPlot(residuals(model), id = TRUE)

#   #    #


#----------------Kruskal–Wallis Test-------------------------------------------
if(!require(FSA)){install.packages("FSA")
  #--------------------------------------------------- kruskal.test(Value ~ Group,
  #---------------------------------------------------              data = Data)
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
#log-odds=-0.7673+0.6719*(parity=1)+2.1535*(parity=2)+1.8659*(parity=3)-15.7988*(parity=4)
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
summary(all)

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

all <- exp(cbind(OR = coef(model), confint(model)))
summary(all)

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


#    #    #




+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#---------------------------- TEST OF NOMINAL VARIABLE ------------------------------------------
-------------------------------------------------------------------------------------------------
  Follow up with post hoc tests (optional for Chi-Square)
#if there are more than two groups in either of the variables with p<0.05, a post hoc test is conducted.
using bonferroni correction----/------post hoc chi square of independence.
#----------------------------------------------------------------------------
1----Exact Goodness-of-fit--------------#whether is difference to hypothised value
  2----Chi-Square Goodness-of -fit--------#Differecnes between observed and expected value
  3----Chi-Square test of independence----#Test of association
  4----Fisher exact test------------- N < 100
5----McNemars test.---------------------#TO compare before and after observations
  6----G test-----------------------------#Can accommodate experimental design


#  #  #  
  
  
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                         Chisquare / Fisher Exact                                              +
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
library(readxl)
library(tidyverse)
library(stats)
cngTB <- read_excel("C:/Users/User/Desktop/MTB only_ Lineage.xlsx")
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
--------------------------------------
library(ggplot2)
#stacked Barchart
ht2 <-ggplot(data = imdata,
             mapping = aes(x = CaseControl, fill = sex))+
  geom_bar()

ht2
---------------
 
   
#  #  #
  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                              Merge Data
--------------------------------------------------------------------------------
library(readxl)
library(readr)
library(writexl)
library(tidyverse)
library(dplyr)

df2 <- read_excel("C:/Users/User/Desktop/Epid.csv")
print(df1)
df1 <- read_excel("C:/Users/User/Desktop/Lab.csv")
print(df2)

names(df1)
names(df2)
joined_df <- left_join(df1, df2, by = "SAMPLE_ID")
View(joined_df)
str(joined_df)

write_xlsx(joined_df, "Mergedoo.xlsx")

#  #  #  
  
  
   
--------------------------------------------------------------------------------
  
setwd("C:/Users/User/Desktop")
ayoola <- read_csv("Untitled.csv")
print(ayoola)

ayoola$Status <-factor(ayoola$Status,
                        levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13),
                        labels = c("M.TB", "Africanm", "Bovis", "Caprae", "No.amplification", "Africanm/Intercellulare",
                                   "M.TB/ Avium", "M.Avium","Intercellulare", "M.TB/Intercellulare", "M.TB/Mucogenicum",
                                   "M.TB/Simae", "M.TB/Abscesses"))

ayoola$Lineage <-factor(ayoola$Lineage,
                       levels = c(1,2,3,4,5,6,7,8),
                       labels = c("L1", "L2", "L3", "L4", "L5", "L6", "Bovis", "NTM"))

str(judith)
print(judith)



p <- ggplot(ayoola, aes(x=  Status, color = Status, fill=Status,xlab="Angle"))+geom_bar(alpha = 0.8)+theme_minimal()
p
p1 <- ggplot(ayoola, aes(x= Status, color = Sublineage, fill=Sublineage))+geom_bar(alpha = 0.8)+theme_minimal()
p1
p2 <- ggplot(ayoola, aes(x=gender, color = imdata$estimated_parasitemia, fill = target))+geom_bar(alpha = 0.8)+theme_minimal()
p2

#to arrange plot for publication
ggarrange(p, p1, p2 + rremove("x.text"), labels = c("A", "B", "c"), ncol = 1, nrow = 3,
          common.legend = TRUE, legend = "bottom")



-------------------------------------------------------------------------------------------------------------
 