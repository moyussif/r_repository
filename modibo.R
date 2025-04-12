rm(list=ls())
gc(reset = TRUE)
#--------------------------- All required packages _____________________________
library(readxl)
library(readr)
library(rio)
library(lubridate)
library(janitor)
library(here)
library(tidyverse)
library(dplyr)
library(dbplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(RColorBrewer)
library(ggsci)
library(ggpubr)
library(ggthemes)
library(ggrepel)
library(gganimate)
library(skimr)
library(rstatix)
library(lessR)
library(car)
library(effsize)
library(pwr)
library(caret)
library(olsrr)
library(Hmisc)
library(plotrix)
library(survival)
library(epitools)
library(epiDisplay)
library(devtools)
library(rvest)
library(dslabs)
library(weights)
library(zoo)
library(gtools)
library(VennDiagram)
library(rafalib)
library(MASS)
library(broom)
library(matrixStats)
library(gridExtra)
library(randomForest)
library(tree)
library(splitstackshape)
library(egg)
library(rpart)
library(rpart.plot)
library(testthat)
library(scales)
library(mvtnorm)
library(deSolve)
library(pdftools)
library(htmlwidgets)
library(rcompanion)
library(psych)

#-----
getwd()
#------------------------ Import functions -------------------------------------
child_data1 <- read.csv(file = 'children_data.csv')                             # import .csv file
child_data22 <- read.csv2(file = 'children_data.csv', sep = ",")                # introducing sep = ","
child_data3 <- read.delim(file = 'children_data.txt')                           # delim file with sep = "\t"
child_data1 <- read.csv("children_data.csv", stringsAsFactors = TRUE)           # for categorical
library(readr)
all_data2 <- read_csv(file = 'children_data.csv')                               # comma delimited files
library(readxl)
imdata <- read_excel("C:/Users/User/Desktop/repos/immunoData.xlsx")
HTdata <- read_excel("C:/Users/User/Desktop/repos/HTdata.xlsx")
library(rio)
import("C:/Users/User/Desktop/repos/NNJ.csv")
#------------------------ EXport function ------------------------------------ 
write.csv(data, file = data.csv)
#------------------------- Explore data ----------------------------------------
View(imdata)
View(HTdata)
library(skimr)
skim_without_charts(imdata) 

#:::::::::::::::: Data manipulations with Dplyr :::::::::::::::::::::::::::::::#
library(tidyverse)
library(dplyr)
data2 <- imdata %>%
  select(CaseControl, expose, age,delivage, hb, plt, parity, bmi ) %>% 
  filter(hb > 10)        
print(data2)
# Finding rows with NA value
with_na <- data2 %>% filter(is.na(hb)) 
print(with_na)
# Finding rows with no NA value
without_na <- data2 %>% filter(!is.na(hb)) 
print(without_na)
# arrange data by age 
data_frm <- data2 %>% filter(!is.na(hb)) %>% 
  arrange(desc(age)) 
print(data_frm)           
# Calculating a variable- / create New variables
mutate(data_frm, RBC = hb/3)
mutate(data_frm, HCT = hb*3)
# print-(only new column)
transmute(data_frm,hb,HCT = hb*3, RBC = hb/3) 
##rename 
rename(data_frm,Delivr_age = delivage, Bmi = bmi)
#summaries
summarise(data_frm,mean_hb = mean(hb))  
summarise(data_frm,median_hb = median(hb))


#:::::::::::::::::::::::::::: reshape with tidyr :::::::::::::::::::::::::::::#
library(tidyr) 
HTdata <- read_excel("C:/Users/User/Desktop/repos/HTdata.xlsx")
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
#--------------------- use pivot()function --------------------
#Pivot_longer
ldata <- pivot_longer(
  data = HTdata,
  cols = c(first_trimester, second_trimester, third_trimester),
  names_to = "trimester1",
  values_to = "value"
)
View(ldata)

#pivot_wider
wdata <- pivot_wider(
  data = ldata,
  names_from = "trimester1",
  values_from = "value",
)
View(wdata)


#::::::::::::::::::::::: Visualization with ggplot2 :::::::::::::::::::::::::::#
library(ggplot2)
library(RColorBrewer)
library(readxl)
imdata <- read_excel("C:/Users/User/Desktop/repos/immunoData.xlsx")
View(imdata)
#plotting points 
immu <- ggplot(data = imdata,
               mapping = aes(x = systol1, y = diastol1, colour = CaseControl, shape = expose))+
  geom_point()+theme_classic()
immu
#creating density plot 
immu1 <- ggplot(data = imdata,
                mapping = aes(x = diastol1, colour = CaseControl))+
  geom_density()
immu1
#point and smoothing
immu2 <- ggplot(data = imdata,
                mapping = aes(x = bmi, y = plt, colour = nicu))+
  geom_smooth()+
  geom_point()
immu2

#plot jitter
immu3 <- ggplot(data = imdata,
                mapping = aes(x = systol1, y = diastol1, colour = CaseControl, shape = expose))+
  geom_jitter()
immu3

#------------------------------ Barchart: =================
ht <-ggplot(data = imdata,
            mapping = aes(x = expose))+
  geom_bar()
ht
#column Barchrt
ht1 <-ggplot(data = imdata,
             mapping = aes(x = expose, colour = expose, fill = expose))+
  geom_bar()
ht1
#stacked Barchart
ht2 <-ggplot(data = imdata,
             mapping = aes(x = CaseControl, fill = expose))+
  geom_bar()
ht2

#------------------------------ Histogram: =================
ht3 <-ggplot(data = imdata,
             mapping = aes(x = systol1))+
  geom_histogram()
ht3

#Histogram with fill
ht4 <-ggplot(data = imdata,
             mapping = aes(x = systol1, fill = expose))+
  geom_histogram()
ht4

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
#-------------------Viridis--------------------------
hh <-ggplot(data = imdata,
            mapping = aes(x = systol1, fill = expose))+
  geom_histogram(bins = 10, position = "dodge")
hh + scale_fill_viridis_d()
hh + scale_fill_viridis_d(direction = -1)
hh

#----------------------------- Boxplot:================
immu <- ggplot(data = imdata,
               mapping = aes(x = systol1, y = diastol1))+
  geom_boxplot()
immu
# Boxplot by category ##
immu <- ggplot(data = imdata,
               mapping = aes(x = systol1, y = diastol1, colour = expose))+
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

# Violin chart 
immu <- ggplot(data = imdata,
               mapping = aes(x = systol1, y = diastol1, colour = expose))+
  geom_violin()
immu 
dd <- immu +scale_x_log10()+
  dd

# Facet wrap 
immwrp <- ggplot(data = imdata,
                 mapping = aes(x = systol1, y = diastol1, colour = grvdty))+
  geom_violin()+
  facet_wrap(~expose)
immwrp

# Facet grid
immgrd <- ggplot(data = imdata,
                 mapping = aes(x = systol1, y = diastol1, colour = grvdty))+
  geom_violin()+
  facet_grid(CaseControl~expose)
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
#--------------viridis(City\nCenter)----------------
immuLAB <- ggplot(data = imdata)+
  geom_point(mapping = aes(x = systol1, y = diastol1, colour = expose))+
  labs(title ="Immunoassay Data", subtitle = "ImmunoAnalysis",caption = "Data collection by Mohammed")+
  annotate( "text",x = 145,y = 90, label ="Malaria in pregnancy                              ", color ="purple",
            fontface ="bold", size =4.0,angle = 30)+
  theme_classic()+scale_colour_viridis_d(option = "City\nCenter")
immuLAB

# Save plot 
ggsave("immuLAB.png")


-------------------------------------------------------------------------------
#:::::::::::::::::::: charts with LessR (Pie/Bar) ::::::::::::::::::::::::::::#
library(lessR)
imm2 <- rd("C:/Users/User/Desktop/repos/immunoData.xlsx")

# Piechart
PieChart(expose, data = imm2, hole = 0, main = NULL)
# Donut chart
imm3 <- rd("C:/Users/User/Desktop/repos/immunoData.xlsx")
PieChart(expose, data = imm3, fill = "viridis", main = NULL, color = "black", lwd = 1.5,
         values_color = c(rep("white", 4), 1), values_size = 0.85)
# Donut chart
data1 <- rd("C:/Users/User/Desktop/repos/immunoData.xlsx")
PieChart(expose, data = data1, fill = "blues", hole_fill = "#B7E3E0", main = NULL)

# Barchart
imm4 <- rd("C:/Users/User/Desktop/repos/immunoData.xlsx")
BarChart(expose, data = imm4, fill = "blues", hole_fill = "#B7E3E0", main = NULL)

data <- rd("C:/Users/User/Desktop/repos/immunoData.xlsx")
BarChart(expose, data = data, fill = "reds", hole_fill = "#B7E3E0", main = NULL)

data <- rd("C:/Users/User/Desktop/repos/immunoData.xlsx")
BarChart(expose, data = data, fill = "viridis", main = NULL, color = "black",lwd = 1.5,
         values_color = c(rep("white", 4), 1), values_size = 0.85)

# slant x labels (45 angle)
BarChart(expose, data = data, fill = "viridis", main = NULL, color = "black",lwd = 1.5,
         rotate_x=45, values_color = c(rep("white", 4), 1), values_size = 0.85)

------------------------------------------------------------------------------
#::::::::::::::::::::::: Descriptive statistics :::::::::::::::::::::::::::::# 
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

#-------------- aggregate data
library(janitor)
library(readxl)
imdata <- read_excel("C:/Users/User/Desktop/repos/immunoData.xlsx")
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

#    #    #
#--------------------------- Data transformation -----------------
library(psych)
library(readxl)
imdata <- read_excel("C:/Users/User/Desktop/repos/immunoData.xlsx")

skew(imdata$age, na.rm = TRUE)
kurtosi(imdata$age, na.rm = TRUE)
mardia(imdata$age,na.rm = TRUE)
#Square root Transformation ==== moderately skew (-1 to -0.5 // 0.5 to 1 )
sq_data <-sqrt(imdata$bmi)
hist(sq_data)
#Cube root==== moderately----right skewed can apply to negative and zero values
cb_data <-sign(imdata$age)*abs(imdata$age)^(1/3)
hist(cb_data)
#Log Transformation =====Highly skewed (above 1),Cannot be applied negative/zero values
lg_data <-log(imdata$systol1)
hist(lg_data)
------------

  
  
#=========================== CONFIDENCE INTERVALS ==============================

#-----------------Confidence interval of the mean
  t.test(imdata$ bwgt,
       conf.level=0.95)         
  
#-----------------confidence intervals for groups defined by a variable
install.packages("Rmisc")
library(Rmisc)
summarySE(data=imdata, measurevar="age", groupvars="expose", conf.interval = 0.95)

#-----------------Confidence interval for proportions___with binom.test function
binom.test(2, 20, 0.5, alternative="two.sided", conf.level=0.95)

#     #     # 


++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#----------------- TEST OF ONE / TWO MEASUREMENRT OF VARIABLES ----------------#

#------------------------One sample t-test with obs as VECTORS
observed    = c(0.52, 0.20, 0.59, 0.62, 0.60)
theoretical = 0

t.test(observed, mu = theoretical, conf.int = 0.95)

#  #  #

#---------------------One sample t-test with obs as DATA.FRAME
observed    = Data$Angle
theoretical = 50
hist(Data$ Angle, col="gray", main="Histogram of values", xlab="Angle")

t.test(observed, mu = theoretical, conf.int=0.95)

#  #  #

#============================== TWO SAMPLE Test ===============================

#---------------------------------Student’s t–test for Two Samples
Two-sample t-test, independent (unpaired) observations
bartlett.test(Value ~ Group, data=Data)
---------------------#If p-value >= 0.05, use var.equal=TRUE below.

#paired t.test  
t.test(Value ~ Group, data=Data, var.equal=TRUE, conf.level=0.95)
#independent t.test
t.test(Value ~ Group, data=Data, var.equal=FALSE, conf.level=0.95)

t.test(age ~ CaseControl, data=imdata, var.equal=FALSE, conf.level=0.95)

#--------------------------------plots:  
# Histogram
hs<-histogram(~ age | CaseControl, col="gray",data = imdata)
hs
# Boxplot
boxplot(age ~ CaseControl, data = imdata, names=c("Case","Control"), ylab="age")
  
#    #    #
  
#-----------------Mann–Whitney and Two-sample Permutation Test------------------
wilcox.test(Value ~ Group, data=Data, exact = FALSE)
wilcox.test(Value ~ Group, data=Data)
#Box plots
boxplot(Value ~ Group, data = Data, names=c("2 pm","5 pm"), ylab="Value")

boxplot(age ~ CaseControl, data = imdata, names=c("Control","Case"), ylab="Value")

#  #  #

#-----------------------------Wilcoxon Signed-rank Test-------------------------
wilcox.test(Data$August, Data$November, paired=TRUE)
#Simple 1-to-1 plot of values
plot(Data$August, Data$November, pch = 16, xlab="August", ylab="November")
abline(0,1, col="blue", lwd=2)  

#   #   #


#=======================ANOVA ANALYSIS----option.1==================
======================== One-way Anova using LessR =================
# visualise statistical assumptions
library(lessR)
library(readxl)
imdata <- read_excel("C:/Users/User/Desktop/repos/immunoData.xlsx")
print(imdata)
#normality assumption
tapply(imdata$age, imdata$expose, shapiro.test)
library(car)
leveneTest(age ~ expose, data=imdata)
#One-way ANOVA
ANOVA(age ~ expose, data=imdata)
#effect size(for groups with significant)
library(effsize)
cohen.d(age ~ expose, data=subset(imdata, expose!= "non exposed"), paired=FALSE)        
cohen.d(age ~ expose, data=subset(imdata, expose!= "singleexposed"),paired=FALSE)        
#Bar charts 
age_means <- tapply(imdata$age, imdata$expose,mean)

BarChart(age_means)
BarChart(age_means, values="off", bxlab = "Malaria_exposed", ylab = "Women Age")

#ggplot2
library(ggplot2)

p <- ggplot(imdata, aes(x=age, color = expose, fill=expose))+ geom_density(alpha = 0.7)
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
imdata <- read_excel("C:/Users/User/Desktop/repos/immunoData.xlsx")

describe(imdata)
#plot
p <- ggplot(imdata, aes(x=age, color = expose, fill=expose))+geom_density(alpha = 0.7)
p
p1 <- ggplot(imdata, aes(x=bmi, color = expose, fill=expose))+geom_density(alpha = 0.4)
p1
p2 <- ggboxplot(imdata, "expose", "age", fill = "expose", palette = get_palette("default", 3), add = "jitter")
p2

#to arrange plot for publication
ggarrange(p, p1, p2 + rremove("x.text"), labels = c("A", "B", "C"), ncol = 3, nrow = 1,
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
one <- aov(bmi ~ grvdty, data = imdata)
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

cor.test( ~ Species + Latitude, data=Data, method = "pearson", conf.level = 0.95)

#Kendall correlation (Non-parametric)
Kendall rank correlation is a non-parametric test that does not assume a distribution of the data.
It ranks the data to determine the degree of correlation.

cor.test( ~ Species + Latitude, data=Data, method = "kendall", continuity = FALSE, conf.level = 0.95)

#Spearman correlation (Non-parametric / ordinals)
Spearman rank correlation is a non-parametric test that does not assume a distribution of the data.
It ranks the data to determine the degree of correlation, and is appropriate for ordinal measurements.

cor.test( ~ Species + Latitude, data=Data, method = "spearman", continuity = FALSE, conf.level = 0.95)

#   #   #


================================================================================

#----------------------------- Regression analysis -----------------------------

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
linear_model1 <- lm(y~x, data=sample_data)
# create a basic scatterplot 
plot(sample_data$x, sample_data$y)
# define x-axis values 
x_axis <- seq(1, 10, length=10)
lines(x_axis, predict(linear_model1, data.frame(x=x_axis)), col='green')


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


#----------------------------------------------------------------------------
==============================  LOGISTIC REGRESSION  ========================
#----------------------------------------------------------------------------
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

imdata <- read_excel("C:/Users/User/Desktop/repos/immunoData.xlsx")
View(imdata)
str(imdata)
#Convert categorical variable to factors
imdata$CaseControl <-as.factor(imdata$CaseControl)
imdata$parity <-as.factor(imdata$parity)
str(imdata)
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


#   #   #

============================== Log-linear model ==============================
#--------------------------- Poisson  regression -----------------------------
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
imdata <- read_excel("C:/Users/User/Desktop/repos/immunoData.xlsx")
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


===============================================================================
#------------------------ TEST OF NOMINAL VARIABLE ----------------------------
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


-------------------- Exact Test of Goodness-of-Fit ----------------------------
      
#The exact test goodness-of-fit can be performed with the binom.test function in the native stats package.
The probability can be entered as a decimal or a fraction.  
#Other options include the confidence level for the confidence interval about the proportion
#In most circumstances, the two-sided test is used.

if(!require(XNomial)){install.packages("XNomial")}
if(!require(pwr)){install.packages("pwr")}
if(!require(BSDA)){install.packages("BSDA")}
      

#Binomial test--------------------proportions-----------------------------------------------
used to determine if the proportion of successes in a binary experiment differs from a hypothesized probability.
#-------------------------------------------------------------------------------------------
                              Exact binomial test
        -------------------------------------------------------------------

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
#---------------------------------------------------------------------
               Probability density plot, binomial distribution
 ---------------------------------------------------------------------
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
#----------------------------------------------------------------------
Exact binomial test, Compares performing a one-sided test and 
doubling the probability, and performing a two-sided test
-----------------------------------------------------------------------
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
----------------------------------------------------------------------
      observed = c(428, 152)
      expected = c(3, 1)

library(XNomial)
xmulti(observed, expected, detail = 2)             

# reports three types of p-value      
P value  (LLR)  =  0.5331   # log-likelihood ratio
P value (Prob)  =  0.5022   # exact probability
P value (Chisq) =  0.5331   # Chi-square probability
      
#     #     #


========================================================================      
#----------------Multinomial test example
#-----------------------------------------------------------------------
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
      
#     #     # 
 

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
      
#    #    #


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


      
#     #     #
      
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


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#--------------------- Time Series Analysis ----------------------------------#
===============================================================================  
#Typical Questions__when is consumption highest / lowest
#___________________variation with season_________trends________change overtime.

library(tidyverse)
library(ggplot2)
library(tseries)
library(forecast)
library(readxl)
library(rio)
tdata <- read_excel("C:/Users/User/Desktop/repos/CTrends.xlsx")
View(tdata)
#---------------------------------------------1
tdata$Date = as.Date(tdata$Date)
View(tdata)
# Checks for stationarity of data
plot.ts(tdata$attendance)
adf.test(tdata$attendance)
# convert data to stationary
tdata=diff(log(tdata$attendance))
plot.ts(tdata)
adf.test(tdata)
# to run auto arima
auto.arima(tdata)
# create model
model_tdata=arima(tdata, order = c(0,0,0))
model_tdata
#diagnostic check
---------
 #create year

#-------------------------------------------2
library(rio)
library(scales)
sdata <- import("C:/Users/User/Desktop/repos/CTrends.xlsx")
View(sdata)
sdata$Date = as.Date(sdata$Date)
View(sdata)

ggplot(sdata, aes(x = Date, y = attendance, color = Name)) + geom_line() + 
  scale_x_date(
    labels = date_format(format = "%b %Y"),
    breaks = date_breaks("3 months")
  )+ theme(axis.text.x = element_text(angle = 45))

#---------------------------------------------3
ttdata <- read_excel("C:/Users/User/Desktop/repos/CTrends.xlsx")
print(ttdata)
ttdata$Date <- as.Date(ttdata$Date)
View(ttdata)

monthly <- ts(ttdata$registrants, start = 2015, frequency = 12)
plot(monthly)
quarterly <- ts(ttdata$registrants, start = 2015, frequency = 4)
plot(quarterly)
tsdata <-data.frame(cbind(ttdata, monthly, quarterly))
plot(tsdata)
View(tsdata)
ds <-tsdata %>% select(!(Date))
plot(ds$quarterly)
View(ds)
------------------------
 f <-ggplot(ds, aes(x = monthly, y = attendance, color = Name)) + geom_line() + 
  scale_x_date(labels = date_format("%Y-%m-%d", locale = NULL))+ 
  theme(axis.text.x = element_text(angle = 45))

f


######################### Mapping with r ######################################

install.packages("sf")
library(tidyverse)
library(mapview)
library(sf)





++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#------------------------- modelling infectious diseases -----------------------
================================================================================

library(readxl)
library(readr)
library(rio)
library(tidyverse)
library(skimr)
library(deSolve)
library(reshape2)

#Part one: SIR Mode----------- (Example one) --------------------------
                        
sir<-function(time,state,parameters)
{with(as.list(c(state,parameters)),
      {dS<--beta*S*I
       dI<-beta*S*I-gamma*I
       dR<-gamma*I
       return(list(c(dS,dI,dR)))})
}

# state=(SIR),   parameters=(beta, gamma) 
 
                       
#Provide the initial values and some parameters as below:
init<-c(S=1-1e-6,I=1e-6,0.0)
parameters<-c(beta=1.4247,gamma=0.14286)
times<-seq(0,70,by=1)
                        
#Put the results in dataframe    Ordinary differential equation (ode)
ode<-as.data.frame(ode(y,times,func,parms))
ode<-as.data.frame(ode(y=init,times=times,func=sir,parms=parameters))
summary(ode)
                        
out<-as.data.frame(ode)
out$time<-NULL
                        
#Ploting the results
matplot(times,out,type="l",xlab="Time",ylab="Susceptible and Recovered",main = "SIR Model",
        lwd = 1, lty = 1, bty = "l", col = 2:4)
legend(40,0.7,c("Susceptible","Infected","Recovered"),pch=1,col=2:4)
                        
                        
infections <- out$I
peak <- max(infections)
match(peak, infections) #this will show the day where peak is high
                        
................................................................................


#--------------Step 1: writing the differential equations with() function below:
                        
sir_2 <- function(time, variables, parameters) {
  with(as.list(c(variables, parameters)), {
    dS <- -beta * I * S
    dI <- beta * I * S - gamma * I
    dR <- gamma * I
    return(list(c(dS, dI, dR)))
  })
}
                        
#with() works on lists only, not on vectors.........
                        
#-------------Step 2: defining some values for the parameters in a named vector:

parameters_values <- c(
  beta = 0.004, # infectious contact rate (/person/day)
  gamma = 0.5 # recovery rate (/day)
)
  
#-------------Step 3: defining initial values for the variables
      #The initial values of the variables need to be defined in a named vector:
initial_values <- c(
  S = 999, # number of susceptible at time = 0
  I = 1, # number of infectious at time = 0
  R = 0 # number of recovered (and immune) at time = 0
)

#-------------Step 4: the points in time where we need to calculate variables values
    #We want to know the values of our SIR model variables at these time points:
time_values <- seq(0, 10) # days
                        
#-------------Step 5: numerically solving the SIR model We have defined all the needed ingredients:
ls()
                        
  ## [1] "infections" "init" "initial_values"
  ## [4] "ode" "out" "parameters"
  ## [7] "parameters_values" "peak" "sir"
  ## [10] "sir_2" "time_values" "times"
                        
                        
sir_2

## function(time, variables, parameters) {
## with(as.list(c(variables, parameters)), {
## dS <- -beta * I * S
## dI <- beta * I * S - gamma * I
## dR <- gamma * I
## return(list(c(dS, dI, dR)))
## })
## }
parameters_values
## beta gamma
## 0.004 0.500
initial_values
## S I R
## 999 1 0
time_values
## [1] 0 1 2 3 4 5 6 7 8 9 10
#-----------------------------                        
#so now we can use the ode() function of the deSolve package to numericallysolve our model:
                        
sir_values_1 <- ode(
  y = initial_values,
  times = time_values,
  func = sir_2,
  parms = parameters_values
)
sir_values_1

#you can use these values for further analytical steps,  
sir_values_1 <- as.data.frame(sir_values_1)  
sir_values_1
                        
with(sir_values_1, {
# plotting the time series of susceptibles:
  plot(time, S, type = "l", col = "blue",
  xlab = "time (days)", ylab = "number of people")
# adding the time series of infectious:
  lines(time, I, col = "red")
# adding the time series of recovered:
  lines(time, R, col = "green")
})

# adding a legend:
legend("right", c("susceptibles", "infectious", "recovered"),
                  col = c("blue", "red", "green"), lty = 1, bty = "n")
---------------------------------------------------------------------

#The value of the R0 is
(999 + 1) * parameters_values["beta"] / parameters_values["gamma"]
# Or __________________assigning N <- (999 + 1)
N <- (999 + 1)
N * parameters_values["beta"] / parameters_values["gamma"]

# beta 8
                        
sir_try <- function(beta, gamma, S0, I0, R0, times) {
  require(deSolve) # for the "ode" function
                          
# the differential equations:
  sir_equations <- function(time, variables, parameters) {
    with(as.list(c(variables, parameters)), {
      dS <- -beta * I * S
      dI <- beta * I * S - gamma * I
      dR <- gamma * I
    return(list(c(dS, dI, dR)))
                              9
          })
    }

# the parameters values:
parameters_values <- c(beta = beta, gamma = gamma)
# the initial values of variables:
initial_values <- c(S = S0, I = I0, R = R0)
# solving
out <- ode(initial_values, times, sir_equations, parameters_values)
# returning the output:
as.data.frame(out)
}

sir_try(beta = 0.004, gamma = 0.5, S0 = 999, I0 = 1, R0 = 0, times = seq(0, 10))

## time S I R
## 1 0 999.0000000 1.00000 0.000000
## 2 1 963.7055761 31.79830 4.496125
## 3 2 461.5687749 441.91575 96.515480
## 4 3 46.1563480 569.50418 384.339476
## 5 4 7.0358807 373.49831 619.465807
## 6 5 2.1489407 230.12934 767.721720
## 7 6 1.0390927 140.41085 858.550058
## 8 7 0.6674074 85.44479 913.887801
## 9 8 0.5098627 51.94498 947.545162
## 10 9 0.4328913 31.56515 968.001960
## 11 10 0.3919173 19.17668 980.431400
                        
------------- Comparing a model’s predictions with data -----------------------  
                        
flu <- read.table("https://bit.ly/2vDqAYN", header = TRUE)
# The above link may be broken in the future so is a good practice to save
  #the data on your computer after first download from the internet
load("flu.RData")
flu
                      
## day cases
## 1 0 1
## 2 1 6
## 3 2 26
## 4 3 73
## 5 4 222
## 6 5 293
## 7 6 258
## 8 7 236
## 9 8 191
## 10 9 124
## 11 10 69
## 12 11 26
## 13 12 11
## 14 13 4

#-------- Plot the points of the flu data set and use the sir_try() function to visually compare 
#                                                   the model’s predictions and the data points:
with(flu, plot(day, cases, pch = 19, col = "red", ylim = c(0, 600)))
predictions <- sir_try(beta = 0.004, gamma = 0.5, S0 = 999, I0 = 1, R0 = 0, times = flu$day)
with(predictions, lines(time, I, col = "red"))

#The above model did not fit the observed data well so we need to train the model on the data by 
#changing beta and gamma parameters.-------In this case, we will change only the beta parameter:
with(flu, plot(day, cases, pch = 19, col = "red", ylim = c(0, 600)))
predictions <- sir_try(beta = 0.0025, gamma = 0.5, S0 = 999, I0 = 1, R0 = 0, times = flu$day)
with(predictions, lines(time, I, col = "red"))

### The above model is better than the first model but not good yet
-----------------------------------                          
#Write a function that takes parameters values as inputs and draws the figure as an output. 
#Play with that function to see how changing the values of parameters can bring the model’s predictions closer to the data points.

model_fit <- function(beta, gamma, data, N = 763, ...) {
  I0 <- data$cases[1] # initial number of infected (from data)
  times <- data$day # time points (from data)
# model's predictions:
  predictions <- sir_try(beta = beta, gamma = gamma, # parameters
                         S0 = N - I0, I0 = I0, R0 = 0, # variables' intial values
                         times = times) # time points
# plotting the observed prevalences:
                   with(data, plot(day, cases, ...))
# adding the model-predicted prevalence:
                   with(predictions, lines(time, I, col = "red"))
                  } 
                        
model_fit(beta = 0.004, gamma = 0.5, flu, pch = 19, col = "red", ylim = c(0, 600))
-------------------------------------
                          
#The above model did not fit the data well so let us change the beta parameter to train the data

model_fit(beta = 0.0025, gamma = 0.5, flu, pch = 19, col = "red", ylim = c(0, 600))  
                        
#The above model reasonably fits the data well. Let us get some model predictions based on the above model:
  beta=0.0025
  gamma=0.5
  s0=762
  I0=1
  R0=0
  time=flu$day
  predictions <- sir_try(beta = beta, gamma =gamma, S0 = s0, I0 = I0, R0 = R0, times = time)
  predictions

#----And we want to compare these model’s predictions with real prevalence data:
           #One simple way to do so is to compute the “sum of squares” as below:

sum((predictions$I - flu$cases)ˆ2)
                        
# Which is the squared sum of the lengths of vertical black segments of the figure below:
# the observed prevalences:

with(flu, plot(day, cases, pch = 19, col = "red", ylim = c(0, 600)))
# the model-predicted prevalences:
with(predictions, lines(time, I, col = "red", type = "o"))
# the "errors":
segments(flu$day, flu$cases, predictions$time, predictions$I)
                        
#And we want to predict beyond the observed time period (i.e., forecast) based on the best parameters from
the fitted model above
newtime=seq(0,20)
model_forecast <- sir_try(beta = beta, gamma =gamma, S0 = s0, I0 = I0, R0 = R0, times = newtime)
summary(model_forecast)

matplot(model_forecast, type="l", lty=1, main="SIRS model", xlab="Time",ylab="Number of people")
legend <- colnames(model_forecast)[2:4]
legend(15000,900000, legend=legend, col=2:4, lty = 1)
                        
                        
#What are the effects of increasing or decreasing the values of the transmission 
#contact rate (β) and the recovery rate (γ) on the shape of the epi curve?
                          
#Estimating model’s parameters Sums of squares

#This is our model’s predictions:
predictions <- sir_try(beta = 0.004, gamma = 0.5, S0 = 999, I0 = 1, R0 = 0, times = flu$day)
predictions

#One simple way to do so is to compute the “sum of squares” as below:
sum((predictions$I - flu$cases)ˆ2)
#Which is the squared sum of the lengths of vertical black segments of the figure below
                      
#the observed prevalences:
with(flu, plot(day, cases, pch = 19, col = "red", ylim = c(0, 600)))
# the model-predicted prevalences:
with(predictions, lines(time, I, col = "red", type = "o"))
# the "errors":
segments(flu$day, flu$cases, predictions$time, predictions$I)
                        
#=================-----------------------------------------=====================
                        
 Shiny
Note the syntax follows that used by the popular ODE solver deSolve. require(shinySIR)
#required package: shinySIR
## Loading required package: dplyr  ## Attaching package: ’dplyr’
 The following objects are masked from ’package:stats’:
## filter, lag
The following objects are masked from ’package:base’:
## intersect, setdiff, setequal, union
## Loading required package: shiny,tidyr,ggplot2
 
run_shiny(model = "SIR")
# Warning in run_shiny(model = "SIR"):The length of the manual colour scale vector (’values’) must equal 
#the number of model variables. Using default ggplot colours instead.
                    
mySIRS <- function(t, y, parms) {
  with(as.list(c(y, parms)),{
# Change in Susceptibles
    dS <- - beta * S * I + delta * R
# Change in Infecteds
    dI <- beta * S * I - gamma * I
# Change in Recovereds
    dR <- gamma * I - delta * R
    return(list(c(dS, dI, dR)))
  })
}

#The interactive plot can then be created by calling this function with neweqns, specifying initial conditions
# for all model variables (ics), and specifying vectors for the parameter attributes, including parameter starting
# values (parm0), names to be displayed in the interactive menu (parm_names), and minimum and maximum
# values for the interactive menu (parm_min and parm_max, respectivel
                        
run_shiny(model = "SIRS (w/out demography)",
          neweqns = mySIRS,
          ics = c(S = 9999, I = 1, R = 0),
          parm0 = c(beta = 5e-5, gamma = 1/7, delta = 0.1),
          parm_names = c("Transmission rate", "Recovery rate", "Loss of immunity"),
          parm_min = c(beta = 1e-5, gamma = 1/21, delta = 1/365),
          parm_max = c(beta = 9e-5, gamma = 1 , delta = 1))

## Warning in run_shiny(model = "SIRS (w/out demography)", neweqns = mySIRS, : 
#The length of the manual colour scale vector (’values’) must equal the number of model variables.Using default ggplot colours instead.
    

============================================================================== 
#---------------------- Part two: SIRS models in R ---------------------------
                      
----------------------- Modelling Waning Immunity ----------------------------
#It is possible that people gain immunity after recovering from the infection but the immunity doesn’t last forever. 
#So, these individuals become susceptible again. Here, σ is the waning rate.The SIRS model is an extension of SIR model. 

#For SIRS, additional compartment called “waning immunity”is added to the SIR model. Thus, 

#The basic SIRS model has three compartments with three parameters 
                        
∂S
∂t
= -(β x S) + (σ x R) . . . . . . . . . . . . (6)
∂I
∂t
= (β x S) - (γ x I) . . . . (7)
∂R
∂t
= (γ x I) - (σ x R) . . . . . . . . . . . . . . . . . . (8)
Waning immunity

Assume σ =0.2 per day,β =0.4 per day and waning rate,*σ =1/10 per year if average immunity period is taken as 10 years. 

The model is run for a time period of 50 years in daily intervals.
require(ggplot2)
require(deSolve)
require(reshape2)
## Loading required package: reshape2
## Attaching package: ’reshape2’
## The following object is masked from ’package:tidyr’:
## smiths
# Model input
initial_values=c(S=999999,I=1,R=0)
parameters=c(gamma=0.2*365,beta=0.4*365,sigma=1/(10))
# Time points
time=seq(from=1,to=50,by=1/365)
#SIR model function
sirs_model <- function(time,state,parameters){
  with(as.list(c(state,parameters)),{
    N=S+I+R
    lambda=beta*(I/N)
    dS=-lambda*S+sigma*R
    dI=lambda*S-gamma*I
    dR=gamma*I-sigma*R
    return(list(c(dS,dI,dR)))
  })
}

# Solving the differential equations:
model_sirs<-as.data.frame(ode(y=initial_values,func = sirs_model,parms=parameters,times = time))
names(model_sirs)
## [1] "time" "S" "I" "R"
matplot(model_sirs, type="l", lty=1, main="SIRS model", xlab="Time",ylab="Number of people")
legend <- colnames(model_sirs)[2:4]
legend(15000,900000, legend=legend, col=2:4, lty = 1)
                        
# Note that We can plot the prevalence instead of the number of people by dividing the values by 1,000,000

#Alternatively, plot the prevalence with ggplot2
model_sirs_long=melt(model_sirs,id="time")
names(model_sirs_long)
## [1] "time" "variable" "value"
#Prevalence plot
ggplot(data = model_sirs_long,
       aes(x = time, y = value/1000000, colour = variable, group = variable)) +
  geom_line() +
  xlab("Time (years)")+
  ylab("Prevalence") +scale_color_discrete(name="State")




==============================================================================
#--------------------------- Part three: SEIR model --------------------------
------------------------------ SEIR model fitting ----------------------------
                        
#The SEIR model is an extension of SIR model. 
#For SEIR, additional compartment called “exposed” is added to the SIR model. 
#Thus, The basic SEIR model has four compartments represented as:
                          
1. S - “Susceptible” – individuals who have not been exposed to the virus
2. E - “Exposed” – individuals exposed to the virus, but not yet infectious
3. I - “Infectious” – exposed individuals who go on to become infectious
4. R - “Recovered” – infectious individuals who recover and become immune to the virus
5. Population size N is the sum of the individuals in the 4 compartments.
                        
#---------Parameters of the SEIR model
#  The flow of individuals between compartments is characterised by a number of parameters.
#  β (beta):is the transmission coefficient.Is the average number of infectious contacts an infectious individual 
#  in the population makes at each time period. A high value of β means the virus has more opportunity to spread.

#  σ (sigma): is the rate at which exposed individuals become infectious. Is the reciprocal of the average time 
#  it takes to become infectious. 
#  That is, if an individual becomes infectious after 4 days on average, σ will be 1/4 (or 0.25).

#  γ (gamma): is the rate at which infectious individuals recover. As before, think of it as the reciprocal of the
#  average time it takes to recover. That is, if it takes 10 days on average to recover, γ will be 1/10 (or 0.1).

#  μ (mu): is an optional parameter to describe the mortality rate of infectious individuals. The higher μ is,
#  the more deadly the virus. From these parameters, you can construct a set of differential equations. 
#  These describe the rate at which each compartment changes size.

Setting-up SEIR Equations----------------------------------------
#Equation (9) - Susceptible
#The first thing to see from the model is that there is no way S can increase over time. 
#There are no flows back into the compartment. Equation (6) must be negative, as S can only ever decrease.

In what ways can an individual leave compartment S? 
#Well, they can become infected by an infectious individual in the population.

#At any stage, the proportion of infectious individuals in the population = I/N.
#And the proportion of susceptible individuals will be S/N.
Under the assumption of perfect mixing 
#(that is, individuals are equally likely to come into contact with any other in the population), 
#the probability of any given contact being between an infectious and susceptible individual is (I / N) * (S / N).
#This is multiplied by the number of contacts in the population. 
#This is found by multiplying the transmission coefficient β, by the population size N. 
#Combining that all together and simplifying gives equation (9): 
  ∂S
∂t
= - (β x S x I) / N . . . . . . . . . . (9)

Equation (10) - Exposed
Next, let’s consider the “exposed” compartment, E. Individuals can flow into and out of this compartment.
The flow into E will be matched by the flow out of S. So the first part of the next equation will simply be
the opposite of the previous term.
Individuals can leave E by moving into the infectious compartment. This happens at a rate determined by two variables 
– the rate σ and the current number of individuals in E. So overall equation (10) is:
                      
  ∂E
∂t
= (β x S x I) - (σ x E) . . . . (10)
Equation (11) - Infectious
he next compartment to consider is the “infectious” compartment, I. There is one way into this compartment, 
which is from the “exposed” compartment.

There are two ways an individual can leave the “infectious” compartment.
Some will move to “recovered”. This happens at a rate γ.
                        
Others will not survive the infection. They can be modeled using the mortality rate μ. So equation (11) looks like:
  ∂I
∂t
= (σ x E) - (γ x I) - (μ x I) . . . . (11)
Equation (12) - Recovered
Now let’s look at the “recovered” compartment, R.
This time, individuals can flow into the compartment (determined by the rate γ).And no individuals can flow out of the compartment 
(although in some models, it is assumed possible to move back into the “susceptible” compartment 
 
- especially infectious diseases where re-infection is possible - COVID-19?). So the overall equation (12) looks like this:
  ∂R
∂t
= γ x I . . . . . . . . . . . . . . . . . . (12)
Equation (13) - Mortality (optional)
Using similar reasoning, you could also construct equation (13) for the change in mortality. 
You mightconsider this a fifth compartment in the model.
  ∂M
∂t
= μ x I . . . . . . . . . . . . . . . . . . (13)
You may set μ to zero (0) to exclude this compartment from the model.

Thus, we have given the full set of differential equations (9-13) Solving equations 9-13 (SEIR model) in R
require(deSolve)
SEIR <- function(time, current_state, params){
  with(as.list(c(current_state, params)),{
     N <- S+E+I+R
    dS <- -(beta*S*I)/N
    dI <- sigma*E - gamma*I - mu*I
    dR <- gamma*I
    dM <- mu*I
    return(list(c(dS, dE, dI, dR, dM)))
  })
}
                        
                        
The above function describes and provides 3 arguments:
(a) The current time step.
(b) A list of the current states of the system (that is, the estimates for each of S, E, I and R at the current time step).
(c) A list of parameters used in the equations (recall these are β, σ, γ, and μ).

Inside the function body, you define the system of differential equations as described above. 
These are evaluated for the given time step and are returned as a list.
The order in which they are returned must match the order in which you provide the current states.
                        
Now take a look at the code below:
params <- c(beta=0.5, sigma=0.25, gamma=0.2, mu=0.001)
initial_state <- c(S=999999, E=1, I=0, R=0, M=0)
times <- 0:365

The above codes initialises the parameters and initial state (starting conditions) for the model.
It also generates a vector of times from zero to 365 days.

Now, create the model:
model <- ode(initial_state, times, SEIR, params)

This uses deSolve’s ode() function to solve the equations with respect to time.
See here for the documentation.
                        
The arguments required are:
1. The initial state for each of the compartments
2. The vector of times (this example solves for up to 365 days)
3. The SEIR() function, which defines the system of equations
4. A vector of parameters to pass to the SEIR() function
                        
Running the command below will give the summary statistics of the model.
                        
summary(model)


## S E I R M
           
Already, you will find some interesting insights.
1. Out of a million individuals, 108,264 did not become infected.
2. At the peak of the epidemic, 126,516 individuals were infectious simultaneously.
3. 887,300 individuals recovered by the end of the model.
4. A total of 4436 individuals died during the epidemic.

You can also visualise the evolution of the pandemic using the matplot() function. colnames(model)
## [1] "time" "S" "E" "I" "R" "M"
matplot(model, type="l", lty=1, main="SEIR model", xlab="Time")
legend <- colnames(model)[2:6]
legend("right", legend=legend, col=2:6, lty = 1)
                        
#Assign both x and y axis labels
matplot(model, type="l", lty=1, main="SEIR model", xlab="Time",ylab="Number of people")
legend <- colnames(model)[2:6]
legend("right", legend=legend, col=2:6, lty = 1)
#Add the peak time line vertically
matplot(model, type="l", lty=1, main="SEIR model", xlab="Time",ylab="Number of people")
abline(v=112,col="blue")
legend <- colnames(model)[2:6]
legend("right", legend=legend, col=2:6, lty = 1) 
                      
#Add the peak time line vertically
matplot(model, type="l", lty=1, main="SEIR model", xlab="Time",ylab="Number of people")
abline(v=112,col="blue")
text(112,920000,"Peak day: 112",cex = 0.7,pos=3)
legend <- colnames(model)[2:6]
legend("right", legend=legend, col=2:6, lty = 1)
                        
#The associated plot is shown above:
#You can also coerce the model output to a dataframe type. Then, you can analyse the model further.
                        
infections <- as.data.frame(model)$I
peak <- max(infections)
match(peak, infections)
## [1] 112
===============================================================================
#------------------- SEIRM model with intervention methods ---------------------

The SEIR model is an interesting example of how an epidemic develops without any changes in the population’s behaviour.
You can build more sophisticated models by taking the SEIR model as a starting point and adding extra features.

This lets you model changes in behaviour (either voluntary or as a result of government intervention).
Many (but not all) countries around the world entered some form of “lockdown” during the coronavirus pandemic of 2019.
                        
Ultimately, the intention of locking down is to alter the course of the epidemic by reducing the transmission coefficient, β.
                        
The code below defines a model which changes the value of β between the start and end of a period of lockdown.

All the numbers used are purely illustrative. You could make an entire research career (several times over)
trying to figure out the most realistic values.
SEIR_lockdown <- function(time, current_state, params){
  with(as.list(c(current_state, params)),{
    beta = ifelse(
    (time <= start_lockdown || time >= end_lockdown),
     0.5, 0.1
    )
    N <- S+E+I+R
    dS <- -(beta*S*I)/N
    dE <- (beta*S*I)/N - sigma*E
    dI <- sigma*E - gamma*I - mu*I
    dR <- gamma*I
    dM <- mu*I
    return(list(c(dS, dE, dI, dR, dM)))
  })
}

The only change is the extra ifelse() statement to adjust the value of β to 0.1 during lockdown and β to 0.5 
before and after the lockdown.
Thus, if before or after the lockdown, give beta value to be 0.5 (i.e., increased transmission rate) 
but if within the lockdown periods, assign beta value to be 0.1 (i.e., reduced transmission
                                                                                                                                                                                                            rate)
You need to pass two new parameters to the model. These are the start and end times of the lockdown periods.
Here, the lockdown begins on day 90, and ends on day 150.

params <- c(
  sigma=0.25,
  gamma=0.2,
  mu=0.001,
  start_lockdown=90,
)

initial_state <- c(S=999999, E=1, I=0, R=0, M=0)
times <- 0:365
model2 <- ode(initial_state, times, SEIR_lockdown, params)

Now you can view the summary and graphs associated with this model.
summary(model2)

## S E I R M
            

This will reveal: You can see:
1. Out of a million individuals, 156,886 did not become infected.
2. At the peak of the epidemic, 72,444 individuals were infectious simultaneously. 838,917 individuals
   recovered by the end of the model.
3. A total of 4195 individuals died during the epidemic.
4. Plotting the model using matplot() reveals a strong “second wave” effect 
   (as was seen across many countries in Europe towards the end of 2020).
matplot(
  model2,
  type="l",
  lty=1,
  main="SEIR model (with intervention)",
  xlab="Time"
)                        
legend <- colnames(model2)[2:6]
legend("right", legend=legend, col=2:6, lty = 1)


Put the start and end of the lockdown periods for better visualization
matplot(
  model2,
  type="l",
  lty=1,
  main="SEIR model (with intervention)",
  xlab="Time",
  ylab="Number of people"
)
abline(v=90, col="blue")
abline(v=150, col="blue")
text(112,600000,"start & end- lockdown: day 90 & 150",cex = 0.7,pos=3)
legend <- colnames(model2)[2:6]
legend("right", legend=legend, col=2:6, lty = 1)                        
                        
                        
Finally, you can coerce the model to a dataframe and carry out more detailed analysis from there.

infections <- as.data.frame(model2)$I
peak <- max(infections)
match(peak, infections)
## [1] 223

1. In this scenario, the number of infections peaked on day 223.
2. In other scenarios, you could model the effect of vaccination. Or, you could build in seasonal differences
in the transmission rate.

Exercise for participants (duration - one hour)
Using the last model above, fit a similar model for the scenarios below separately, and interpret your results for each scenario:
  
(1) vary the value of β between the start and end of a period of lockdown as 0.5 and 0.2 respectively.
(2) vary the value of β between the start and end of a period of lockdown as 0.6 and 0.2 respectively.
(3) vary the value of γ to 0.4 and β between the start and end of a period of lockdown as 0.5 and 0.1 respectively.                        

         

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
=========================== MACHINE LEARNING ============================
 
  
#------------------------- BUILDING FUNCTIONS ---------------------------  
e.g-1
Area <- function(Length, Breadth){
  Area <- Length * Breadth
  return(Area)
}
#therefore,
Area (3,7)
----------
e.g-2  

BMI <- function(weight,height){
  BMI <- weight/height*2
  return(BMI)
}  
BMI(62,58)  
  
   
# R program to illustrate
# Graph plotting in
# Polynomial regression
# Importing required library
library(tidyverse)
library(caret)
theme_set(theme_classic())

# Load the data
data("Boston", package = "MASS")
# Split the data into training and test set
set.seed(123)
training.samples <- Boston$medv %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data <- Boston[training.samples, ]
test.data <- Boston[-training.samples, ]

# Build the model
model <- lm(medv ~ poly(lstat, 5, raw = TRUE), data = train.data)
# Make predictions
predictions <- model %>% predict(test.data)
# Model performance
data.frame(RMSE = RMSE(predictions, test.data$medv),
           R2 = R2(predictions, test.data$medv))

ggplot(train.data, aes(lstat, medv) ) + geom_point() + 
  stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE))


