rm(list=ls())
gc(reset = TRUE)
------------------------
library(readxl)
library(readr)
library(rio)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
library(lessR)
library(car)
library(psych)
library(scales)

library(readxl)
imdata <- read_excel("C:/Users/User/Desktop/repos/immunoData.xlsx")
library(rio)
import("C:/Users/User/Desktop/repos/NNJ.csv")
#------------------------ EXport function ------------------------------------ 
write.csv(data, file = data.csv)
#------------------------- Explore data ----------------------------------------
View(imdata)

#:::::::::::::::: Data manipulations with Dplyr :::::::::::::::::::::::::::::::#
library(tidyverse)
library(dplyr)
###--------------------------------========
imdata <- read_excel("C:/Users/User/Desktop/repos/immunoData.xlsx")
data2 <- imdata

# Finding rows with NA value
#with_na <- data2 %>% filter(is.na(hb)) 
#print(with_na)

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
  annotate( "text",x = 145,y = 90, label ="Malaria in pregnancy       ", color ="purple",
            fontface ="bold", size =4.0,angle = 45)
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
data1 <- rd("C:/Users/User/Desktop/repos/immunoData.xlsx")
PieChart(expose, data = data1, fill = "blues", hole_fill = "#B7E3E0", main = NULL)

# Barchart
imm4 <- rd("C:/Users/User/Desktop/repos/immunoData.xlsx")
BarChart(expose, data = imm4, fill = "blues", hole_fill = "#B7E3E0", main = NULL)

data <- rd("C:/Users/User/Desktop/repos/immunoData.xlsx")
BarChart(expose, data = data, fill = "reds", hole_fill = "#B7E3E0", main = NULL)

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


#----------------- TEST OF ONE / TWO MEASUREMENRT OF VARIABLES ----------------#
  
#------------------------One sample t-test with obs as VECTORS
  observed    = c(0.52, 0.20, 0.59, 0.62, 0.60)
theoretical = 0

t.test(observed, mu = theoretical, conf.int = 0.95)

#  #  #

#---------------------One sample t-test with obs as DATA.FRAME
observed    = imdata$hb
theoretical = 50
hist(imdata$hb, col="gray", main="Histogram of values", xlab="Angle")

t.test(observed, mu = theoretical, conf.int=0.95)

#  #  #

#============================== TWO SAMPLE Test ===============================

library(car)
QQ <- qqPlot(library(car)
QQ <- qqPlot(imdata$hb, id = TRUE)  #id=TRUE to remove point identification
shapiro.test(imdata$hb) #normality, id = TRUE)  #id=TRUE to remove point identification
===============================================================================
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
                    
