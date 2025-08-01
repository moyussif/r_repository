rm(list=ls())
gc(reset = TRUE)
#--------------------------- required packages _________________________________
library(readxl)
library(readr)
library(writexl)
library(tidyverse)
library(RColorBrewer)
library(ggpubr)
library(psych)
library(car)
library(lessR)
library(plotrix)
library(FSA)
library(Hmisc)
library(epitools)

-------------------------------------------------------------------------------------------------------------

setwd("C:/Users/User/Desktop")
ghdata <- read_excel("amp.xlsx")
print(ghdata)

p <- ggplot(ghdata, aes(x=  Status, color = Count, fill=Lineage,xlab="Angle"))+geom_bar(alpha = 0.8)+theme_minimal()
p
p1 <- ggplot(ghdata, aes(x= Status, color = Count, fill=Status))+geom_bar(alpha = 0.8)+theme_minimal()
p1
p2 <- ggplot(imdata, aes(x=gender, color = imdata$estimated_parasitemia, fill = target))+geom_bar(alpha = 0.8)+theme_minimal()
p2

#to arrange plot for publication
ggarrange(p, p1, p2 + rremove("x.text"), labels = c("A", "B", "c"), ncol = 1, nrow = 3,
          common.legend = TRUE, legend = "bottom")



-------------------------------------------------------------------------------------------------------------




#---------------------------- Import functions ---------------------------------
#set directorate 
setwd("C:/Users/User/Desktop")
ghsdata <- read_excel("yussif.xlsx")

library(readr)
csv <- read_csv("yussif.csv")
#simply---------------------
library(readxl)
ghsdata <- read_excel("C:/Users/User/Desktop/yussif.xlsx")

View(ghsdata)
print(ghsdata)
#--------------------------- EXport function  
library(writexl)
write_xlsx(ghsdata, "newData.xlsx")

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

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                             Data flow Dplyr                                  +
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
library(tidyverse)
library(dplyr)
ghsdata1 <- ghsdata %>%
  select(Status, Age, Gravidity, Parity, bld_grp, Weight, Height, Systolic, Diastolic, ModeOfDelivery, Babysex, Apgar1min, -Apgar5min) %>% 
  filter(Age > 20)        
print(ghsdata1)

# Remove the 'Apgar1min' column
ghsdata1 <- ghsdata1 %>% select(-Apgar1min)
print(ghsdata1)
#Remove multiple columns
ghsdata1 <- ghsdata1 %>% select(-c(Age, Gender))
#  #  #
-------------------------- Data Conversion ------------------------------------ 
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
#----------------------------Examples
library(ggplot2)
library(RColorBrewer)
library(plotrix)
library(webr)
library(readxl)
str(ghsdata1)
ghsdata1$Age <-as.integer(ghsdata1$Age)
ghsdata1$Gravidity <- as.integer(ghsdata1$Gravidity)
ghsdata1$Parity <- as.integer(ghsdata1$Parity)
ghsdata1$Status <-factor(ghsdata1$Status,
                                  levels = c(1,2),
                                  labels = c("control", "case"))

ghsdata1$Babysex <-factor(ghsdata1$Babysex,
                          levels = c(1,2),
                          labels = c("Male", "Female"))

ghsdata1$ModeOfDelivery <-factor(ghsdata1$ModeOfDelivery,
                        levels = c(1,2),
                        labels = c("Vaginal", "Caesarean"))

ghsdata1$bld_grp <-factor(ghsdata1$bld_grp,
                            levels = c(1,2,3,4),
                            labels = c("A positive", "B positive", "AB positive", "O positive"))
str(ghsdata1)
print(ghsdata1)
#   #   #

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                               Explore data                                   +
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
#_______________________________________________________________________________
 +                            HANDLING MISSING DATA                            +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Missing Data Summary-----------------
summary(ghsdata1)
# Count missing values in each column
missing_per_column <- colSums(is.na(ghsdata1))
print(missing_per_column)
# Remove rows with any missing values 
cleaned_data <- na.omit(ghsdata1)
print(cleaned_data)

verify_missing <- colSums(is.na(cleaned_data))
print(verify_missing)

#  #  #  

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
                             Data Transformation 
#-------------------------------------------------------------------------------------------------------
skewness(-1+1)---- statistic + 1.96   | Kurtosis(1-3)---- statistic + 1.96     #Skewness = Mean - Median
---------- -        |                   --------- -          #            -------------
SE              |                     SE                 #                Sd
#_____________________________________|________________________________________# Q1-1.5(IQR) ___________
POS_skewed = Mean > Median/Mode      | NEG_skewed = Mean < Median/Mode        # Q3+1.5(IQR)        
#-------------------------------------------------------------------------------------------------------
library(psych)
library(readxl)

skew(ghsdata1$Age, na.rm = TRUE)
kurtosi(ghsdata1$Age, na.rm = TRUE)


#Square root Transformation ==== moderately skew (-1 to -0.5 // 0.5 to 1 )
sq_data <-sqrt(ghsdata1$Age)
hist(sq_data)
#Cube root==== moderately----right skewed can apply to negative and zero values
cb_data <-sign(ghsdata1$Age)*abs(ghsdata1$Age)^(1/3)
hist(cb_data)
#Log Transformation====highly skewed (above 1),Cannot be applied negative/zero values
lg_data <-log(imdata$systol1)
hist(lg_data)

#-----------------Confidence interval of the mean
t.test(ghsdata1$Age,
       conf.level=0.95)         

#   #   #
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                            Descriptive statistics                            +
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(skimr)
skim_without_charts(ghsdata1)
library(psych)
summary(ghsdata1)
describe(ghsdata1$Age)

summary(imdata$bwgt)
describe(imdata$bwgt)

Histogram
hist(ghsdata1$Age, col="gray", main="NCD for pregnant women", xlab="Birthweight")
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


setwd("C:/Users/User/OneDrive - University of Ghana/myComputer@space/repos")
imdata <- read_excel("immuData.xlsx")
View(imdata)

#-------------------------------- Pie Chart ---------------------------------
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

#------------------------------- Donut chart --------------------------------
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

#------------------------------ plotting points ----------------------------- 
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

#------------------------------ Barchart: -----------------------------------
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

#------------------------------ Histogram:-----------------------------------
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
#--------------------------Viridis-(Histogram)-------------------------------
hh <-ggplot(data = imdata,
            mapping = aes(x = systol1, fill = expose))+
  geom_histogram(bins = 10, position = "dodge")
hh + scale_fill_viridis_d()
hh + scale_fill_viridis_d(direction = -1)
hh

#----------------------------- Boxplot:-------------------------------------
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
 
# Facet grid
immgrd <- ggplot(data = amdata,
                   mapping = aes(x = gender, y =age, colour = estimated_parasitemia))+
  geom_violin()+
  facet_grid(estimated_parasitemia~age)
immgrd

#---------------------- Label & Annotation
immuLAB <- ggplot(data = imdata)+
  geom_point(mapping = aes(x = systol1, y = diastol1, colour = CaseControl, shape = expose))+
  labs(title ="Immunoassay Data", subtitle = "ImmunoAnalysis",caption = "Data collection by Mohammed")
immuLAB

print(immuLAB)

# Save plot 
ggsave("immuLAB.png")

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

#-----------------One sample t-test with obs as DATA.FRAME--------------------- 1_sample
observed    = Data$Age
theoretical = 40
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
  
#------------------------------- Correlation -------------------------------
  Correlation
  Correlation can be performed with the cor.test function in the native stats package.  
  It can perform Pearson, Kendall, and Spearman correlation procedures.  
  
  #Pearson correlation (Parametric)
  Pearson correlation is a parametric test, and assumes that the data are linearly related 
  and that the residuals are normally distributed.
  --------
    cor.test( ~ age + bmi, data=imdata, method = "pearson", conf.level = 0.95)
  
  #Spearman correlation (Non-parametric / ordinals)
  Spearman rank correlation is a non-parametric test that does not assume a distribution of the data.
  It ranks the data to determine the degree of correlation, and is appropriate for ordinal measurements.
  --------
    cor.test( ~ age + bmi, data=imdata, method = "spearman", continuity = FALSE, conf.level = 0.95)
  
  #Kendall correlation (Non-parametric)
  Kendall rank correlation is a non-parametric test that does not assume a distribution of the data.
  It ranks the data to determine the degree of correlation.
  --------
  cor.test( ~ age + bmi, data=imdata, method = "kendall", continuity = FALSE, conf.level = 0.95)
  
  #   #   #
  
  
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                           Regression analysis                                +
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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


#   #   #
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                      Poisson  Regression                      #Log-linear model
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#---------------------------- TEST OF NOMINAL VARIABLE ---------------------------------------------
----------------------------------------------------------------------------------------------------
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
  
  
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                    Chisquare - ODDRATIO - RISKRATIO                          +
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
library(epitools)
library(readxl)
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
  