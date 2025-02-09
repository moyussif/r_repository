rm(list=ls())
gc(reset = TRUE)
# -------------------All packages ---------------------- #
library(readxl)
library(readr)
library(rio)
library(lubridate)
library(janitor)
library(here)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(ggsci)
library(ggpubr)
library(ggthemes)
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
library(gganimate)
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
#--------------------Importing data -------------------
library(readr)
# import white space delimited files
all_data1 <- read_table(file = 'children_data.txt', col_names = TRUE)
# import comma delimited files
all_data2 <- read_csv(file = 'children_data.csv')
# import tab delimited files
all_data3 <- read_delim(file = 'children_data.txt', delim = "\t")
# or use
all_data4 <- read_tsv(file = 'children_data.txt' )
#---------------------------------------------------#
# import .csv file
child_data1 <- read.csv(file = 'children_data.csv') 
# import .csv file with dec = "," and sep = ";"
child_data2 <- read.csv2(file = 'children_data.csv')
#correct by introducing sep = ","
child_data22 <- read.csv2(file = 'children_data.csv', sep = ",")
# import tab delim file with sep = "\t"
child_data3 <- read.delim(file = 'children_data.txt')
#---------------------------------------------------------------------- #
#=======================R Companion ====================================#

#--------------------DECSRIPTIVES STATISTICS----------------------------#
if(!require(psych)){install.packages("psych")}
if(!require(DescTools)){install.packages("DescTools")}

Arithmetic mean 
mean(Data$ Fish, na.rm=TRUE)

Geometric mean
library(psych)
geometric.mean(Data$ Fish)

library(DescTools)
Gmean(Data$ Fish) 

Harmonic mean
library(psych)
harmonic.mean(Data$ Fish)

library(DescTools)
Hmean(Data$ Fish)

Median
median(Data$ Fish, na.rm=TRUE)

Mode
library(DescTools)
Mode(Data$ Fish)


summary(Data$ Fish)          # Also works on whole data frames
                             # Will also report count of NA’

library(psych)
describe(Data$ Fish,          # Also works on whole data frames
               type=2)        # Type of skew and kurtosis

range(Data$ Fish, na.rm=TRUE)
max(Data$ Fish, na.rm=TRUE) - min(Data$ Fish, na.rm=TRUE)
var(Data$ Fish, na.rm=TRUE)    #Sample variance
sd(Data$ Fish, na.rm=TRUE)     #Standard deviation

 sd(Data$ Fish, na.rm=TRUE)/
   mean(Data$ Fish, na.rm=TRUE)*100       #Coefficient of variation, as percent
===============
Custom function of desired measures of central tendency and dispersion
### Note NA’s removed in the following function

summary.list = function(x)list(
 N.with.NA.removed= length(x[!is.na(x)]),
 Count.of.NA= length(x[is.na(x)]),
 Mean=mean(x, na.rm=TRUE),
 Median=median(x, na.rm=TRUE),
 Max.Min=range(x, na.rm=TRUE),
 Range=max(Data$ Fish, na.rm=TRUE) - min(Data$ Fish, na.rm=TRUE),
 Variance=var(x, na.rm=TRUE),
 Std.Dev=sd(x, na.rm=TRUE),
 Coeff.Variation.Prcnt=sd(x, na.rm=TRUE)/mean(x, na.rm=TRUE)*100,
 Std.Error=sd(x, na.rm=TRUE)/sqrt(length(x[!is.na(x)])),
 Quantile=quantile(x, na.rm=TRUE)
)
summary.list(Data$ Fish)
-----
sd(Data$ Fish, na.rm=TRUE) /  
   sqrt(length(Data$Fish[!is.na(Data$ Fish)]))      # Standard error
------
t.test(Data$ Fish,
       conf.level=0.95)         # Confidence interval of the mean
------
library(Rmisc)

summarySE(data=D2,               # Will produce confidence intervals
          measurevar="Count",    #  for groups defined by a variable
          groupvars="Animal",    
          conf.interval = 0.95)
----
Confidence interval for proportions
The confidence interval for a proportion can be determined with the binom.test function, 
and more options are available in the BinomCI function and MultinomCI function in the
DescTools package.  More advanced techniques for confidence intervals on proportions
and differences in proportions can be found in the PropCIs package.

      binom.test(2, 20, 0.5,
           alternative="two.sided",
           conf.level=0.95)
--------------
Confidence interval for single proportion
### --------------------------------------------------------------
library(DescTools)
BinomCI(2, 20,
        conf.level = 0.95,
        method = "modified wilson")
---------
Confidence interval for multinomial proportion
### --------------------------------------------------------------
observed = c(35,74,22,69)
library(DescTools)
MultinomCI(observed, conf.level=0.95, method="goodman")

#     #     # 
=======
Histogram
hist(Data$ Fish,   
    col="gray", 
    main="Maryland Biological Stream Survey",
    xlab="Fish count")    
#     #     #
#----------------TEST OF ONE / TWO MEASUREMENRT OF VARIABLES -----------#

#--------One sample t-test with observations as vector-------------------------------
observed    = c(0.52, 0.20, 0.59, 0.62, 0.60)
theoretical = 0
t.test(observed,
       mu = theoretical,
       conf.int = 0.95)
#-------------------One sample t-test with observations in data frame
observed    = Data$ Angle
theoretical = 50
t.test(observed,
       mu = theoretical,
       conf.int=0.95)

-------Histogram
 hist(Data$ Angle,   
    col="gray", 
    main="Histogram of values",
    xlab="Angle")

#-------Power analysis
Power analysis for one-sample t-test
M1  = 70                        # Theoretical mean
M2  = 71                        # Mean to detect
S1  =  2.4                      # Standard deviation
S2  =  2.4                      # Standard deviation

Cohen.d = (M1 - M2)/sqrt(((S1^2) + (S2^2))/2) 
                                        
library(pwr)                                  
pwr.t.test(
       n = NULL,                  # Observations
       d = Cohen.d,           
       sig.level = 0.05,          # Type I probability
       power = 0.90,              # 1 minus Type II probability
       type = "one.sample",       # Change for one- or two-sample
       alternative = "two.sided")

#---------------Student’s t–test for Two Samples-----------------------#
Two-sample t-test, independent (unpaired) observations
bartlett.test(Value ~ Group, data=Data)
### If p-value >= 0.05, use var.equal=TRUE below

t.test(Value ~ Group, data=Data,
       var.equal=TRUE,
       conf.level=0.95)
---------------------
t.test(Value ~ Group, data=Data,
       var.equal=FALSE,
       conf.level=0.95)
----------------------
Plot of histograms
library(lattice)
histogram(~ Value | Group,
          data=Data,
          layout=c(1,2)      #  columns and rows of individual plots
          )
---------------------
boxplot(Value ~ Group,
        data = Data,
        names=c("2 pm","5 pm"),
        ylab="Value")
---------------------
##Similar tests......................................................
Welch’s t-test is discussed below.  
The paired t-test and signed-rank test are discussed in this book in their own chapters. 
Analysis of variance (anova) is discussed in several subsequent chapters.

As non-parametric alternatives, 
the Mann–Whitney U-test and the permutation test for two independent samples are discussed
in the chapter Mann–Whitney and Two-sample Permutation Test.

Welch’s t-test
Welch’s t-test is shown above in the “Example” section (“Two sample unpaired t-test”). 
It is invoked with the var.equal=FALSE option in the t.test function.
#........................................................................
#--------Power analysis
Power analysis for t-tes---------------------------

M1  = 100.6                      # Mean for sample 1
M2  = 103.6                      # Mean for sample 2
S1  =  5.26                      # Std dev for sample 1
S2  =  5.26                      # Std dev for sample 2

Cohen.d = (M1 - M2)/sqrt(((S1^2) + (S2^2))/2) 
                                        
library(pwr)                                  
pwr.t.test(
       n = NULL,                   # Observations in _each_ group
       d = Cohen.d,           
       sig.level = 0.05,           # Type I probability
       power = 0.90,               # 1 minus Type II probability
       type = "two.sample",        # Change for one- or two-sample
       alternative = "two.sided")

#-----------Mann–Whitney and Two-sample Permutation Test---------------
Box plots
 boxplot(Value ~ Group,
        data = Data,
        names=c("2 pm","5 pm"),
        ylab="Value")
---------
wilcox.test(Value ~ Group, data=Data)
#-------------------- One-way Anova ----------------------------------
The following commands will install these packages if they are not already installed:
if(!require(dplyr)){install.packages("dplyr")}
if(!require(FSA)){install.packages("FSA")}
if(!require(car)){install.packages("car")}
if(!require(agricolae)){install.packages("agricolae")}
if(!require(multcomp)){install.packages("multcomp")}
if(!require(DescTools)){install.packages("DescTools")}
if(!require(lsmeans)){install.packages("lsmeans")}
if(!require(multcompView)){install.packages("multcompView")}
if(!require(Rmisc)){install.packages("Rmisc")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(pwr)){install.packages("pwr")}
----------
#Specify the order of factor levels for plots and Dunnett comparison
library(dplyr)
Data =
mutate(Data,
       Location = factor(Location, levels=unique(Location)))

----------- 
#Produce summary statistics
library(FSA)  
Summarize(Aam ~ Location,
           data=Data,
           digits=3)
---------

#Fit the linear model and conduct ANOVA 
model = lm(Aam ~ Location,
           data=Data)

library(car)
Anova(model, type="II")                    # Can use type="III"

### If you use type="III", you need the following line before the analysis
### options(contrasts = c("contr.sum", "contr.poly"))

anova(model)                               # Produces type I sum of squares
summary(model)     # Produces r-square, overall p-value, parameter estimates
----------
#Checking assumptions of the model

hist(residuals(model),
     col="darkgray")
-------
plot(fitted(model),
     residuals(model))

A plot of residuals vs. predicted values.The residuals should be unbiased and homoscedastic.  
For an illustration of these properties,see this diagram by Steve Jost at DePaul University: 
condor.depaul.edu/sjost/it223/documents/resid-plots.gif.

### additional model checking plots with: plot(model)
### alternative: library(FSA); residPlot(model)

Tukey and Least Significant Difference mean separation tests (pairwise comparisons)
Tukey and other multiple comparison tests can be performed with a handful of functions. 
The functions TukeyHSD, HSD.test, and LSD.test are probably not appropriate for cases where 
there are unbalanced data or unequal variances among levels of the factor, 
though TukeyHSD does make an adjustment for mildly unbalanced data.  
It is my understanding that the multcomp and lsmeans packages are more appropriate for unbalanced data.
Another alternative is the DTK package that performs mean separation tests on data with unequal sample sizes and 
no assumption of equal variances.

------------
Tukey comparisons in agricolae package
library(agricolae)
(HSD.test(model, "Location"))          # outer parentheses print resul

-----------
LSD comparisons in agricolae package
library(agricolae)
(LSD.test(model, "Location",   # outer parentheses print result
           alpha = 0.05,      
           p.adj="none"))      # see ?p.adjust for options
------------
Multiple comparisons in multcomp package
Note that “Tukey” here does not mean Tukey-adjusted comparisons. 
It just sets up a matrix to compare each mean to each other mean.

library(multcomp)
mc = glht(model,
          mcp(Location = "Tukey"))
mcs = summary(mc, test=adjusted("single-step"))
mcs
### Adjustment options: "none", "single-step", "Shaffer",
###                     "Westfall", "free", "holm", "hochberg",
###                     "hommel", "bonferroni", "BH", "BY", "fdr"

cld(mcs,
    level=0.05,
    decreasing=TRUE)
---------------
Multiple comparisons to a control in multcomp package
### Control is the first level of the factor
library(multcomp)
mc = glht(model,
          mcp(Location = "Dunnett"))
summary(mc, test=adjusted("single-step"))

### Adjustment options: "none", "single-step", "Shaffer",
###                     "Westfall", "free", "holm", "hochberg",
###                     "hommel", "bonferroni", "BH", "BY", "fdr"
------------------
Multiple comparisons to a control with Dunnett Test
### The control group can be specified with the control option,
###   or will be the first level of the factor

library(DescTools)
DunnettTest(Aam ~ Location,
            data = Data)
----------------
Multiple comparisons with least square means
Least square means can be calculated for each group.  
Here a Tukey adjustment is applied for multiple comparisons among group least square means.
The multiple comparisons can be displayed as a compact letter display.

library(lsmeans)
library(multcompView)
leastsquare = lsmeans(model,
                      pairwise ~ Location,
                      adjust = "tukey")

cld(leastsquare,
    alpha   = 0.05,
    Letters = letters,
    adjust="tukey")
-------------------
 
Welch’s anova
Bartlett’s test and Levene’s test can be used to check the homoscedasticity of groups from a one-way anova.
A significant result for these tests (p < 0.05) suggests that groups are heteroscedastic.
One approach with heteroscedastic data in a one way anova is to use the Welch correction with the oneway.test function in the native stats package. 
A more versatile approach is to use the white.adjust=TRUE option in the Anova function from the car package.

### Bartlett test for homogeneity of variance
bartlett.test(Aam ~ Location,
       data = Data)
--------------
### Levene test for homogeneity of variance
library(car)
leveneTest(Aam ~ Location,
           data = Data)
--------------
### Welch’s anova for unequal variances
oneway.test(Aam ~ Location,
            data=Data,
            var.equal=FALSE)
--------------
### White-adjusted anova for heteroscedasticity
model = lm(Aam ~ Location,
           data=Data)
library(car)
Anova(model, Type="II",
      white.adjust=TRUE)

-----------
#Power analysis
Power analysis for one-way anova

library(pwr) 
groups = 5
means = c(10, 10, 15, 15, 15)
sd = 12
grand.mean  = mean(means)
Cohen.f = sqrt( sum( (1/groups) * (means-grand.mean)^2) ) /sd

pwr.anova.test(k = groups,
               n = NULL,
               f = Cohen.f,
               sig.level = 0.05,
               power = 0.80)

#----------------Kruskal–Wallis Test-------------------------------------------------#
if(!require(FSA)){install.packages("FSA")}
if(!require(DescTools)){install.packages("DescTools")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(multcompView)){install.packages("multcompView")}
if(!require(PMCMRplus)){install.packages("PMCMRplus ")}
--------------------
#Kruskal–Wallis test
In this case, there is a significant difference in the distributions of values among groups, 
as is evident both from the histograms and from the significant Kruskal–Wallis test.  
Only in cases where the distributions in each group are similar in shape and 
spread can a significant Kruskal–Wallis test be interpreted as a difference in medians.

kruskal.test(Value ~ Group,
             data = Data)

#Medians and descriptive statistics
library(FSA)
Summarize(Efficiency ~ Health,
          data = Data)

------------
#Kruskal–Wallis test
kruskal.test(Efficiency ~ Health,
             data = Data)


#Dunn test for multiple comparisons
If the Kruskal–Wallis test is significant, 
a post-hoc analysis can be performed to determine which levels of the independent variable differ from each other level. 
Probably the most popular test for this is the Dunn test, which is performed with the dunnTest function in the FSA package.  
  Adjustments to the p-values could be made using the method option to control the familywise error rate or to control the false discovery rate.  
See ?p.adjust for details. Zar (2010) states that the Dunn test is appropriate for groups with unequal numbers of observations.

If there are several values to compare, it can be beneficial to have R convert this table to a compact letter display for you. 
The cldList function in the rcompanion package can do this.

### Order groups by median
Data$Health = factor(Data$Health,
                     levels=c("OAD", "Normal", "Asbestosis"))

### Dunn test
library(FSA)

PT = dunnTest(Efficiency ~ Health,
              data=Data,
              method="bh")    # Can adjust p-values;
                              # See ?p.adjust for options

PT
-------------
### Specify the order of factor levels
##   otherwise R will alphabetize them
Data$Sex = factor(Data$Sex, levels=unique(Data$Sex))

### Examine data frame
str(Data)
### Summarize data
library(FSA)
Summarize(Rank ~ Sex,
          data = Data)

#------------------------ Two-way Anova -------------------------
if(!require(FSA)){install.packages("FSA")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(car)){install.packages("car")}
if(!require(multcompView)){install.packages("multcompView")}
if(!require(lsmeans)){install.packages("lsmeans")}
if(!require(grid)){install.packages("grid")}
if(!require(nlme)){install.packages("nlme")}
if(!require(lme4)){install.packages("lme4")}
if(!require(lmerTest)){install.packages("lmerTest")} if(!require(rcompanion)){install.packages("rcompanion")}

----------------------
#Means and summary statistics by group
 
library(Rmisc)
sum = summarySE(Data,
                measurevar="Activity",
                groupvars=c("Sex","Genotype"))
sum

---------------
Interaction plot using summary statistics
 
library(ggplot2)
pd = position_dodge(.2)
ggplot(sum, aes(x=Genotype,
                y=Activity,
                color=Sex)) +
    geom_errorbar(aes(ymin=Activity-se,
                      ymax=Activity+se),
                   width=.2, linewidth=0.7, position=pd) +
    geom_point(shape=15, size=4, position=pd) +
    theme_bw() +
    theme(
          axis.title.y = element_text(vjust= 1.8),
          axis.title.x = element_text(vjust= -0.5),
          axis.title = element_text(face = "bold")) +
    scale_color_manual(values = c("black", "blue"))

-----------
#Simple box plot of main effect and interaction

boxplot(Activity ~ Genotype,
        data = Data,
        xlab = "Genotype",
        ylab = "MPI Activity",
        col  = "white")

---------------
Fit the linear model and conduct ANOVA
 model = lm(Activity ~ Sex + Genotype + Sex:Genotype,
           data=Data)
library(car)
Anova(model, type="II")                    # Can use type="III"

### If you use type="III", you need the following line before the analysis
### options(contrasts = c("contr.sum", "contr.poly"))
anova(model)                               # Produces type I sum of squares
summary(model)     # Produces r-square, overall p-value, parameter estimates
------------------
#Checking assumptions of the model
 
hist(residuals(model),
     col="darkgray")

plot(fitted(model),
     residuals(model))

-----------------------
Fit the linear model and conduct ANOVA
 model = lm(Openings ~ Day + Snake,
           data=Data)
library(car)
Anova(model, type="II")                    # Can use type="III"
anova(model)                               # Produces type I sum of squares
summary(model)     # Produces r-square, overall p-value, parameter estimates

---------
Checking assumptions of the model
 
hist(residuals(model),
     col="darkgray")

plot(fitted(model),
     residuals(model))

#----------------------Paired t–test----------------------------
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(coin)){install.packages("coin")}
if(!require(pwr)){install.packages("pwr")}

#Paired t-test
 

t.test(Data$Typical,
       Data$Odd,
       paired=TRUE,
       conf.level=0.95)

#Simple plot of differences
Difference = Data$Odd - Data$Typical
plot(Difference,
     pch = 16,
     ylab="Difference (Odd – Typical)")
abline(0,0, col="blue", lwd=2)

--------------
#Simple 1-to-1 plot of values
plot(Data$Typical, Data$Odd,
     pch = 16,
     xlab="Typical feathers",
     ylab="Odd feathers")
abline(0,1, col="blue", lwd=2)
-----------

#-----------------------------Wilcoxon Signed-rank Test---------------
wilcox.test(Data$August,
            Data$November,
            paired=TRUE)

#Simple 1-to-1 plot of values
plot(Data$August, Data$November,
     pch = 16,
     xlab="August",
     ylab="November")
abline(0,1, col="blue", lwd=2)

#-------------------------  Correlation and Linear Regression  --------------------------#
Correlation
Correlation can be performed with the cor.test function in the native stats package.  
It can perform Pearson, Kendall, and Spearman correlation procedures.  
Methods for multiple correlation of several variables simultaneously are discussed in the Multiple regression chapter.
 
Pearson correlation
Pearson correlation is the most common form of correlation. 
It is a parametric test, and assumes that the data are linearly related and that the residuals are normally distributed.

cor.test( ~ Species + Latitude,
         data=Data,
         method = "pearson",
         conf.level = 0.95)

Kendall correlation
Kendall rank correlation is a non-parametric test that does not assume a distribution of the data 
or that the data are linearly related.  It ranks the data to determine the degree of correlation.

cor.test( ~ Species + Latitude,
         data=Data,
         method = "kendall",
         continuity = FALSE,
         conf.level = 0.95)

Spearman correlation
Spearman rank correlation is a non-parametric test that does not assume a distribution of the data or 
that the data are linearly related.  
It ranks the data to determine the degree of correlation, and is appropriate for ordinal measurements.

cor.test( ~ Species + Latitude,
         data=Data,
         method = "spearman",
         continuity = FALSE,
         conf.level = 0.95)

Linear regression
Linear regression can be performed with the lm function in the native stats package.
A robust regression can be performed with the lmrob function in the robustbase package.

model = lm(Species ~ Latitude,
           data = Data)

summary(model)                    # shows parameter estimates,
                                  # p-value for model, r-square

library(car)
Anova(model, type="II")              # shows p-value for effects in model


Plot linear regression
int =  model$coefficient["(Intercept)"]
slope =model$coefficient["Latitude"]
plot(Species ~ Latitude,
     data = Data,
     pch=16,
     xlab = "Latitude",
     ylab = "Species")
abline(int, slope,
       lty=1, lwd=2, col="blue")     #  style and color of line 
--------------
#Checking assumptions of the model

hist(residuals(model),
     col="darkgray")
plot(fitted(model),
     residuals(model))
----------
A plot of residuals vs. predicted values.
The residuals should be unbiased and homoscedastic.
For an illustration of these properties, see this diagram by Steve Jost at DePaul University:
condor.depaul.edu/sjost/it223/documents/resid-plots.gif.

### additional model checking plots with: plot(model)
### alternative: library(FSA); residPlot(model)

#----------Power analysis
Power analysis for correlation
pwr.r.test(n = NULL,
           r = 0.500,
           sig.level = 0.05,
           power = 0.80,
           alternative = "two.sided")

#-------------------- Curvilinear Regression -----------------------
How to do the test
This chapter will fit models to curvilinear data using three methods: 
1) Polynomial regression;  2) B-spline regression with polynomial splines;  and 
3) Nonlinear regression with the nls function.  
In this example, each of these three will find essentially the same best-fit curve with very similar p-values and R-squared values. 

Polynomial regression
Polynomial regression is really just a special case of multiple regression, which is covered in the Multiple regression chapter.  
In this example we will fit a few models, as the Handbook does, and then compare the models with the extra sum of squares test, 
the Akaike information criterion (AIC), and the adjusted R-squared as model fit criteria. 

For a linear model (lm), the adjusted R-squared is included with the output of the summary(model) statement.  
The AIC is produced with its own function call, AIC(model).  
The extra sum of squares test is conducted with the anova function applied to two models. 

For AIC, smaller is better.  For adjusted R-squared, larger is better. 
A non-significant p-value for the extra sum of squares test comparing model a to model b indicates that 
the model with the extra terms does not significantly reduce the error sum of squares over the reduced model.  
Which is to say, a non-significant p-value suggests the model with the additional terms is not better than the reduced model.
Simple plot of model
 
----------------------
plot(Clutch ~ Length,
     data = Data,
     pch=16,
     xlab = "Carapace length",
     ylab = "Clutch") 
i = seq(min(Data$Length), max(Data$Length), len=100)       #  x-values for line
predy = predict(model, data.frame(Length=i))               #  fitted values
lines(i, predy,                                            #  spline curve
      lty=1, lwd=2, col="blue")                            #  style and color


#---------------------Multiple Regression---------------------------- 
library(psych)
corr.test(Data.num,
          use = "pairwise",
          method="pearson",
          adjust="none",     # Can adjust p-values; see ?p.adjust for options
          alpha=.05)

 pairs(data=Data,
      ~ Longnose + Acerage + DO2 + Maxdepth + NO3 + SO4 + Temp)


library(PerformanceAnalytics)
chart.Correlation(Data.num,
                   method="pearson",
                   histogram=TRUE,
                   pch=16)
-------------
Multiple regression
 

Model selection using the step function
The step function has options to add terms to a model (direction="forward"), remove terms from a model (direction="backward"), or to use a process that both adds and removes terms (direction="both").  It uses AIC (Akaike information criterion) as a selection criterion.  You can use the option k = log(n) to use BIC instead. 

 

You can add the test="F" option to see the p-value for adding or removing terms, but the test will still follow the AIC statistic.  If you use this, however, note that a significant p-value essentially argues for the term being included in the model, whether it’s its addition or its removal that’s being considered.

 

A full model and a null are defined, and then the function will follow a procedure to find the model with the lowest AIC.
The final model is shown at the end of the output, with the Call: indication, and lists the coefficients for that model.

#Stepwise procedure
 
model.null = lm(Longnose ~ 1,
                data=Data)
model.full = lm(Longnose ~ Acerage + DO2 + Maxdepth + NO3 + SO4 + Temp,
                data=Data)
    
step(model.null,
     scope = list(upper=model.full),
             direction="both",
             data=Data)
---------------

#Define final model
 
model.final = lm(Longnose ~ Acerage + Maxdepth + NO3,
                 data=Data)
summary(model.final)      # Show coefficients, R-squared, and overall p-value

#Simple plot of predicted values with 1-to-1 line
 
Data$predy = predict(model.final)
plot(predy ~ Longnose,
     data=Data,
     pch = 16,
     xlab="Actual response value",
     ylab="Predicted response value")
abline(0,1, col="blue", lwd=2)

#-----------------------------------Simple Logistic Regression----------------------
if(!require(car)){install.packages("car")}
if(!require(lmtest){install.packages("lmtest")}
if(!require(tidyr)){install.packages("tidyr")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(FSA){install.packages("FSA")}
if(!require(popbio)){install.packages("popbio")}

   
plot(Factor.num  ~ Continuous,
     data = Data,
     xlab="Continuous",
     ylab="Factor",
     pch=19)             
curve(predict(model,data.frame(Continuous=x),type="response"),
      lty=1, lwd=2, col="blue",                           
      add=TRUE)   

   ------------  
library(popbio)
logi.hist.plot(Data$Continuous,
               Data$Factor.log,
               boxp=FALSE,
               type="hist",
               col="gray",
               xlabel="Height")


#-----------------TEST OF NOMINAL VARIABLE------------------------------#
#--------------Exact Test of Goodness-of-Fit----------------------------#

#The exact test goodness-of-fit can be performed with the binom.test function in the native stats package.#
The arguments passed to the function are: the number of successes, the number of trials, and the hypothesized probability of success.#
The probability can be entered as a decimal or a fraction.  
#Other options include the confidence level for the confidence interval about the proportion,
# and whether the function performs a one-sided or two-sided (two-tailed) test.  
#In most circumstances, the two-sided test is used.

#Packages used in this chapter
The following commands will install these packages if they are not already installed:

if(!require(XNomial)){install.packages("XNomial")}
if(!require(pwr)){install.packages("pwr")}
if(!require(BSDA)){install.packages("BSDA")}

#How the test works
#Binomial test examples

### --------------------------------------------------------------
### Cat paw example, exact binomial test, pp. 30–31
### --------------------------------------------------------------
      ### In this example:
      ###   2 is the number of successes
      ###   10 is the number of trials
      ###   0.5 is the hypothesized probability of success

dbinom(2, 10, 0.5)            # Probability of single event only!
                              #   Not binomial test!

[1] 0.04394531  


binom.test(2, 10, 0.5,
           alternative="less",       # One-sided test
           conf.level=0.95) 

p-value = 0.05469

binom.test(2, 10, 0.5,
           alternative="two.sided",  # Two-sided test
           conf.level=0.95)


p-value = 0.1094

# #     
#Probability density plot

### --------------------------------------------------------------
### Probability density plot, binomial distribution, p. 31
### --------------------------------------------------------------
      # In this example:
      #   You can change the values for trials and prob
      #   You can change the values for xlab and ylab

trials = 10
prob = 0.5

x = seq(0, trials)                   # x is a sequence, 1 to trials
y = dbinom(x, size=trials, p=prob)   # y is the vector of heights

barplot (height=y,
         names.arg=x,
         xlab="Number of uses of right paw",
         ylab="Probability under null hypothesis")


#Comparing doubling a one-sided test and using a two-sided test
 
### --------------------------------------------------------------
### Cat hair example, exact binomial test, p. 31–32
###  Compares performing a one-sided test and doubling the
###    probability, and performing a two-sided test
### --------------------------------------------------------------

binom.test(7, 12, 3/4,
           alternative="less",
           conf.level=0.95) 

p-value = 0.1576

Test = binom.test(7, 12, 3/4,             # Create an object called
                  alternative="less",     #  Test with the test
                  conf.level=0.95)        #  results.

2 * Test$ p.value               # This extracts the p-value from the
                                #   test result, we called Test
                                #   and multiplies it by 2

[1] 0.3152874

 
binom.test(7, 12, 3/4, alternative="two.sided", conf.level=0.95)

p-value = 0.1893      # Equal to the "small p values" method in the Handbook
#     #     #


Post-hoc test

Post-hoc example with manual pairwise tests
A multinomial test can be conducted with the xmulti function in the package XNomial. 
This can be followed with the individual binomial tests for each proportion, as post-hoc tests.

 

### --------------------------------------------------------------
### Post-hoc example, multinomial and binomial test, p. 33
### --------------------------------------------------------------

observed = c(72, 38, 20, 18)
expected = c(9, 3, 3, 1)

library(XNomial)
xmulti(observed,
       expected,
       detail = 2)         # 2: Reports three types of p-value

 
P value  (LLR)  =  0.003404  # log-likelihood ratio

P value (Prob)  =  0.002255  # exact probability

P value (Chisq) =  0.001608  # Chi-square probability

 

### Note last p-value below agrees with Handbook

successes   = 72
total       = 148
numerator   = 9
denominator = 16

binom.test(successes, total, numerator/denominator,
           alternative="two.sided", conf.level=0.95) 

p-value = 0.06822 

successes   = 38
total       = 148
numerator   = 3
denominator = 16

binom.test(successes, total, numerator/denominator,
           alternative="two.sided", conf.level=0.95) 

p-value = 0.03504


successes   = 20
total       = 148
numerator   = 3
denominator = 16

binom.test(successes, total, numerator/denominator,
           alternative="two.sided", conf.level=0.95) 

p-value = 0.1139

successes   = 18
total       = 148
numerator   = 1
denominator = 16

binom.test(successes, total, numerator/denominator,
           alternative="two.sided", conf.level=0.95) 

p-value = 0.006057
#     #     #

Post-hoc test alternate method with custom function
When you need to do multiple similar tests, however, 
#it is often possible to use the programming capabilities in R to do the tests more efficiently.
#The following example may be somewhat difficult to follow for a beginner.  
#It creates a data frame and then adds a column called p.Value that contains the p-value 
from the binom.test performed on each row of the data frame.

### --------------------------------------------------------------
### Post-hoc example, multinomial and binomial test, p. 33
###    Alternate method for multiple tests
### --------------------------------------------------------------

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
### --------------------------------------------------------------
### Parasitoid examples, exact binomial test, p. 34
### --------------------------------------------------------------
binom.test(10, (17+10), 0.5,
           alternative="two.sided",
           conf.level=0.95)
p-value = 0.2478
binom.test(36, (7+36), 0.5,
           alternative="two.sided",
           conf.level=0.95)

p-value = 8.963e-06
#     #     #

### --------------------------------------------------------------
###  Drosophila example, exact binomial test, p. 34
### --------------------------------------------------------------

binom.test(140, (106+140), 0.5,
           alternative="two.sided",
           conf.level=0.95)
p-value = 0.03516
#     #     #

#Sign test example
The following is an example of the two-sample dependent-samples sign test.
The data are arranged as a data frame in which each row contains the values 
for both measurements being compared for each experimental unit.
This is sometimes called “wide format” data.  
The SIGN.test function in the BSDA package is used.  
The option md=0 indicates that the expected difference in the medians is 0 (null hypothesis).
#This function can also perform a one-sample sign test.

### --------------------------------------------------------------
###  Tree beetle example, two-sample sign test, p. 34–35
### --------------------------------------------------------------

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

SIGN.test(x = Data$ A.count,
          y = Data$ B.count,
          md = 0,                 
          alternative = "two.sided",
          conf.level = 0.95)

Binomial test examples
### --------------------------------------------------------------
###  First Mendel example, exact binomial test, p. 35
### --------------------------------------------------------------

binom.test(428, (428+152), 0.75, alternative="two.sided",
           conf.level=0.95)

 

p-value = 0.5022          # Value is different than in the Handbook

                          #     See next example
#     #     #

 

 

### --------------------------------------------------------------
###  First Mendel example, exact binomial test, p. 35
###     Alternate method with XNomial package
### --------------------------------------------------------------

observed = c(428, 152)
expected = c(3, 1)

library(XNomial)

xmulti(observed,
       expected,
       detail = 2)             # 2: reports three types of p-value

 
P value  (LLR)  =  0.5331   # log-likelihood ratio

P value (Prob)  =  0.5022   # exact probability

P value (Chisq) =  0.5331   # Chi-square probability

### Note last p-value below agrees with Handbook
#     #     #

  

Multinomial test example
### --------------------------------------------------------------
###  Second Mendel example, multinomial exact test, p. 35–36
### --------------------------------------------------------------
observed = c(315, 108, 101, 32)
expected = c(9, 3, 3, 1)
library(XNomial)

xmulti(observed,
       expected,
       detail = 2)              # reports three types of p-value

P value  (LLR)  =  0.9261    # log-likelihood ratio
P value (Prob)  =  0.9382    # exact probability
P value (Chisq) =  0.9272    # Chi-square probability

### Note last p-value below agrees with Handbook,
###   and agrees with SAS Exact Pr>=ChiSq
#     #     # 


#---------------Chi-square Test of Goodness-of-Fit--------------#
The following commands will install these packages if they are not already installed:

if(!require(dplyr)){install.packages("dplyr")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(grid)){install.packages("grid")}
if(!require(pwr)){install.packages("pwr")}

Chi-square goodness-of-fit example
### --------------------------------------------------------------
### Drosophila example, Chi-square goodness-of-fit, p. 46
### --------------------------------------------------------------

observed = c(770, 230)        # observed frequencies
expected = c(0.75, 0.25)      # expected proportions

chisq.test(x = observed,
           p = expected)

X-squared = 2.1333, df = 1, p-value = 0.1441
#     #     #

Post-hoc test
Assumptions
See the Handbook for information on these topics.

 
Examples: extrinsic hypothesis
### --------------------------------------------------------------
### Crossbill example, Chi-square goodness-of-fit, p. 47
### --------------------------------------------------------------

observed = c(1752, 1895)    # observed frequencies
expected = c(0.5, 0.5)      # expected proportions

chisq.test(x = observed,
           p = expected)


X-squared = 5.6071, df = 1, p-value = 0.01789
#     #     #

 
### --------------------------------------------------------------
### Rice example, Chi-square goodness-of-fit, p. 47
### --------------------------------------------------------------
observed = c(772, 1611, 737)
expected = c(0.25, 0.50, 0.25)

chisq.test(x = observed,
           p = expected)

X-squared = 4.1199, df = 2, p-value = 0.1275
#     #     #

### --------------------------------------------------------------
### Bird foraging example, Chi-square goodness-of-fit, pp. 47–48
### --------------------------------------------------------------
observed = c(70, 79, 3, 4)
expected = c(0.54, 0.40, 0.05, 0.01)

chisq.test(x = observed,
           p = expected)

X-squared = 13.5934, df = 3, p-value = 0.0035
#     #     #

 Example: intrinsic hypothesis  
### --------------------------------------------------------------
### Intrinsic example, Chi-square goodness-of-fit, p. 48
### --------------------------------------------------------------
observed       = c(1203,  2919,  1678)
expected.prop  = c(0.211, 0.497, 0.293)
expected.count = sum(observed)*expected.prop
chi2 = sum((observed- expected.count)^2/ expected.count)
chi2


[1] 1.082646

pchisq(chi2,
       df=1,
       lower.tail=FALSE)    

[1] 0.2981064
#     #     #

Simple bar plot with barplot
 ### --------------------------------------------------------------
### Simple bar plot of proportions, p. 49
###      Uses data in a matrix format
### --------------------------------------------------------------
observed = c(70, 79, 3, 4)
expected = c(0.54, 0.40, 0.05, 0.01)

total = sum(observed)
observed.prop = observed / total

observed.prop 

### Re-enter data as a matrix
Input =("
Value     Douglas.fir  Ponderosa.pine  Grand.fir   Western.larch
Observed  0.4487179    0.5064103       0.01923077  0.02564103
Expected  0.5400000    0.4000000       0.05000000  0.01000000  
")

Matriz = as.matrix(read.table(textConnection(Input),
                   header=TRUE,
                   row.names=1))
Matriz 

         Douglas fir Ponderosa pine  Grand fir Western larch

Observed   0.4487179      0.5064103 0.01923077    0.02564103
Expected   0.5400000      0.4000000 0.05000000    0.01000000

barplot(Matriz,
        beside=TRUE,
        legend=TRUE,
        ylim=c(0, 0.6),
        xlab="Tree species",
        ylab="Foraging proportion")
#     #     #

Simple bar plot with barplot
### --------------------------------------------------------------
### Simple bar plot of proportions, p. 49
###      Uses data in a matrix format
### --------------------------------------------------------------
observed = c(70, 79, 3, 4)
expected = c(0.54, 0.40, 0.05, 0.01)

total = sum(observed)
observed.prop = observed / total
observed.prop

[1] 0.44871795 0.50641026 0.01923077 0.02564103

 

 

### Re-enter data as a matrix

Input =("
Value     Douglas.fir  Ponderosa.pine  Grand.fir   Western.larch
Observed  0.4487179    0.5064103       0.01923077  0.02564103
Expected  0.5400000    0.4000000       0.05000000  0.01000000  
")

Matriz = as.matrix(read.table(textConnection(Input),
                   header=TRUE,
                   row.names=1))

Matriz 

         Douglas fir Ponderosa pine  Grand fir Western larch

Observed   0.4487179      0.5064103 0.01923077    0.02564103
Expected   0.5400000      0.4000000 0.05000000    0.01000000

 barplot(Matriz,
        beside=TRUE,
        legend=TRUE,
        ylim=c(0, 0.6),
        xlab="Tree species",
        ylab="Foraging proportion")
#     #     #


#------------------------Power Analysis-------------------------#
if(!require(pwr)){install.packages("pwr")}
Power analysis for binomial test
##--------------------------------------------------------------#
## Power analysis, binomial test, pea color, p. 43
##--------------------------------------------------------------#
P0 = 0.75
P1 = 0.78
H  = ES.h(P0,P1)               # This calculates effect size

library(pwr)

pwr.p.test(
       h=H,
       n=NULL,                  # NULL tells the function to
       sig.level=0.05,          #     calculate this
       power=0.90,              # 1 minus Type II probability
       alternative="two.sided")


n = 2096.953   # Somewhat different than in Handbook
#     #     #

#--------------Power analysis for unpaired t-test---------------#

##--------------------------------------------------------------#
### Power analysis, t-test, student height, pp. 43–44
### ------------------------------------------------------------#
M1  = 66.6                      # Mean for sample 1
M2  = 64.6                      # Mean for sample 2
S1  =  4.8                      # Std dev for sample 1
S2  =  3.6                      # Std dev for sample 2

Cohen.d = (M1 - M2)/sqrt(((S1^2) + (S2^2))/2) 
                                         
library(pwr)                                  
pwr.t.test(
       n = NULL,                  # Observations in _each_ group
       d = Cohen.d,           
       sig.level = 0.05,          # Type I probability
       power = 0.80,              # 1 minus Type II probability
       type = "two.sample",       # Change for one- or two-sample
       alternative = "two.sided"
       )

 
Two-sample t test power calculation
n = 71.61288
NOTE: n is number in *each* group 71.61288
#     #     #


#Similar tests   - Chi-square vs. G–test

See the Handbook for information on these topics.  
The exact test of goodness-of-fit, the G-test of goodness-of-fit, 
and the exact test of goodness-of-fit tests are described elsewhere in this book.

 
#How to do the test-----------------------------------------------
Chi-square goodness-of-fit example

### --------------------------------------------------------------
### Pea color example, Chi-square goodness-of-fit, pp. 50–51
### --------------------------------------------------------------
observed = c(423, 133)  
expected = c(0.75, 0.25)
 
chisq.test(x = observed,
           p = expected)

X-squared = 0.3453, df = 1, p-value = 0.5568
#     #     #

Power analysis
Power analysis for chi-square goodness-of-fit
### --------------------------------------------------------------
### Power analysis, Chi-square goodness-of-fit, snapdragons, p. 51
### --------------------------------------------------------------
library(pwr)

P0      = c(0.25,  0.50, 0.25)
P1      = c(0.225, 0.55, 0.225)

effect.size = ES.w1(P0, P1) 
degrees = length(P0) - 1

pwr.chisq.test(
               w=effect.size,
               N=NULL,            # Total number of observations
               df=degrees,
               power=0.80,        # 1 minus Type II probability
               sig.level=0.05)    # Type I probability


N = 963.4689
#     #     #


#---------------G–test of Goodness-of-Fit-------------------------------
The following commands will install these packages if they are not already installed:

if(!require(DescTools)){install.packages("DescTools")}
if(!require(RVAideMemoire)){install.packages("RVAideMemoire")}

Examples: extrinsic hypothesis
G-test goodness-of-fit test with DescTools and RVAideMemoire

### --------------------------------------------------------------
### Crossbill example, G-test goodness-of-fit, p. 55
### --------------------------------------------------------------
observed = c(1752, 1895)    # observed frequencies
expected = c(0.5, 0.5)      # expected proportions

library(DescTools)
GTest(x=observed,
      p=expected,
      correct="none")       # "none" "williams" "yates"

library(RVAideMemoire)

G.test(x=observed,
       p=expected)
#  #  #

G-test goodness-of-fit test by manual calculation
### --------------------------------------------------------------
### Crossbill example, G-test goodness-of-fit, p. 55
###   Manual calculation
### --------------------------------------------------------------
observed      = c(1752, 1895)     # observed frequencies
expected.prop = c(0.5, 0.5)       # expected proportions

degrees = 1                       # degrees of freedom
expected.count = sum(observed)*expected.prop

G = 2 * sum(observed * log(observed / expected.count))
G                          

[1] 5.608512

pchisq(G,
       df=degrees,
       lower.tail=FALSE) 

[1] 0.01787343
#     #     #


Examples of G-test goodness-of-fit test with DescTools and RVAideMemoire
 ### --------------------------------------------------------------
### Rice example, G-test goodness-of-fit, p. 55
### --------------------------------------------------------------
observed = c(772, 1611, 737)
expected = c(0.25, 0.50, 0.25)

library(DescTools)

GTest(x=observed,
      p=expected,
      correct="none")            # "none" "williams" "yates"

 

Log likelihood ratio (G-test) goodness of fit test
G = 4.1471, X-squared df = 2, p-value = 0.1257


library(RVAideMemoire)
G.test(x=observed,
       p=expected)

 G-test for given probabilities
G = 4.1471, df = 2, p-value = 0.1257

#     #     #

### --------------------------------------------------------------
### Foraging example, G-test goodness-of-fit, pp. 55–56
### --------------------------------------------------------------

observed = c(70, 79, 3, 4)
expected = c(0.54, 0.40, 0.05, 0.01)

library(DescTools)   

GTest(x=observed,
      p=expected,
      correct="none")            # "none" "williams" "yates"
     

Log likelihood ratio (G-test) goodness of fit test
G = 13.145, X-squared df = 3, p-value = 0.004334

library(RVAideMemoire)
G.test(x=observed,
       p=expected)

G-test for given probabilities
G = 13.1448, df = 3, p-value = 0.004334
#     #     #


Example: intrinsic hypothesis
### --------------------------------------------------------------
### Intrinsic example, G-test goodness-of-fit, amphipod, p. 56
### --------------------------------------------------------------
observed       = c(1203,  2919,  1678)
expected.prop  = c(.21073, 0.49665, 0.29262)
 
### Note: These are recalculated for more precision
###       In this case, low precision probabilities
###         change the results
expected.count = sum(observed)*expected.prop
  
G = 2 * sum(observed * log(observed / expected.count))
G                         

[1] 1.032653

pchisq(G,
       df=1,
       lower.tail=FALSE)  
 

[1] 0.3095363
#     #     #

#-------------------------Chi-square Test of Independence-----------------------
he Chi-square test of independence can be performed with the chisq.test function in the native stats package in R.
For this test, the function requires the contingency table to be in the form of matrix.
Depending on the form of the data to begin with, this can require an extra step, either combing vectors into a matrix,
or cross-tabulating the counts among factors in a data frame.  None of this is too difficult, 
but it requires following the correct example depending on the initial form of the data. 
 
When using read.table and as.matrix to read a table directly as a matrix, 
be careful of extra spaces at the end of lines or extraneous characters in the table, as these can cause errors.

Packages used in this chapter
The following commands will install these packages if they are not already installed:

if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(grid)){install.packages("grid")}
if(!require(pwr)){install.packages("pwr")}

When to use it

Example of chi-square test with matrix created with read.table
 ### --------------------------------------------------------------
### Vaccination example, Chi-square independence, pp. 59–60
###      Example directly reading a table as a matrix
### --------------------------------------------------------------
Input =("
Injection.area  No.severe  Severe
Thigh           4788       30
Arm             8916       76
")

Matriz = as.matrix(read.table(textConnection(Input),
                   header=TRUE,
                   row.names=1))
Matriz  

chisq.test(Matriz,
           correct=TRUE)      # Continuity correction for 2 x 2
                              #      table
chisq.test(Matriz,
           correct=FALSE)      # No continuity correction for 2 x 2
                               #      table


#--Example of chi-square test with matrix created by combining vectors
 ### --------------------------------------------------------------
### Vaccination example, Chi-square independence, pp. 59–60
###  Example creating a matrix from vectors
### --------------------------------------------------------------
R1 = c(4788, 30)
R2 = c(8916, 76)
rows   = 2
Matriz = matrix(c(R1, R2),
                nrow=rows,
                byrow=TRUE)

rownames(Matriz) = c("Thigh", "Arm")          # Naming the rows and
colnames(Matriz) = c("No.severe", "Severe")   #  columns is optional.
Matriz

chisq.test(Matriz,
           correct=TRUE)      # Continuity correction for 2 x 2
                              #      table
chisq.test(Matriz,
           correct=FALSE)      # No continuity correction for 2 x 2
                               #      table
=====================================================================
Post-hoc tests
For the following example of post-hoc pairwise testing, 
we’ll use the pairwiseNominalIndependence function from the package rcompanion to make the task easier.
Then we’ll use pairwise.table in the native stats package as an alternative.

Post-hoc pairwise chi-square tests with rcompanion
### --------------------------------------------------------------
### Post-hoc example, Chi-square independence, pp. 60–61
### --------------------------------------------------------------
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


Post-hoc pairwise chi-square tests with pairwise.table
### --------------------------------------------------------------
### Post-hoc example, Chi-square independence, pp. 60–61
### As is, this code works on a matrix with two columns,
###   and compares rows
### --------------------------------------------------------------
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

=======================

Examples

Chi-square test of independence with continuity correction and without correction
### --------------------------------------------------------------
### Helmet example, Chi-square independence, p. 63
### --------------------------------------------------------------
Input =("
PSE        Head.injury  Other.injury
Helemt     372          4715
No.helmet  267          1391
")

Matriz = as.matrix(read.table(textConnection(Input),
                   header=TRUE,
                   row.names=1))
Matriz
 
chisq.test(Matriz,
           correct=TRUE)       # Continuity correction for 2 x 2
                               #      table

chisq.test(Matriz,
           correct=FALSE)      # No continuity correction for 2 x 2
                               #      table 
#     #     #

Chi-square test of independence
### --------------------------------------------------------------
### Gardemann apolipoprotein example, Chi-square independence,
###   p. 63
### --------------------------------------------------------------
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

Simple bar plot with error bars showing confidence intervals
### --------------------------------------------------------------
### Plot example, herons and egrets, Chi-square test of association,
###   pp. 63–64
### --------------------------------------------------------------
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

                      
### Plot (Bar chart plot)
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


Bar plot with categories and no error bars
 

### --------------------------------------------------------------
### Plot example, herons and egrets, Chi-square independence,
###   p. 64
### --------------------------------------------------------------

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


### Specify the order of factor levels
library(dplyr)
Birds=
mutate(Birds,
       Habitat = factor(Habitat,levels=unique(Habitat)),
       Bird = factor(Bird,levels=unique(Bird))
       )
      
### Add sums and proportions
Birds$ Sum[Birds$ Bird == 'Heron'] =
       sum(Birds$ Count[Birds$ Bird == 'Heron'])

Birds$ Sum[Birds$ Bird == 'Egret'] =
       sum(Birds$ Count[Birds$ Bird == 'Egret'])

Birds=
mutate(Birds,
       prop = Count / Sum
       )
Birds 


### Plot adapted from:
### shinyapps.stat.ubc.ca/r-graph-catalog/

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
In the following example for the chi-square test of independence, 
the data is read in as a data frame, not as a matrix as in previous examples.
This allows more flexibility with how data are entered.  
For example you could have counts for same genotype and health distributed among several lines,
or have a count of 1 for each row, with a separate row for each individual observation.  
The xtabs function is used to tabulate the data and convert them to a contingency table.
### --------------------------------------------------------------
### Gardemann apolipoprotein example, Chi-square independence,
###      SAS example, pp. 65–66
###      Example using cross-tabulation
### --------------------------------------------------------------
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


###  Cross-tabulate the data
Data.xtabs = xtabs(Count ~ Genotype + Health,
                   data=Data.frame)
Data.xtabs 

summary(Data.xtabs)     # includes N and factors

###  Chi-square test of independence
chisq.test(Data.xtabs)
#     #     #

Power analysis
Power analysis for chi-square test of independence
 ### --------------------------------------------------------------
### Power analysis, chi-square independence, pp. 66–67
### --------------------------------------------------------------
# This example assumes you are using a Chi-square test of
#   independence.  The example in the Handbook appears to use
#   a Chi-square goodness-of-fit test
# In the pwr package, for the Chi-square test of independence,
#   the table probabilities should sum to 1

Input =("
Genotype  No.cancer Cancer
GG        0.18      0.165
GA        0.24      0.225
AA        0.08      0.110
")

P = as.matrix(read.table(textConnection(Input),
              header=TRUE,
              row.names=1))
P
==
sum(P)        # Sum of values in the P matrix
[1] 1

library(pwr)
effect.size = ES.w2(P) 
degrees = (nrow(P)-1)*(ncol(P)-1)  # Calculate degrees of freedom

pwr.chisq.test(
       w=effect.size,
       N=NULL,            # Total number of observations
       df=degrees,
       power=0.80,        # 1 minus Type II probability
       sig.level=0.05)    # Type I probability  
#     #     #


#------------------------G–test of Independence-------------------
G-test example with functions in DescTools and RVAideMemoire
###--------------------------------------------------------------
### Vaccination example, G-test of independence, pp. 68–69
### --------------------------------------------------------------
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
For the following example of post-hoc pairwise testing, we’ll use the pairwise.G.test function from the package RVAideMemoire to make the task easier.  Then we’ll use pairwise.table in the native stats package as an alternative.

 

Post-hoc pairwise G-tests with RVAideMemoire
 

### --------------------------------------------------------------
### Post-hoc example, G-test of independence, pp. 69–70
### --------------------------------------------------------------

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
                                             # see ?p.adjust for options


-------
Post-hoc pairwise G-tests with pairwise.table
As is, this function works on a matrix with two columns, and compares rows.
### --------------------------------------------------------------
### Post-hoc example, G-test of independence, pp. 69–70
### --------------------------------------------------------------
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


#---------------------Fisher’s Exact Test of Independence------------------
if(!require(rcompanion)){install.packages("rcompanion")}

Post-hoc pairwise Fisher’s exact tests with RVAideMemoire
### --------------------------------------------------------------
### Post-hoc example, Fisher’s exact test, p. 79
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


-----
Examples
Examples of Fisher’s exact test with data in a matrix
### --------------------------------------------------------------
### Chipmunk example, Fisher’s exact test, p. 80
### --------------------------------------------------------------
 
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
### Drosophila example, Fisher’s exact test, p. 81
### --------------------------------------------------------------

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

 

 

p-value = 0.006653

 

#     #     #

 

 

### --------------------------------------------------------------
### King penguin example, Fisher’s exact test, p. 81
### --------------------------------------------------------------

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
### Moray eel example, Fisher’s exact test, pp. 81–82
### --------------------------------------------------------------

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
### Herons example, Fisher’s exact test, p. 82
### --------------------------------------------------------------

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
Care is needed in setting up the data for McNemar’s test.  For a before-and-after test, the contingency table is set-up as before and after as row and column headings, or vice-versa.  Note that the total observations in the contingency table is equal to the number of experimental units.  That is, in the following example there are 62 men, and the sum of the counts in the contingency table is 62.  If you set up the table incorrectly, you might end with double this number, and this will not yield the correct results.

 

McNemar’s test with data in a matrix
 

### --------------------------------------------------------------
### Dysfunction example, McNemar test, pp. 82–83
### --------------------------------------------------------------

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

 

 

McNemar’s test with data in a data frame
### --------------------------------------------------------------
### Dysfunction example, McNemar test, pp. 82–83
###    Example using cross-tabulation
### --------------------------------------------------------------

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


How to do the test
Fisher’s exact test with data as a data frame
### --------------------------------------------------------------
### Chipmunk example, Fisher’s exact test, SAS example, p. 83
### Example using cross-tabulation
### --------------------------------------------------------------

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



### Fisher’s exact test of independence

fisher.test(Data.xtabs,
             alternative="two.sided")
#     #     #

### --------------------------------------------------------------
### Bird example, Fisher’s exact test, SAS example, p. 84
### Example using cross-tabulation
### --------------------------------------------------------------
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


How to do the test
Repeated G–tests of goodness-of-fit example
### --------------------------------------------------------------
### Arm crossing example, Repeated G–tests of goodness-of-fit,
###      pp. 91–93
### --------------------------------------------------------------
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
 

Data.matrix = as.matrix(Data[c("D", "S")])      # We need a data matrix
                                                #   to run G-test
Data.matrix                                     #   for heterogeneity
 

G.test(Data.matrix)                             # Heterogeneity


 
Pooled G-test
 

Total.D = sum(Data$D)                           # Set up data for pooled
Total.S = sum(Data$S)                           #   G-test

observed = c(Total.D, Total.S)
expected = c(0.5, 0.5)

G.test(x=observed,                              # Pooled
       p=expected)

                      
Total G-test
 
Total.G = sum(Data$G)                           # Set up data for total
                                                #   G-test
degrees = 3
  
Total.G  = sum(Data$G)                          # Set up data for total
                                                #   G-test                                      
Total.df = sum(Data$df)

Total.G                                         # Total

Total.df

pchisq(Total.G,
       df=Total.df,
       lower.tail=FALSE)
#     #     #  

#=========== Cochran–Mantel–Haenszel Test for Repeated Tests of Independence ===========
if(!require(dplyr)){install.packages("dplyr")}
if(!require(DescTools)){install.packages("DescTools")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(grid)){install.packages("grid")}
if(!require(vcd)){install.packages("vcd")


Cochran–Mantel–Haenszel Test with data read by read.ftable
 

### --------------------------------------------------------------
### Handedness example, Cochran–Mantel–Haenszel test, p. 97–98
###    Example using read.ftable
### --------------------------------------------------------------

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

ftable(Tabla)                        # Display a flattened table


                  
Cochran–Mantel–Haenszel test
mantelhaen.test(Tabl

                
Woolf test

library(vcd)
oddsratio(Tabla, log=TRUE)            # Show log odds for each 2x2

library(vcd)
woolf_test(Tabla)                # Woolf test for homogeneity of
                                 #   odds ratios across strata.
                                 #   If significant, C-M-H test
                                 #   is not appropriate

Breslow-Day test
 
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

 ### Note: "Group" must be the name of the stratum variable
--------------

Cochran–Mantel–Haenszel Test with data entered as a data frame
### --------------------------------------------------------------
### Mussel example, Cochran–Mantel–Haenszel test, pp. 98–99
###    Example using cross-tabulation of a data frame
### --------------------------------------------------------------

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
                
### Specify the order of factor levels
### Otherwise, R will alphabetize them

library(dplyr)

Data =
mutate(Data,
       Location = factor(Location, levels=unique(Location)),
       Habitat = factor(Habitat, levels=unique(Habitat)),
       Allele = factor(Allele, levels=unique(Allele))
       )

### Cross-tabulate the data
###   Note here, Location is stratum variable (is last)
###              Habitat x Allele are 2 x 2 tables

Data.xtabs = xtabs(Count ~ Allele + Habitat + Location,
                   data=Data)

ftable(Data.xtabs)                      # Display a flattened table

                
Cochran–Mantel–Haenszel test

mantelhaen.test(Data.xtabs)

#that is fine for now
