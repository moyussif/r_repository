rm(list=ls())
gc(reset = TRUE)
#--------------------------- required package
install.packages("AICcmodavg")
install.packages("DescTools")
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                          Inferential Statistics                              +
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(readxl)
library(readr)
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
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Import data
  Sars_2 <- read_excel("C:/Users/User/Desktop/Sars-2.xlsx")
  View(Sars_2)
  print(Sars_2)
  str(Sars_2)
 --------------------------------------------------------------------------------
                  Data Conversion _(CODING)  
 -------------------------------------------------------------------------------- 
  Sars_2$AgeCategory <- as.factor(Sars_2$AgeCategory)
  Sars_2$SEX <- as.factor(Sars_2$SEX)
  Sars_2$SarsCov_Strain <- as.factor(Sars_2$SarsCov_Strain)
  Sars_2$Hospitalstatus <- as.factor(Sars_2$Hospitalstatus)
  Sars_2$categoryofcases <- as.factor(Sars_2$categoryofcases)
  Sars_2$TreatmentOUTCOME <- as.factor(Sars_2$TreatmentOUTCOME)
  Sars_2$infection <- as.factor(Sars_2$infection)
  Sars_2$Resistance <- as.factor(Sars_2$Resistance)
  Sars_2$organism <- as.factor(Sars_2$organism)
  Sars_2$smoking <- as.factor(Sars_2$smoking)
  
  # revisit the data structure
  str(Sars_2)

  # Explore the data by using descriptive statistics----------------------------

  summary(Sars_2)
  #further insight into data normality
  describeBy(Sars_2) 
  # use normality test
  shapiro.test(Sars_2$Age)
  # Basic plots
  hist(Sars_2$Age)

  #Transform the data
  SarsCoV$Agee <-log(SarsCoV$Age) 
  
  str(SarsCoV)
  
  hist(SarsCoV$Agee)
  
  shapiro.test(SarsCoV$Agee)
  #
  #Barplot
  plot(SarsCoV$SEX)
  #boxplot
  plot(x= SarsCoV$SEX,y = SarsCoV$Age)
  #scatterplot
  plot(x= SarsCoV$systolic1, y= SarsCoV$sugar_level)
  
  #pie / donut -----depending the input at the hole
  PieChart(categoryofcases, data = SarsCoV, hole = 0.5, main = NULL)
 #
  
+++++++++++++++++ One sample test of Mean ++++++++++++++++++++++++++++++++++++++
#One sample t.test
  
   theorical = 35
  observed = SarsCoV$Age
  
  t.test(observed,mu=theorical, conf.int=95.0 )# Option1 
  #
  t.test(SarsCoV$Age,mu=35, conf.int=95.0 )  #Option 2
  hist(SarsCoV$Age)  
    
    

+++++++++++++++++ Two sample test of Mean ++++++++++++++++++++++++++++++++++++++
#Independent t.test
t.test(Age ~ SEX, data = SarsCoV, Var.equal =FALSE, conf.level = 0.95)  
#
hist(Age ~ SEX, data = SarsCoV, names=c("Female","Male"), ylab="Age")  
#
boxplot(Age ~ SEX, data = SarsCoV, names=c("Female","Male"), ylab="Age")
  
#Paried t.test
t.test(Sars_2$systolic1,Sars_2$systolic2,paired = TRUE,conf.level = 0.95) 
#

# # #
++++++++++++++++++++ More than Two sample (ANOVA) ++++++++++++++++++++++++++++++    
library(lessR)
library(car)
library(psych)
library(ggplot2)
library(ggpubr)
library(readxl)
library(AICcmodavg) 
library(DescTools)  
#==============================================================================
library(lessR)
library(readxl)
print(Sars_2)
#
Plot(Duration_days, data=SarsCoV, facet1 = categoryofcases)
#normality 
tapply(Sars_2$diastolic1, Sars_2$categoryofcases,shapiro.test)
#equality of variance
leveneTest(Duration_days ~ categoryofcases, data=Sars_2)
#One-way ANOVA
ANOVA(Duration_days ~ categoryofcases, data=Sars_2)
#Bar charts 
Duration_mean <- tapply(Sars_2$Duration_days, Sars_2$categoryofcases,mean)
BarChart(Duration_mean)

#==============================================================================
#--------------------- One_Way ANOVA (Between group )---------------------------
#
#Step1-----fit model for residuals
redd <- aov(Age ~ categoryofcases, data = SarsCoV) 
redd 
summary(redd)

#step2-----Check Normality
shapiro.test(redd$residuals)

#step3-----homogeneity of variance
leveneTest(Age ~ categoryofcases, data = SarsCoV)

#Step4----------plots Histogram & QQplot
par(mfrow = c(1,2))
  #histogram
  hist(redd$residuals)
  #QQ-plot
  QQ <- qqPlot(redd$residuals, id = TRUE)             

# # # 
#--------------------- Factorial ANOVA ---------------------------------------
  #
  #homogeneity of variance multiple variable
  leveneTest(Age ~ categoryofcases*SEX*smoking*TreatmentOUTCOME*SarsCov_Strain, data = SarsCoV) 
  # #
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
  
  
  
  
  
  
  
  
  
  
  
  
  
     
#Repeated measure ANOVA(Within group)
    
+++++++++++++++++ One sample test of proportions +++++++++++++++++++++++++++++++  
#Chisqure Goodness of Fit
    
+++++++++++++++++ Two sample test of Asociation ++++++++++++++++++++++++++++++++ 
#Chisquare of independence
  
  
  