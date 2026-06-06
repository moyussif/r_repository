


#-------------- Barchart
Hanisah100 <- as.data.frame(Hanisah)
BarChart(Hanisah100, Delivery_mode)


#---------------- Histogram:
Histogram
hist(Hanisah$mum_Age, col= "turquoise", main="Maternal Age", xlab="Age")

# # #

#---------------- Using GGPLOT2
library(ggplot2)

ggplot(data = DATASET,
       mapping = aes(x = COLUMN, y = COLUMN, colour, fill, alpha))+
  geom()



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

# Facet wrap 
immwrp <- ggplot(data = Hanisah25,
                 mapping = aes(x = Baby_sex, y = Baby_Weight, fill = Diagnosis3))+
  geom_violin()+
  facet_wrap(~Diagnosis3)
immwrp

# Facet grid
immwrp <- ggplot(data = Hanisah25,
                 mapping = aes(x = Baby_sex, y = Baby_Weight, fill = Baby_G6PD))+
  geom_violin()+
  facet_grid(~Diagnosis3)
immwrp

#----------------------Viridis ---
immuLAB <- ggplot(data = imdata)+
  geom_point(mapping = aes(x = systol1, y = diastol1, colour = expose))+
  labs(title ="Immunoassay Data", subtitle = "ImmunoAnalysis",caption = "Data collection by Mohammed")+
  annotate( "text",x = 145,y = 90, label ="Malaria in pregnancy                              ", color ="purple",
            fontface ="bold", size =4.0,angle = 30)+
  theme_classic()+scale_colour_viridis_d()
immuLAB



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


# Save plot 
ggsave("p.png")








++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                            Descriptive statistics                            
library(skimr)
str(Hanisah)
skim_without_charts(Hanisah)

library(psych)
describeBy(Hanisah)

#   #   #



  #   #    #
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
  
  +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
?friedman.test()
  
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
================================================================================
                      Chisquare - Fisher Exact
________________________________________________________________________________

cngTB <- read_excel("C:/Users/User/Desktop/MTB only_ Lineage.xlsx")
str(cngTB)

table(cngTB$Lineage,cngTB$Gender)
# Perform Fisher's Exact Test
fisher.test(cngTB$Lineage,cngTB$Gender, simulate.p.value = TRUE)
# Perform Chi-square Test
chisq.test(cngTB$Lineage,cngTB$Gender, simulate.p.value = TRUE)



================================================================================
+                     Multinomial regression                                   +
================================================================================ 
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
=========================
+++++++++++++++++++++++++
