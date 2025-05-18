++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Practical Guide to Statistical Power and Sample Size in R 
---------------------------------------------------------------------------
# d = effect size ---- sig level=0.05 ---- power = 80/90 --- type = type of test
#-----Effect size 0.2=small, 0.5=medium, 0.8=large
#-----Effect size = (M2 -M1)/SD 
  
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
------------------ Power analysis for one-sample t-test -------------------
  
  M1  = 70                        # Theoretical mean
  M2  = 71                        # Mean to detect
  S1  =  2.4                      # Standard deviation
  S2  =  2.4                      # Standard deviation
  
  Cohen.d = (M1 - M2)/sqrt(((S1^2) + (S2^2))/2) 
  
# pwr.t.test(d = , sig.level = , power = , type = "one.sample") ----------- 
  
library(pwr)                                  
pwr.t.test(n = NULL, d = Cohen.d, sig.level = 0.05, power = 0.90, type = "one.sample", 
           alternative = "two.sided")
  
#   #   #     


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
----------------- Power analysis for Single proportion test ---------------  
#effect size = h
?pwr.p.test     

h= 2*asin(sqrt(0.00495))-2*asin(sqrt(0.00490))    
h
pwr.p.test(h = 0.0007, n = NULL, sig.level = 0.05, power = 0.80, alternative = "two.sided") 

#effect size   0.2(small),  0.5(medium), 0.8(large)
pwr.p.test(h = 0.2, n = NULL, sig.level = 0.05, power = 0.80, alternative = "two.sided")   
    
#   #   #


++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   
----------------- Power analysis for Two proportion test -----------------       
#effect size = h
h= 2*asin(sqrt(0.090))-2*asin(sqrt(0.050))    
h
pwr.2p.test(h = 0.158, n = NULL, sig.level = 0.05, power = 0.80, alternative = "two.sided") 
  
#   #   #  

  
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#----One Sample test for Categorical (Probability against theoretical value)
-------------------- Power analysis for binomial test ----------------------

  P0 = 0.75
  P1 = 0.78
  H  = ES.h(P0,P1)               # This calculates effect size
  
  library(pwr)
  pwr.p.test(h=H, n=NULL, sig.level=0.05, power=0.90, alternative="two.sided")
  
  #     #     # 

    
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
---------------- Power analysis for chi-square Goodness-Of-Fit -------------
# Power analysis, Chi-square goodness-of-fit, snapdragons
#effect size   0.1(small),  0.3(medium), 0.5(large)    
library(pwr)
  
 P0      = c(0.25,  0.50, 0.25)
P1      = c(0.225, 0.55, 0.225)
effect.size = ES.w1(P0, P1) 
degrees = length(P0) - 1
  
pwr.chisq.test(w=effect.size, N=NULL, df=degrees, power=0.80, sig.level=0.05)
  
#   #   #
  
  
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
---------- Power analysis for chi-square test of independence -------------
    
# In the pwr package, for the Chi-square test of independence,the table probabilities should sum to 1
    
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
# Sum of values in the P matrix
sum(P)        
  
library(pwr)
effect.size = ES.w2(P)
degrees = (nrow(P)-1)*(ncol(P)-1) 
  
pwr.chisq.test(w=effect.size, N=NULL, df=degrees, power=0.80, sig.level=0.05)  
  
#     #     #
  
  
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                          Two_Three sample test
----------------------------------------------------------------------------
# Effect size ------ 0.2=small,   0.5=medium,     0.8=large
#Hypothesis
#------alternative = "greater",       "less",           "two-sided"
#----------where      greater(H1>H0),  less(H1<H0),      two-sided(difference)
    
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
--------------------- Power analysis for unpaired t-test -------------------
  M1  = 66.6                      # Mean for sample 1
  M2  = 64.6                      # Mean for sample 2
  S1  =  4.8                      # Std dev for sample 1
  S2  =  3.6                      # Std dev for sample 2
  
  Cohen.d = (M1 - M2)/sqrt(((S1^2) + (S2^2))/2) 
  
library(pwr)                                  
pwr.t.test(n = NULL, d = Cohen.d, sig.level = 0.05, power = 0.80, type = "two.sample",       
           alternative = "two.sided")

#   #    #

  
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Name of Test-------------- Two Sample Paired T test------------------------ 
  library(pwr)
  M1  = 66.6                      # Mean for sample 1
  M2  = 66.7                      # Mean for sample 2
  S1  =  4.8                      # Std dev for sample 1
  S2  =  4.6                      # Std dev for sample 2
  
  Cohen.d = (M1 - M2)/sqrt(((S1^2) + (S2^2))/2) 
  
library(pwr)                                  
pwr.t.test(n = NULL, d = Cohen.d, sig.level = 0.05, power = 0.80, type = "paired",       
           alternative = "two.sided")


#   #   #


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++             
Name of Test -------------- Power analysis for one-way anova --------------
  library(pwr) 
  groups = 5
  means = c(10, 10, 15, 15, 15)
  sd = 12
  grand.mean  = mean(means)
  Cohen.f = sqrt( sum( (1/groups) * (means-grand.mean)^2) ) /sd
  
pwr.anova.test(k = groups, n = NULL, f = Cohen.f, sig.level = 0.05, power = 0.80) 

#   #   #
library(effsize)
cohen.d(age ~ expose, data=subset(imdata, expose!= "non exposed"), paired=FALSE)        
cohen.d(age ~ expose, data=subset(imdata, expose!= "singleexposed"),paired=FALSE)
#   #   #

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
----------------------- Power analysis for correlation -------------------
  
pwr.r.test(n = NULL, r = 0.500, sig.level = 0.05, power = 0.80, alternative = "two.sided")
  
#    #    # 


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                          All non parametric 
One sample/paired Wilcoxon, Mann-Whitney, Kruskal Wallis ----> must add +15% to the estimated sample size   
__________________________________________________________________________    
# example,
# An estimated sample size with parametric test(72.70583) ------> sample size
# must be estimated for non-parametric as(72.70583*1.15,0) ------> sample size


#    #     #


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
--------------- Power analysis for Simple linear regression ---------------  
# effect size = f2,--------------------------> f2= R = sqrt(R * R)
# df numerator = u                            R = Correlation coefficient
# df denominator = v                          R_squared = goodness of fit
#                                             use adjusted R_squared

#effect size   0.02(small),  0.15(medium), 0.35(large) 
u = 1
   
pwr.f2.test(u = 1,  f2 = 0.15, sig.level = 0.05, power = 0.80)
#round(52.315)+2 ----------> sample size

#   #   #      


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++    
-------------- Power analysis for Multiple linear regression --------------
u = 3   
    
pwr.f2.test(u = 3,  f2 = 0.15, sig.level = 0.05, power = 0.80)   
#round(72.70583)+4  ------> sample size    
    

#    #    #    
    
  
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Multi_way ANOVA ---- Non linear regression ---- Multilevel modelling
---------------------------- clinical trials ------------------------------
                       install.packages("WebPower")


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-------------- Power analysis for Repeated Measures ANOVA -----------------
library(WebPower) 
wp.rmanova(ng = NULL, nm = NULL, f = NULL, nscor = 1, alpha = 0.05, power = 0.80, type = (0,1,2))  
  
# ng------number of groups
# nm------number of measurement
# nscor----nonsphericity correction coefficient-(assumed to be 1)
# f-------Effect size 0.1(small), 0.25(medium), 0.4(large)
# Type 0(between-effect),1(within-effect), 2(interaction effect)

wp.rmanova(ng = 1, nm = 4, f = 0.25, nscor = 1, alpha = 0.05, power = 0.80, type = 1)


#To generate a power curve given a sequence of sample sizes:
rang_all <- wp.rmanova(n=seq(30,150,20), ng=3, nm=4, f=0.36, nscor=0.7)

rang_all

plot(rang_all)

#   #   #


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
---- Power analysis for Multi_way ANOVA ( 1 categorical of interest) ------
#----when there is more than one category but the interest is on one while
#    the other category is controlled (blocking,nested,random effect)
library(WebPower)  
# ndf -----numerator degree of freedom
wp.kanova(ndf = NULL, f = NULL, ng = NULL, alpha = 0.05, power = 0.80)

wp.kanova(ndf = 2, f = 0.25, ng = 12, alpha = 0.05, power = 0.80)
#   sample(157.3764)/ 12  ===== 14 per each 12 groups


#   #   #


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
---- Power analysis for Multi_way ANOVA (>1 categorical of interest) ------
#----when there is more than one category and each group is of interest. 

example: 
#----is there different in treatment(A,B,C) across age (Child, Adult, Elder) and
#    Cancer stage (i,ii,iii,iv,v)
#Categorical of interest----(TREATMENT, AGE and CANCER Stage)   
solution:  
# Numerator df -----> Treat df, Age df, Stage df ---->(3-1)*(3-1)*(5-1) ====2*2*4 ==== 16  
# Number of groups---->Treat * Age * Stage ----> (3*3*5) ====== 45 

 wp.kanova(ndf = 16, f = 0.10, ng = 45, alpha = 0.05, power = 0.80)
#   sample (1941)/ 45  ===== 44 per each 45 groups

  
#   #   # 


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
----------- Power analysis for Non parametric regression(Logistic) --------
?wp.logistic
wp.logistic(n = NULL, p0 = NULL, P1 = NULL, alpha = 0.05, power = 0.80,
            alternative = c("two.sided", "less", "greater"),
            family = c("Bernoulli", "exponential", "lognormal", "normal", "poisson", "uniform"),
            parameter = NULL)
  
  
wp.logistic(n = NULL, p0 = 0.15, p1 = 0.25, alpha = 0.05, power = 0.80,
            alternative = "two.sided",
            family = "normal")  
  
#To generate a power curve given a sequence of sample sizes:
range_size <- wp.logistic(n = seq(250,600,50), p0 = 0.15, p1 = 0.1, alpha = 0.05,
                   power = NULL, family = "normal", parameter = c(0,1))
range_size


#To plot the power curve:
plot(range_size)

#   #   #
  
 
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
----------------- Power analysis for poisson regression ------------------
  ?wp.poisson              
  install.packages("WebPower")           
  library(WebPower)            
  
  wp.poisson(exp0 = , exp1 = , alpha = 0.05, power = 0.80, alternative = "two.sided", family = "Bernoulli")
  
  #Derive exp0 (Base rate) and exp1 (Rate ratio) from the exponentials,
  #then plug in the Incidence Base rate and Rate ratio
  exp0 <-exp(13.0/100000)
  exp0
  exp1 <-exp(-0.05/100000)
  exp1
  
  #1---------To calculate power given sample size and effect size:
  POWER <-wp.poisson(n = 4406, exp0 = 2.798, exp1 = 0.8938, alpha = 0.05,
                     power = NULL, family = "Bernoulli", parameter = 0.53)
  POWER
  
  
  #2--------To calculate the required sample size given power and effect size:
  One.sample <-wp.poisson(n = NULL, exp0 = 1.00013, exp1 = 0.8938, alpha = 0.05,
                          power = 0.80, alternative = ("two.sided"),
                          family = ("Bernoulli"), 
                          parameter = NULL)
  One.sample
  
  
#3--------To generate sequence of sample sizes given power and effect size:
multi.samples <- wp.poisson(n = seq(400, 1600, 100), exp0 = 2.798, exp1 = 0.8938,
                            alpha = 0.05, power = NULL, family = "Bernoulli", parameter = 0.53)
multi.samples  
#4--------To plot the power curve:  
plot(multi.samples) 
  
#    #    # 

  
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
#---------------- Power analysis for Multilevel modelling ------------------
library(WebPower)-----------------------------------------------------------
?wp.crt2arm
                        Multilevel modeling-CRT
  #CRT ----> are type of multilevel design where the entire cluster is randomly
  #          assigned to control arm or one/more treatment arms
-------------------- Cluster Randomized Trial (CRT) ------------------------   
# f----------Effect size 0.1(small), 0.25(medium), 0.4(large)
# n ---------sample size.
# j ---------number of clusters
# icc -------intra class corellation (degree to which two randomly obs within a cluster are correlated )
wp.crt2arm(n = NULL, f = NULL, J = NULL, icc = NULL, alpha = 0.05, power = 0.80,alternative = ("two.sided", "one.sided"))
  
#----CRT sample size(number per cluster)    
wp.crt2arm(n = NULL, f = 0.25, J = 100, icc = 0.1, alpha = 0.05, power = 0.80, alternative = "two.sided")      
#----CRT sample size(number of clusters)  
wp.crt2arm(n = 15, f = 0.25, J = NULL, icc = 0.1, alpha = 0.05, power = 0.80, alternative = "two.sided") 
--------------------------------------------------
wp.crt2arm(f = 0.25, n = NULL, J = 10, icc = 0.1, alpha = 0.05, power = 0.8)
#Generate sequence of sample sizes:
range.sequence <- wp.crt2arm(f = 0.6, n = seq(20,100,10), J = 10, icc = 0.1, alpha = 0.05, power = NULL)
range.sequence

plot(range.sequence)

#    #    #


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# -------------------------------- G L M M --------------------------------
----------- Power analysis for Generalized Linear Mixed Model ------------- 

library(lme4) # to create models
library(simr) # to get Sample size

  


  
#    #    #
            
