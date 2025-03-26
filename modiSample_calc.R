





------------------- Power analysis for one-sample t-test -----------------------
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


  --------------------- Power analysis for unpaired t-test --------------------
  #-------------------- Power analysis, t-test --------------------------------
  
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


  ---------------------Power analysis for one-way anova--------------------------
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

  
  
---------------------------Power analysis for correlation-----------------------
    pwr.r.test(n = NULL,
               r = 0.500,
               sig.level = 0.05,
               power = 0.80,
               alternative = "two.sided") 
  
  
---------------------- Power analysis for binomial test ---------------------
    if(!require(pwr)){install.packages("pwr")}
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
  
  #     #     #  
  
  ---------------- Power analysis for chi-square goodness-of-fit ------
    # Power analysis, Chi-square goodness-of-fit, snapdragons
    ## ------------------------------------------------------------------
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
  
  
  ------------ Power analysis for chi-square test of independence ----------------
    #--------------------------------------------------------------
  Power analysis, chi-square independence, pp. 66–67
  --------------------------------------------------------------
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
  # Sum of values in the P matrix
  sum(P)        
  
  
  library(pwr)
  effect.size = ES.w2(P)
  degrees = (nrow(P)-1)*(ncol(P)-1)  # Calculate degrees of freedom
  
  pwr.chisq.test(
    w=effect.size,
    N=NULL,          # Total number of observations
    df=degrees,
    power=0.80,      # 1 minus Type II probability
    sig.level=0.05)  # Type I probability  
  
  
  #     #     #
  
 
  
  
  
  
  
  

+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Practical Guide to Statistical Power and Sample Size Calculations in R ==== *1*
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

install.packages("pwrss")
library(pwr)
library(pwrss)

#t Test (one sample)
power.t.test(ncp = 1.96, df = 99, alpha = 0.05,
             alternative = "equivalent", plot = TRUE)

#z Test(one sample)
power.z.test(ncp = 1.96, alpha = 0.05, 
             alternative = "not equal", plot = TRUE)

#χ2Test (chisquare)
power.chisq.test(ncp = 15, df = 20,
                 alpha = 0.05, plot = TRUE)

#F Test (Anova)
power.f.test(ncp = 3, df1 = 2, df2 = 98,
             alpha = 0.05, plot = TRUE)

#Multiple parameters
power.t.test(ncp = c(0.50, 1.00, 1.50, 2.00, 2.50), plot = FALSE,
             df = 99, alpha = 0.05, alternative = "not equal")

power.z.test(alpha = c(0.001, 0.010, 0.025, 0.050), plot = FALSE,
             ncp = 1.96, alternative = "greater")

power.chisq.test(df = c(80, 90, 100, 120, 150, 200), plot = FALSE, 
                 ncp = 2, alpha = 0.05)

#Type I and Type II Error Plots
# comparing two means
design1 <- pwrss.t.2means(mu1 = 0.20, margin = -0.05, paired = TRUE,
                          power = 0.80, alpha = 0.05,
                          alternative = "non-inferior")
plot(design1)

# ANCOVA design
design2 <- pwrss.f.ancova(eta2 = 0.10, n.levels = c(2,3),
                          power = .80, alpha = 0.05)
plot(design2)


# indirect effect in mediation analysis
design3 <- pwrss.z.med(a = 0.10, b = 0.20, cp = 0.10,
                       power = .80, alpha = 0.05)
plot(design3)

-------------------------------------------------------------
#--------------- Mean Difference (t Tests) -----------------

#Independent Samples t Test----------------------

#What is the statistical power given that the sample size for the second group is 50 (n2 = 50) and 
#groups have equal sample sizes (kappa = n1 / n2 = 1)?
  
  pwrss.t.2means(mu1 = 30, mu2 = 28, sd1 = 12, sd2 = 8, kappa = 1, 
                 n2 = 50, alpha = 0.05,
                 alternative = "not equal")


#What is the minimum required sample size given that groups have equal sample sizes (kappa = 1)? (κ=n1/n2)

pwrss.t.2means(mu1 = 30, mu2 = 28, sd1 = 12, sd2 = 8, kappa = 1, 
               power = .80, alpha = 0.05,
               alternative = "not equal")

#Paired Samples t Test

#assume a correlation of 0.50 between first and second measurements (by default paired.r = 0.50). What is the minimum required sample size?
  
  pwrss.t.2means(mu1 = 30, mu2 = 28, sd1 = 12, sd2 = 8, 
                 paired = TRUE, paired.r = 0.50,
                 power = .80, alpha = 0.05,
                 alternative = "not equal")


#Non-parametric Tests--------------------
For non-parametric tests use pwrss.np.2groups() function instead of pwrss.t.2means().

#Mann-Whitney Test
pwrss.np.2groups(mu1 = 30, mu2 = 28, sd1 = 12, sd2 = 8, kappa = 1, 
                 power = .80, alpha = 0.05,
                 alternative = "not equal") 

#Wilcoxon Signed-rank Test

pwrss.np.2groups(mu1 = 30, mu2 = 28, sd1 = 12, sd2 = 8, 
                 paired = TRUE, paired.r = 0.50,
                 power = .80, alpha = 0.05,
                 alternative = "not equal")
-------------------------------------------------------------------------------
#parametric
#----------- Non-inferiority, Superiority, and Equivalence --------------------
#Non-inferiority: 
#The mean of group 1 is practically not smaller than the mean of group 2. T
#he mu1 - mu2 difference can be as small as -1 (margin = -1). 
#What is the minimum required sample size?
pwrss.np.2groups(mu1 = 30, mu2 = 28, sd1 = 12, sd2 = 8, 
                 margin = -1, power = 0.80,
                 alternative = "non-inferior"

#Superiority: 
#The mean of group 1 is practically greater than the mean of group 2. 
#The mu1 - mu2 difference is at least greater than 1 (margin = 1). 
#What is the minimum required sample size?
pwrss.t.2means(mu1 = 30, mu2 = 28, sd1 = 12, sd2 = 8, 
               margin = 1, power = 0.80,
               alternative = "superior")


#Equivalence: 
#The mean of group 1 is practically same as mean of group 2. 
#The mu1 - mu2 difference can be as small as -1 and as high as 1 (margin = 1). 
#What is the minimum required sample size? Specify the absolute value for the margin.

pwrss.t.2means(mu1 = 30, mu2 = 30, sd1 = 12, sd2 = 8, 
               margin = 1, power = 0.80,
               alternative = "equivalent")



#Non-parametric Tests--------------------------------

##Non-inferiority:
#The mean of group 1 is practically not smaller than the mean of group 2. 
#The mu1 - mu2 difference can be as small as -1 (margin = -1). 
#What is the minimum required sample size?
  
  pwrss.np.2groups(mu1 = 30, mu2 = 28, sd1 = 12, sd2 = 8, 
                   margin = -1, power = 0.80,
                   alternative = "non-inferior")

#Superiority: 
#The mean of group 1 is practically greater than the mean of group 2. 
#The mu1 - mu2 difference is at least greater than 1 (margin = 1). 
#What is the minimum required sample size?

pwrss.np.2groups(mu1 = 30, mu2 = 28, sd1 = 12, sd2 = 8, 
                 margin = 1, power = 0.80,
                 alternative = "superior")

#Equivalence: 
#The mean of group 1 is practically same as mean of group 2. 
#The mu1 - mu2 difference can be as small as -1 and as high as 1 (margin = 1).
#What is the minimum required sample size?

pwrss.np.2groups(mu1 = 30, mu2 = 30, sd1 = 12, sd2 = 8, 
                 margin = 1, power = 0.80,
                 alternative = "equivalent")


#------------------------------------------------
+++++++++++++++++++++++++++++++++++++++++++++++++
#Linear Regression (F and t Tests)

# Omnibus F Test
# R2>0 in Linear Regression
#Omnibus F test in multiple liner regression is used to test whether R2 is greater than 0 (zero). 
#Assume that we want to predict a continuous variable Y using X1, X2, and X2 variables (a combination of binary or continuous)

#We are expecting that these three variables explain 30% of the variance in the outcome (R2=0.30 or r2 = 0.30 in the code). 
What is the minimum required sample size?
#
pwrss.f.reg(r2 = 0.30, k = 3, power = 0.80, alpha = 0.05)
--------------------------------------------------------

#R2>0 in Hierarchical Regression
#Assume that we want to add two more predictors to the earlier regression model (X4, and X5; thus m = 2) 
# and test whether increase in R2 is greater than 0 (zero). 
# In total there are five predictors in the model (k = 5). 
# By adding two predictors we are expecting an increase of ΔR2=0.15
.What is the minimum required sample size?
#
pwrss.f.reg(r2 = 0.15, k = 5, m = 2, power = 0.80, alpha = 0.05)


#------------- Single Regression Coefficient (z or t Test) --------

#Standardized versus Unstandardized Input

#In the earlier example, assume that we want to predict a continuous variable Y using a continuous predictor X1 
#but control for X2, and X2 variables (a combination of binary or continuous). 
#We are mainly interested in the effect of X1 and expect a standardized regression coefficient of β1=0.20
.
# Again, we are expecting that these three variables explain 30% of the variance in the outcome (R2=0.30). 
# What is the minimum required sample size? It is sufficient to provide standardized regression coefficient for beta1
# because sdx = 1 and sdy = 1 by default.
pwrss.t.reg(beta1 = 0.20, k = 3, r2 = 0.30, 
            power = .80, alpha = 0.05, alternative = "not equal")



#-----------Logistic Regression (Wald’s z Test) ----------

#A base probability of P0=0.15. This is the rate when predictor X1=0 or when β1=0
.#Increasing X1 from 0 to 1 reduces the probability of being in group 1 from 0.15 to 0.10 (P1=0.10).
#What is the minimum required sample size?

# There are three types of specification to statistical power or sample size calculations; 
# (i) probability specification, (ii) odds ratio specification, and
# (iii) regression coefficient specification (as in standard software output).

#Probability specification:
  pwrss.z.logreg(p0 = 0.15, p1 = 0.10, r2.other.x = 0.20,
                 power = 0.80, alpha = 0.05, 
                 dist = "normal")

#Odds ratio specification:
pwrss.z.logreg(p0 = 0.15, odds.ratio = 0.6296, r2.other.x = 0.20,
                 alpha = 0.05, power = 0.80,
                 dist = "normal")

#Regression coefficient specification:
pwrss.z.logreg(p0 = 0.15, beta1 = -0.4626, r2.other.x = 0.20,
               alpha = 0.05, power = 0.80,
               dist = "normal")

#----------- Poisson Regression (Wald’s z Test) -------------

#in Poisson regression a count outcome variable 
# (e.g. number of hospital/store/website visits, number of absence/dead/purchase in a day/week/month) 
# is modeled by predicting incidence rate (λ) via logarithmic transformation (natural logarithm of rates). 
# A model with one main predictor (X1) and two other covariates (X2 and X3) can be constructed as

Assume
The expected base incidence rate is 1.65: exp(0.50)=1.65
Increasing X1 from 0 to 1 reduces the mean incidence rate from 1.65 to 0.905: exp(−0.10)=0.905
#What is the minimum required sample size? 
There are two types of specification;
#(i) rate ratio specification (exponentiated regression coefficient), and 
#(ii) raw regression coefficient specification (as in standard software output).

#Regression coefficient specification:
  pwrss.z.poisreg(beta0 = 0.50, beta1 = -0.10,
                  power = 0.80, alpha = 0.05, 
                  dist = "normal")

#Rate ratio specification:
  pwrss.z.poisreg(exp.beta0 = exp(0.50),
                  exp.beta1 = exp(-0.10),
                  power = 0.80, alpha = 0.05, 
                  dist = "normal")

The function accomodates other types of distribution. 
For example, the main predictor can be binary (e.g. treatment/control groups).
  
#Regression coefficient specification:
    pwrss.z.poisreg(beta0 = 0.50, beta1 = -0.10,
                    alpha = 0.05, power = 0.80,
                    dist = "bernoulli")

#Rate ratio specification:
      
   pwrss.z.poisreg(exp.beta0 = exp(0.50),
                   exp.beta1 = exp(-0.10),
                   alpha = 0.05, power = 0.80,
                   dist = "bernoulli")

#------------------ One-way -------------------------

# ANOVA
#A researcher is expecting a difference of Cohen’s d = 0.50 between treatment and 
# control groups (two levels) translating into η2=0.059(eta2 = 0.059). 
# Means are not adjusted for any covariates. What is the minimum required sample size?
     
     pwrss.f.ancova(eta2 = 0.059, n.levels = 2,
                    power = .80, alpha = 0.05)


#ANCOVA
#A researcher is expecting an adjusted difference of Cohen’s d = 0.45 between treatment and
#control groups (n.levels = 2) after controlling for the pretest (n.cov = 1) translating into partial η2=0.048
#(eta2 = 0.048). What is the minimum required sample size?
     
     pwrss.f.ancova(eta2 = 0.048, n.levels = 2, n.cov = 1,
                    alpha = 0.05, power = .80)

#-------------------Two-way ---------------------------
## ANOVA
#A researcher is expecting a partial η2=0.03 (eta2 = 0.03) for interaction of treatment/control (Factor A: two levels)
#with gender (Factor B: two levels). Thus, n.levels = c(2,2). What is the minimum required sample size?
       
       pwrss.f.ancova(eta2 = 0.03, n.levels = c(2,2),
                      alpha = 0.05, power = 0.80)
#ANCOVA
#A researcher is expecting a partial η2=0.02 (eta2 = 0.02) for interaction of treatment/control (Factor A)
#with gender (Factor B) adjusted for the pretest (n.cov = 1). What is the minimum required sample size?
       
      pwrss.f.ancova(eta2 = 0.02, n.levels = c(2,2), n.cov = 1,
                      alpha = 0.05, power = .80)
      
#------------------ Three-way -------------------------      
#ANOVA
#A researcher is expecting a partial η2=0.02 (eta2 = 0.02) for interaction of treatment/control (Factor A: two levels), 
#gender (Factor B: two levels), and socio-economic status (Factor C: three levels). Thus, n.levels = c(2,2,3). 
#What is the minimum required sample size?
        pwrss.f.ancova(eta2 = 0.02, n.levels = c(2,2,3),
                       alpha = 0.05, power = 0.80)      
      
#ANCOVA
#A researcher is expecting a partial η2=0.01 (eta2 = 0.01) for interaction of treatment/control (Factor A),
#gender (Factor B), and socio-economic status (Factor C: three levels) adjusted for the pretest (n.cov = 1). 
#What is the minimum required sample size?
        pwrss.f.ancova(eta2 = 0.01, n.levels = c(2,2,3), n.cov = 1,
                       alpha = 0.05, power = .80)      
 
        
        
             
#----------------Repeated Measures (F Test) ----------      
#The Group Effect (Between)

#Example 1: Posttest only design with treatment and control groups.
        
#A researcher is expecting a difference of Cohen’s d = 0.50 on the posttest score between treatment 
#and control groups (n.levels = 2), translating into η2=0.059 (eta2 = 0.059). 
#The test is administered at a single time point; thus, the ‘number of repeated measures’ is n.rm = 1. 
#What is the minimum required sample size?
          
          pwrss.f.rmanova(eta2 = 0.059,  n.levels = 2, n.rm = 1,
                          power = 0.80, alpha = 0.05,
                          type = "between")      
      
#The Time Effect (Within)
# Example 2: Pretest-posttest design with treatment group only.
#A researcher is expecting a difference of Cohen’s d = 0.30 between posttest and pretest scores,
# translating into η2=0.022(eta2 = 0.022). 
#The test is administered before and after the treatment; thus, the ‘number of repeated measures’ is n.rm = 2.
#There is treatment group but no control group (n.levels = 1).
#The researcher also expects a correlation of 0.50 (corr.rm = 0.50) between pretest and posttest scores.
#What is the minimum required sample size?
           pwrss.f.rmanova(eta2 = 0.022,  n.levels = 1, n.rm = 2,
                          power = 0.80, alpha = 0.05,
                          corr.rm = 0.50, type = "within")      
      
      
#The Group x Time Interaction
# Example 3: Pretest-posttest control-group design.
          
#A researcher is expecting a difference of Cohen’s d = 0.40 on the posttest scores between treatment and 
# control groups (n.levels = 2) after controlling for pretest, translating into partial η2=0.038 (eta2 = 0.038). 
#The test is administered before and after the treatment; thus, the ‘number of repeated measures’ is n.rm = 2.
#The researcher also expects a correlation of 0.50 (corr.rm = 0.50) between pretest and posttest scores. 
#What is the minimum required sample size?
             pwrss.f.rmanova(eta2 = 0.038,  n.levels = 2, n.rm = 2,
                            power = 0.80, alpha = 0.05, 
                            corr.rm = 0.50, type = "between")      
      
      
#------------------- GOF or Independence (χ2 Test) ----------------
#Alternative hypothesis state that 28% of the workforce in STEM field is women whereas 72% is men (from the article).
#The null hypothesis assume 50% is women and 50% is men.
             
             prob.mat <- c(0.28, 0.72) 
             pwrss.chisq.gofit(p1 = c(0.28, 0.72), 
                               p0 = c(0.50, 0.50),
                               alpha = 0.05, power = 0.80             
      
#Option 2: Use Cohen’s W = 0.44. Degrees of freedom is k - 1 for Cohen’s W.
                               
          pwrss.chisq.gofit(w = 0.44, df = 1,
                            alpha = 0.05, power = 0.80)     
      
      
#------------- Contingency Table: 2 x 2 ---------------------
#Effect size: Phi Coefficient (or Cramer’s V or Cohen’s W) for independence test (2 x 2 contingency table).      
      
#Option 1: Use cell probabilities.5.6% of girls and 13.2% of boys are diagnosed with ADHD (from the article).
          prob.mat <- rbind(c(0.056, 0.132),
                            c(0.944, 0.868))
          colnames(prob.mat) <- c("Girl", "Boy")
          rownames(prob.mat) <- c("ADHD", "No ADHD")
          prob.mat      
    
#Option 2: Use Phi coefficient = 0.1302134.Degrees of freedom is 1 for Phi coefficient.
          pwrss.chisq.gofit(w = 0.1302134, df = 1,
                            alpha = 0.05, power = 0.80)     
      
      
#------------ Correlation(s) (z Test) --------------------
          
#One Correlation
#One-sided Test: Assume that the expected correlation is 0.20 and it is greater than 0.10. 
#What is the minimum required sample size?
            pwrss.z.corr(r = 0.20, r0 = 0.10,
                         power = 0.80, alpha = 0.05, 
                         alternative = "greater") 
#Two-sided Test: Assume that the expected correlation is 0.20 and it is different from 0 (zero). 
#The correlation could be 0.20 as well as -0.20. What is the minimum required sample size?
            pwrss.z.corr(r = 0.20, r0 = 0,
                         power = 0.80, alpha = 0.05, 
                         alternative = "not equal")          
          
          
#--------------- Proportion(s) (z Test) ---------------          
#One Proportion
#In the following examples p is the proportion under alternative hypothesis and 
#p0 is the proportion under null hypothesis.One-sided Test: Expecting p - p0 smaller than 0 (zero).
        
# normal approximation
            pwrss.z.prop(p = 0.45, p0 = 0.50,
                         alpha = 0.05, power = 0.80,
                         alternative = "less",
                         arcsin.trans = FALSE)          
          
          
#------------- Difference between Two Proportions (Independent) ------
            pwrss.z.2props(p1 = 0.45, p2 = 0.50,
                           alpha = 0.05, power = 0.80,
                           alternative = "less",
                           arcsin.trans = FALSE)           
          
          

            
            
            
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Practical Guide to Statistical Power and Sample Size Calculations in R  ==== *2*
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Name of Test
#----Repeated Measures ANOVA
#----Multi_way ANOVA ( 1 categorical of interest)
#----Multi_way ANOVA (>1 categorical of interest) 
#----Non parametric regression(Logistic)
#----Non parametric regression(Poisson)
#----Multilevel modelling (CRT)  
#----Multilevel modelling (MRT) 

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
  
#------------GLMM 
install.packages("simr")
install.packages("lme4")
library(simr)
library(lme4)
