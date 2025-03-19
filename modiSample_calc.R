
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Practical Guide to Statistical Power and Sample Size Calculations in R
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

install.packages("pwrss")
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
          
          
          
          
            
            
            
            
            
            
            
            