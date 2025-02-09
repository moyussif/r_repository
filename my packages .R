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
#--------------------------------------------------
# import .csv file
child_data1 <- read.csv(file = 'children_data.csv') 
# import .csv file with dec = "," and sep = ";"
child_data2 <- read.csv2(file = 'children_data.csv')
#correct by introducing sep = ","
child_data22 <- read.csv2(file = 'children_data.csv', sep = ",")
# import tab delim file with sep = "\t"
child_data3 <- read.delim(file = 'children_data.txt')
#---------------------------------------------------------------------- #
#---------------------R Companion---------------------------------------TEST OF NOMINAL VARIABLE------------------------------------#
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
A multinomial test can be conducted with the xmulti function in the package XNomial.  This can be followed with the individual binomial tests for each proportion, as post-hoc tests.

 

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
