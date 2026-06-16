#
rm(list=ls())
gc(reset = TRUE)
#...............
ls()
search()
library(installr)
updateR()
#--------------------------- required packages ---------------------------------
library(XLConnect)
library(readxl)
library(readr)
library(writexl)
library(lubridate)
library(skimr)
library(tidyverse)
library(dbplyr)
library(RColorBrewer)
library(gganimate)
library(ggpubr)
library(ggfortify)
library(ggrepel)
library(plotrix)
library(psych)
library(lessR)
library(car)
library(FSA)
library(Hmisc)
library(stats)
library(epitools)
library(PMCMRplus)
#------------------
library(stringr)
library(gtools)
library(zoo)
library(mvtnorm)
library(devtools)
library(rvest)
library(dslabs)
library(weights)
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
library(survival)
library(epiDisplay)
library(htmlwidgets)
library(pdftools)
library(joineR)
library(lattice)
library(latticeExtra)
library(zscorer)
library(COUNT)
library(reshape2)
library(ggExtra)
library(systemfonts)
library(forcats)
objects(grep("forcats", search()))
library(finalfit)
Sys.time()
Sys.setenv(TZ='GMT')
Sys.time()

#------ Directory Functions
                   getwd()#-------------Checks your current active folder path.
                   setwd("path")#-------Changes your working directory.
                   dir.create("name")#--Creates a brand new folder.
                   dir.exists("path")#--Checks if specific folder already exists.
                   
#--------------------------- set directorate -----------------------------------Option.1
setwd("C:/Users/User/Desktop")
mpData <- read_excel("mparasite.xlsx")

covid01 <- read_csv("covid01.csv") 
# # #
dir()
data.read<-loadWorkbook(file.choose(), create=T) # from XLConnect package
data.read
DATA<-readWorksheet(data.read, sheet = "Sheet1")
str(DATA)
##
summary(DATA[,"Age"])

subset(DataTraining,GADX_Oncho=="Negative"&DDTD_Ocho=="Negative", select=c("BARCODES","Age","Sex","District","DDTD_Ocho","GADX_Oncho")) %>%  
  
summarise(count = n())

#-------------------------- Import functions------------------------------------Option.2
#.xlsx format
mparasite <- read_excel("C:/Users/User/Desktop/mparasite.xlsx")

# import comma delimited files
all_data2 <- read_csv(file = 'children_data.csv')
#If you want your variables to be factors
child_data1 <- read.csv("children_data.csv", stringsAsFactors = TRUE)
# import white space delimited files
all_data1 <- read_table(file = 'children_data.txt', col_names = TRUE)
# import tab delimited files
all_data3 <- read_delim(file = 'children_data.txt', delim = "\t")
# or use
all_data4 <- read_tsv(file = 'children_data.txt' )

#--------------------------- EXport function------------------------------------  
library(writexl)
write_xlsx(mparasite, "falciparum.xlsx")


#-------------Direct Console Output to file Using sink()------------------------
setwd("C:/Users/User/Desktop")
# Begin with sink ()                                      
sink("mparasite.xlsx")
sink(file ="mparasite.xlsx", append = F)

print(mparasite)

# Piechart
PieChart(marital_status, data = mparasite, hole = 0.5, main = NULL)
# Donut chart
PieChart(marital_status, data = mparasite, fill = "blues", hole_fill = "#B7E3E0", main = NULL)
# Barchart
BarChart(marital_status, data = mparasite, fill = "blues", main = NULL)
#Histogram
hist(mparasite$Age, col="gray", main="covid data", xlab="age")
hist(mparasite$Age, col= "turquoise", main="Covid data", xlab="Age")
summary(mparasite$Age)
#
#++++++++++++++++++++ To end export outside the console ++++++++++++++++++++++++
sink()
#To show text file in the console
file.show("mparasite.txt")                                       # Other Options
#To show excel file
read.table("mparasite.xlsx")

--------------------------------------------------------------------------------
                     Understand Data Structures                            
--------------------------------------------------------------------------------  
str(mparasite)   #data structure
print(mparasite) #view data in console

-------------------------------------------------------------------------------
                          Data Wrangling                        #The pipe: `%>%`
#The processes of *transforming* or *manipulating* raw data into a useful format 
-------------------------------------------------------------------------------
library(dplyr)
#---------Selecting column 
columns_needed <- covid02 %>% select(c(Age,AgeCategory,SEX,SarsCovStrain,Hospitalstatus,Durationdays,Durationweeks,
                                       categoryofcases,TreatmentOUTCOME,Noofsymptoms,Coinfection,NoofResistance,Numberoforganism))
str(columns_needed)
#----------Remove a column
remove_one <- columns_needed %>% select(-Numberoforganism)
str(remove_one)

#--Remove multiple columns
remove_more <- remove_one %>% select(-c(Coinfection,NoofResistance))
str(remove_more)

#---------filter rows ?filter()
Hanisah <- remove_more %>% select(c(Age,AgeCategory,SEX,SarsCovStrain,Hospitalstatus,Durationdays,Durationweeks,
                                    categoryofcases,TreatmentOUTCOME,Noofsymptoms)) %>% 
  filter(Age < 30)
#
filter(Hanisah, Age <= 30)

#---------Create New column (mutate)
mutate_colmn <- rename_colmns %>% mutate(bmi=weight/height*2)
#
Hanisah1 <- mutate(Hansiah, mortality_rate = total / population * 100000)

#---------show only New column (transmute)
transmutate_col <- mutate_colmn %>% transmute(bmi)

print(Hanisah)
str(Hanisah)

#---------Change character type(rename_with)
Upper_caps <- rename_colmns %>% rename_with(Mother_group,toupper)
Lower_caps <- rename_colmns %>% rename_with(DeliveryMode,tolower)

#---------rename a column
Hanisah1 <- Hanisah %>% rename(mum_Age = motherAge)
names(Hanisah1)
#Data %>% rename(NewColumn=OldColumn)
names(Hanisah)[1]<- "mum_Age"
names(Hanisah)[4]<- "Delivery_mode"
names(Hanisah)[6]<- "Baby_Age"
names(Hanisah)[7]<- "Baby_Gest_Age"
str(Hanisah)

#---------summarise()
Hanisah1 <- Hanisah %>% summarise(sum_MotherAge=sum(Motherage))
print(Hanisah1)
Hanisah2 <- Hanisah %>% summarise(mean_Age=mean(Motherage))
print(Hanisah2)
Hanisah3 <- Hanisah %>% summarise(sum_BabyWt=sum(Baby_age_days),Mean_BabyWt=mean(Baby_age_days))
print(Hanisah3)

#---------group_by()
AverageWeightbyGender <-Hanisah %>% group_by(Baby_sex) %>% summarise(mean = mean(Baby_Weight))
print(AverageWeightbyGender)
#--------Arrange()
Hanisah1 %>% 
  arrange(Durationinweeks) %>% 
  head()
#--------Sorting descending order
Hanisah1 %>% 
  arrange(desc(Durationinweeks)) %>% 
  head()

#-------Descriptive statistics using`dplyr` 
basic_stats<-Hanisah1%>%
  group_by(sex)%>%
  summarise(mean_age=mean(age),mean_bmi=mean(BMI),sd_age=sd(age),sd_bmi=sd(BMI))

basi_stats

#Basic plots 

#----------Scatter plots
Hanisah_plt<-Hanisah1%>%filter(Age>25)
age25<-Hanisah$Age
bmi25<-Hanisah$BMI

plot(age25, bmi25)

#For a quick plot that avoids accessing variables twice, we can use the `with` function
with(Hanisah_plt, plot(Age, bmi25))
# Histograms
hist(Hanisah_plt$Age)

# Boxplot
boxplot(Age~ sex, data = Hanisah_plt)


# # #
--------------------------------------------------------------------------------
               Reshaping Data -(LONG.data / WIDE.data) 
--------------------------------------------------------------------------------
library(tidyr) 
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

# # #
--------------------------------------------------------------------------------
                       HANDLING  DUPLICATE DATA
--------------------------------------------------------------------------------
BF_data = data.frame#Data
#
duplicated(BF_data)#base functions
sum(duplicated(BF_data))  
#
unique(BF_data) #base functions
#
distinct(BF_data)#Dplyr package  

# # #
--------------------------------------------------------------------------------
                         HANDLING MISSING DATA
--------------------------------------------------------------------------------
# Count missing values in each column
missing_per_column <- colSums(is.na(Hanisah))
print(missing_per_column)
# Remove rows with any missing values 
Cleaned_Hanisah <- na.omit(Hanisah)
print(Cleaned_Hanisah)
#
Check_Clean_Hanisah <- colSums(is.na(Cleaned_Hanisah))
print(Check_Clean_Hanisah)

# # #
--------------------------------------------------------------------------------
                         Data Conversion _(CODING)                        Option 1
--------------------------------------------------------------------------------
str(Hanisah)
#Date to time series.........................................
tdata$Date = as.Date(tdata$Date, format = "%Y/%m/%d")
hhdata = ts(tdata$attendance,start = min(tdata$Date), end = max(tdata$Date),frequency = 1)

#-------Continuous data......................................
Hanisah$mum_Age <-as.integer(Hanisah$mum_Age)
Hanisah$Baby_Age <-as.integer(Hanisah$Baby_Age)
Hanisah$Baby_Weight <-as.integer(Hanisah$Baby_Weight)
#
salarylevel <-if else(data$salary < 5000,c("Low"),c("High"))
SES <- as.factor(data$salary)
# or creating new variable with coded values
Incomelevel <-if else(data$salary == "LowIncome",0,1)
SES <- as.factor(data$salary)


#-------Categorical data.....................................
Hanisah$Baby_sex <-factor(Hanisah$Baby_sex,
                          levels = c(0,1),
                          labels = c("Female", "Male"))

Hanisah$Baby_bloodgroup <-factor(Hanisah$Baby_bloodgroup,
                                 levels = c(0,1,2,3,4,5,6,7),
                                 labels = c("Notdone", "A-","A+", "B-","B+","AB+", "AB-","O+"))

Hanisah$Baby_G6PD <-factor(Hanisah$Baby_G6PD,
                           levels = c(0,1,2,3),
                           labels = c("No defect","Full defect","Partial defect","Not done"))


Hanisah$Delivery_mode <-factor(Hanisah$Delivery_mode,
                               levels = c(0,1,2),
                               labels = c("Vaginal", "Caesarean", "Assisted"))

Hanisah$Baby_Gest_Age <-factor(Hanisah$Baby_Gest_Age,
                               levels = c(0,1),
                               labels = c("Pretern", "Fullterm"))

str(Hanisah)
print(Hanisah)

# # #
--------------------------------------------------------------------------------
                          Reverse Coding                                 Option 2
--------------------------------------------------------------------------------
Hanisah77$SarsCovStrain <-factor(Hanisah77$SarsCovStrain,
                                   levels = c("delta", "omicron"),
                                   labels = c(1, 2))

Hanisah77$categoryofcases <-factor(Hanisah77$SarsCovStrain,
                                   levels = c("mild", "moderate", "severe"),
                                   labels = c(1,2,3))

# # #
--------------------------------------------------------------------------------
                        CHECKING FOR DATA NORMALITY           
--------------------------------------------------------------------------------
#POS_skewed = Mean > Median/Mode         NEG_skewed = Mean < Median/Mode        LOW  = Q1-1.5(IQR)        
#                                                                               HIGH = Q3+1.5(IQR)
library(psych)
skew(Hanisah30$mum_Age)
kurtosi(Hanisah30$Baby_Age, na.rm = TRUE)
mardia(Hanisah30$Baby_Age)
describeBy(Hanisah30)

#  #  #
--------------------------------------------------------------------------------
                         Transforming skewed data
--------------------------------------------------------------------------------
#Square root Transformation---------------------------------------moderately skewed (-1 to -0.5//0.5 to 1)
sq_data <-sqrt(Hanisah$Baby_Weight)
hist(sq_data)
#Cube root Transformation-----------------------------------------moderately Right skewed(Negative//Zero)
cb_data <-sign(Hanisah$Baby_Weight)*abs(Hanisah$Baby_Weight)^(1/3)
hist(cb_data)
#Log Transformation---------------------------------------Highly skewed(above1),But not (Negative//Zero)
Hanisah30$lg_mum_age <-log(Hanisah30$mum_Age)
hist(lg_data)

# # #
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                        
+                            Descriptive statistics                            +
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(psych)
describeBy(Hanisah30)
library(skimr)
str(Hanisah30)
skim_without_charts(Hanisah30)

#-----------------Confidence interval of the mean
t.test(Hanisah$Baby_Weight,
       conf.level=0.95)       

#-----------------confidence intervals for groups defined by a variable
install.packages("Rmisc")
library(Rmisc)
summarySE(data=covid01, measurevar="Age", groupvars="AgeCategory", conf.interval = 0.95)

# categorical
coronary %>% dplyr::select(race, gender) %>% tbl_summary() %>% as_gt()


# # #
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                             Data Visualizations
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#------------------------------ Simple plots -----------------------------------  
#Barplot
plot(SarsCoV$SEX)
#pie
pie(table(data$SES))
pie(table(data$SES), col = c("white","gray90","gray60"))
#boxplot
plot(x= SarsCoV$SEX,y = SarsCoV$Age)
#scatterplot
plot(x= SarsCoV$systolic1, y= SarsCoV$sugar_level)
# 
#-------------------------- #charts with LessR --------------------------Option.1
library(lessR)

covid01 <- read_excel("C:/Users/User/Desktop/covid01.xlsx")

# Piechart
PieChart(AgeCategory, data = covid01, hole = 0, main = NULL)
# Donut chart
PieChart(AgeCategory, data = covid01, fill = "blues", hole_fill = "#B7E3E0", main = NULL)
# Barchart
BarChart(AgeCategory, data = covid01, fill = "blues", main = NULL)
#colorviridis
BarChart(AgeCategory, data = covid01, fill = "viridis", main = NULL, color = "black",lwd = 1.5,
         values_color = c(rep("white", 4), 1), values_size = 0.85)
# slant x labels (45 angle)
BarChart(AgeCategory, data = covid01, fill = "viridis", main = NULL, color = "black",lwd = 1.5,
         rotate_x=45, values_color = c(rep("white", 4), 1), values_size = 0.85)

#Histogram
hist(covid01$Age, col="gray", main="covid data", xlab="age")
hist(covid01$Age, col= "turquoise", main="Covid data", xlab="Age")
summary(covid01$Age)

# Boxplot
boxplot(Age ~ SEX, data = covid01)

# # #
#------------------------- Using GGPLOT2 ---------------------------------Option.2
library(ggplot2)

#============================= Barchart  =======================================
ht <-ggplot(data = covid01,
            mapping = aes(x = AgeCategory))+
  geom_bar()+theme_classic()
ht
#covid01$SEX <- factor(covid01$SEX)
#Stacked Barchart
ht2 <-ggplot(data = covid01,
             mapping = aes(x = AgeCategory, colour = SEX, fill = SEX))+
  geom_bar()+
  theme_classic()
ht2

#Clustered Barchart
ht3 <-ggplot(covid01,
             mapping = aes(x = AgeCategory, y = Age,  fill = SEX)) +
  geom_col(position = "dodge") +
  theme_minimal()
ht3

#flip axis
ht4 <-ggplot(covid01,
             mapping = aes(x = AgeCategory, y = Age,  fill = SEX)) +
  geom_col(position = "dodge") +
  coord_flip()+
  theme_minimal()
ht4

#=========================== Histogram =========================================

ht5 <-ggplot(data = covid01,
             mapping = aes(x = Age))+
  geom_histogram()

ht5

#Histogram with stacked
ht6 <-ggplot(data = covid01,
             mapping = aes(x = Age, fill = SEX))+
  geom_histogram(bins = 10, position = "stack")

ht6

#Histogram with dodge
ht7 <-ggplot(data = covid01,
             mapping = aes(x = Age, fill = SEX))+
  geom_histogram(bins = 10, position = "dodge")

ht7
#flip axis
ht8 <-ggplot(data = covid01,
             mapping = aes(x = Age, fill = SEX))+
  geom_histogram(bins = 10, position = "dodge")+
  coord_flip()

ht8

#============================= Boxplot =========================================
imm <- ggplot(data = covid01,
              mapping = aes(x = SEX, y = Age))+
  geom_boxplot()
imm
# Boxplot by category ##
immu <- ggplot(data = covid01,
               mapping = aes(x = SEX, y = Age, fill = SEX))+
  geom_boxplot()
immu

#Boxplot with coord_flip
immp <- ggplot(data = covid01,
               mapping = aes(x = SEX, y = Age, fill = SEX))+
  geom_boxplot()+coord_flip()
immp 

#plotting by factor
imu <- ggplot(data = covid01,
              mapping = aes(x = SEX, y = Age))+
  geom_boxplot(aes(fill = factor(SarsCovStrain)))

imu

#Adding labels
imml <- ggplot(data = covid01,
               mapping = aes(x = SEX, y = Age))+
  geom_boxplot(aes(fill = factor(SarsCovStrain)))+
  coord_flip()+
  labs(title ="Immunoassay Data", subtitle = "ImmunoAnalysis",caption = "Data collection by Mohammed")+
  annotate( "text",x = 2,y = 70, label ="covid-19 patients", color ="purple",
            fontface ="bold", size =4.0,angle = 30)
imml

# covid01$categoryofcases <-factor(covid01$categoryofcases)
# Facet wrap 
imwrp <- ggplot(data = covid01,
                mapping = aes(x = SEX, y = Age, fill = categoryofcases))+
  geom_violin()+
  facet_wrap(~categoryofcases)
imwrp

# Facet grid
imwgg <- ggplot(data = covid01,
                mapping = aes(x = SEX, y = Age, fill = categoryofcases))+
  geom_violin()+
  facet_grid(~categoryofcases)+
  labs(title ="Covid Data", subtitle = "ImmunoAnalysis",caption = "Data collection by Mohammed")+
  annotate( "text",x = 2,y = 70, label ="Covid patients", color ="purple",
            fontface ="bold", size =4.0,angle = 30)
imwgg

#============================ Combo plots ======================================
library(RColorBrewer)
library(ggpubr)
par()


p <-imu <- ggplot(data = covid01,
                  mapping = aes(x = SEX, y = Age))+
  geom_boxplot(aes(fill = factor(SarsCovStrain)))+
  coord_flip()+
  labs(title ="Immunoassay Data", subtitle = "ImmunoAnalysis",caption = "Data collection by Mohammed")+
  annotate( "text",x = 2,y = 70, label ="covid-19 patients", color ="purple",
            fontface ="bold", size =4.0,angle = 30)
p


p1 <-ggplot(data = covid01,
            mapping = aes(x = AgeCategory, colour = SEX, fill = SEX))+
  geom_bar()+
  theme_classic()
p1


p2 <-ggplot(data = covid01,
            mapping = aes(x = Age, fill = SEX))+
  geom_histogram(bins = 10, position = "dodge")+
  coord_flip()
p2

#to arrange plot for publication
ggarrange(p, p1, p2 + rremove("x.text"), labels = c("A", "B", "c"), ncol = 1, nrow = 3,
          common.legend = TRUE, legend = "bottom")

# Save plot 
ggsave("p.png")


# # #
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                          Inferential Statistics                              +
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if(!require(XNomial)){install.packages("XNomial")}
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
library(RVAideMemoire)
library(grid)
#
#======================= ONE/TWO SAMPLE  PROPORTION ============================
#
0-Chi-square for proportions---------------------prop.test()#for one or more proportions
1-Exact Goodness-of-fit-------------------------binom.test()#whether difference to hypothised value
2-Chi-Square test of independence---------------chisq.test()#Test of association independence      
3-Fisher exact test----------------------------fisher.test()#N < 100, cell count<5 // Likelihood ratio chi-square
4-McNemars chisquare test---------------------mcnemar.test()#Paired categorical data
5-Mantel-Haenszel chisquare test-----------mantelhaen.test()#for stratified 2×2 tables (confounders)
6-Posthoc-chisquare of independence-----chisq.posthoc.test()#pairwise comparisons after Chi-square

# # #  
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
----------------------- One Sample test of Proportion --------------------------
#Is the proportion of HIV in female =5.2% ?
  
prop1_table <-table(data$HIV)
prop1_table                         # E.g,where the output is row.25, column.143
prop1_table <- matrix(c(25, 143), ncol = 2)
#Therefore
prop.test(prop1_table, p = 0.052)

# # #
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
------------------------ Two Sample test of Proportion -------------------------
#Get table of sex by HIV status

table_status <- table(Gender = data$sex, HIVstatus = data$HIV) 
table_status                        # E.g,where the output is column1-> 15, 143.column2-> 7, 23.
# Input results
table_status <- matrix(c(15, 143, 7, 23), ncol = 2)
colnames(table_status)<- c("Reactive","Non reactive")
rownames(table_status)<- C("Female","Male")
#
prop.test(table_status)

# # #
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
--------------------------- Test of Association --------------------------------
# Is gender associated with HIVstatus ?
  
#---Chisquare of Independence--------
# 
chsq_table <-table(data$sex,data$HIV)
chsq_table
#To add marginal totals
addmargins(chsq_table,margin = c(1,2))
#To get proportions
prop.table(chsq_table)
#To get percentages
prop.table(chsq_table)*100
#To round off values
round(prop.table(chsq_table)*100, 2)
#plot
barplot(prop.table(chsq_table)*100)
# Perform chis-square
chisq.test(table(data$sex,data$HIV))

# --------------- Fisher's Exact Test ....................... for  cell count <2
fisher.test(table(data$sex,data$HIV))

# # #
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
---------------------------- Oddratio & riskratio ------------------------------                                 
library(epitools)
#------------ Oddratio
oddsratio(data$sex,data$HIV)
#           or
OR <-epitab(data$sex,data$HIV,method = "oddsratio",conf.level = 0.95)
OR
#----------- riskratio
riskratio(data$sex,data$HIV)
#           or
RR <-epitab(data$sex,data$HIV,method = "riskratio",conf.level = 0.95)

RR

# # #

#Package function specifications
#___________________________________________________
method = c("oddsratio", "riskratio", "rateratio")
#
rev = c("neither", "rows", "columns", "both")
#
oddsratio = c("wald", "fisher", "midp", "small")
#
riskratio = c("wald", "boot", "small")
#
rateratio = c("wald", "midp")
#
pvalue = c("fisher.exact", "midp.exact", "chi2")
#
correction = FALSE, verbose = FALSE)
#___________________________________________________

# # #
mcnemar.test(Matriz, correct=FALSE) 
mcnemar.test(Data.xtabs, correct=FALSE)

# # #
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
----------------------- Exact Test of Goodness-of-Fit -------------------------- 
#To determine if the proportion of successes in a binary experiment differs from hypothesized probability.
binom.test()
#Where:
      2 is the number of successes
      10 is the number of trials
      0.5 is the hypothesized probability of success
#
dbinom(2, 10, 0.5)   
#One.sided
binom.test(2, 10, 0.5, alternative="less", conf.level=0.95) 
#two.sided
binom.test(2, 10, 0.5, alternative="two.sided", conf.level=0.95)  # In most circumstances, the two-sided test is used.

#Probability Density plot-(binomial distribution)    
trials = 10                          #You can change the values for trials & prob // for xlab & ylab.
prob = 0.5                                                         

x = seq(0, trials)                                                     # x is a sequence, 1 to trials
y = dbinom(x, size=trials, p=prob)                                       # y is the vector of heights
#
barplot (height=y, names.arg=x,
         xlab="Number of uses of right paw",
         ylab="Probability under null hypothesis")

# # #     
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
------------------ Chi-square Test of Goodness-of-Fit --------------------------
#
observed = c(770, 230)        # observed frequencies
expected = c(0.75, 0.25)      # expected proportions

chisq.test(x = observed, p = expected)


# # #
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
--------------- Post-hoc with multinomial and binomial test --------------------
#post hoc tests
Follow up with post hoc tests (optional for Chi-Square)if there are more than two groups in
either of the variables with p<0.05, a post hoc test is conducted.using bonferroni correction.
#---------------
library(XNomial)
#
observed = c(72, 38, 20, 18)
expected = c(9, 3, 3, 1)
#
xmulti(observed, expected, detail = 2)         

# Reports three types of p-value
P value  (LLR)  =  0.003404  # log-likelihood ratio
P value (Prob)  =  0.002255  # exact probability
P value (Chisq) =  0.001608  # Chi-square probability

# # #
#Note last p-value below agrees with Handbook--------------------------
successes   = 72
total       = 148
numerator   = 9
denominator = 16

binom.test(successes, total, numerator/denominator, alternative="two.sided", conf.level=0.95) 

#Binomial test examples
#-----------------------------------------------------------------------
Parasitoid examples, exact binomial test
------------------------------------------------------------------------
binom.test(10, (17+10), 0.5, alternative="two.sided", conf.level=0.95)
#-----------------------------------------------------------------------
Drosophila example, exact binomial test
------------------------------------------------------------------------
binom.test(140, (106+140), 0.5, alternative="two.sided", conf.level=0.95)

# # #
#------------------------- multinomial exact test -----------------------------
observed = c(315, 108, 101, 32)
expected = c(9, 3, 3, 1)

library(XNomial)
xmulti(observed, expected, detail = 2)              
# reports three types of p-value      
P value  (LLR)  =  0.9261    # log-likelihood ratio
P value (Prob)  =  0.9382    # exact probability
P value (Chisq) =  0.9272    # Chi-square probability


# # #
+++++++++++++++++++++++++ One sample test of Mean ++++++++++++++++++++++++++++++
#-------------------------------------------------------------------------------
observed    = covid01$Age
theoretical = 46

t.test(observed, mu = theoretical, conf.int=0.95)                      #Option.1

t.test(covid01$Age, mu = 46, conf.int=0.95)                            #Option.2
t.test(covid01$Age, mu = 46, conf.int=0.95, alternative="greater")


hist(covid01$Age, col="green", main="Histogram of values", xlab="Age")

#  #  #

+++++++++++++++++++ Two Samples test of Mean Comparison ++++++++++++++++++++++++
  #-------------------------------------------------------------------------------
Two-sample t-test, independent (unpaired) observations
bartlett.test(Value ~ Group, data=Data) #If p-value >= 0.05, use var.equal=TRUE below.

#paired t.test  
t.test(Age ~ SEX, data=covid01, var.equal=TRUE, conf.level=0.95)
t.test(Age ~ SEX, data=covid01, var.equal=TRUE, conf.level=0.95, alternative="less")

#independent t.test
t.test(Age ~ SEX, data=covid01, var.equal=FALSE, conf.level=0.95)
t.test(Age ~ SEX, data=covid01, var.equal=FALSE, conf.level=0.95, alternative="less")

# Boxplot
boxplot(Age ~ SEX, data = covid01, names=c("Female","Male"), ylab="Age")
#

#========================= Non parametric ======================================
#------- Mann–Whitney Test
wilcox.test(Age ~ SEX, data=covid01, exact = FALSE)
#Box plots
boxplot(Age ~ SEX, data = covid01, names=c("Female","Male"), ylab="Age")

# # #

#------ Wilcoxon Signed-rank Test
wilcox.test(Data$August, Data$November, paired=TRUE)
#Simple 1-to-1 plot of values
plot(Data$August, Data$November, pch = 16, xlab="August", ylab="November")
abline(0,1, col="blue", lwd=2)  

# # #
+++++++++++++++++++++++++++++ANOVA Between Groups ++++++++++++++++++++++++++++++
#-------------------------- One_Way ANOVA (LessR) ------------------------------

#visualise statistical assumptions
library(lessR)
library(readxl)

print(covid01)
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
#
BarChart(age_means)
BarChart(age_means, values="off", bxlab = "Malaria_exposed", ylab = "Women Age")

# # # 
#==============================  Factorial Anova ==============================# 
library(psych)
library(ggplot2)
library(ggpubr)
library(readxl)

describe(amdata)

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
one <- aov(estimated_parasitemia ~ age, data = imdata)
summary(one)
#Two_way Anova
two <- aov(bmi ~ expose + grvdty, data = imdata)
summary(two)
#three_way Anova
three <- aov(bmi ~ expose + grvdty + sex, data = imdata)
summary(three)
#interaction
interaction <- aov(bmi ~ expose + grvdty + sex + expose*grvdty, data = imdata)
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

#post hoc analysis-------------------------
tukey.interaction <- TukeyHSD(interaction)
tukey.interaction

tukey.plot.aov <- aov(bmi ~ expose:CaseControl, data = imdata)
tukey.plot.test <-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test,las = 2)

# # #
----------------------------------------------------------
Tukey and LSD mean separation tests (pairwise comparisons)
TukeyHSD, HSD.test, and LSD.test are not appropriate for cases where there are unequal variances
though TukeyHSD does make an adjustment for mildly unequal sample sizes.
----------------------------------------------------------
  
# # #
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
#----------------Kruskal–Wallis Test-------------------------------------------
if(!require(FSA)){install.packages("FSA")
 
#-------------------------------------- kruskal.test(Value ~ Group, data = Data)
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
  
  
  #  #  #

++++++++++++++++++++++++++++ANOVA Within Groups +++++++++++++++++++++++++++++++
#------------------------- Repeated Measures ANOVA --------------------------#
    # Create a data frame
    wgdata <- data.frame(subject, time, response)
  
  # Perform Repeated-Measures ANOVA
  aov_model <- aov(response ~ time + Error(subject/time), data = wgdata)
  
  # View the summary of the ANOVA
  summary(aov_model) 
  
  ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #------------------------------ Friedman test -------------------------------#
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
  
  #----------------------- Correlation and Linear Regression ---------------------
  Correlation
  Correlation can be performed with the cor.test function in the native stats package.  
  It can perform Pearson, Kendall, and Spearman correlation procedures.  
  
  #Pearson correlation (Parametric)
  Pearson correlation is a parametric test, and assumes that the data are linearly related 
  and that the residuals are normally distributed.
  --------
    cor.test( ~ age + bmi, data=imdata, method = "pearson", conf.level = 0.95)
  
  #Kendall correlation (Non-parametric)
  Kendall rank correlation is a non-parametric test that does not assume a distribution of the data.
  It ranks the data to determine the degree of correlation.
  --------
    cor.test( ~ age + bmi, data=imdata, method = "kendall", continuity = FALSE, conf.level = 0.95)
  ======================
  library(ggpubr)
  #CorrelationPlot----------ggscatter() 
  Hanisah90 <- read_excel("C:/Users/User/Desktop/covid02.xlsx")
  str(Hanisah90)
  
  ggscatter(Hanisah90, x = "Noofsymptoms", y = "Durationdays",
            add = "reg.line",                                 # Add regression line
            conf.int = TRUE,                                  # Add confidence interval
            add.params = list(color = "blue",
                              fill = "lightgray"))+
    stat_cor(method = "pearson", label.x = 3, label.y = 30)   # Add correlation coefficient   
  
  ==================================================================================
    install.packages("GGally")
  
  library(psych)
  library(ggplot2)
  library(GGally)   
  library(readxl)
  EGF01 <- read_excel("C:/Users/User/Desktop/EGF01.xlsx")
  print(EGF01)
  GGally::ggcorr(ARMdata1)
  GGally::ggpairs(ARMdata1)
  psych::pairs.panels(ARMdata1)
  ================================================================================== 
    
    #Spearman correlation (Non-parametric / ordinals)
    Spearman rank correlation is a non-parametric test that does not assume a distribution of the data.
  It ranks the data to determine the degree of correlation, and is appropriate for ordinal measurements.
  --------
    cor.test( ~ age + bmi, data=imdata, method = "spearman", continuity = FALSE, conf.level = 0.95)
  
# # #
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                            Regression analysis                                                 +
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Regression analysis study the relationship between variables:
    
  1-By identifying (Linear, Curvilinear, or Quadratic).
  2-By estimating parameters of the relationships(intercept(B0) and slope(B1)).
  3-By validating the relationship (The  assumptions for regression).
  
  #The process of finding the line equation (Best fit the data values)------------regression line(linear)
  #Linear equation-------------------------------------------------------------- y= B0 + B1x
  #B0(y intercept)--------what is y when x = 0 ?
  #B1(slope line)---------how much is y changes, for each unit increase in x, 
  Note,
  1,The equation gives pedicted value y-hat(y) at a given x.
  2,Each value will not fall exactly on the line of best fit: the difference---residual/error. y(observed)-y-hat(predicted)
  3,Least ordinary square(OLS)--it tries to minimise the difference between the observed and the fitted values by 
    determining the slope and the intercept.
  
  # B0 and B1 are the Least Square Estimates (LSE)
  
  # B0 = y - B1x  ---------------- where x(mean),y(mean). -------------------     B1 = Sy
  ----
  # R squared = 0 - 1,---------R squared = SSE,        Rsquared = 1 - SSR.             Sx
    ----                       ----
  #                                        SSR                        SST                 
    
  R squared (coefficient of determination)-------------useful for model building___(fit model for multiple regression).  
  thus it tells us the amount of variables explain in the reponse(y) after fitting the model.
  
  #R squared = explain variation of model-y
  ----------------------------
  #              Total variation of model                                         df =n-2
    
  The **broom** package has a function called `augment()` which can calculate:
    
  1. estimated log odds 
  2. probabilities
  2. residuals
  3. hat values
  4. Cooks distance
  5. standardized residuals
  
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++      
#---------------------- Checking assumptions of the model ----------------------
  Assumptions
  #-y is a random variable and normally distributed with mean(u) and variance(Q)squared.
  #The unknown standard error (residual) are independent normally distributed. mean = 0, variance squared.
  
  hist(residuals(model), col="darkgray")
  
  plot(fitted(model), residuals(model))
  ----------
  A plot of residuals vs. predicted values.  #The residuals should be unbiased & homoscedastic.
  
  #Note,
  R squared = good for simple regression. 
  Adjusted R squared =for multiple regression (model building)
  #
  model = lm(Species ~ Latitude, data = Data)
  #shows parameter estimates, p-value for model, r-square
  summary(model)                    
  
  library(car)
  #shows p-value for effects in model
  Anova(model, type="II")              

  #Plot linear regression
  int = model$coefficient["(Intercept)"]
  slope = model$coefficient["Latitude"]
  plot(Species ~ Latitude, data = Data, pch=16, xlab = "Latitude", ylab = "Species")
  abline(int, slope, lty=1, lwd=2, col="blue")     #  style and color of line 
  
# # #   
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
------------------------- Curvilinear Regression -------------------------------
#
How to fit models to curvilinear data using three methods:
    
1) Polynomial regression;  
2) B-spline regression with polynomial splines;  
3) Nonlinear regression with the nls function.  

Each of these three will find essentially the same best-fit curve with very similar p-values and R-squared values. 

# create sample data 
sample_data <- data.frame(x=1:10, 
                          y=c(25, 22, 13, 10, 5, 9, 12, 16, 34, 44)) 
View(sample_data)
print(sample_data)

#------------ fit linear -----------------
# create a basic scatterplot 
plot(sample_data$x, sample_data$y)
# define x-axis values 
x_axis <- seq(1, 10, length=10)
#fit model
linear_model1 <- lm(y~x, data=sample_data)

lines(x_axis, predict(linear_model1, data.frame(x=x_axis)), col='green')

summary(linear_model1)



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

# # #

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

# # #
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


# # #
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
#------when x is continuous===========================================
logistic <- glm(CaseControl ~ age, data = imdata, family = "binomial" )
summary(logistic)

#log-odds=-1.92264+0.05512*age
exp(0.05512)

# oddratio only
exp(coef(logistic))

#interpret
-----a unit increase in age,the odds of having case is increase by factor 1.06. 
holding other factors constant(e.g multiple logistic regression).

#------when x is categorical============================================
logist1 <- glm(CaseControl ~ parity, data = imdata, family = "binomial" )
logist1
summary(logist1)
#log-odds=-0.7673+0.6719*(parity=1)+2.1535*(parity=2)+1.8659*(parity=1)-15.7988*(parity=4)
parity0 is used as reference

#odd ratio only
exp(coef(logist1))
parity-1 is 1.96 more likely to have case compared to parity-0
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


# # #
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                             Poisson  Regression                                #Log-linear model
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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

#  #  #

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                            Multinomial Logistic Regression                                       +
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Introduction
Variables with more than two levels are known as either:
1.  multinomial data
2.  polychotomous data
3.  polytomous data

If we employ logistic regression to such data, then the analysis is known as polytomous or
multinomial logistic regression. Again, the polychotomous outcome does not have any natural order. 
When the categories of the outcome variable do have a natural order, ordinal logistic regression is preferred. 

A numerical outcome can be categorized based on different cut-off points. 
The newly created categorical variable is now either treated as a nominal, polychotomous, multinomial or as ordinal outcome. 
That justifies the use of multinomial logistic regression. 

## Models for multinomial outcome data
In multinomial logistic regression, one of the categories of the outcome variable is designated 
as the reference category and each of the other levels is compared with this reference.
The choice of reference category can be arbitrary and is at the discretion of the researcher. 
Most software set the first category as the reference (also known as the baseline category) by default.

# Estimation for Multinomial logit model
In multinomial logistic regression,we would like to model the relationship between covariates with the outcome variable 
that has more than two categories but without ordering or ranking.

Multinomial logistic regression can be thought as of simultenously fitting binary logits 
for all comparisons among the alternatives.

### Log Odds and Odds Ratios
In the special case where the covariate is binary, coded 0 or 1, 
we simplify the notation to $OR_j = OR_j(1,0)$.
Let use an example where data have 3 categories of outcome; 0,1 and 2. 
  
### Load libraries
library(here)
library(tidyverse)
library(haven)
library(gtsummary)
library(VGAM)
library(kableExtra)
### Read data
ms <- read_dta('metabolic_syndrome.dta')

glimpse(ms)

### Data Wrangling
We will
- select only variable `fbs`, `totchol`, `hpt` and `weight`
- convert all character variables to factor variables 
- then take a glance of the updated `ms` dataset again

# That is
ms <- ms %>% 
  select(fbs, totchol, hpt , weight) %>% 
  mutate(across(where(is.character), as_factor))
glimpse(ms)

### Create new categorical variable from fbs
Let us create a categorical (also known as a factor variable) based on this classification: 
  
1. Normal if fbs is less than 6.1 mmol/L
2. Impaired Fasting Glucose (IFG)  if fbs is between 6.1 mmol/L to 6.9 mmol/L
3. Diabetis Mellitus (DM) if fbs is 7.00 mmol/L or higher 
#
ms <- ms %>%
  mutate(cat_fbs = cut(fbs, 
                       breaks = c(2.50 , 6.10 , 6.90 , 28.01 ),
#                       labels = c("Normal","IFG", "DM")))
ms %>% 
  count(cat_fbs)
#
We notice that there were 251 data has no values recorded as `NA`. Thus, 
we decide to remove observations when there are missing values for variable `cat_fbs`.
#
ms <- ms %>%
  filter(!is.na(cat_fbs)) 
ms %>%
  count(cat_fbs)

### Exploratory data analysis
Next, is to return the table of summary statistics 

ms %>%
  tbl_summary(by = cat_fbs,
              statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)"),
              type = list(where(is.logical) ~ "categorical"),
              label = list(fbs ~ "Fasting Blood Sugar (mmol/L)", 
                           totchol ~ "Total Cholesterol (mmol/L)", hpt~"Hypertension", weight ~ "Weight (kg)"),
              missing_text = "Missing") %>% 
  modify_caption("**Table 1. Survey Participant Characteristic**")  %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~ "**Glycemic Control Status**") %>%
  modify_footnote(all_stat_cols() ~ "Mean (SD) or Frequency (%)") %>%
  bold_labels() %>%
  as_gt()

### Confirm the order of cat_fbs
levels(ms$cat_fbs)
#
However, we would like the DM as the smallest category. 
To do that we will use the `fct_relevel()` function. 
#
ms <- ms %>% 
  mutate(cat_fbs = fct_relevel(cat_fbs, 
                               c("DM", 'IFG', 'Normal')))
levels(ms$cat_fbs)
## Estimation
Our intention to investigate the relationship between totchol, hpt and weight 
with the outcome variables `cat_fbs`. Thus, 
we will perform multinomial logistic regression model to estimate the relation for 2 models:
#  
- Model 1: DM vs Normal
- Model 2: IFG vs Normal

In both models, the reference group is Normal

# # # Single Independent Variable 
For independent variable totchol 

log_chol <- vglm(cat_fbs ~ totchol, multinomial, data = ms)
summary(log_chol)

#This is the model where hpt is the independent variable
log_hpt <- vglm(cat_fbs ~ hpt, multinomial, data = ms)
summary(log_hpt)

#And lastly, the independent variable is weight
log_wt <- vglm(cat_fbs ~ weight, multinomial, data = ms)
summary(log_wt)

### Multiple Independent Variables
We feel that totchol, hpt and weight are all important independent variables. 
Hence, we want to fit a model with the three independent variables as the covariates. 
#---------
mlog <- vglm(cat_fbs ~ totchol + hpt + weight, multinomial, data = ms)
summary(mlog)
### Model with interaction term between independent variables 
Then, we hypothesize that there could be a significant interaction between totchol and weight. 
And to test the hypothesis, 
we extend the multivariable logistic regression model by adding an interaction term. 
This interaction term is a product between variable weight and totchol.
#
mlogi <- vglm(cat_fbs ~ totchol + hpt + weight + totchol*weight, multinomial, data = ms)
summary(mlogi)
#
The interaction term in our model showed p-values above 0.05 (p-values of 0.80 and  0.07, respectively). 
As the p-value is bigger than the level of significance at $5\%$ and 
the value of regression parameters for the interaction terms are likely not clinically meaningful, 
we have decided not to use the model with an interaction term. 
## Inferences
For the inference, we will
- calculate the $95\%$ CI (interval estimates)
- calculate the p-values (hypothesis testing)

There is no facility inside the`broom::tidy()` function to generate confidence intervals for object with class `vglm`.
Because of that we will use the `coef()`, `confint()` and `cind()` unctions to produce a rather nice table of inferences. 
#We are going to follow these steps:
- set the number of digits equal to 2 to limit the decimal numbers
- return the regression coefficents for all $\hat\beta$ as an object named `b_fitmlog2`
- return the the confidence intervals for all $\hat\beta$ as an object named `ci_fitmlog2` 
- combine the $\hat\beta$ and the corresponding $95\%$ CIs
#
b_mlog <- coef(mlog)
ci_mlog <- confint(mlog) 
b_ci_mlog <- data.frame(b_mlog,ci_mlog) %>%
  rename("log odds" = b_mlog, "Lower CI" = X2.5.., "Upper CI" = X97.5..)
b_ci_mlog %>% 
  kbl(digits = 2, booktabs = T, caption = "Log odds from multinomial logistic regression") %>%
  kable_styling(position = "center")
#
Afterwards, we will *exponentiate* the coefficients to obtain the **relative-risk ratio**.
We then combine the results to the previous table. 
Finally, we will name the columns of the object `tab_fitmlog2`. 
#
rrr_mlog <- exp(b_ci_mlog)
tab_mlog <- cbind(b_ci_mlog, rrr_mlog)
colnames(tab_mlog) <- c('b', 'lower b', 'upper b',
                        'RRR', 'lower RRR', 'upper RRR')
tab_mlog %>%
  kbl(digits = 2, booktabs = T, caption = "Log odds and RRR from multinomial logistic regression") %>%
  kable_styling(position = "center")

## Interpretation
The result from our multivariable logistic regression models can be interpreted as below: 
  
-Every increment 1 mmol/L of totchol (Total cholesterol) when controlling for hypertension status and weight; a) 
the odds of being in DM group (in comparison to Normal) change by 0.277 (Adjusted RRR = 1.32, $95\%$ CI : 1.232, 1.413, p-value \<0.001) and b) 
the odds of being in IFG group (in comparison to being in Normal) change by 0.239 (Adjusted RRR = 1.27, $95\%$ CI : 1.169,1.381, p-value \<0.001).
    
-Every increment 1 kg of weight when controlling for hypertension status and total cholesterol; a) 
the odds of being in DM group (in comparison to being in Normal) increase by 0.023 (Adjusted RRR = 1.02, $95\%$ CI : 1.017,1.029, p-value \<0.001), and b) 
the odds of being in IFG group (in comparison being in Normal) increase by 0.022 (Adjusted RRR = 1.02, $95\%$ CI : 1.015,1.030, p-value \<0.001). 

-In the population with hypertension (as compared with participant without hypertension) when controlling for weight and total cholesterol; a) 
their odds of being in DM group (in comparison to being in Normal) change by 0.900 (Adjusted RRR = 2.5, $95\%$ CI : 1.942, 3.114, p-value \<0.001).

When repeated research on this population, the odds ranging between reduced the odds by 0.481 to 0.677, and b) 
and their odds of being in IFG group (in comparison to being Normal) change by 0.867 (Adjusted RRR = 2.38, 95% CI : 1.789, 3.166, p-value \<0.001).  

## Prediction
We can calculate the predicted probability of each category of outcome by using the `predict()` function. 
Below are the result for the top 10 observations. 
#
predict.vgam(mlog, type = 'response') %>% head(10)

# Presentation of multinomial regression model  
We can make a *better* table using `knitr` and `kableExtra` packages, or 
we can save the results as a `.csv` file so we can edit it using spreadsheets.  

tab_mlog %>%
write_csv("results_multinomial.csv")




