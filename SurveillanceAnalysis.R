#
rm(list=ls())
gc(reset = TRUE)
# Load packages
library(readxl)
library(readr)
library(janitor)
library(tidyverse)
library(psych)
library(car)
library(lessR)
library(Hmisc)
library(stats)
library(epitools)
library(lubridate)
library(gtsummary)
library(surveillance)
 
# # #
--------------------------------------------------------------------------------
#------------------------- Set working Directorate ----------------------------Step.1
--------------------------------------------------------------------------------
#
setwd("C:/Users/User/Downloads")
# # #
--------------------------------------------------------------------------------
#------------------------- import Surveillance data ----------------------------Step.2
--------------------------------------------------------------------------------
#
FLU100 <- read_excel("FLU100.xlsx")
str(FLU100)
# # #
print(df)
print(df, n = Inf, width = Inf)
summary(df)

# # #
--------------------------------------------------------------------------------
#------------------------------ Clean the data --- -----------------------------Step.3
--------------------------------------------------------------------------------
#Remove duplicates
malaria <- malaria %>% distinct()
#Check missing values
colSums(is.na(malaria))
#Remove missing observations
malaria <- malaria %>% drop_na()

# # #
--------------------------------------------------------------------------------
#------------------------------ Data Conversion -------------------------------- 
--------------------------------------------------------------------------------
#df$DATE_ONSET <- as.Date(df$DATE_ONSET, format = "%Y/%m/%d")
#
df$AGE <-as.integer(df$AGE)# For whole Numbers
df$AGE <-as.numeric(df$AGE)# For Decimal Numbers
df$SEX <- as.factor(df$SEX)
df$age_group <- factor(df$age_group)
df$age_group <- as.factor(df$age_group)
df$REGION <- as.factor(df$REGION)
df$District <- as.factor(df$District)  
df$ILI_SARI <- as.factor(df$ILI_SARI) 
df$FEVER <- as.factor(df$FEVER)  
df$COUGH <- as.factor(df$COUGH)  
df$THROAT <- as.factor(df$THROAT)
df$CORYZA <- as.factor(df$CORYZA) 
df$MYALGIA <- as.factor(df$MYALGIA)
df$HEADACHE <- as.factor(df$HEADACHE)
df$BREATH_DIFFICULTY <- as.factor(df$BREATH_DIFFICULTY)
df$FLUMATRIX <- as.factor(df$FLUMATRIX)
# 
str(df)


# # #
--------------------------------------------------------------------------------
#---------------------- Create epidemiological variables -----------------------Step.4
--------------------------------------------------------------------------------
#--------------Convert Dates-------------------
malaria$Date_Onset <- as.Date(malaria$Date_Onset)
#Create epidemiological Year
malaria$Year <- year(malaria$Date_Onset)
#Create epidemiological Month
malaria$Month <- month(malaria$Date_Onset,label = TRUE)
#Create epidemiological weeks (start on Monday)
malaria$Week <- isoweek(malaria$Date_Onset)
#
#------------- Create Agegroup -------------------
df <- FLU100 %>% mutate(age_group = case_when(
  AGE < 5 ~ "0-4",
  AGE < 11 ~ "5-10",
  AGE < 18 ~ "11-17", 
  AGE < 35 ~ "18-34", 
  AGE < 50 ~ "35-49",
  AGE < 65 ~ "50-64",
  TRUE ~ "65+"))
#
str(df)
print(df)
print(df, n = Inf, width = Inf)
summary(df)
describe.by(df)
# # #
--------------------------------------------------------------------------------
#---------------------------- Descriptive analysis -----------------------------Step.5
--------------------------------------------------------------------------------
#Total Cases
nrow(malaria)
#Cases by sex
table(malaria$Sex)
# or
malaria %>% count(Sex)
#Cases by District
malaria %>% count(District)
#Cases by month
malaria %>% count(Month)
# # #
--------------------------------------------------------------------------------
#--------------------------- Calculate incidence rate --------------------------Step.6
--------------------------------------------------------------------------------
#
population <- 150000
cases <- nrow(malaria)
#
incidence <- (cases / population) * 100000
#
incidence
# # #
--------------------------------------------------------------------------------
#---------------------------- Create epidemic curve ----------------------------Step.7
--------------------------------------------------------------------------------
#
  ggplot(malaria,aes(Date_Onset)) +
  geom_histogram(binwidth = 7, fill = "steelblue",color = "black") +
  labs(title = "Weekly Malaria Cases",x = "Date",y = "Cases") +
  theme_minimal()

# # #
--------------------------------------------------------------------------------
#-------------------------------- Monthly trend --------------------------------Step.8
--------------------------------------------------------------------------------
#
  monthly_cases <- malaria %>% count(Month)

ggplot(monthly_cases, aes(Month, n, group = 1)) +
  geom_line() +
  geom_point(size = 3) +
  labs(title = "Monthly Malaria Cases",y = "Cases")
  
# # #
--------------------------------------------------------------------------------
#---------------------------- Bar chart by district ----------------------------Step.9
--------------------------------------------------------------------------------
#  
  ggplot(malaria,aes(District)) +
  geom_bar(fill = "darkgreen") +
  coord_flip()

# # #
--------------------------------------------------------------------------------
#------------------------------- Age distribution ------------------------------Step.10
--------------------------------------------------------------------------------
#
  ggplot(malaria,aes(Age)) +
  geom_histogram(binwidth = 5,fill = "orange")

# # #
--------------------------------------------------------------------------------
#------------------------------- Cross-tabulation ------------------------------Step.11
--------------------------------------------------------------------------------
#
  table(malaria$Sex, malaria$Outcome)  
  
# # #
--------------------------------------------------------------------------------
#-------------------------------- Chi-square test ------------------------------Step.12
--------------------------------------------------------------------------------
# 
  chisq.test(table(malaria$Sex, malaria$Outcome))  
  
# # #
--------------------------------------------------------------------------------
#------------------------------- Logistic regression ---------------------------Step.13
--------------------------------------------------------------------------------
#  
malaria$Death <- ifelse(malaria$Outcome=="Dead",1,0)
#
model <- glm(Death ~ Age + Sex, family=binomial, data=malaria)
summary(model)  
  
# # #
--------------------------------------------------------------------------------
#-------------------------------- Time series ----------------------------------Step.14
--------------------------------------------------------------------------------
#  
weekly_cases <- malaria %>% count(Week)

plot(weekly_cases$Week, weekly_cases$n, type="l")  
  
# # #
--------------------------------------------------------------------------------
#------------------------------ Detect outbreaks -------------------------------Step.15
--------------------------------------------------------------------------------
# 
library(surveillance)
# Example workflow (requires surveillance time-series object)
# sts_object <- sts(...)
# alarms <- farrington Flexible(sts_object)  
  
# # #
--------------------------------------------------------------------------------
#---------------------- Generate a surveillance report -------------------------Step.16
--------------------------------------------------------------------------------
#  
malaria %>% summarise(Total_Cases = n(),Mean_Age = mean(Age),Median_Age = median(Age))  
#Cases by district  
malaria %>% group_by(District) %>% summarise(Cases = n()) 
  



  
  
