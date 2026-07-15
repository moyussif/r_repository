#
rm(list=ls())
gc(reset = TRUE)
# Load packages
library(readxl)
library(readr)
library(janitor)
library(tidyverse)
library(patchwork)
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
FLU10 <- read_excel("FLU100.xlsx")
str(FLU10)
# # #
print(FLU10, width = Inf)
summary(FLU10)

# # #
--------------------------------------------------------------------------------
#------------------------------ Clean the data --- -----------------------------Step.3
--------------------------------------------------------------------------------
#Remove duplicates
  FLU10 <- FLU10 %>% distinct()
#..............................
#Check missing values
colSums(is.na(FLU10))
#Remove missing observations
FLU_clean <- FLU10 %>% drop_na()
#verify clean data
colSums(is.na(FLU_clean))
# # #
str(FLU_clean)
--------------------------------------------------------------------------------
#------------------------------ Data Conversion -------------------------------- 
--------------------------------------------------------------------------------
#df$DATE_ONSET <- as.Date(df$DATE_ONSET, format = "%Y/%m/%d")
#
FLU10$AGE <- as.integer(FLU10$AGE)# For whole Numbers
FLU10$AGE <-as.numeric(FLU10$AGE)# For Decimal Numbers
#
FLU10$SEX <- as.factor(FLU10$SEX)
FLU10$REGION <- as.factor(FLU10$REGION)
FLU10$District <- as.factor(FLU10$District)  
FLU10$ILI_SARI <- as.factor(FLU10$ILI_SARI) 
FLU10$FEVER <- as.factor(FLU10$FEVER)  
FLU10$COUGH <- as.factor(FLU10$COUGH)  
FLU10$THROAT <- as.factor(FLU10$THROAT)
FLU10$CORYZA <- as.factor(FLU10$CORYZA) 
FLU10$MYALGIA <- as.factor(FLU10$MYALGIA)
FLU10$HEADACHE <- as.factor(FLU10$HEADACHE)
FLU10$BREATH_DIFFICULTY <- as.factor(FLU10$BREATH_DIFFICULTY)
FLU10$FLUMATRIX <- as.factor(FLU10$FLUMATRIX)
# 
str(FLU10)

# # #
--------------------------------------------------------------------------------
#---------------------- Create epidemiological variables -----------------------Step.4
--------------------------------------------------------------------------------
#--------------Convert Dates-------------------
FLU10$DATE_ONSET <- as.Date(FLU10$DATE_ONSET)
#Create epidemiological Year
#FLU_clean$Year <- as.integer(format(FLU_clean$DATE_ONSET, "%Y"))
FLU10$Year <- format(FLU10$DATE_ONSET, "%Y")
#Create epidemiological Month
FLU10$Month <- month(FLU10$DATE_ONSET,label = TRUE)
#Create epidemiological weeks (start on Monday)
FLU10$Week <- isoweek(FLU10$DATE_ONSET)
#
#------------- Create Agegroup -------------------
df <- FLU10 %>% mutate(Age_group = case_when(
  AGE < 5 ~ "0-4",
  AGE < 11 ~ "5-10",
  AGE < 18 ~ "11-17", 
  AGE < 35 ~ "18-34", 
  AGE < 50 ~ "35-49",
  AGE < 65 ~ "50-64",
  TRUE ~ "65+"))
#
str(df)
print(df, width = Inf)
print(df, n = Inf, width = Inf)
summary(df)
describe.by(df)
# # #
--------------------------------------------------------------------------------
#---------------------------- Descriptive analysis -----------------------------Step.5
--------------------------------------------------------------------------------
--------- Total Cases by COUNT - PREVALENCE - PROPORTION - INCIDENCE -----------
#
#describe_FLU <- df %>% group_by(REGION, SEX, Age_group) %>%
#  summarise(total = n(),
#            FLU_cases = sum(FLUMATRIX != "NEG", na.rm = TRUE),
#            negative = sum(FLUMATRIX == "NEG", na.rm = TRUE),
#            percent_positive = 100 * FLU_cases / total,
#            Proportion_positive = FLU_cases / total) %>% 
#  arrange(desc(percent_positive))
#-------------------------------------------------------------------------------  
#Total Cases
FLU_cases <- sum(df$FLUMATRIX != "NEG", na.rm = TRUE)
FLU_cases
#Cases by sex
df %>%
  group_by(SEX) %>%
  summarise(
    total = n(),
    FLU_cases = sum(FLUMATRIX != "NEG", na.rm = TRUE),
    negative = sum(FLUMATRIX == "NEG", na.rm = TRUE),
    percent_positive = 100 * FLU_cases / total
  )

#..................Cases by District...............................
cases %>% count(REGION, sort = TRUE)
#Sort by Count
df %>%
  group_by(REGION) %>%
  summarise(
    total = n(),
    FLU_cases = sum(FLUMATRIX != "NEG", na.rm = TRUE),
    negative = sum(FLUMATRIX == "NEG", na.rm = TRUE),
    percent_positive = 100 * FLU_cases / total) %>%
  arrange(desc(FLU_cases))

# Sort by Prevalence
df %>%
  group_by(REGION) %>%
  summarise(
    total = n(),
    FLU_cases = sum(FLUMATRIX != "NEG", na.rm = TRUE),
    negative = sum(FLUMATRIX == "NEG", na.rm = TRUE),
    percent_positive = 100 * FLU_cases / total
  ) %>%
  arrange(desc(percent_positive))

#Sort by Proportion
df %>%
  group_by(REGION) %>%
  summarise(
    total = n(),
    FLU_cases = sum(FLUMATRIX != "NEG", na.rm = TRUE),
    negative = sum(FLUMATRIX == "NEG", na.rm = TRUE),
    Proportion_positive = FLU_cases / total
  ) %>%
  arrange(desc(Proportion_positive))
#
#Cases by month
df %>% count(Month)
# # #
--------------------------------------------------------------------------------
#--------------------------- Calculate incidence rate --------------------------Step.6
--------------------------------------------------------------------------------
str(df)
#For incidence 
population <- 810
FLU_cases <- sum(df$FLUMATRIX != "NEG", na.rm = TRUE)
#Incidence
incidence <- FLU_cases / population
#Incidence percentage
incidence_percent <- (FLU_cases / population) * 100
incidence_percent
#For incidence per 1,000 people:
incidence_per_1000 <- (FLU_cases / population) * 1000
incidence_per_1000

#For incidence by Region
df %>%
  group_by(REGION) %>%
  summarise(
    population = first(population),
    FLU_cases = sum(FLUMATRIX != "NEG", na.rm = TRUE),
    incidence_per_1000 = (FLU_cases / population) * 1000
  ) %>%
  arrange(desc(incidence_per_1000))

# # #
--------------------------------------------------------------------------------
#---------------------------- Create epidemic curve ----------------------------Step.7
--------------------------------------------------------------------------------
# Epidemic curve (weekly)
p1 <- df %>%
  filter(FLUMATRIX != "NEG", !is.na(DATE_ONSET)) %>%
  ggplot(aes(x = DATE_ONSET)) +
  geom_histogram(
    binwidth = 7,
    fill = "steelblue",
    color = "black"
  ) +
  labs(
    title = "Epidemic Curve of Influenza Cases",
    x = "Date of Symptom Onset",
    y = "Number of Cases"
  ) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d-%b") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Monthly trend
monthly_cases <- df %>%
  filter(FLUMATRIX != "NEG") %>%
  count(Month)

p2 <- ggplot(monthly_cases, aes(Month, n, group = 1)) +
  geom_line(color = "darkred", size = 1) +
  geom_point(size = 3, color = "darkred") +
  labs(
    title = "Monthly Influenza Cases",
    x = "Month",
    y = "Number of Cases"
  ) +
  theme_minimal()

# Combine plots
p1 / p2


# # #
--------------------------------------------------------------------------------
#---------------------------- Bar chart by district ----------------------------Step.9
--------------------------------------------------------------------------------
#  
df %>%
  count(REGION, District, name = "FLU_cases") %>%
  left_join(
    df %>%
      group_by(REGION, District) %>%
      summarise(population = first(population), .groups = "drop"),
    by = c("REGION", "District")
  ) %>%
  mutate(prevalence = 100 * FLU_cases / population) %>%
  ggplot(aes(x = reorder(District, FLU_cases), y = FLU_cases)) +
  geom_col(fill = "steelblue") +
  geom_text(
    aes(label = paste0(FLU_cases, " (", round(prevalence, 1), "%)")),
    hjust = -0.1,
    size = 3
  ) +
  coord_flip() +
  facet_wrap(~REGION, scales = "free_y") +
  labs(
    x = "District",
    y = "Number of cases",
    title = "Flu Cases and Prevalence by District Within Region"
  ) +
  theme_gray() +
  expand_limits(y = max(df$FLU_cases, na.rm = TRUE) * 1.2)
# # #
--------------------------------------------------------------------------------
#------------------------------- Age distribution ------------------------------Step.10
--------------------------------------------------------------------------------
# Histogram of age distribution
p1 <- ggplot(df, aes(AGE)) +
  geom_histogram(binwidth = 5, fill = "orange") +
  labs(
    title = "Age Distribution of Cases",
    x = "Age (years)",
    y = "Number of Cases"
  ) +
  theme_minimal()

# Age group bar plot
p2 <- df %>%
  count(Age_group) %>%
  ggplot(aes(x = Age_group, y = n)) +
  geom_col(fill = "gray") +
  labs(
    title = "Cases by Age Group",
    x = "Age Group (years)",
    y = "Number of Cases"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Combine plots
p1 + p2

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
  



  
  
