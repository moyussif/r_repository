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
EPo2 <- read_excel("EPo2.xlsx")
str(EPo2)
# # #
print(EPo2, width = Inf)
describe(EPo2) #categorical data(HMISC)
# # #
#...............................................................................
-------------------- Clean data ( missing & duplicates) -------------------Step.2
#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#................................ Option.1 .....................................
describe(EPo2)#(HMISC)                 

#................................ Option.2 .....................................
#===================== Duplicates
#View duplicates
EPo2 %>% group_by(ID, Date) %>% filter(n() > 1)
#Remove duplicates, keeping the first row
EPo2 <- EPo2 %>% distinct(ID, Date, .keep_all = TRUE)
#remove all duplicates
EPo2 <- EPo2 %>% distinct()
#==================== Missing
#View missing
colSums(is.na(EPo2))
#Remove missing
EPo2_clean <- EPo2 %>% drop_na()
#verify clean data
colSums(is.na(EPo2_clean))
# # #
#-------------------------------------------------------------------------------
------------------------------- Data Conversion -------------------------------- 
#-------------------------------------------------------------------------------
#df$DATE_ONSET <- as.Date(df$DATE_ONSET, format = "%Y/%m/%d")
#
EPo2_clean$Sex <- as.factor(EPo2_clean$Sex)
EPo2_clean$AgeGroup <- as.factor(EPo2_clean$AgeGroup)
EPo2_clean$Region <- as.factor(EPo2_clean$Region)  
EPo2_clean$Fever <- as.factor(EPo2_clean$Fever) 
EPo2_clean$Bleeding <- as.factor(EPo2_clean$Bleeding)  
EPo2_clean$Bleeding_type <- as.factor(EPo2_clean$Bleeding_type)  
EPo2_clean$Marburg <- as.factor(EPo2_clean$Marburg)
EPo2_clean$Dengue <- as.factor(EPo2_clean$Dengue) 
EPo2_clean$Lassa_fever <- as.factor(EPo2_clean$Lassa_fever)
EPo2_clean$Yellow_Fever <- as.factor(EPo2_clean$Yellow_Fever)
EPo2_clean$Patient_statu <- as.factor(EPo2_clean$Patient_statu)
# 
str(EPo2_clean)

# # #
#-------------------------------------------------------------------------------
----------------------- Create epidemiological variables -----------------------Step.4
#----------------------------- Convert Dates -----------------------------------

EPo2_clean$DateReceived <- as.Date(EPo2_clean$DateReceived, format = "%Y/%m/%d")
#Create epidemiological Year
#FLU_clean$Year <- as.integer(format(FLU_clean$DATE_ONSET, "%Y"))
EPo2_clean$Year <- format(EPo2_clean$DateReceived, "%Y")
#Create epidemiological Month
EPo2_clean$Month <- month(EPo2_clean$DateReceived,label = TRUE)
#Create epidemiological weeks (start on Monday)
EPo2_clean$Week <- isoweek(EPo2_clean$DateReceived)
#
str(EPo2_clean)
# # #
--------------------------------------------------------------------------------
#----------------------------- Descriptive analysis -----------------------------Step.5
---------------------------------Demographics-----------------------------------
#
table1 <- EPo2_clean %>% select(Sex, AgeGroup, Patient_statu, Fever, Bleeding, 
                                Bleeding_type, Marburg, Dengue, Lassa_fever, Yellow_Fever) %>%
  tbl_summary(by = Fever,statistic = list(all_continuous() ~ "{median} ({p25}, {p75})",
                                          all_categorical() ~ "{n} ({p}%)"),
              digits = all_continuous() ~ 1,missing = "ifany") %>%
  add_p() %>%          # Adds p-values comparing groups
  add_overall() %>%    # Adds an Overall column
  bold_labels()

table1
# # #
#--------- Total Cases by COUNT - PREVALENCE - PROPORTION - INCIDENCE -----------.2

#--------------------- Marburg
describe_Marburg <- EPo2_clean %>% group_by(Region) %>%
  summarise(total = n(),
            Positive = sum(Marburg == "POSITIVE", na.rm = TRUE),
            Negative = sum(Marburg == "NEGATIVE", na.rm = TRUE),
            Percent_positive = round(100 * Positive / total, 1),
            Proportion_positive = round(Positive / total, 3),.groups = "drop") %>%
  filter(Positive > 0) %>% 
  arrange(desc(Percent_positive))
#---------------------- Dengue
describe_Dengue <- EPo2_clean %>% group_by(Region) %>%
  summarise(total = n(),
            Positive = sum(Dengue == "POSITIVE", na.rm = TRUE),
            Negative = sum(Dengue == "NEGATIVE", na.rm = TRUE),
            Percent_positive = round(100 * Positive / total, 1),
            Proportion_positive = round(Positive / total, 3),.groups = "drop") %>%
  filter(Positive > 0) %>% 
  arrange(desc(Percent_positive))
#--------------------- Lassa fever
describe_Lassa <- EPo2_clean %>% group_by(Region) %>%
  summarise(total = n(),
            Positive = sum(Lassa_fever == "POSITIVE", na.rm = TRUE),
            Negative = sum(Lassa_fever == "NEGATIVE", na.rm = TRUE),
            Percent_positive = round(100 * Positive / total, 1),
            Proportion_positive = round(Positive / total, 3),
            .groups = "drop") %>%
  filter(Positive > 0) %>%
  arrange(desc(Percent_positive))
#-------------------- Yellow fever
describe_YellowFever <- EPo2_clean %>% group_by(Region) %>%
  summarise(total = n(),
            Positive = sum(Yellow_Fever == "POSITIVE", na.rm = TRUE),
            Negative = sum(Yellow_Fever == "NEGATIVE", na.rm = TRUE),
            Percent_positive = round(100 * Positive / total, 1),
            Proportion_positive = round(Positive / total, 3),.groups = "drop") %>%
  filter(Positive > 0) %>%
  arrange(desc(Percent_positive))
#
#---------------------------- All Diseases -------------------------------------
describe_all <- bind_rows(describe_Marburg %>% mutate(Disease = "Marburg"),
                          describe_Dengue %>% mutate(Disease = "Dengue"),
                          describe_Lassa %>% mutate(Disease = "Lassa fever"),
                          describe_YellowFever %>% mutate(Disease = "Yellow fever")) %>%
  select(Disease, everything()) %>% 
  arrange(Disease, desc(Percent_positive))
#------------------------
print(describe_all, n = Inf, width = Inf)

####
--------------------------------------------------------------------------------
#--------------------------- Calculate incidence rate --------------------------Step.6
--------------------------------------------------------------------------------
  str(df)
#For incidence 
disease_cols <- c("Marburg","Dengue", "Lassa_fever", "Yellow_Fever")
population <- nrow(EPo2_clean)

#For incidence per 100, and per 1,000 people:
all_incidence <- tibble(Disease = disease_cols,
                        Cases = sapply(disease_cols,function(x) sum(EPo2_clean[[x]] == "POSITIVE", na.rm = TRUE))) %>%
  mutate(Population = population,
         Incidence = Cases / Population,
         Incidence_per_100 = round(100 * Incidence, 1),
         Incidence_per_1000 = round(1000 * Incidence, 1))

all_incidence

#+++++++++++++++++++++++++++++++++++++++

# For incidence by Region
all_incidence_region <- EPo2_clean %>% select(Region, all_of(disease_cols)) %>%
  pivot_longer(cols = all_of(disease_cols),names_to = "Disease",values_to = "Result") %>%
  group_by(Region, Disease) %>%
  summarise(Population = n(),
            Cases = sum(Result == "POSITIVE", na.rm = TRUE),
            Incidence = Cases / Population,Incidence_per_100 = round(100 * Incidence, 1),
            Incidence_per_1000 = round(1000 * Incidence, 1),.groups = "drop") %>%
  filter(Cases > 0) %>%
  arrange(Region, desc(Incidence_per_100))
#
print(all_incidence_region, n = Inf)
# # #
--------------------------------------------------------------------------------
#---------------------------- Create epidemic curve ----------------------------Step.7
--------------------------------------------------------------------------------
#Prepare data
disease_cols <- c("Marburg", "Dengue", "Lassa_fever", "Yellow_Fever")
#
long_df <- EPo2_clean %>%
  pivot_longer(cols = all_of(disease_cols),
               names_to = "Disease",
               values_to = "Cases") %>% mutate(date = as.Date(DateReceived))
#Weekly epidemic curve
weekly_epi <- long_df %>%
  mutate(
    Period = floor_date(date, unit = "week")
  ) %>%
  group_by(Period, Disease) %>%
  summarise(
    Cases = sum(Cases, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(weekly_epi, aes(x = Period, y = Cases, color = Disease)) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    title = "Weekly Epidemic Curve",
    x = "Week",
    y = "Cases"
  ) +
  theme_minimal()

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
  EPo2_clean %>%
  count(Region, name = "disease_cols") %>%
  left_join(
    df %>%
      group_by(Region) %>%
      summarise(population = first(population), .groups = "drop"),
    by = c("REGION", "District")
  ) %>%
  mutate(prevalence = 100 * Positive / total) %>%
  ggplot(aes(x = reorder(District, FLU_cases), y = disease_cols)) +
  geom_col(fill = "steelblue") +
  geom_text(
    aes(label = paste0(Diseases, " (", round(prevalence, 1), "%)")),
    hjust = -0.1,
    size = 3
  ) +
  coord_flip() +
  facet_wrap(~Region, scales = "free_y") +
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
  p1 <- ggplot(EPo2_clean, aes(Age)) +
  geom_histogram(binwidth = 5, fill = "orange") +
  labs(
    title = "Age Distribution of Cases",
    x = "Age (years)",
    y = "Number of Cases"
  ) +
  theme_classic()

# Age group bar plot
p2 <- EPo2_clean %>%
  count(AgeGroup) %>%
  ggplot(aes(x = AgeGroup, y = n)) +
  geom_col(fill = "orange") +
  labs(
    title = "Cases by Age Group",
    x = "Age Group (years)",
    y = "Number of Cases"
  ) +
  theme_sub_panel() +
  theme(
    panel.background = element_rect(fill = "grey95", colour = NA),
    plot.background = element_rect(fill = "grey100", colour = NA),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Combine plots
p1 + p2

# # #
--------------------------------------------------------------------------------
  #------------------------------- Cross-tabulation ------------------------------Step.11
  --------------------------------------------------------------------------------
  str(df)
#
table(df$SEX, df$FLUMATRIX)  

# # #
--------------------------------------------------------------------------------
  #-------------------------------- Chi-square test ------------------------------Step.12
  --------------------------------------------------------------------------------
  # 
  chisq.test(table(df$SEX, df$FLUMATRIX))  

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
  weekly_cases <- df %>% count(Week)

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
  df %>% summarise(Total_Cases = n(),Mean_Age = mean(AGE),Median_Age = median(AGE))  
#Cases by district  
df %>% group_by(District) %>% summarise(Cases = n()) 

###