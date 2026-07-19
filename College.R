#  #  #
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
============================ Surveillance FLU_data =============================
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
  library(Hmisc)
  library(stats)
  library(epitools)
  library(lubridate)
  library(gtsummary)
  library(surveillance)
  
  # # #
  setwd("C:/Users/User/Downloads")
  #
  FLU10 <- read_excel("FLU100.xlsx")
  str(FLU10)
  # # #
  print(FLU10, width = Inf)
  
  #------------------------------ Data Conversion ------------------------------
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
  #-------------------Convert Dates Conversion ---------------------------------
  FLU10$DATE_ONSET <- as.Date(FLU10$DATE_ONSET)
  #Create epidemiological Month
  FLU10$Month <- month(FLU10$DATE_ONSET,label = TRUE)
  
  #Create epidemiological weeks (start on Monday)
  FLU10$Week <- isoweek(FLU10$DATE_ONSET)
  #
  #------------- Create Agegroup 
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
  
  #---------------------------- Descriptive analysis ---------------------------
  #
  table1 <- df %>%
    select(ILI_SARI, SEX, Age_group, REGION, FLUMATRIX) %>%
    tbl_summary(by = ILI_SARI,
                statistic = list(all_continuous() ~ "{median} ({p25}, {p75})",
                                 all_categorical() ~ "{n} ({p}%)"),
                digits = all_continuous() ~ 1,missing = "ifany") %>%
    add_p() %>%          # Adds p-values comparing groups
    add_overall() %>%    # Adds an Overall column
    bold_labels()
  
  table1  
  #
  #..................Total Cases ......................
  FLU_cases <- sum(df$FLUMATRIX != "NEG", na.rm = TRUE)
  FLU_cases
  #..................Cases by sex
  df %>%
    group_by(SEX) %>%
    summarise(
      total = n(),
      FLU_cases = sum(FLUMATRIX != "NEG", na.rm = TRUE),
      negative = sum(FLUMATRIX == "NEG", na.rm = TRUE),
      percent_positive = 100 * FLU_cases / total
    )
  #..................Cases by District................
  #case_Count
  df %>%
    group_by(REGION) %>%
    summarise(
      total = n(),
      FLU_cases = sum(FLUMATRIX != "NEG", na.rm = TRUE),
      negative = sum(FLUMATRIX == "NEG", na.rm = TRUE),
      percent_positive = 100 * FLU_cases / total) %>%
    arrange(desc(FLU_cases))
  
  #case_Prevalence
  df %>%
    group_by(REGION) %>%
    summarise(
      total = n(),
      FLU_cases = sum(FLUMATRIX != "NEG", na.rm = TRUE),
      negative = sum(FLUMATRIX == "NEG", na.rm = TRUE),
      percent_positive = 100 * FLU_cases / total
    ) %>%
    arrange(desc(percent_positive))
  
  #case_Proportion
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
  #--------------------------- Calculate incidence rate --------------------------
  str(df)
  #For incidence 
  population <- 810
  FLU_cases <- sum(df$FLUMATRIX != "NEG", na.rm = TRUE)
  
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
  #---------------------------- Create epidemic curve ----------------------------
  # Epidemic curve (weekly)
  df %>%
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
  
  # # #
  #---------------------------- Bar chart by district 
  #  
  df %>%
    count(REGION, District, name = "FLU_cases") %>%
    left_join(df %>% group_by(REGION, District) %>%
                summarise(population = first(population), .groups = "drop"),
              by = c("REGION", "District")) %>%
    mutate(prevalence = 100 * FLU_cases / population) %>%
    ggplot(aes(x = reorder(District, FLU_cases), y = FLU_cases)) +
    geom_col(fill = "steelblue") +
    geom_text(aes(label = paste0(FLU_cases)),hjust = -0.1, size = 3) +
    coord_flip() +
    facet_wrap(~REGION, scales = "free_y") +
    labs(x = "District",y = "Number of cases",title = "Flu Cases by District Within Region")+
    theme_classic()
  # # #
  #------------------------------- Age distribution ----------------------------
  # Histogram of age distribution
  ggplot(df, aes(AGE)) +geom_histogram(binwidth = 5, fill = "orange") +
    labs(title = "Age Distribution of Cases",x = "Age (years)", y = "Number of Cases")+
    theme_minimal()
  # Age group bar plot
  df %>% count(Age_group) %>%
    ggplot(aes(x = Age_group, y = n)) + geom_col(fill = "gray") +
    labs(title = "Cases by Age Group", x = "Age Group (years)", y = "Number of Cases")+
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # # #
  #-------------------------------- Time series ----------------------------------
  #  
  weekly_cases <- df %>% count(Week)
  
  plot(weekly_cases$Week, weekly_cases$n, type="l")  
  
  
# # #
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                                 Malaria_data
--------------------------------------------------------------------------------
 #Task;
      (1)Five_year incidence of malaria.

      (2)Five_year uptake of IPTp.
 
      (3)Factors associated with adequate IPTp (3doses and 5doses).
 
      (4)Risk of malaria, given exposure to adequate IPTp coverages.
  
      (5)Any other
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
gc(reset = TRUE)
library(readxl)
library(tidyverse)
library(gtsummary)
# import data
setwd("C:/Users/User/Downloads")

GAR_data <- read_excel("GAR_Malaria.xlsx")

str(GAR_data)

#Reshape to long format.........................................................
#
malaria <- GAR_data %>%pivot_longer(cols = starts_with("Confirmed"),
                                    names_to = "Year",
                                    values_to = "Malaria")
#
iptp <- GAR_data %>% pivot_longer(cols = starts_with("Percentage of Pregnant"),
                                  names_to = "Year",
                                  values_to = "IPTp")
#
#(1)Five-year incidence of malaria................................................................................
malaria_summary <- malaria %>% group_by(District) %>% summarise(Mean_Malaria = mean(Malaria, na.rm = TRUE),
                                                                SD = sd(Malaria, na.rm = TRUE),
                                                                Median = median(Malaria, na.rm = TRUE),
                                                                Minimum = min(Malaria, na.rm = TRUE),
                                                                Maximum = max(Malaria, na.rm = TRUE),
                                                                Cumulative_Incidence = sum(Malaria, na.rm = TRUE))
#
print(malaria_summary, n = Inf)
#

#
#(2)Five-year uptake of IPTp......................................................................................
iptp_summary <- iptp %>% group_by(District) %>% summarise(Mean_IPTp = mean(IPTp, na.rm = TRUE),
                                                          SD = sd(IPTp, na.rm = TRUE),
                                                          Median = median(IPTp, na.rm = TRUE),
                                                          Minimum = min(IPTp, na.rm = TRUE),
                                                          Maximum = max(IPTp, na.rm = TRUE),
                                                          Cumulative_IPTp = sum(IPTp, na.rm = TRUE))
#
iptp_summary
#

#Objective 3: Factors Associated with Adequate IPTp (3 and 5 doses)..............................................
#
This cannot be done with the provided dataset. 
The dataset contains only district-level IPTp3 percentages. 
To identify factors associated with adequate IPTp uptake, you would need individual-level or 
additional district-level predictors such as maternal age, parity, education, ANC attendance, residence, or
socioeconomic status. If you had those data, you could fit a logistic regression model, for example:
#
# Adequate IPTp (3+ doses)
  df$Adequate_IPTp3 <- ifelse(df$IPTp_doses >= 3, 1, 0)

# Adequate IPTp (5+ doses)
df$Adequate_IPTp5 <- ifelse(df$IPTp_doses >= 5, 1, 0)
#
glm(IPTp3_Adequate ~ Age + Parity + ANC_Visits + Education + Residence,family = binomial)
#
#Objective 4: Risk of Malaria Given Adequate IPTp Coverage......................................................
#
With the current district-level data, you can estimate an ecological association, not an individual risk. 
A suitable model is:
#
lmer(Malaria ~ IPTp + Year + (1 | District), data = long)
#
The regression coefficient for IPTp indicates the average change in malaria incidence (cases per 1,000 population) 
associated with a one-percentage-point increase in IPTp coverage, 
after accounting for time (and district, in the mixed model).
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Trend Analysis...........................................................................
ggplot(malaria,aes(Year, Malaria, group=District, colour=District))+
  geom_line(size=1)+
  geom_point()+
  coord_flip()
#
ggplot(iptp,
       aes(Year, IPTp,group=District,colour=District))+
  geom_line(size=1)+
  geom_point()+
  coord_flip()
#Correlation between IPTp.................................................................
cor.test(iptp$IPTp,malaria$Malaria,method="pearson")#(method="spearman")
#
#Linear regression........................................................................
dataCombined <- data.frame(Malaria = malaria$Malaria,IPTp = iptp$IPTp)
#
model <- lm(Malaria ~ IPTp, data = dataCombined)
summary(model)
#Predict Malaria from IPTp
predict(model)

#Regression Table
tbl_regression(model)

malaria_summary %>%
  
  gt()

#Adjusting for Time.......................................................................
dataCombined <- data.frame(Malaria = malaria$Malaria,IPTp = iptp$IPTp,Year = iptp$Year)
#
model2 <- lm(Malaria ~ IPTp + Year,data=dataCombined)
summary(model2)

#Mixed effect model......................................................................

#
dataCombinedm <- data.frame(District = malaria$District,Year = malaria$Year,Malaria = malaria$Malaria,IPTp = iptp$IPTp)
#
mixed <-lmer(Malaria ~ IPTp + Year +(1|District),data=dataCombinedm)

summary(mixed)

#Annual percentage change...............................................................
Percentchange <- dataCombinedm %>% 
  group_by(District) %>% 
  arrange(Year) %>% 
  mutate(AnnualChange=100*(Malaria-lag(Malaria))/lag(Malaria))
#
Percentchange

#District Ranking.......................................................................
#Malaria
malaria_summary %>%arrange(desc(Mean_Malaria))
#IPTp 
iptp_summary %>% arrange(desc(Mean_IPTp))

#Scatter
ggplot(dataCombined,aes(IPTp,Malaria))+geom_point(size=3)+geom_smooth(method="lm")