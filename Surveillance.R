--------------------------------------------------------------------------------
  |||||||||||||||||||||||||| Analysis of Flu data ||||||||||||||||||||||||||||||||
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  rm(list=ls())
gc(reset = TRUE)  
#--------------load required packages
library(readxl)
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(forcats)
library(psych)
library(car)
library(lessR)
library(FSA)
library(Hmisc)
library(stats)
library(epitools)
library(gtsummary)
library(lubridate)

#  
FLU100 <- read_excel("C:/Users/User/Desktop/FLU100.xlsx")
str(FLU100)  

--------------------------------------------------------------------------------
  Create Age group
--------------------------------------------------------------------------------
  #
  df <- FLU100 %>%
  mutate(age_group = case_when(
    AGE < 5 ~ "0-4",
    AGE < 11 ~ "5-10",
    AGE < 18 ~ "11-17",
    AGE < 35 ~ "18-34",
    AGE < 50 ~ "35-49",
    AGE < 65 ~ "50-64",
    TRUE ~ "65+"
  ))

str(df)
print(df)

--------------------------------------------------------------------------------
  Data Conversion  
--------------------------------------------------------------------------------
  #
  df$DATE_ONSET <- as.Date(df$DATE_ONSET, format = "%Y/%m/%d")
#
df$WEEK <- isoweek(df$DATE_ONSET)#weeks start on Monday
df$WEEKdiff <- as.numeric(difftime(df$DATE_ONSET,min(df$DATE_ONSET, na.rm = TRUE),
                                   units = "weeks"))#Number of weeks since a reference date
#
df$SEX <- as.factor(df$SEX)
#df$AGE <-as.integer(df$AGE)# For whole Numbers 
#df$age_group <- factor(df$age_group, exclude=NULL)
df$AGE <-as.numeric(df$AGE)# For Decimal Numbers
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
print(df)
print(df, n = Inf, width = Inf)
#
summary(df)
describe.by(df)
#
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  --------------------------------------------------------------------------------
  ######################## Table - Descriptive Statistics  #######################
table1 <- df %>%
  select(ILI_SARI, AGE, SEX, age_group, REGION, District, FLUMATRIX) %>%
  tbl_summary(
    by = ILI_SARI,
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1,
    missing = "ifany"
  ) %>%
  add_p() %>%          # Adds p-values comparing groups
  add_overall() %>%    # Adds an Overall column
  bold_labels()

table1
#
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ########################## PREVALENCE BY AGE GROUP #############################
plot_data <- df %>%
  filter(!is.na(FLUMATRIX), !FLUMATRIX %in% c("SARI", "NEG")) %>%
  count(SEX, age_group, FLUMATRIX) %>%
  group_by(SEX, age_group) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  ungroup() %>%
  mutate(age_group = fct_reorder(age_group, n, .fun = sum, .desc = TRUE))

ggplot(plot_data, aes(x = age_group, y = n, fill = FLUMATRIX)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_text(
    aes(label = paste0(n, "\n(", sprintf("%.1f%%", percent), ")")),
    position = position_dodge(width = 0.8),
    vjust = -0.3,
    size = 3.5
  ) +
  facet_wrap(~ SEX) +
  labs(
    title = "ILI Cases by Age Group and Sex",
    x = "Age Group",
    y = "Number of Cases",
    fill = "Case Type"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  expand_limits(y = max(plot_data$n) + 5)

# # # 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ############################ PREVALENCE BY REGION ##############################
plot_data <- df %>%
  filter(!is.na(FLUMATRIX), !FLUMATRIX %in% c("SARI", "NEG")) %>%
  count(SEX, REGION, FLUMATRIX) %>%
  group_by(SEX, REGION) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  ungroup() %>%
  mutate(REGION = fct_reorder(REGION, n, .fun = sum, .desc = TRUE))

ggplot(plot_data, aes(x = REGION, y = n, fill = FLUMATRIX)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_text(
    aes(label = paste0(n, "\n(", sprintf("%.1f%%", percent), ")")),
    position = position_dodge(width = 0.8),
    vjust = -0.3,
    size = 3.5
  ) +
  facet_wrap(~ SEX) +
  labs(
    title = "ILI Cases by Region and Sex",
    x = "Region",
    y = "Number of Cases",
    fill = "Case Type"
  ) +
  theme_grey() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  expand_limits(y = max(plot_data$n) + 5)

# # # 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ############################## Weekly trends ###################################
##----------------#######-----------------------------
plot_data <- df %>%
  filter(!is.na(FLUMATRIX), !FLUMATRIX %in% c("SARI", "NEG")) %>%
  count(SEX, WEEK, FLUMATRIX) %>%
  group_by(SEX, WEEK) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  ungroup()

# Calculate total weekly cases for trend line
trend_data <- plot_data %>%
  group_by(SEX, WEEK,FLUMATRIX) %>%
  summarise(total_cases = sum(n), .groups = "drop")

ggplot(plot_data, aes(x = WEEK, y = n, fill = FLUMATRIX)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_line(
    data = trend_data,
    aes(x = WEEK, y = total_cases, group = SEX),
    color = "black",
    linewidth = 1
  ) +
  geom_point(
    data = trend_data,
    aes(x = WEEK, y = total_cases),
    color = "black",
    size = 2
  ) +
  geom_text(
    aes(label = paste0(n, "\n(", sprintf("%.1f%%", percent), ")")),
    position = position_dodge(width = 0.8),
    vjust = -0.3,
    size = 3
  ) +
  facet_wrap(~ SEX) +
  labs(
    title = "Weekly ILI Cases by Sex with Trend Line",
    x = "Epidemiological Week",
    y = "Number of Cases",
    fill = "Case Type"
  ) +
  theme_grey() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  scale_x_continuous(breaks = unique(plot_data$WEEK)) +
  expand_limits(y = max(trend_data$total_cases) + 5)
########$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
plot_data <- df %>%
  filter(!is.na(FLUMATRIX), !FLUMATRIX %in% c("SARI", "NEG")) %>%
  count(SEX, WEEK, FLUMATRIX) %>%
  group_by(SEX, WEEK) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  ungroup()

ggplot(plot_data, aes(x = WEEK, y = n, color = FLUMATRIX, group = FLUMATRIX)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ SEX) +
  labs(
    title = "Weekly ILI Cases Trend by Sex",
    x = "Epidemiological Week",
    y = "Number of Cases",
    color = "Case Type"
  ) +
  theme_grey() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  scale_x_continuous(breaks = unique(plot_data$WEEK))
