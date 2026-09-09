#
rm(list=ls())
gc(reset = TRUE)
#===============================================================================
#                              RotaVirus Analysis
#===============================================================================
install.packages(c("naniar","tableone"))
#
library(tidyverse)
library(lubridate)
library(janitor)
library(gtsummary)
library(epiDisplay)
library(broom)
library(scales)
library(naniar)
library(tableone)

# ==============

RotavacVaccine <- read_excel("C:/Users/User/Downloads/RotavacVaccine.xlsx")

# Inspect
str(RotavacVaccine)


# Missing data summary
missing_summary <- RotavacVaccine %>%
  summarise(across(everything(),
                   ~ sum(is.na(.)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing_n"
  ) %>%
  mutate(
    missing_percent = 100 * missing_n / nrow(RotavacVaccine)
  ) %>%
  arrange(desc(missing_percent))

missing_summary
#
vis_miss(RotavacVaccine)


#r#rotavirus_stool

table(RotavacVaccine$rotavirus_stool, useNA = "ifany")
#

data <- RotavacVaccine %>%
  mutate(
    rotavirus_positive = case_when(
      rotavirus_stool %in% c("Positive", "positive", "POS", 1) ~ 1,
      rotavirus_stool %in% c("Negative", "negative", "NEG", 0) ~ 0,
      TRUE ~ NA_real_
    )
  )
#
data %>%
  summarise(
    tested = sum(!is.na(rotavirus_positive)),
    positive = sum(rotavirus_positive == 1, na.rm = TRUE),
    positivity = positive / tested * 100
  )
#

#Age
summary(data$age_months)

data %>%
  summarise(
    median_age = median(age_months, na.rm = TRUE),
    mean_age = mean(age_months, na.rm = TRUE),
    q1 = quantile(age_months, 0.25, na.rm = TRUE),
    q3 = quantile(age_months, 0.75, na.rm = TRUE)
  )

#
#
data <- data %>%
  mutate(
    age_group = case_when(
      age_months < 6 ~ "<6 months",
      age_months < 12 ~ "6–11 months",
      age_months < 24 ~ "12–23 months",
      age_months < 36 ~ "24–35 months",
      age_months >= 36 ~ "≥36 months",
      TRUE ~ NA_character_
    )
  )

# Rotavirus positive by age

age_results <- data %>%
  filter(!is.na(rotavirus_positive)) %>%
  group_by(age_group) %>%
  summarise(
    tested = n(),
    positive = sum(rotavirus_positive == 1),
    positivity = positive / tested * 100
  )

age_results

#graph

ggplot(age_results,
       aes(x = age_group, y = positivity)) +
  geom_col(fill = "#0072B2") +
  geom_text(
    aes(label = paste0(round(positivity, 1), "%")),
    vjust = -0.3
  ) +
  labs(
    x = "Age group",
    y = "Rotavirus positivity (%)",
    title = "Rotavirus positivity by age group"
  ) +
  theme_minimal()

#sex

sex_results <- data %>%
  filter(!is.na(rotavirus_positive),
         !is.na(sex)) %>%
  group_by(sex) %>%
  summarise(
    tested = n(),
    positive = sum(rotavirus_positive == 1),
    positivity = positive / tested * 100
  )

sex_results

#
tab_sex <- table(
  data$sex,
  data$rotavirus_positive
)

chisq.test(tab_sex)
#
fisher.test(tab_sex)

#Ecological_belt

belt_results <- data %>%
  filter(!is.na(rotavirus_positive),
         !is.na(Ecological_belt)) %>%
  group_by(Ecological_belt) %>%
  summarise(
    tested = n(),
    positive = sum(rotavirus_positive == 1),
    positivity = 100 * positive / tested
  )

belt_results

#plot

ggplot(belt_results,
       aes(x = reorder(Ecological_belt, positivity),
           y = positivity)) +
  geom_col(fill = "#009E73") +
  coord_flip() +
  labs(
    x = "Ecological belt",
    y = "Rotavirus positivity (%)",
    title = "Rotavirus positivity by ecological belt"
  ) +
  theme_minimal()

#

#Hospital-level analysis

hospital_results <- data %>%
  filter(!is.na(rotavirus_positive),
         !is.na(hospital_name)) %>%
  group_by(hospital_name) %>%
  summarise(
    tested = n(),
    positive = sum(rotavirus_positive == 1),
    positivity = 100 * positive / tested
  ) %>%
  arrange(desc(positivity))

hospital_results
#

#Temporal analysis
data <- data %>%
  mutate(
    int_date = as.Date(int_date),
    year = year(int_date),
    month = month(int_date),
    month_name = month(int_date, label = TRUE)
  )
#

#annual positivity
annual_results <- data %>%
  filter(!is.na(rotavirus_positive)) %>%
  group_by(year) %>%
  summarise(
    tested = n(),
    positive = sum(rotavirus_positive == 1),
    positivity = 100 * positive / tested
  )

annual_results
#

#plot
ggplot(annual_results,
       aes(x = year, y = positivity)) +
  geom_line(linewidth = 1.2, color = "#D55E00") +
  geom_point(size = 3, color = "#D55E00") +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    x = "Year",
    y = "Rotavirus positivity (%)",
    title = "Annual trend in rotavirus positivity"
  ) +
  theme_minimal()
#

#Monthly seasonality:
monthly_results <- data %>%
  filter(!is.na(rotavirus_positive)) %>%
  group_by(year, month, month_name) %>%
  summarise(
    tested = n(),
    positive = sum(rotavirus_positive == 1),
    positivity = 100 * positive / tested,
    .groups = "drop"
  )

ggplot(monthly_results,
       aes(x = month, y = positivity, group = year,
           color = factor(year))) +
  geom_line() +
  geom_point() +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  labs(
    x = "Month",
    y = "Rotavirus positivity (%)",
    color = "Year",
    title = "Monthly rotavirus positivity"
  ) +
  theme_minimal()


#######################
Vaccination analysis
Your dataset contains:
  
vaccinated
received_rotateq_dose1
received_rotateq_dose2
received_rotateq_dose3
reasons for non-vaccination
###########################


table(data$vaccinated, useNA = "ifany")

table(data$received_rotateq_dose1, useNA = "ifany")
table(data$received_rotateq_dose2, useNA = "ifany")
table(data$received_rotateq_dose3, useNA = "ifany")
#

#Calculat vaccination coverage
data %>%
  summarise(
    vaccinated_n = sum(vaccinated == "Yes", na.rm = TRUE),
    vaccinated_percent =
      100 * vaccinated_n / sum(!is.na(vaccinated))
  )

#compare rotavirus positivity according to vaccination status:

data %>%
  filter(!is.na(rotavirus_positive),
         !is.na(vaccinated)) %>%
  group_by(vaccinated) %>%
  summarise(
    n = n(),
    positive = sum(rotavirus_positive == 1),
    positivity = 100 * positive / n
  )

#Test association
table_vaccine <- table(
  data$vaccinated,
  data$rotavirus_positive
)

chisq.test(table_vaccine)

#

##Clinical characteristics

days_diarrhea_before_admission
max_diarrhea_episodes_24hours
vomiting
max_vomiting_episodes_24hrs
days_vomiting_before_admission
highest_temp
sunken_eyes
thirst_status
capillary_refill
iv_fluids
general_condition
ors

#

##
clinical_summary <- data %>%
  group_by(rotavirus_positive) %>%
  summarise(
    n = n(),
    median_diarrhea_days =
      median(days_diarrhea_before_admission, na.rm = TRUE),
    median_diarrhea_episodes =
      median(max_diarrhea_episodes_24hours, na.rm = TRUE),
    median_vomiting_episodes =
      median(max_vomiting_episodes_24hrs, na.rm = TRUE),
    median_fever =
      median(highest_temp, na.rm = TRUE)
  )

clinical_summary

##

### Descriptive Table 1
data %>%
  select(
    rotavirus_positive,
    age_months,
    sex,
    Ecological_belt,
    hospital_name,
    breastfed,
    vaccinated,
    chronic_conditions,
    hiv_aids,
    vomiting,
    ors,
    iv_fluids,
    general_condition,
    sunken_eyes,
    thirst_status
  ) %>%
  tbl_summary(
    by = rotavirus_positive,
    missing = "ifany"
  ) %>%
  add_p() %>%
  bold_labels()
#

# Logistic regression
# modelling rotavirus positivity as the dependent variable.


model <- glm(
  rotavirus_positive ~
    age_months +
    sex +
    ecological_belt +
    vaccinated +
    breastfed +
    chronic_conditions +
    hiv_aids +
    vomiting +
    days_diarrhea_before_admission +
    highest_temp,
  data = data,
  family = binomial
)

summary(model)

#convert coefficient to odd ratio

model_results <- broom::tidy(
  model,
  exponentiate = TRUE,
  conf.int = TRUE
)

model_result

#cleaner Table
model %>%
  tbl_regression(
    exponentiate = TRUE
  )








