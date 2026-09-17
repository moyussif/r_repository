#================== Chisquare and Logistic Regression ==========================
rm(list=ls())
gc(reset = TRUE)
#
#---------------------- Load required packages
library(readxl)
library(readr)
library(tidyverse)
library(car)
library(pROC)
library(psych)
library(lessR)
library(Hmisc)
library(stats)
library(broom)
library(epitools)
library(ResourceSelection)

#------------------Import data------------
health_data <- read_excel("C:/Users/User/Downloads/health_data.xlsx")
#
str(health_data) 
print(health_data, width = Inf)
#
#Create Hypetensive variable
health_data$hypertensive <- ifelse(health_data$BP_systolic >= 140,"Yes", "No")

#Create Age_group
healthData = health_data %>% mutate(Age_group = case_when(
  Age  < 5 ~ "0-4",
  Age  < 11 ~ "5-10",
  Age  < 18 ~ "11-17",
  Age  < 35 ~ "18-34",
  Age  < 50 ~ "35-49",
  Age  < 65 ~ "50-64",
  TRUE ~ "65+"
))

str(healthData)
#
#-------------------------------------------------------------------------------
            Data Conversion _(CODING)  
#-------------------------------------------------------------------------------
#
healthData$Age_group <- as.factor(healthData$Age_group)

healthData$sex <- as.factor(healthData$sex)

healthData$marital_status <- as.factor(healthData$marital_status)

healthData$smoking  <- as.factor(healthData$smoking)

healthData$hypertensive <- as.factor(healthData$hypertensive)
#
str(healthData)


# ==============================
############################################################
# MASTER R SCRIPT: RUN ALL COMMON REGRESSIONS
############################################################

# ==========================================================
# 1. INSTALL AND LOAD PACKAGES
# ==========================================================

packages <- c(
  "tidyverse",
  "readxl",
  "haven",
  "broom",
  "lmtest",
  "sandwich",
  "car",
  "fixest",
  "modelsummary",
  "performance",
  "psych"
)

installed <- rownames(installed.packages())

for (p in packages) {
  if (!(p %in% installed)) {
    install.packages(p)
  }
}

library(tidyverse)
library(readxl)
library(haven)
library(broom)
library(lmtest)
library(sandwich)
library(car)
library(fixest)
library(modelsummary)
library(performance)
library(psych)


# ==========================================================
# 2. IMPORT DATA
# ==========================================================

# ---- CSV ----
data <- read.csv("your_data.csv")

# ---- OR Excel ----
# data <- read_excel("your_data.xlsx")

# ---- OR Stata ----
# data <- read_dta("your_data.dta")


# ==========================================================
# 3. INSPECT DATA
# ==========================================================

str(data)
summary(data)
head(data)
dim(data)

# Missing values
colSums(is.na(data))

# Variable names
names(data)


# ==========================================================
# 4. DEFINE VARIABLES
# ==========================================================

# CHANGE THESE TO YOUR ACTUAL VARIABLE NAMES

outcome <- "Y"

main_x <- "X"

control1 <- "age"
control2 <- "gender"
control3 <- "education"
control4 <- "income"


# ==========================================================
# 5. DESCRIPTIVE STATISTICS
# ==========================================================

describe(data)

# Summary statistics for selected variables
data %>%
  select(all_of(c(
    outcome,
    main_x,
    control1,
    control2,
    control3,
    control4
  ))) %>%
  psych::describe()


# ==========================================================
# 6. CORRELATION MATRIX
# ==========================================================

cor_data <- data %>%
  select(where(is.numeric))

cor_matrix <- cor(
  cor_data,
  use = "pairwise.complete.obs"
)

round(cor_matrix, 3)


# ==========================================================
# 7. SIMPLE OLS REGRESSION
# ==========================================================

model1 <- lm(
  Y ~ X,
  data = data
)

summary(model1)


# ==========================================================
# 8. OLS WITH CONTROL VARIABLES
# ==========================================================

model2 <- lm(
  Y ~ X + age + gender + education + income,
  data = data
)

summary(model2)


# ==========================================================
# 9. FULL OLS MODEL
# ==========================================================

model3 <- lm(
  Y ~ X +
    age +
    gender +
    education +
    income,
  data = data
)

summary(model3)


# ==========================================================
# 10. POLYNOMIAL / NON-LINEAR EFFECT
# ==========================================================

model4 <- lm(
  Y ~ X + I(X^2) +
    age +
    gender +
    education +
    income,
  data = data
)

summary(model4)


# ==========================================================
# 11. INTERACTION / MODERATION EFFECT
# ==========================================================

model5 <- lm(
  Y ~ X * gender +
    age +
    education +
    income,
  data = data
)

summary(model5)


# ==========================================================
# 12. ANOTHER INTERACTION
# ==========================================================

model6 <- lm(
  Y ~ X * education +
    age +
    gender +
    income,
  data = data
)

summary(model6)


# ==========================================================
# 13. ROBUST STANDARD ERRORS
# ==========================================================

coeftest(
  model3,
  vcov = vcovHC(
    model3,
    type = "HC1"
  )
)


# ==========================================================
# 14. HETEROSKEDASTICITY TEST
# ==========================================================

bptest(model3)


# ==========================================================
# 15. MULTICOLLINEARITY / VIF
# ==========================================================

vif(model3)


# ==========================================================
# 16. NORMALITY OF RESIDUALS
# ==========================================================

par(mfrow = c(2, 2))

plot(model3)

par(mfrow = c(1, 1))


# ==========================================================
# 17. BREUSCH-PAGAN / KOENKER TEST
# ==========================================================

bptest(model3)


# ==========================================================
# 18. RESET SPECIFICATION TEST
# ==========================================================

resettest(model3)


# ==========================================================
# 19. INFLUENTIAL OBSERVATIONS
# ==========================================================

cooks.distance(model3)

plot(
  cooks.distance(model3),
  type = "h",
  main = "Cook's Distance",
  ylab = "Cook's Distance"
)


# ==========================================================
# 20. LOGISTIC REGRESSION
# ==========================================================

# Use this when Y is binary: 0/1

logit1 <- glm(
  Y ~ X +
    age +
    gender +
    education +
    income,
  data = data,
  family = binomial(link = "logit")
)

summary(logit1)


# Odds ratios
exp(coef(logit1))

# Odds ratios with confidence intervals
exp(
  cbind(
    OR = coef(logit1),
    confint(logit1)
  )
)


# ==========================================================
# 21. PROBIT REGRESSION
# ==========================================================

probit1 <- glm(
  Y ~ X +
    age +
    gender +
    education +
    income,
  data = data,
  family = binomial(link = "probit")
)

summary(probit1)


# ==========================================================
# 22. POISSON REGRESSION
# ==========================================================

# Use for count dependent variables

poisson1 <- glm(
  Y ~ X +
    age +
    gender +
    education +
    income,
  data = data,
  family = poisson(link = "log")
)

summary(poisson1)


# ==========================================================
# 23. NEGATIVE BINOMIAL REGRESSION
# ==========================================================

# Uncomment if needed

# install.packages("MASS")
# library(MASS)

# nb1 <- glm.nb(
#   Y ~ X + age + gender + education + income,
#   data = data
# )

# summary(nb1)


# ==========================================================
# 24. FIXED-EFFECTS REGRESSION
# ==========================================================

# Example:
# individual_id = individual identifier
# year = time variable

fe_model <- feols(
  Y ~ X +
    age +
    gender +
    education +
    income |
    individual_id + year,
  data = data
)

summary(fe_model)


# ==========================================================
# 25. FIXED EFFECTS WITH CLUSTERED STANDARD ERRORS
# ==========================================================

fe_clustered <- feols(
  Y ~ X +
    age +
    gender +
    education +
    income |
    individual_id + year,
  data = data,
  cluster = ~individual_id
)

summary(fe_clustered)


# ==========================================================
# 26. TWO-WAY FIXED EFFECTS
# ==========================================================

twfe_model <- feols(
  Y ~ X +
    age +
    gender +
    education +
    income |
    individual_id + year,
  data = data,
  cluster = ~individual_id
)

summary(twfe_model)


# ==========================================================
# 27. CLUSTERED STANDARD ERRORS
# ==========================================================

# Example: cluster by individual_id

cluster_model <- feols(
  Y ~ X +
    age +
    gender +
    education +
    income,
  data = data,
  cluster = ~individual_id
)

summary(cluster_model)


# ==========================================================
# 28. MODEL COMPARISON
# ==========================================================

modelsummary(
  list(
    "Model 1" = model1,
    "Model 2" = model2,
    "Model 3" = model3,
    "Model 4" = model4,
    "Model 5" = model5,
    "Model 6" = model6
  ),
  stars = TRUE
)


# ==========================================================
# 29. REGRESSION TABLE WITH ROBUST SE
# ==========================================================

modelsummary(
  list(
    "OLS" = model3,
    "Interaction" = model5,
    "Fixed Effects" = fe_model,
    "TWFE" = twfe_model
  ),
  vcov = "HC1",
  stars = TRUE
)


# ==========================================================
# 30. EXPORT REGRESSION TABLE
# ==========================================================

modelsummary(
  list(
    "Model 1" = model1,
    "Model 2" = model2,
    "Model 3" = model3,
    "Model 4" = model4,
    "Model 5" = model5,
    "Model 6" = model6
  ),
  stars = TRUE,
  output = "regression_results.docx"
)


# ==========================================================
# 31. SAVE RESULTS
# ==========================================================

sink("regression_results.txt")

cat("\n================ MODEL 1 ================\n")
print(summary(model1))

cat("\n================ MODEL 2 ================\n")
print(summary(model2))

cat("\n================ MODEL 3 ================\n")
print(summary(model3))

cat("\n================ MODEL 4 ================\n")
print(summary(model4))

cat("\n================ MODEL 5 ================\n")
print(summary(model5))

cat("\n================ MODEL 6 ================\n")
print(summary(model6))

cat("\n================ LOGIT ================\n")
print(summary(logit1))

cat("\n================ PROBIT ================\n")
print(summary(probit1))

sink()


# ==========================================================
# 32. SAVE R WORKSPACE
# ==========================================================

save.image(
  file = "all_regression_results.RData"
)


############################################################
# END OF SCRIPT
############################################################