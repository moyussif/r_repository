#---------------------- Biostatistical  analysis -------------------------------
#__________________________ Dataset contains ___________________________________
#Variable	          Description                        Type
id                  Participant ID	                   Identifier
age	                Age in years	                     Continuous
sex	                Sex	                               Categorical
bmi	                Body mass index	                   Continuous
smoking	            Smoking status	                   Categorical
diabetes	          Diabetes	                         Binary
hypertension	      Hypertension	                     Binary
treatment	          Treatment group                    Binary
sbp	                Systolic BP	                       Continuous
dbp	                Diastolic BP	                     Continuous
cholesterol	        Total cholesterol	                 Continuous
ldl	                LDL cholesterol	                   Continuous
hba1c	              HbA1c	                             Continuous
crp	                C-reactive protein	               Continuous/skewed
cv_event	          Cardiovascular event               Binary
hospitalization	    Cardiovascular hospitalization	   Binary
followup_months	    Follow-up duration	               Time
death	              Death	                             Binary/time-to-event
qol_baseline	      Baseline quality of life	         Continuous
qol_12m	            12-month quality of life	         Continuous
#_______________________________________________________________________________
################################################################################
packages <- c("tidyverse", "gtsummary", "tableone", "janitor", "rstatix","broom", "broom.mixed", "car", "lmtest",
              "sandwich", "MASS","survival", "survminer", "pROC", "ResourceSelection", "lme4","geepack", "emmeans",
              "MatchIt", "survey", "epiR", "epitools","boot", "performance", "mice")
# Find packages that are not installed
missing_packages <- packages[!packages %in% rownames(installed.packages())]
# Install only missing packages
if (length(missing_packages) > 0) {install.packages(missing_packages, dependencies = TRUE)}
# Load all packages
invisible(lapply(packages, library, character.only = TRUE))
#
################################################################################
#                  SIMULATED MEDICAL RESEARCH DATA
################################################################################
set.seed(2026)


n <- 1000

dat <- tibble(id = 1:n,
              age = round(rnorm(n, 58, 12)),
              sex = factor(sample(c("Female", "Male"),n,replace = TRUE)),
              bmi = round(rnorm(n, 28, 5),1),
              smoking = factor(sample(c("Never", "Former", "Current"),n,replace = TRUE,prob = c(0.50, 0.30, 0.20))),
              diabetes = factor(rbinom(n, 1, 0.25),levels = c(0, 1),labels = c("No", "Yes")),
              hypertension = factor(rbinom(n, 1, 0.45),levels = c(0, 1),labels = c("No", "Yes")),
              treatment = factor(rbinom(n, 1, 0.50),levels = c(0, 1),labels = c("Control", "Intervention")))

dat12 <- dat %>%
  mutate(
    sbp = round(115 +0.65 * age +5 * (sex == "Male") +8 * 
                       (hypertension == "Yes") -4 * 
                       (treatment == "Intervention") +rnorm(n, 0, 12)),
    dbp = round(65 +0.25 * age +3 * (sex == "Male") +5 * 
                  (hypertension == "Yes") - 2 * 
                  (treatment == "Intervention") +rnorm(n, 0, 8)),
    cholesterol = round(190 +10 * (diabetes == "Yes") +4 * (sex == "Male") +rnorm(n, 0, 30)),
    ldl = round(cholesterol * 0.60 +rnorm(n, 0, 15)),
    hba1c = round(5.4 + 0.9 * (diabetes == "Yes") + 0.02 * (bmi - 25) +rnorm(n, 0, 0.5),2),
    death = round(1 + 0.9 * (diabetes == "Yes") +0.50 * (smoking == "Current")+rnorm(n, 0, 0.5)),
    crp = round(exp(rnorm(n,log(2.5),0.8)),2))

str(dat12)

print(dat12)

library(writexl)
write_xlsx(dat12, "data12.xlsx")



# Cardiovascular-event probability
event_probability <- plogis(-5 +0.045 * dat$age +0.035 * (dat$bmi - 25) +0.60 * 
                              (dat$smoking == "Current") +0.50 * (dat$diabetes == "Yes") +0.55 * 
                              (dat$hypertension == "Yes") -0.50 * (dat$treatment == "Intervention"))

dat <- dat %>%
  mutate(
    cv_event = rbinom(n, 1, event_probability),
    hospitalization = rbinom(n, 1,plogis(-3 + 0.035 * age +0.50 * (diabetes == "Yes") +0.40 * (hypertension == "Yes") -0.40 * (treatment == "Intervention"))),
    followup_months = round(runif(n, 6, 24), 1),
    death = rbinom(n, 1,plogis(-7 + 0.055 * age +0.75 * cv_event +0.60 * (diabetes == "Yes") +0.50 * (smoking == "Current"))),
    qol_baseline = pmax(0,pmin(100,75 - 0.25 * (age - 50) -3 * (diabetes == "Yes") +rnorm(n, 0, 10))),
    qol_12m = pmax(0,pmin(100,qol_baseline +5 * (treatment == "Intervention") +rnorm(n, 2, 8)))) %>%
  select(-all_of("event_probability"))

str(dat)

################################################################################
#                             DATA QUALITY CONTROL
################################################################################

# Missing values
colSums(is.na(dat12))

# Percentage missing
round(colMeans(is.na(dat12)) * 100,2)

# Duplicate participants
dat12 %>% count(id) %>%filter(n > 1)

# Check age
range(dat12$age,na.rm = TRUE)

# Check BMI
range(dat12$bmi,na.rm = TRUE)

# Check SBP
range(dat12$sbp,na.rm = TRUE)

# Frequency distributions
table(dat12$sex)
table(dat12$smoking)
table(dat12$diabetes)
table(dat12$hypertension)
table(dat12$treatment)

#Interpretation
Data cleaning is not a hypothesis test. The objective is to identify:
  
Missing observations.
Duplicate records.
Impossible values.
Incorrect variable types.
Outliers.
Coding errors.
For example, an adult dataset containing an age of −5 or an SBP of 900 should be investigated before analysis.

#Descriptive statistics
Mean and SD
mean(dat$age)
sd(dat$age)

mean(dat$sbp)
sd(dat$sbp)

#Interpretation
The mean describes the central tendency, while the SD describes variability.

Example:
  
  Mean age = 58.2 ± 11.7 years.

This means participants had an average age of 58.2 years with an SD of 11.7 years.

Median and IQR
median(dat$crp)
IQR(dat$crp)

#Interpretation
The median represents the middle observation, while the IQR contains the middle 50% of observations.

Example:
  
  Median CRP was 2.7 mg/L (IQR 1.4–5.4).

This is usually preferable for strongly right-skewed biomarkers such as CRP.

Categorical variables
table(dat$diabetes)

prop.table(table(dat$diabetes)) * 100

Interpretation
If 250 of 1,000 participants have diabetes:
  
  Diabetes was present in 250/1,000 participants (25.0%).

Categorical variables are generally reported as n (%).

# Publication-quality Table 1
table1 <- dat12 %>%
  select(treatment,age,sex,bmi,smoking,diabetes,hypertension,sbp,dbp,cholesterol,ldl,hba1c) %>%
  tbl_summary(by = treatment,
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)"),missing = "no") %>%
  add_overall() %>%
  add_p()

table1

#Interpretation
Table 1 describes the study population and compares baseline characteristics between groups.

The P-values indicate whether there is statistical evidence of differences between groups.

In randomized trials, baseline P-values are generally not the main criterion for judging whether randomization worked. 
Descriptive balance and standardized differences are often more informative.

6. Normality assessment
hist(dat$sbp)

qqnorm(dat$sbp)
qqline(dat$sbp)

shapiro.test(dat$sbp)

Statistical hypotheses
H₀: data are consistent with a normal distribution.
H₁: data are not consistent with a normal distribution.
Interpretation
If:
  
  P > 0.05

there is insufficient evidence of non-normality.

If:
  
  P < 0.05

there is evidence of departure from normality.

Important: With large samples, do not base the entire analysis on Shapiro-Wilk.
Examine the histogram and Q-Q plot and consider whether the statistical method is robust to modest non-normality.

# Independent t-test
Research question
Does mean SBP differ between the intervention and control groups?
  
t_test_result <- t.test(sbp ~ treatment,data = dat)

t_test_result

Hypotheses
H₀: μ₁ = μ₂
H₁: μ₁ ≠ μ₂
Interpretation
If P < 0.05:
  
There is evidence that mean SBP differs between the two treatment groups.

If P ≥ 0.05:
  
There is insufficient evidence of a difference in mean SBP.

Do not report only the P-value. Report the mean difference and 95% CI.

Example:
  
Mean SBP was 129 mmHg in the intervention group and 134 mmHg in the control group, 
with an adjusted/unadjusted mean difference of −5 mmHg (95% CI −8 to −2; P = 0.002).

#Effect size — Cohens d
dat %>%
  cohens_d(sbp ~ treatment)

Interpretation
Cohens d describes the difference in means in SD units.

Rough guidelines:
  
0.2 = small
0.5 = moderate
0.8 = large
A statistically significant result does not necessarily mean the effect is clinically important.

# Paired t-test
Research question
Did participants SBP change between baseline and 12 months?

dat <- dat %>%
  mutate(sbp_baseline = sbp + rnorm(n, 5, 8),sbp_12m =sbp_baseline - 4 * (treatment == "Intervention") + rnorm(n, 0, 8))

paired_t <- t.test(dat$sbp_baseline,dat$sbp_12m,paired = TRUE)

paired_t

#Interpretation
If P < 0.05:

There is evidence of a statistically significant mean change in SBP between baseline and 12 months.

Report:

Mean SBP decreased by X mmHg (95% CI X to X; P = X).

# Mann–Whitney U test
wilcox.test(crp ~ treatment,data = dat)

Interpretation
This tests whether the distributions of CRP differ between two independent groups.

If P < 0.05:

There is evidence that the distribution of CRP differs between the groups.

For a skewed biomarker, report median and IQR.

#Wilcoxon signed-rank test
wilcox.test(dat$sbp_baseline, dat$sbp_12m,paired = TRUE)

#Interpretation
This is the non-parametric counterpart to the paired t-test.

If P < 0.05:

There is evidence of systematic change in SBP between baseline and follow-up.

#One-way ANOVA
Research question
Does mean SBP differ between never, former, and current smokers?

anova_model <- aov(sbp ~ smoking,data = dat)

summary(anova_model)

Hypotheses
H₀: all group means are equal.
H₁: at least one mean differs.
Interpretation
If P < 0.05:

Mean SBP differs across at least one smoking group.

ANOVA does not identify which groups differ.

Use:

TukeyHSD(anova_model)

to identify pairwise differences.

# Kruskal–Wallis test
kruskal.test(crp ~ smoking,data = dat)

Interpretation
This is a non-parametric alternative to one-way ANOVA.

If P < 0.05:

The distribution of CRP differs among at least one of the smoking categories.

#Post-hoc analysis:

dat %>%
  dunn_test(crp ~ smoking,p.adjust.method = "holm")

The adjusted P-values indicate which pairs differ.

#Chi-square test
Research question
Is diabetes associated with treatment group?

tab <- table(dat$diabetes,dat$treatment)

chisq.test(tab)

Hypotheses
H₀: the categorical variables are independent.
H₁: the variables are associated.
Interpretation
If P < 0.05:

There is evidence of an association between diabetes status and treatment group.

Remember: an association does not establish causality.

# Fisher's exact test
fisher.test(tab)

Interpretation
Fishers exact test is appropriate when expected cell counts are small.

If P < 0.05:

There is evidence of an association between the categorical variables.

#Pearson correlation
cor.test(dat$age,dat$sbp,method = "pearson")

Interpretation
The Pearson correlation coefficient r ranges from −1 to +1.

For example:

r = 0.42
P < 0.001

means:

Age had a moderate positive linear association with SBP.

A correlation of 0 does not necessarily mean there is no relationship; there may be a nonlinear relationship.

#Spearman correlation
cor.test(dat$crp,dat$hba1c,method = "spearman")

Interpretation
Spearmans ρ measures the strength of a monotonic relationship.

Example:
  
CRP was positively associated with HbA1c (Spearmans ρ = 0.31, P < 0.001).

Correlation should not be interpreted as evidence of causality.

#Simple linear regression
lm_simple <- lm(sbp ~ age,data = dat)

summary(lm_simple)

tidy(lm_simple, conf.int = TRUE)

Interpretation
Suppose:

β = 0.65
95% CI = 0.50–0.80
P < 0.001

Then:

Each additional year of age was associated with a 0.65-mmHg higher SBP (95% CI 0.50–0.80; P < 0.001).

The coefficient represents the expected change in the outcome for a one-unit increase in the predictor.

# Multiple linear regression
lm_multiple <- lm(sbp ~age +sex +bmi +smoking +diabetes +hypertension +treatment,data = dat)

summary(lm_multiple)

tidy(lm_multiple, conf.int = TRUE)

Interpretation
Each coefficient represents an association adjusted for the other variables in the model.

Example:

After adjustment for age, sex, BMI, smoking, diabetes, and hypertension, 
intervention treatment was associated with a 4.2-mmHg lower SBP (95% CI −6.1 to −2.3; P < 0.001).

20. Linear-regression diagnostics
par(mfrow = c(2, 2))
plot(lm_multiple)
par(mfrow = c(1, 1))

#Check:

Linearity
Homoscedasticity
Residual distribution
Influential observations
Multicollinearity
vif(lm_multiple)

Rough guide:

VIF ≈ 1: little concern
VIF > 5: potentially concerning
VIF > 10: substantial concern
# Breusch–Pagan test
bptest(lm_multiple)

Hypotheses
H₀: constant residual variance.
H₁: non-constant residual variance.
If P < 0.05:

There is evidence of heteroscedasticity.

Use robust standard errors:

coeftest(lm_multiple,vcov = vcovHC(lm_multiple,type = "HC3"))

# Logistic regression
Research question
Which factors are associated with cardiovascular events?

logit_model <- glm(cv_event ~age +sex +bmi +smoking +diabetes +hypertension +treatment,data = dat,family = binomial)

summary(logit_model)

Convert coefficients to odds ratios:

logit_results <- tidy(logit_model,exponentiate = TRUE,conf.int = TRUE)

logit_results

Interpretation
Suppose:

Treatment OR = 0.62
95% CI = 0.43–0.89
P = 0.009

Then:

After adjustment for the other covariates, intervention treatment was associated with 
38% lower odds of cardiovascular events compared with control (adjusted OR 0.62, 95% CI 0.43–0.89; P = 0.009).

Because:

1 − 0.62 = 0.38

Important
An OR of 0.62 does not necessarily mean cardiovascular risk was 38% lower. It means the odds were 38% lower.

23. Publication-ready logistic regression
tbl_regression(logit_model,exponentiate = TRUE)

#Interpretation:

OR > 1 → higher odds
OR < 1 → lower odds
OR = 1 → no association
If the 95% CI includes 1, the association is statistically compatible with no association at the conventional 5% level.

24. Predicted probabilities
dat <- dat %>%
  mutate(predicted_probability =predict(logit_model,type = "response"))

head(
  dat %>%
    select(id,cv_event,predicted_probability))

Interpretation
The predicted probability is the model-estimated probability of cardiovascular events for each participant.

For example:

Predicted probability = 0.23

means:

The model estimates approximately a 23% probability of the outcome for that participant, 
conditional on the variables included in the model.

# ROC curve and AUC
roc_object <- roc(dat$cv_event,dat$predicted_probability)

plot(roc_object,main = "ROC Curve",col = "blue")

auc(roc_object)

Interpretation
AUC measures discrimination:

#AUC	General interpretation
0.50	No discrimination
0.60–0.70	Poor
0.70–0.80	Acceptable
0.80–0.90	Good
>0.90	Excellent

For example:

The model demonstrated good discrimination (AUC = 0.82, 95% CI 0.78–0.86).

AUC does not tell you whether predicted probabilities are well calibrated.

# Sensitivity, specificity, PPV and NPV
coords(roc_object,"best",
       ret = c("threshold","sensitivity","specificity","ppv","npv"))

#Interpretation
Sensitivity:

Among participants who truly have the outcome, the proportion correctly identified.

Specificity:

Among participants without the outcome, the proportion correctly classified as negative.

PPV:

Among those testing positive, the proportion who actually have the outcome.

NPV:

Among those testing negative, the proportion who do not have the outcome.

PPV and NPV depend strongly on outcome prevalence.

# Hosmer–Lemeshow test
hoslem.test(dat$cv_event,fitted(logit_model),g = 10)

Interpretation
This evaluates agreement between predicted and observed event frequencies.

P > 0.05 → no strong evidence of poor calibration.
P < 0.05 → evidence of possible calibration problems.
Do not interpret P > 0.05 as proof of perfect calibration. Calibration plots are also recommended.

# Poisson regression
For count outcomes:

dat <- dat %>%
  mutate(
    hospital_visits = rpois(n,exp(-0.8 +0.02 * age +0.25 * (diabetes == "Yes") +0.20 * (hypertension == "Yes"))))

poisson_model <- glm(hospital_visits ~age +sex +diabetes +hypertension +treatment,data = dat,family = poisson)

tidy(poisson_model,exponentiate = TRUE,conf.int = TRUE)

Interpretation
Exponentiated coefficients are interpreted as incidence rate ratios (IRRs).

For example:

IRR = 1.30

means:

The expected rate of hospital visits was 30% higher in the exposed group, holding other variables constant.

# Poisson overdispersion
dispersion <- sum(residuals(poisson_model,type = "pearson")^2)/df.residual(poisson_model)

dispersion
#
Interpretation
A dispersion statistic substantially greater than 1 suggests overdispersion.

If there is substantial overdispersion, ordinary Poisson regression may underestimate standard errors.

Consider negative binomial regression.

# Negative binomial regression
nb_model <- glm.nb(hospital_visits ~age +sex +diabetes +hypertension +treatment,data = dat)

tidy(nb_model,exponentiate = TRUE,conf.int = TRUE)

Interpretation
The exponentiated coefficient is an IRR.

For example:

An IRR of 1.45 indicates that the expected hospitalization rate was approximately 45% higher in the exposed group, 
after adjustment.

# Kaplan–Meier survival analysis
survival_object <- Surv(dat$followup_months,dat$death)

km_model <- survfit(survival_object ~ treatment,data = dat)

ggsurvplot(km_model,data = dat,risk.table = TRUE,pval = TRUE,conf.int = TRUE,
           xlab = "Follow-up (months)",
           ylab = "Survival probability")

#Interpretation
The Kaplan–Meier curve estimates the probability of remaining alive/event-free over time.

The risk table shows how many participants remain under observation at different time points.

# Log-rank test
survdiff(Surv(followup_months,death) ~ treatment,data = dat)

Interpretation
H₀: survival curves are equal.
H₁: survival curves differ.
If P < 0.05:

There is evidence that survival differs between treatment groups.

The log-rank test is primarily an unadjusted comparison.

#Cox proportional-hazards regression
cox_model <- coxph(Surv(followup_months,death) ~treatment +age +sex +bmi +smoking +diabetes +hypertension,data = dat)

summary(cox_model)

#Hazard ratios:

tidy(cox_model,exponentiate = TRUE,conf.int = TRUE)

Interpretation
Suppose:

HR = 0.70
95% CI = 0.52–0.94

Then:

After adjustment for the covariates, the intervention group had an estimated 30% 
lower hazard of death than the control group.

Because:

1 − 0.70 = 0.30

Do not describe an HR as simply "30% lower risk." It represents a relative hazard under the proportional-hazards model.

#Test proportional hazards
ph_test <- cox.zph(cox_model)
ph_test

plot(ph_test)

Interpretation
H₀: proportional-hazards assumption holds.
P < 0.05: evidence of violation.
P ≥ 0.05: no strong evidence of violation.
If the assumption is violated, consider stratification or time-varying effects.

# ANCOVA
ancova_model <- lm(sbp_12m ~treatment +sbp_baseline +age +sex,data = dat)
summary(ancova_model)

Interpretation
ANCOVA compares follow-up SBP between treatment groups while adjusting for baseline SBP and other prespecified covariates.

Example:

After adjustment for baseline SBP, age, and sex, 12-month SBP was 5.1 mmHg lower 
in the intervention group than in the control group (95% CI −7.4 to −2.8; P < 0.001).

This is often preferable to simply comparing change scores in randomized trials.

# Repeated-measures analysis
Convert to long format:

long_bp <- dat %>%
  select(id,treatment,age,sex,sbp_baseline,sbp_12m) %>%
  pivot_longer(cols = c(sbp_baseline,sbp_12m),
               names_to = "time",
               values_to = "sbp") %>%
  mutate(time = factor(time,
                       levels = c("sbp_baseline","sbp_12m"),
                       labels = c("Baseline","12 months")))

# Linear mixed-effects model
mixed_model <- lmer(sbp ~time * treatment +age +sex + (1 | id),data = long_bp)
summary(mixed_model)

#Interpretation
The most important coefficient is usually:

time:treatment

This evaluates whether the change over time differs between treatment groups.

If significant:

There is evidence that the trajectory of SBP over time differed between the intervention and control groups.

# Estimated marginal means
emmeans(mixed_model,~ time * treatment)

Pairwise comparisons:

pairs(emmeans(mixed_model,~ time * treatment))

# Interpretation
Estimated marginal means provide model-adjusted group means.

They are particularly useful for explaining interactions and repeated-measures analyses.

# Generalized estimating equations
gee_model <- geeglm(cv_event ~treatment +age +sex +diabetes,
                    id = id,
                    data = dat,
                    family = binomial,
                    corstr = "exchangeable")

tidy(gee_model,exponentiate = TRUE,conf.int = TRUE)

# Interpretation
GEE accounts for correlation among repeated measurements and provides population-average effects.

For example:

An OR of 0.70 indicates that, at the population level, treatment was associated with 30% lower odds of the outcome after adjustment.

# Interaction/effect modification
interaction_model <- glm(cv_event ~treatment *sex +age +bmi +smoking +diabetes +hypertension,data = dat,family = binomial)

tbl_regression(interaction_model,exponentiate = TRUE)

# Interpretation
The interaction asks:

Does the treatment effect differ between males and females?

A significant interaction term provides evidence that the treatment-outcome association varies by sex.

Do not conclude effect modification simply because one subgroup has P < 0.05 and 
another does not. The interaction term itself should be evaluated.

# Stratified analysis
dat %>%
  group_by(sex) %>%
  summarise(n = n(),
            events = sum(cv_event),
            risk = mean(cv_event))

# Interpretation
This describes outcome risk separately within strata.

Stratification can help identify:

Confounding.
Effect modification.
Differences in baseline risk.
# Confounding: crude vs adjusted model
crude_model <- glm(cv_event ~ treatment,data = dat,family = binomial)

adjusted_model <- glm(cv_event ~treatment +age +sex +bmi +smoking +diabetes +hypertension,data = dat,family = binomial)

tidy(crude_model,exponentiate = TRUE,conf.int = TRUE)

tidy(adjusted_model,exponentiate = TRUE,conf.int = TRUE)

Interpretation
Compare the crude and adjusted treatment estimates.

For example:

Crude OR = 0.75
Adjusted OR = 0.62

The change after adjustment suggests that the measured covariates affected the crude association.

A commonly used descriptive criterion is a substantial change in the exposure estimate after adjustment, 
but confounder selection should ideally be based on subject-matter knowledge and 
causal reasoning rather than an automated P-value rule.

#Propensity-score matching
ps_model <- matchit(treatment ~age +sex +bmi +smoking +diabetes +hypertension +cholesterol,
                    data = dat,
                    method = "nearest",
                    ratio = 1)

summary(ps_model)

matched_dat <- match.data(ps_model)

# Interpretation
Propensity-score matching attempts to create treatment groups with similar distributions of measured baseline characteristics.

After matching, examine covariate balance. The goal is balance, not simply a non-significant P-value.

# Treatment analysis:

matched_model <- glm(cv_event ~ treatment,data = matched_dat,family = binomial,weights = weights)

tbl_regression(matched_model,exponentiate = TRUE)

Interpret the OR as the treatment association in the matched population, subject to the assumptions of the matching procedure.

# Missing-data analysis
# Artificially create missing LDL values
set.seed(10)

dat_missing <- dat

dat_missing$ldl[sample(seq_len(nrow(dat_missing)),50)] <- NA

colSums(is.na(dat_missing))

Interpretation
First determine:

How much data are missing?
Which variables have missing data?
Are missing values related to observed characteristics?
Is missingness plausibly MCAR, MAR, or MNAR?
Avoid automatically performing complete-case analysis without considering its assumptions.

# Multiple imputation
################################################################################
#                            MULTIPLE IMPUTATION
################################################################################

imp <- mice(dat_missing %>%
              select(age,sex,bmi,smoking,diabetes,hypertension,treatment,sbp,cholesterol,ldl,hba1c,cv_event),
            m = 20,
            method = "pmm",
            seed = 2026)

summary(imp)

Fit model:

fit_imp <- with(imp,glm(cv_event ~treatment +age +sex +bmi +smoking +diabetes +hypertension +ldl,family = binomial))
pooled_results <- pool(fit_imp)

summary(pooled_results,exponentiate = TRUE,conf.int = TRUE)

#Interpretation
Multiple imputation accounts for uncertainty introduced by missing observations under its assumed missing-data mechanism.

The pooled estimates combine information across the imputed datasets.

#Bootstrap confidence intervals
boot_function <- function(data,indices) {d <- data[indices, ]

  mean(d$sbp[d$treatment == "Intervention"]) -
    mean(d$sbp[d$treatment == "Control"])}

boot_result <- boot(dat,statistic = boot_function,R = 2000)

boot.ci(boot_result,type = c("perc","bca"))

#Interpretation
Bootstrap CIs are estimated by repeatedly resampling the observed dataset.

For example:

The bootstrap-estimated mean difference was −5.4 mmHg (95% bootstrap CI −8.7 to −2.2).

A CI excluding zero provides evidence against a null difference of zero.

# Bland–Altman analysis
measurement_data <- dat %>% transmute(method_A = sbp_baseline,
                                      method_B = sbp_baseline +rnorm(n, 0, 5)) %>%
  mutate(mean_measurement = (method_A + method_B) / 2,difference = method_A - method_B)

mean_difference <- mean(measurement_data$difference)

sd_difference <-sd(measurement_data$difference)

upper_limit <- mean_difference + 1.96 * sd_difference

lower_limit <- mean_difference - 1.96 * sd_difference

#Plot:
ggplot(measurement_data,aes(x = mean_measurement,y = difference)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = mean_difference,color = "blue") +
  geom_hline(yintercept = c(upper_limit,lower_limit),linetype = "dashed",color = "red") +
  theme_minimal()

# Interpretation
Bland–Altman assesses agreement, not merely correlation.

Report:

Mean bias.
Upper limit of agreement.
Lower limit of agreement.
Whether the limits are acceptable should be determined using clinically meaningful predefined limits.

# Multiple-comparison adjustment
p_values <- c(0.001,0.014,0.032,0.047,0.20,0.003)

data.frame(raw_p = p_values,
           bonferroni = p.adjust(p_values,"bonferroni"),
           holm = p.adjust(p_values,"holm"),
           FDR =p.adjust(p_values,"BH"))

# Interpretation
Multiple testing increases the probability of false-positive findings.

For example:

Raw P = 0.03
Adjusted P = 0.12

means the result is no longer statistically significant after adjustment using that method.

# Number needed to treat
risk_control <- mean(dat$cv_event[dat$treatment == "Control"])

risk_intervention <- mean(dat$cv_event[dat$treatment == "Intervention"])

absolute_risk_difference <-risk_intervention - risk_control

NNT <- 1 / abs(absolute_risk_difference)
NNT

Interpretation
If:

Absolute risk reduction = 0.05

then:

NNT = 1 / 0.05 = 20

# Interpretation:

Approximately 20 patients would need to receive the intervention, rather than control, 
to prevent one additional event over the study period.

NNT must always be accompanied by the time horizon and should ideally have a confidence interval.

# Risk difference
risk_control
risk_intervention

risk_intervention - risk_control

Interpretation
Suppose:

Control risk = 20%
Intervention risk = 15%

Then:

Risk difference = −5 percentage points

# Interpretation:

The intervention was associated with an absolute 5-percentage-point reduction in cardiovascular events.

Absolute effects are often more clinically interpretable than relative effects alone.

# Relative risk reduction
RR <- risk_intervention /risk_control

RRR <- 1 - RR

RR
RRR

If:

RR = 0.75

then:

RRR = 25%

# Interpretation:

The intervention was associated with a 25% relative reduction in risk compared with control.

Always report the absolute risk difference as well when possible.

# Publication-ready forest plot
logit_results %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = estimate,y = reorder(term,estimate))) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),height = 0.2) +
  geom_vline(xintercept = 1,linetype = "dashed") +
  scale_x_log10() +
  labs(x = "Odds Ratio",y = NULL) +
  theme_minimal()

Interpretation
For an OR forest plot:

Point estimate left of 1 → lower odds.
Point estimate right of 1 → higher odds.
CI crossing 1 → statistically compatible with no association.
Narrow CI → greater precision.
Wide CI → greater uncertainty.

# Master statistical-test decision guide
Question	Analysis	Main effect measure	Null value
Describe continuous variable	Mean/SD	Mean	—
Skewed continuous variable	Median/IQR	Median	—
Compare 2 independent means	t-test	Mean difference	0
Compare 2 independent non-normal groups	Mann–Whitney	Distribution/rank difference	—
Compare paired means	Paired t-test	Mean change	0
Compare paired non-normal data	Wilcoxon signed-rank	Rank/change	0
Compare ≥3 means	ANOVA	Mean differences	0
Compare ≥3 non-normal groups	Kruskal–Wallis	Rank distributions	—
Two categorical variables	Chi-square	Association	—
Small categorical counts	Fisher exact	Association	—
Two continuous variables	Pearson correlation	r	0
Non-normal/monotonic relationship	Spearman	ρ	0
Continuous outcome	Linear regression	β	0
Binary outcome	Logistic regression	OR	1
Cohort risk	Risk analysis	RR/RD	RR=1/RD=0
Count outcome	Poisson regression	IRR	1
Overdispersed count	Negative binomial	IRR	1
Time-to-event	Kaplan–Meier	Survival probability	—
Compare survival curves	Log-rank	Survival distribution	—
Adjusted time-to-event	Cox regression	HR	1
Repeated continuous outcome	Mixed model	β	0
Repeated binary outcome	GEE	OR/RR depending model	1
Diagnostic test	ROC	AUC	0.50
Agreement of methods	Bland–Altman	Bias/LoA	0
Observational treatment effect	Propensity methods	OR/RR/HR/etc.	Depends
Many comparisons	Multiplicity correction	Adjusted P	—


# How to interpret statistical results correctly
For medical and public-health research, use this hierarchy:

A. Start with the effect estimate
Examples:

Mean difference = −5.2 mmHg
OR = 0.62
RR = 0.75
HR = 0.70
IRR = 1.40
β = −3.5
r = 0.42

B. Give the 95% CI
The confidence interval communicates uncertainty around the estimate.

C. Give the P-value
Use it as evidence against the null hypothesis, not as a measure of effect size.

D. Determine statistical significance
For a conventional two-sided α = 0.05:

P < 0.05 → statistically significant
P ≥ 0.05 → not statistically significant

But avoid saying:

"P ≥ 0.05 proves there is no effect."

It means the data did not provide sufficient evidence to reject the specified null hypothesis.

E. Assess clinical/public-health importance
Ask:

Is the magnitude large enough to matter to patients or populations?

F. Consider study design
A randomized trial can potentially support causal conclusions.

An observational study generally supports statements about association, 
unless strongerThe prediction model demonstrated good discrimination, 
with an AUC of 0.82 causal assumptions and methods justify causal interpretation.

# Recommended manuscript wording
Statistically significant result
The intervention was associated with a statistically significant reduction in systolic blood pressure compared with control
(mean difference −5.2 mmHg, 95% CI −8.1 to −2.3; P < 0.001)."

Non-significant result
"There was insufficient evidence of a difference in systolic blood pressure between treatment groups 
(mean difference −2.1 mmHg, 95% CI −5.8 to 1.6; P = 0.26)."

Logistic regression
"After adjustment for age, sex, BMI, smoking, diabetes, and hypertension, intervention treatment was associated with 
lower odds of cardiovascular events (adjusted OR 0.62, 95% CI 0.43–0.89; P = 0.009)."

Cox regression
"After multivariable adjustment, intervention treatment was associated with a lower hazard of death 
(adjusted HR 0.70, 95% CI 0.52–0.94)."

Correlation
"Age was moderately positively correlated with systolic blood pressure (Pearsons r = 0.42, P < 0.001)."

Diagnostic test
"The prediction model demonstrated good discrimination, with an AUC of 0.82 (95% CI 0.78–0.86)."

The key rule for interpreting P-values
Avoid:
"P = 0.03, therefore the treatment works.

Prefer:
The intervention was associated with a 5.2-mmHg lower mean SBP (95% CI −8.1 to −2.3; P = 0.03).

The second statement tells the reader:

What changed.
By how much.
How precise the estimate is.
How compatible the result is with the null hypothesis.
That is the standard you should aim for throughout a clinical or epidemiological analysis.

# Suggested full library structure
For a serious reusable biostatistics project, I recommend splitting the above into:

R_Biostatistics_Library/
│
├── 00_setup.R
├── 01_simulate_or_import_data.R
├── 02_data_cleaning.R
├── 03_descriptive_statistics.R
├── 04_table1.R
├── 05_normality_assumptions.R
│
├── 06_t_tests.R
├── 07_nonparametric_tests.R
├── 08_anova.R
├── 09_chi_square_fisher.R
├── 10_correlation.R
│
├── 11_linear_regression.R
├── 12_logistic_regression.R
├── 13_poisson_negative_binomial.R
│
├── 14_survival_kaplan_meier.R
├── 15_cox_regression.R
│
├── 16_ancova.R
├── 17_repeated_measures.R
├── 18_mixed_models.R
├── 19_GEE.R
│
├── 20_epidemiological_measures.R
├── 21_confounding.R
├── 22_interaction_effect_modification.R
├── 23_propensity_scores.R
│
├── 24_missing_data.R
├── 25_multiple_imputation.R
├── 26_bootstrap.R
├── 27_multiple_testing.R
│
├── 28_diagnostic_accuracy.R
├── 29_bland_altman.R
├── 30_sensitivity_analysis.R
│
├── 31_publication_tables.R
├── 32_publication_figures.R
└── 33_results_interpretation.R

This gives you a full medical biostatistics framework, rather than merely a list of R commands. 
The important distinction is that every test should answer a defined research question and
be accompanied by its assumptions → effect estimate → 95% CI → P-value → statistical interpretation → clinical interpretation.
