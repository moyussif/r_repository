

1. Simple Linear Regression
# Example data
data <- mtcars

# Predict MPG from horsepower
model_lm <- lm(mpg ~ hp, data = data)

summary(model_lm)

# Predictions
predict(model_lm)

# Plot
plot(data$hp, data$mpg,
     main = "Simple Linear Regression",
     xlab = "Horsepower",
     ylab = "MPG")

abline(model_lm, col = "red", lwd = 2)



y
^
  =
  b
0
+
  b
1
x
2. Multiple Linear Regression
model_multiple <- lm(mpg ~ hp + wt + cyl, data = mtcars)

summary(model_multiple)

# Predictions
pred <- predict(model_multiple)

head(pred)

3. Polynomial Regression
model_poly <- lm(mpg ~ hp + I(hp^2), data = mtcars)

summary(model_poly)

# Plot
plot(mtcars$hp, mtcars$mpg)

hp_seq <- seq(min(mtcars$hp), max(mtcars$hp), length.out = 100)

pred_poly <- predict(
  model_poly,
  newdata = data.frame(hp = hp_seq)
)

lines(hp_seq, pred_poly, col = "blue", lwd = 2)

4. Logistic Regression
Use this when the dependent variable is binary, such as 0/1, Yes/No, disease/no disease.

# Create binary outcome
mtcars$high_mpg <- ifelse(mtcars$mpg > 20, 1, 0)

model_logistic <- glm(
  high_mpg ~ hp + wt,
  data = mtcars,
  family = binomial
)

summary(model_logistic)

# Probabilities
probabilities <- predict(
  model_logistic,
  type = "response"
)

head(probabilities)

# Convert probabilities to classes
predicted_class <- ifelse(probabilities >= 0.5, 1, 0)

table(
  Actual = mtcars$high_mpg,
  Predicted = predicted_class
)

5. Multinomial Logistic Regression
For a dependent variable with more than two unordered categories.

install.packages("nnet")
library(nnet)

# Example categorical outcome
mtcars$category <- factor(
  sample(c("Low", "Medium", "High"),
         nrow(mtcars),
         replace = TRUE)
)

model_multinom <- multinom(
  category ~ hp + wt + cyl,
  data = mtcars
)

summary(model_multinom)

# Predicted categories
predict(model_multinom)

6. Ordinal Logistic Regression
For ordered categories such as:
  
  Poor < Average < Good < Excellent

install.packages("MASS")
library(MASS)

mtcars$rating <- ordered(
  sample(c("Poor", "Average", "Good"),
         nrow(mtcars),
         replace = TRUE),
  levels = c("Poor", "Average", "Good")
)

model_ordinal <- polr(
  rating ~ hp + wt + cyl,
  data = mtcars,
  Hess = TRUE
)

summary(model_ordinal)

# Predictions
predict(model_ordinal)

7. Poisson Regression
Useful when the dependent variable is a count, such as number of visits, accidents, calls, etc.

model_poisson <- glm(
  carb ~ hp + wt,
  data = mtcars,
  family = poisson
)

summary(model_poisson)

# Predictions
predict(model_poisson, type = "response")

8. Negative Binomial Regression
Useful for count data when overdispersion makes ordinary Poisson regression inappropriate.

install.packages("MASS")
library(MASS)

model_nb <- glm.nb(
  carb ~ hp + wt,
  data = mtcars
)

summary(model_nb)

predict(model_nb, type = "response")

9. Ridge Regression
Ridge regression applies L2 regularization.

install.packages("glmnet")
library(glmnet)

x <- model.matrix(mpg ~ hp + wt + cyl + disp + drat,
                  mtcars)[, -1]

y <- mtcars$mpg

ridge_model <- glmnet(
  x,
  y,
  alpha = 0
)

plot(ridge_model)

# Cross-validation
cv_ridge <- cv.glmnet(
  x,
  y,
  alpha = 0
)

plot(cv_ridge)

# Best lambda
cv_ridge$lambda.min

# Predictions
predict(
  cv_ridge,
  newx = x,
  s = "lambda.min"
)

10. Lasso Regression
Lasso uses L1 regularization and can shrink some coefficients to exactly zero.

lasso_model <- glmnet(
  x,
  y,
  alpha = 1
)

plot(lasso_model)

cv_lasso <- cv.glmnet(
  x,
  y,
  alpha = 1
)

plot(cv_lasso)

cv_lasso$lambda.min

coef(
  cv_lasso,
  s = "lambda.min"
)

11. Elastic Net Regression
Combines Ridge and Lasso.

elastic_model <- glmnet(
  x,
  y,
  alpha = 0.5
)

cv_elastic <- cv.glmnet(
  x,
  y,
  alpha = 0.5
)

plot(cv_elastic)

coef(
  cv_elastic,
  s = "lambda.min"
)

12. Robust Regression
Useful when your data contains outliers that can strongly influence ordinary least squares.

install.packages("MASS")
library(MASS)

model_robust <- rlm(
  mpg ~ hp + wt + cyl,
  data = mtcars
)

summary(model_robust)

predict(model_robust)

13. Quantile Regression
Instead of predicting the conditional mean, quantile regression can model the median or another quantile.

install.packages("quantreg")
library(quantreg)

# Median regression
model_quantile <- rq(
  mpg ~ hp + wt + cyl,
  data = mtcars,
  tau = 0.5
)

summary(model_quantile)

# 25th percentile
model_q25 <- rq(
  mpg ~ hp + wt + cyl,
  data = mtcars,
  tau = 0.25
)

summary(model_q25)

# 75th percentile
model_q75 <- rq(
  mpg ~ hp + wt + cyl,
  data = mtcars,
  tau = 0.75
)

summary(model_q75)

14. Stepwise Regression
Can be used for automated variable selection.

full_model <- lm(
  mpg ~ hp + wt + cyl + disp + drat + qsec + gear + carb,
  data = mtcars
)

step_model <- step(full_model)

summary(step_model)

15. Generalized Linear Regression
You can use glm() for several types of outcomes.

# Gaussian
model_gaussian <- glm(
  mpg ~ hp + wt,
  data = mtcars,
  family = gaussian
)

# Binomial
model_binomial <- glm(
  high_mpg ~ hp + wt,
  data = mtcars,
  family = binomial
)

# Poisson
model_poisson <- glm(
  carb ~ hp + wt,
  data = mtcars,
  family = poisson
)

#Quick guide
#Regression	               Dependent variable 
Simple Linear	             Continuous, 1 predictor
Multiple Linear	           Continuous, multiple predictors
Polynomial	               Continuous, nonlinear relationship
Logistic Binary            categorical 2
Multinomial Logistic     	 3+ unordered categories
Ordinal Logistic         	 Ordered categories
Poisson	                   Count
Negative Binomial	         Overdispersed count
Ridge	                     Continuous + multicollinearity
Lasso	                     Continuous + variable selection
Elastic Net                Combination of Ridge + Lasso
Robust Regression	         Continuous + outliers
Quantile Regression	       Conditional quantiles
Stepwise Regression	       Automated variable selection