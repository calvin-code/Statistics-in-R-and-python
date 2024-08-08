library(dplyr)
library(car)
data <- read_excel("C:/Users/admin/Desktop/MAX R/equity.xlsx")
data <- data[data$value <= 10000, ]
full_model <- lm(value ~ debt + sales + income + assets + seo + debt * seo + sales * seo + income * seo + assets * seo, data = data)
# BIC for full model
bic_full_model <- BIC(full_model)
reduced_model <- step(full_model, direction = "backward")
# BIC for the reduced model
bic_reduced_model <- BIC(reduced_model)
summary(reduced_model)
cat("BIC for Full Model is:", bic_full_model, "\n")
cat("BIC for Reduced Model is:", bic_reduced_model, "\n")

library(wooldridge)
library(dplyr)
data("wage1")
model_a <- lm(log(wage) ~ married * female, data = wage1)
summary(model_a)

library(car)
model_b <- lm(log(wage) ~ married * female + educ + exper + I(exper^2) + tenure + I(tenure^2), data = wage1)
Anova(model_b, type = "III", test = "F")

library(car)
model_c <- lm(log(wage) ~ married * female + educ + exper + I(exper^2) + tenure + I(tenure^2), data = wage1)
test_result <- linearHypothesis(model_c, c("female"), test = "Chisq")
test_result

equity <- read_excel("C:/Users/admin/Desktop/MAX R/equity.xlsx")
model <- lm(value ~ debt + sales + income + assets + seo + debt*seo + sales*seo + income*seo + assets*seo, data = data)
summary(model)

library(aod)
seo <- gqtest(model, order.by = data$seo)
print(seo)
sales <- gqtest(model, order.by = data$sales)
print(sales)

library(lmtest)
b_p_test <- bptest(model)
print(b_p_test)

library(car)
weights <- 1 / data$sales
wls <- lm(value ~ debt + sales + income + assets + seo + debt * seo + sales * seo + income * seo + assets * seo, data = data, weights = weights)
white_test <- bptest(wls)
print(white_test)

original_residuals <- residuals(model)
modelb <- lm(log(original_residuals^2) ~ debt + sales + income + assets + seo, data = data)
summary(modelb)

weights <- 1 / exp(modelb$fitted.values)
w_model <- lm(value ~ debt + sales + income + assets + seo + debt * seo + sales * seo + income * seo + assets * seo, data = data, weights = weights)
summary(w_model)

library(sandwich)
library(lmtest)
w_se <- sqrt(diag(vcovHC(model, type = "HC0")))
result <- data.frame(coef = coef(model), w_se)
print(result)

library(lmtest)
library(aod)
library(car)
library(sandwich)
data <- as.data.frame(data)
data_cleaned <- subset(data, value <= 10000)
model_a <- lm(value ~ debt + sales + income + assets + seo + debt * seo + sales * seo + income * seo + assets * seo, data = data_cleaned)
print(summary(model_a))
#  Goldfeld-Quandt Test
gq_seo <- gqtest(model_a)
print(gq_seo)
#  Weighted Least Squares with weights as inverse of Sales
weights <- 1 / data_cleaned$sales
wls_model <- lm(value ~ debt + sales + income + assets + seo + debt * seo + sales * seo + income * seo + assets * seo, data = data_cleaned, weights = weights)
white_test <- bptest(wls_model)
print(white_test)
#  Estimate variances from the regression of log(residuals^2) on DEBT, SALES, INCOME, ASSETS, and SEO
auxiliary_model <- lm(log(residuals(model_a)^2) ~ debt + sales + income + assets + seo, data = data_cleaned)
print(summary(auxiliary_model))

