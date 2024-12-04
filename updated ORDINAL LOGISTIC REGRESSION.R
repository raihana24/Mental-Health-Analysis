# Prepare data
library(readxl)
my_fyp <- read_excel(file.choose())
head(my_fyp)
summary(my_fyp)
str(my_fyp)

# Check for missing values
sum(is.na(my_fyp))  # Check for missing values

# Categorize GPA
summary(my_fyp$GPA)
my_fyp$GPA_Cat <- cut(my_fyp$GPA,
                      breaks = c(1.00, 2.33, 3.33, 4.00),
                      labels = c("Lulus", "Kepujian", "Cemerlang"),
                      include.lowest = TRUE)

#frequency table for GPA categories
GPA_table <- table(my_fyp$GPA_Cat)
GPA_table

##DO RFE FIRST##
# Recursive Feature Elimination - CHOOSING THE SELECTED VARIABLES
library(caret)

# 1. Tentukan pemboleh ubah input (predictors) dan output (target)
predictors_olr <- data[, c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9")]
target_olr <- my_fyp$GPA

# 2. Menentukan kontrol untuk RFE
control_olr <- rfeControl(functions = lmFuncs, method = "cv", number = 10)  # 10-fold cross-validation

# 3. Jalankan RFE untuk memilih pemboleh ubah terbaik
rfe_results2 <- rfe(predictors_olr, target_olr, sizes = c(3:9), rfeControl = control_olr)
# Menggunakan 'sizes' yang lebih besar (contoh: 3 hingga 9) untuk memilih lebih banyak pemboleh ubah jika perlu

# 4. Lihat hasil RFE untuk mengetahui pemboleh ubah yang dipilih
rfe_results2

# 5. Dapatkan pemboleh ubah terpilih
selected_predictors_olr <- predictors[, predictors(rfe_results2)]
selected_predictors_olr

# Jalankan Ordinal Logistic Regression (OLR)
olr_model <- polr(GPA ~ ., data = data.frame(selected_predictors_olr, GPA = my_fyp$GPA_Cat), Hess = TRUE)
summary(olr_model)

### Calculate Odds Ratios and Confidence Intervals

# Extract the coefficients and standard errors
coefficients <- summary(olr_model)$coefficients[, 1]  # Coefficients
std_errors <- summary(olr_model)$coefficients[, 2]   # Standard errors

# Calculate the odds ratios (OR) by exponentiating the coefficients
odds_ratios <- exp(coefficients)

# Calculate the 95% Confidence Intervals (CIs)
CI_lower <- exp(coefficients - 1.96 * std_errors)
CI_upper <- exp(coefficients + 1.96 * std_errors)

# Combine the results into a data frame for easier reading
results <- data.frame(
  Predictor = names(coefficients),
  Odds_Ratio = odds_ratios,
  CI_Lower = CI_lower,
  CI_Upper = CI_upper
)

# Display the results
results

### Performing inferences by comparing full and reduced models 

# Ensure GPA is a factor
my_fyp$GPA_Cat <- factor(my_fyp$GPA_Cat, levels = c("Lulus", "Kepujian", "Cemerlang"))

# Fit the reduced and full models
full_model <- polr(GPA_Cat ~ X6 + X2 + X9 + X7 + X4 + X5, data = my_fyp, Hess = TRUE)
reduced_model <- polr(GPA_Cat ~ X6 + X2, data = my_fyp, Hess = TRUE)

# Get AIC, logLik, and perform likelihood ratio test
reduced_aic <- AIC(reduced_model)
full_aic <- AIC(full_model)

reduced_logLik <- logLik(reduced_model)
full_logLik <- logLik(full_model)

# Likelihood Ratio Test
lr_stat <- 2 * (full_logLik - reduced_logLik)  # Likelihood ratio statistic
df <- length(coef(full_model)) - length(coef(reduced_model))  # Degrees of freedom
p_value <- pchisq(lr_stat, df, lower.tail = FALSE)  # p-value for the test

# Create an Inferences Table
inferences_table <- data.frame(
  Model = c("reduced_model", "full_model"),
  no.par = c(length(coef(reduced_model)), length(coef(full_model))),
  AIC = c(reduced_aic, full_aic),
  logLik = c(reduced_logLik, full_logLik),
  LR.stat = c(NA, lr_stat),  # LR statistic only for full model comparison
  df = c(NA, df),
  Pr.Chisq = c(NA, p_value)
)

# Display the table
inferences_table
