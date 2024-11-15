#to fit latent variable for construct sem
library(lavaan)
#for cronbach alpha
library(psych)

# Calculate Cronbach's alpha for the mental health factors
alpha(my_fyp[, c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9")])

# Define the CFA model
cfa_model <- '
  mental_health =~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9
'

# Fit the CFA model
cfa_fit <- cfa(cfa_model, data = my_fyp)

# Check the CFA model fit
summary(cfa_fit, fit.measures = TRUE, standardized = TRUE)


# Define the SEM model
SEMmodel <- '
  # Measurement model
    # Mental health factors (X1 to X9)
    mental_health =~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9
  
  # Regressions
    # External factors affecting mental health
    mental_health ~ job + accommodation + study +  gadgets +  social_media
    
    # Mental health and external factors affecting CGPA
    GPA ~ mental_health + job + accommodation + study +  gadgets +  social_media
'

# Fit the model
fit <- sem(SEMmodel, data = my_fyp)

# Check the model fit
summary(fit, fit.measures = TRUE)

# Goodness-of-fit indices
fitMeasures(fit)

library(semPlot)
semPaths(fit, what = "est", whatLabels = "est", layout = "tree", edge.label.cex = 0.8, sizeMan = 10, sizeLat = 12)

# Plot the SEM model with customizations
semPaths(fit, 
         layout = "tree", 
         sizeLat = 8, 
         sizeMan = 8, 
         edge.label.cex = 1.2, 
         node.width = 1.5, 
         node.height = 1.5, 
         fontSize = 12, 
         mar = c(5, 5, 5, 5), 
         asize = 4, 
         residuals = TRUE, 
         variances = TRUE, 
         fade = FALSE, 
         edge.color = "darkgray", 
         nodeColor = c("lightblue", "lightgreen"))

#cubaan menggunakan estimator Maximum Likelihood Robust (MLR) untuk fix model sebelum yang ada NA:
fit_try <- sem(SEMmodel, data = my_fyp, estimator = "MLR")
summary(fit_try, fit.measures = TRUE)
