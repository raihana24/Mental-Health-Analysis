library(readxl)
data <- read_excel(file.choose())
head(data)
summary(data)
str(data)

#to fit latent variable for construct sem
library(lavaan)
#for cronbach alpha
library(psych)

# display top few rows of data
head(data)

# Calculate Cronbach's alpha for the mental health factors
alpha(data[, c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9")])

data$job <- as.factor(data$job)
data$accommodation <- as.factor(data$accommodation)
data$study <- as.factor(data$study)
data$gadgets <- as.factor(data$gadgets)
data$social_media <- as.factor(data$social_media)

# Menggabungkan kategori dalam variabel 'job'
data$job <- ifelse(data$job %in% c("Full time", "Part time"), "Yes", data$job)
data$job <- ifelse(data$job == "No", "No", data$job)

# Menggabungkan kategori dalam variabel 'accommodation'
data$accommodation <- ifelse(data$accommodation %in% c("Home (with parents)", "University hall of residence"), "Home_Uni", data$accommodation)
data$accommodation <- ifelse(data$accommodation == "Private rented accommodation", "Other", data$accommodation)

# Menggabungkan kategori dalam variabel 'study'
data$study <- ifelse(data$study %in% c("1 - 2 hours", "2 - 4 hours"), "1-4 hours", data$study)
data$study <- ifelse(data$study == "More than 4 hours", "More than 4 hours", data$study)

# Menggabungkan kategori dalam variabel 'gadgets'
data$gadgets <- ifelse(data$gadgets %in% c("1 - 3", "4 - 6"), "1-6", data$gadgets)
data$gadgets <- ifelse(data$gadgets == "More than 6", "More than 6", data$gadgets)

# Menggabungkan kategori dalam variabel 'social_media'
data$social_media <- ifelse(data$social_media %in% c("1 - 2 Hours", "2 - 4 Hours"), "1-4 Hours", data$social_media)
data$social_media <- ifelse(data$social_media == "More than 4 Hours", "More than 4 Hours", data$social_media)

table(data$job)
table(data$accommodation)
table(data$study)
table(data$gadgets)
table(data$social_media)

# Check structure after combining
str(data)

# Define the CFA model
cfa_model <- '
  mental_health =~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9
'

# Fit the CFA model
cfa_fit <- cfa(cfa_model, data = data)

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
fit <- sem(SEMmodel, data = data)
summary(fit, fit.measures = TRUE, standardized = TRUE)

# Check fit measures
fitMeasures(fit, c("gfi", "cfi", "tli", "rmsea", "srmr"))

# Goodness-of-fit indices
fitMeasures(fit)

library(semPlot)
semPaths(fit, what = "est", whatLabels = "est", layout = "tree", edge.label.cex = 0.8, sizeMan = 10, sizeLat = 12)
#atau
# Plotting the SEM path diagram
semPaths(fit, whatLabels = "est", edge.label.cex = 0.8, fade = FALSE)
