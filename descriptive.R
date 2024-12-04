library(readxl)
my_fyp <- read_excel(file.choose())
head(my_fyp)
summary(my_fyp)
str(my_fyp)

#GPA
summary(my_fyp$GPA)
my_fyp$GPA_Cat <- cut(my_fyp$GPA,
                         breaks = c(1.00, 2.33, 3.33, 4.00),
                         labels = c("Lulus", "Kepujian", "Cemerlang"),
                         include.lowest = TRUE)

#frequency table for GPA categories
GPA_table <- table(my_fyp$GPA_Cat)
GPA_table

#bar plot
barplot(GPA_table,
        main = "GPA Categories",
        xlab = "GPA Category",
        ylab = "Frequency",
        col = c("lightcoral", "lightblue", "lightgreen"),
        border = "black")

#frequency table for gender
gender_table <- table(my_fyp$Gender)
#barplot
barplot(gender_table,
        main = "Gender Categories",
        xlab = "Gender Category",
        ylab = "Frequency",
        col = c("pink", "blue"),
        border = "black")

#frequency table for age
age_table <- table(my_fyp$Age)
#barplot
barplot(age_table,
        main = "Age Categories",
        xlab = "Age Category",
        ylab = "Frequency",
        col = c("green", "blue", "purple"),
        border = "black")

#frequency table for edu
edu_table <- table(my_fyp$Educational_level)
#barplot
barplot(edu_table,
        main = "Educational Level Categories",
        xlab = "Edu Category",
        ylab = "Frequency",
        col = c("yellow", "brown", "orange"),
        border = "black")

# Create frequency tables for each mental health predictor (X1 to X9)
for (i in c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9")) {
  cat("\nPrevalence for", i, ":\n")
  print(table(my_fyp[[i]]))  # Replace 'my_fyp' with your actual dataset name
}

# Calculate prevalence for each predictor
for (i in c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9")) {
  cat("\nPrevalence for", i, ":\n")
  prevalence <- prop.table(table(my_fyp[[i]]))  # Proportions for each category
  print(prevalence)
}

# Load the ggplot2 package
library(ggplot2)

# Prepare a data frame with the prevalence counts
prevalence_data <- data.frame(
  Predictor = rep(c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9"), each = 4),
  Category = rep(1:4, times = 9),
  Count = c(55, 147, 108, 42,    # X1
            100, 120, 81, 51,    # X2
            58, 88, 79, 127,     # X3
            36, 99, 98, 119,     # X4
            74, 97, 74, 107,     # X5
            93, 92, 89, 78,      # X6
            111, 118, 60, 63,    # X7
            186, 86, 50, 30,     # X8
            231, 65, 30, 26)     # X9
)

# Check the data frame
print(prevalence_data)

# Create separate bar plots for each predictor
ggplot(subset(prevalence_data, Predictor == "X1"), aes(x = factor(Category), y = Count, fill = factor(Category))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Prevalence for X1", x = "Category", y = "Prevalence (Count)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(subset(prevalence_data, Predictor == "X2"), aes(x = factor(Category), y = Count, fill = factor(Category))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Prevalence for X2", x = "Category", y = "Prevalence (Count)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(subset(prevalence_data, Predictor == "X3"), aes(x = factor(Category), y = Count, fill = factor(Category))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Prevalence for X3", x = "Category", y = "Prevalence (Count)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(subset(prevalence_data, Predictor == "X4"), aes(x = factor(Category), y = Count, fill = factor(Category))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Prevalence for X4", x = "Category", y = "Prevalence (Count)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(subset(prevalence_data, Predictor == "X5"), aes(x = factor(Category), y = Count, fill = factor(Category))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Prevalence for X5", x = "Category", y = "Prevalence (Count)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(subset(prevalence_data, Predictor == "X6"), aes(x = factor(Category), y = Count, fill = factor(Category))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Prevalence for X6", x = "Category", y = "Prevalence (Count)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(subset(prevalence_data, Predictor == "X7"), aes(x = factor(Category), y = Count, fill = factor(Category))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Prevalence for X7", x = "Category", y = "Prevalence (Count)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(subset(prevalence_data, Predictor == "X8"), aes(x = factor(Category), y = Count, fill = factor(Category))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Prevalence for X8", x = "Category", y = "Prevalence (Count)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(subset(prevalence_data, Predictor == "X9"), aes(x = factor(Category), y = Count, fill = factor(Category))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Prevalence for X9", x = "Category", y = "Prevalence (Count)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
