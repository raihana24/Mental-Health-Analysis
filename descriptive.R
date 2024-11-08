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
