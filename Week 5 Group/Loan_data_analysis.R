libs <- c("tidyverse", "dplyr", "gridExtra", "ggplot2", "ggfortify", "broom", "ggcorrplot", "corrr", "corrplot", "MASS", "datarium", "broom")

for (ilib in libs) {
  if (!(ilib %in% installed.packages())) {
    install.packages(ilib)
  }
  library(ilib, character.only = TRUE)
}

theme_set(theme_classic())

df <- read.csv("C:/Users/HP/OneDrive - Strathmore University/STRATH UNI/FIRST YEAR/SEMESTER III/UNITS/LINEAR MODELS/LINEAR-MODELS/Data/loan_data.csv")

dim(df)

names(df)

View(df)

sapply(df, anyNA) # Checking for null values
# There are no null values in this dataset.

# EDA to have a view of the data and check for possible outliers.

diff(range(df$person_income))
ggplot(df, aes( x = person_income)) +
  geom_histogram(binwidth = 1000, fill = "blue", color = "black") +
  labs(title = "Histogram of personal income", x = "Personal Income", y = "Count")
# The histogram of personal income was quite positively skewed. This was a hint of a log transformation to be soon done.
# This was due to the large dataset.

# Log transform
ggplot(df, aes( x = log(person_income))) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Histogram of Log-personal income", x = "Personal Income", y = "Count")


diff(range(df$loan_amnt))
ggplot(df, aes(x = loan_amnt)) +
  geom_histogram(binwidth = 1000, fill = "red", colour = "black") +
  labs(title = "Histogram of Loan Amount", x = "Personal Income", y = "Count")
# The histogram of the loan amount is positively skewed.

ggplot(df) + 
  geom_point(mapping = aes(x = person_income, y = loan_amnt), alpha = 0.5, color = "gold")

# Log transform of the above
ggplot(df) +
  geom_point(mapping = aes(x = log(person_income), y = log(loan_amnt)), alpha = 0.5, color = "green") + 
  geom_smooth(mapping = aes(x = log(person_income), y = log(loan_amnt)), alpha = 0.5, color = "black")
             
             