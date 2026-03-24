libs <- c("tidyverse", "dplyr", "gridExtra", "ggplot2", "ggfortify", "broom", "ggcorrplot", "corrr", "corrplot", "MASS", "datarium", "broom")
for (ilib in libs) {
  if (!(ilib %in% installed.packages())) {
    install.packages(ilib)
  }
  library(ilib, character.only = TRUE)
}

theme_set(theme_classic())

# Gradient Descent
data <- read.csv("C:/Users/HP/OneDrive - Strathmore University/STRATH UNI/FIRST YEAR/SEMESTER III/UNITS/LINEAR MODELS/CSVs/data.csv")

X <- data[,1]
Y <- data[,2]

#Plotting data
plot(X, Y, main = "Scatter Plot of Data", xlab = "X", ylab = "Y", col = "blue", pch = 16)

# Initializing parameters
m <- 0 # Gradient(slope)
c <- 0 # Intercept
epochs <- 1000000 # Number of iterations
L <- 0.0038 # Learning rate
n <- length(X) # Number of data points

# Gradient Descent Algorithm
for (i in 1:epochs) {
  Y_pred <- m * X + c # predicted Y values
  D_m <- (-2/n) * sum (X*(Y - Y_pred)) # partial derivative with respect to m
  D_c <- (-2/n) * sum (Y - Y_pred) # Partial derivative wrt to c
  
  m <- m - L * D_m # Update slope
  c <- c - L * D_c # Update intercept
  
  if (m <= 0) {# stop when minimum error is reached
    break
  }
}

# Printing final parameters
print(paste("Final m:", m, "Final C:", c))

# Plotting the regression line
plot (X, Y, main = "Gradient Descent Linear Regression", xlab = "X", ylab = "Y", col = "blue", pch = 16)
abline (a = c, b = m, col = "red", lwd = 2)