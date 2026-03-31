libs <- c("tidyverse", "dplyr", "gridExtra", "ggplot2", "ggfortify", 
          "broom", "ggcorrplot", "corrr", "corrplot", "MASS", "datarium", "broom")

for (ilib in libs) {
  if (!(ilib %in% installed.packages())) {
    install.packages(ilib)
  }
  library(ilib, character.only = TRUE)
}

theme_set(theme_classic())

df <- read.csv("C:/Users/HP/OneDrive - Strathmore University/STRATH UNI/FIRST YEAR/SEMESTER III/UNITS/LINEAR MODELS/LINEAR-MODELS/Data/FuelConsumption.csv")

cdf <- df[, c("ENGINESIZE", "CYLINDERS", "FUELCONSUMPTION_COMB", "CO2EMISSIONS")] # selecting relevant columns

dim(df)

names(df)

View(df)

# Running some EDA by checking skewness and trying to spot some outliers

diff(range(cdf$CYLINDERS))
ggplot(cdf,aes(x = CYLINDERS)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histogram of Cylinders", x = "Cylinders", y = "Count")
# The above histogram is positively skewed. Most vehicles had less than 6 cylinders

ggplot(cdf, aes(x = CYLINDERS)) +
  geom_bar(fill = "blue", color = "black") + 
  labs(title = "Bar graph showing the no. of Cylinders", x = "Cylinders", y = "Count")
# From the bar graph, most vehicles seemed to have approximately 4 cylinders.

summary(cdf$ENGINESIZE)
diff(range(cdf$ENGINESIZE)) # Used this to aid in identifying the bin width
ggplot(cdf,aes(x = ENGINESIZE)) +
  geom_histogram(binwidth = 0.2, fill = "red", color = "black") +
  labs(title = "Histogram of Engine Size", x = "Engine Size", y = "Count")
# This histogram is somewhat positively skewed

diff(range(cdf$FUELCONSUMPTION_COMB))
ggplot(cdf,aes(x = FUELCONSUMPTION_COMB)) +
  geom_histogram(binwidth = 0.7, fill = "green", color = "black") +
  labs(title = "Histogram of Fuel Consumption", x = "Fuel Consumption", y = "Count")
# This histogram is positively skewed.

diff(range(cdf$CO2EMISSIONS))
ggplot(cdf,aes(x = CO2EMISSIONS)) +
  geom_histogram(binwidth = 10, fill = "purple", color = "black") +
  labs(title = "Histogram of CO2 Emissions", x = "CO2 Emissions", y = "Count")
# This histogram is slightly normal with a some positive skewness

# Key takeaways
# 1. Most histograms happened to have a positive skew as there were some outliers seen on the tails
# 2. diff(range()) helped to identify the bins in the histogram. This was to prevent some random guesses and
#     be rooted mathematically
# 3. The cylinder feature had 2 plots, the histogram and the bar graph. The bar graph was more intuitive.
#     This is because the number if cylinders are discrete and not continuous. A bar graph was more preferred.


### DENSITY PLOT
ggplot(cdf,aes(x = CYLINDERS)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Histogram of Cylinders", x = "Cylinders", y = "Count")


ggplot(cdf,aes(x = ENGINESIZE)) +
  geom_density(fill = "red", alpha = 0.5) +
  labs(title = "Histogram of Engine Size", x = "Engine Size", y = "Count")


ggplot(cdf,aes(x = FUELCONSUMPTION_COMB)) +
  geom_density(fill = "green", alpha = 0.5) +
  labs(title = "Histogram of Fuel Consumption", x = "Fuel Consumption", y = "Count")

ggplot(cdf,aes(x = CO2EMISSIONS)) +
  geom_density(fill = "purple", alpha = 0.5) +
  labs(title = "Histogram of CO2 Emissions", x = "CO2 Emissions", y = "Count")

### SCATTER PLOTS
ggplot(cdf, aes (x = ENGINESIZE, y = CO2EMISSIONS)) +
  geom_point(color = "blue") +
  labs (x = "Engine Size", y = "CO2 Emissions")

ggplot(cdf, aes (x = FUELCONSUMPTION_COMB, y = CO2EMISSIONS)) +
  geom_point(color = "blue") +
  labs (x = "Fuel Consumption (Combined)", y = "CO2 Emissions")

ggplot(cdf, aes (x = CYLINDERS, y = CO2EMISSIONS)) +
  geom_point(color = "blue") +
  labs (x = "Cylinders", y = "CO2 Emissions")

### Train-test
set.seed(123)
msk <- runif(nrow(cdf)) <0.8
train <- cdf[msk, ]
test <- cdf[!msk, ]

# Fitting simple lm
model <- lm(CO2EMISSIONS ~ ENGINESIZE, data = train)

# Displaying model summary
summary(model)

#Fit regression line
ggplot(train, aes (x = ENGINESIZE, y = CO2EMISSIONS)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs (x = "Engine Size", y = "CO2 Emissions", title = "Linear Regression Model")

# Making predictions on test data
test$predictions <- predict(model, newdata = test)

ggplot(test, aes (x = ENGINESIZE, y = predictions)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs (x = "Engine Size", y = "predictions", title = "Linear Regression Model")

# Model evaulation
mse <- mean((test$CO2EMISSIONS - test$predictions)^2)
rmse <- sqrt(mse)
cat("Mean Squared Error:", mse, "\n")
cat("Root Mean Squared Error:", rmse, "\n")

# Plot actual
ggplot(test, aes (x = CO2EMISSIONS, y = predictions)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs (x = "Actual CO2 Emission", y = "Predicted CO2 Emissions", title = "Actual vs Predicted CO2 Emissions")

# Fitting simple lm
model2 <- lm(CO2EMISSIONS ~ ENGINESIZE + FUELCONSUMPTION_COMB, data = train)

# Displaying model summary
summary(model2)

VI#Fit regression line
ggplot(train, aes (x = ENGINESIZE, y = CO2EMISSIONS)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs (x = "Engine Size", y = "CO2 Emissions", title = "Linear Regression Model")

# Making predictions on test data
test$predictions2 <- predict(model2, newdata = test)

ggplot(test, aes (x = ENGINESIZE, y = predictions2)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs (x = "Engine Size", y = "predictions", title = "Linear Regression Model")

# Model evaulation
mse <- mean((test$CO2EMISSIONS - test$predictions2)^2)
rmse <- sqrt(mse)
cat("Mean Squared Error:", mse, "\n")
cat("Root Mean Squared Error:", rmse, "\n")

# Plot actual
ggplot(test, aes (x = CO2EMISSIONS, y = predictions2)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs (x = "Actual CO2 Emission", y = "Predicted CO2 Emissions", title = "Actual vs Predicted CO2 Emissions")


