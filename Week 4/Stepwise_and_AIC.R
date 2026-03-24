library(MASS)
names(Cars93)
pairs(Cars93[c("MPG.highway", "Horsepower", "RPM", "Weight")], col = 2)
lmMPG1 <- lm(MPG.highway~Horsepower+RPM + Weight, data =  Cars93)
summary(lmMPG1)

# Model specification
# a) Forward search
lmMPG3 <- lm(MPG.highway ~ Weight, data = Cars93)


# addterm -  avariable that is capable of adding variables
addterm(lmMPG3, ~ .+ EngineSize + Horsepower +RPM + Rev.per.mile + Fuel.tank.capacity + Length + Wheelbase + Width +
          Turn.circle, test = "F")
# After checking the AIC, Fuel.tank.capacity, Length, Wheelbase, Width are the highest. It shows they are significant. This is
# also backed by the p-values.
# <none> shows the threshhold. In our case its AIC is 214.74. Anything above it should be discarded. It is noise.

lmMPG4 <- update(lmMPG3, ~ .+ Length)
summary(lmMPG4)

addterm(lmMPG4, ~ .+ EngineSize + Horsepower +RPM + Rev.per.mile + Fuel.tank.capacity + Wheelbase + Width +
          Turn.circle, test = "F")
# It was seen that wheel base is significant together with Fuel.tank.capacity, but since we are adding one at a time,
# we added wheel base.

lmMPG5 <- update(lmMPG4, ~ .+ Wheelbase)
summary(lmMPG5)


#b) Backward Search
lmMPG6 <- lm(MPG.highway ~ Weight + EngineSize + Horsepower +
               RPM + Rev.per.mile + Fuel.tank.capacity + Length + Wheelbase + Width + Turn.circle, data = Cars93)
# Here we begin with the full model

dropterm(lmMPG6, test = "F")
# The variable with the highest p-value is Horsepower. It is the first to be dropped.

lmMPG7 <- update(lmMPG6, ~ -Horsepower)

dropterm(lmMPG7, test = "F")





# c) step-wise selection
lmMPG9 <- lm(MPG.highway ~ 1, data = Cars93)
step(lmMPG9, scope = list (upper = lmMPG6), direction = "forward")
#any other variable added after <none> as it is at the bottom is going to worsen the model
# Forward adds the variables one by one (Begins with one variable)
step(lmMPG6, scope = list (lower = lmMPG9), direction = "backward")
# Begins with full model and drops the variables 
