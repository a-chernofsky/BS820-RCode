###########################################################################################################
#
# BS820 R Code: Lecture1 Code
# Description: logistic regression with a single predictor
# Dataset: data/HELPJsat.xlsx
# created: 10/22/2020
#
############################################################################################################


# install/load packages ---------------------------------------------------
if (!require('readxl')) install.packages('readxl') 
if (!require('broom')) install.packages('broom') 
if (!require('epitools')) install.packages('epitools') 

library(readxl)
library(broom)
library(epitools)

# read in data ------------------------------------------------------------

zero <- read_excel("data/HELPJsat.xlsx")

one <- zero

#create binary drinks variables
one$drinks <- ifelse(one$i1 > 13, 1, 
                     ifelse(one$i1 <= 13, 0, NA))

table(one$homeless)

#histogram of drinks per day
hist(one$i1, 
     freq = F, 
     xlim = c(-18, 162), 
     main = "Distribution of i1")
lines(density(one$i1))

#scatter plot of homeless against drinks per day
plot(one$i1, one$homeless, 
     col = "red",
     pch = 19,
     xlab = "Drinks Per Day",
     ylab = "Homeless",
     main = "Homeless by Drinks")

#2 sample t-test of mean drinks per day by homeless with unequal and equal variance
t.test(one$i1 ~ one$homeless, var.equal = F)
t.test(one$i1 ~ one$homeless, var.equal = T)

#linear regression model
linar <- lm(homeless ~ i1, data = one)
#nicely print out lm results
tidy(linar)

#scatter plot of homeless against drinks per day with overlayed fitted values
plot(one$i1, one$homeless, 
     col = "red",
     pch = 19,
     xlab = "Drinks Per Day",
     ylab = "Homeless",
     main = "Homeless by Drinks",
     ylim = c(0, 1.5))
points(one$i1, linar$fitted.values, pch = 19, col = "blue")
lines(one$i1, linar$fitted.values, col = "blue")

# 2 by 2 table of homeless by drinks
table(one$homeless, one$drinks,dnn = c("homeless", "drinks"))

#Chi squared test
chisq.test(one$homeless, one$drinks, correct = F)

#from epitools package - prints output similar to proc freq
oddsratio(one$drinks, one$homeless, method = "wald")


# logistic regression with 1 cont predictor -------------------------------
m1_logistic <- glm(homeless ~ i1, data = one, family = binomial())

tidy(m1_logistic)
tidy(m1_logistic, exp = T, conf.int = T)

plot(one$i1, one$homeless, 
     col = "red",
     pch = 19,
     xlab = "Drinks Per Day",
     ylab = "Homeless",
     main = "Homeless by Drinks")
points(one$i1, m1_logistic$fitted.values, pch = 19, col = "blue")
lines(one$i1[order(one$i1)], m1_logistic$fitted.values[order(one$i1)], col = "blue")


# logistic regression with 1 binary predictor -----------------------------
m2_logistic <- glm(homeless ~ drinks, data = one, family = binomial())
tidy(m2_logistic)

#from broom package nicely print out odds ratio and 95% CI
tidy(m2_logistic, exp = T, conf.int = T)
#from broom package nicely print out model fit statistics
glance(m2_logistic)

plot(one$i1, one$homeless, 
     col = "red",
     pch = 19,
     xlab = "Drinks Per Day",
     ylab = "Homeless",
     main = "Homeless by Drinks")
points(one$i1, m2_logistic$fitted.values, pch = 19, col = "blue")
lines(one$i1[order(one$i1)], m2_logistic$fitted.values[order(one$i1)], col = "blue")


# Pearson Residual Plots ---------------------------------------------------------------

plot(one$i1, residuals(m1_logistic, "pearson"), 
     col = "red",
     pch = 19,
     xlab = "Drinks Per Day",
     ylab = "Homeless",
     main = "Homeless by Drinks")

plot(one$i1, residuals(m2_logistic, "pearson"), 
     col = "red",
     pch = 19,
     xlab = "Drinks Per Day",
     ylab = "Homeless",
     main = "Homeless by Drinks")
