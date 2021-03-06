###########################################################################################################
#
# BS820 R Code: Lecture3 Code
# Description: non-linear predictors and model fit
# Dataset: data/support.xlsx
# created: 10/27/2020
#
############################################################################################################


# install/load packages ---------------------------------------------------
if (!require('readxl')) install.packages('readxl') 
if (!require('broom')) install.packages('broom') 
if (!require('epitools')) install.packages('epitools') 
if (!require('car')) install.packages('car') 
if (!require('splines')) install.packages('splines') 
if (!require('rms')) install.packages('rms') 
if (!require('ResourceSelection')) install.packages('ResourceSelection') 

library(readxl)
library(broom)
library(epitools)
library(car)
library(splines)
library(rms)
library(ResourceSelection)

# read in data ------------------------------------------------------------

zero <- read_excel("data/support.xlsx")

one <- as.data.frame(zero)


# linear fit --------------------------------------------------------------

logreg0 <- glm(hospdead ~ meanbp, data = one, family = binomial())

plot(one$meanbp, one$hospdead, 
     col = "red",
     pch = 19,
     xlab = "Mean BP",
     ylab = "Hospital Death",
     main = "Predicted values by Blood Pressure")
points(one$meanbp, logreg0$fitted.values, pch = 19, col = "blue")
lines(one$meanbp[order(one$meanbp)], logreg0$fitted.values[order(one$meanbp)], col = "blue")


plot(one$meanbp, residuals(logreg0, type = "pearson"), 
     col = "red",
     pch = 19,
     xlab = "Predicted values by Blood Pressure",
     ylab = "Pearson Residual",
     main = "Residuals by Blood Pressure")
abline(h = 3)

# loess fit ---------------------------------------------------------------

#fit loess model
loessmod <- loess(hospdead ~ meanbp, data = one, span = 0.265)

#plot the raw outcome against meanbp with a loess fit
o <- order(one$meanbp)

plot(one$meanbp, one$hospdead, 
     col = "red",
     main = "Hospital Death by Blood Pressure",
     xlab = "Mean 3-day Blood Pressure",
     ylab = "Hospital Vital Status")
lines(one$meanbp[o], loessmod$fitted[o])


# model with BP quartiles -------------------------------------------------

#create a 4 level variable for mean bp
one$bpqrt <- ifelse(one$meanbp <= 64, 1,
                    ifelse(one$meanbp > 64 & one$meanbp  <= 78, 2,
                           ifelse(one$meanbp > 78 & one$meanbp <= 107, 3,
                                  ifelse(one$meanbp > 107, 4, NA))))
one$bpqrt <- factor(one$bpqrt, levels = c(2, 1, 3, 4))

logreg1 <- glm(hospdead ~ bpqrt, data = one, family = binomial())

tidy(logreg1)
tidy(logreg1, conf.int = T, exp = T)
glance(logreg1)
Anova(logreg1)

plot(one$meanbp, logreg1$fitted.values, 
     col = "red",
     ylab = "Probability of Hospital Death",
     xlab = "Mean 3-day Blood Pressure",
     main = "Dummy Variable Quartiles",
     ylim = c(0,1))
lines(one$meanbp[o], logreg1$fitted.values[o], col = "red")


plot(logreg1$fitted.values, residuals(logreg1, type = "pearson"), 
     col = "red",
     ylab = "Pearson Residual",
     xlab = "Estimated Probability",
     main = "Dummy Variable Residual Plot", 
     pch = 20,
     ylim = c(-1, 3),
     cex = 2)


# fit grouped linear quartiles trend ------------------------------------------------

levels(one$bpqrt)
one$bpqrt_num <- as.numeric(relevel(one$bpqrt, ref = "1"))


logreg2 <- glm(hospdead ~ bpqrt_num, data = one, family = binomial())

tidy(logreg2)
tidy(logreg2, conf.int = T, exp = T)
glance(logreg2)
Anova(logreg2)

plot(one$meanbp, logreg2$fitted.values, 
     col = "blue",
     ylab = "Probability of Hospital Death",
     xlab = "Mean 3-day Blood Pressure",
     main = "Grouped Linear Quartiles",
     ylim = c(0,1))
lines(one$meanbp[o], logreg2$fitted.values[o], col = "blue")


plot(logreg2$fitted.values, residuals(logreg2, type = "pearson"), 
     col = "blue",
     ylab = "Pearson Residual",
     xlab = "Estimated Probability",
     main = "Grouped Linear Residual Plot", 
     pch = 20,
     ylim = c(-1, 3),
     cex = 2)



# fit with quadratic term -------------------------------------------------

logreg3 <- glm(hospdead ~ meanbp + I(meanbp^2), data = one, family = binomial())

tidy(logreg3)
tidy(logreg3, conf.int = T, exp = T)
glance(logreg3)
Anova(logreg3)

plot(one$meanbp, logreg3$fitted.values, 
     col = "darkgreen",
     ylab = "Probability of Hospital Death",
     xlab = "Mean 3-day Blood Pressure",
     main = "Quadratic Model",
     ylim = c(0,1))
lines(one$meanbp[o], logreg3$fitted.values[o], col = "darkgreen")


plot(logreg3$fitted.values, residuals(logreg3, type = "pearson"), 
     col = "red",
     ylab = "Pearson Residual",
     xlab = "Estimated Probability",
     main = "Quadratic Model Residual Plot", 
     pch = 20,
     ylim = c(-6, 3),
     cex = 2)


# fit with natural cubic splines ------------------------------------------

logreg4 <- glm(hospdead ~ ns(meanbp, df = 3, intercept = T), data = one, family = binomial())

tidy(logreg4)
glance(logreg4)
Anova(logreg4)

plot(one$meanbp, logreg4$fitted.values, 
     col = "darkgreen",
     ylab = "Probability of Hospital Death",
     xlab = "Mean 3-day Blood Pressure",
     main = "Continuous Spline Predicted against BP",
     ylim = c(0,1))
lines(one$meanbp[o], logreg4$fitted.values[o], col = "darkgreen")

plot(logreg4$fitted.values, residuals(logreg4, type = "pearson"), 
     col = "red",
     ylab = "Pearson Residual",
     xlab = "Estimated Probability",
     main = "Grouped Linear Residual Plot", 
     pch = 20,
     ylim = c(-6, 3),
     cex =2)

# Assessing Model Fit -----------------------------------------------------

rms::lrm(hospdead ~ bpqrt, data = one)

#Hosmer-Lemeshow test
ResourceSelection::hoslem.test(logreg1$y, fitted(logreg1))

# Assessing Model Fit -----------------------------------------------------

plot(logreg4)

save <- rms::lrm(hospdead ~ bpqrt, data = one, y = T, x = T)
residuals(save)

o2 <- order(logreg4$fitted.values)

plot(logreg4$fitted.values[o2], residuals(logreg4, type = "deviance")[o2], 
     col = "darkgreen",
     ylab = "Pearson Residual",
     xlab = "Estimated Probability",
     main = "Continuous BP with Spline", 
     pch = 20,
     ylim = c(-6, 3))
                 