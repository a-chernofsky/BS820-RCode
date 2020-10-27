###########################################################################################################
#
# BS820 R Code: Lecture1 Hands-On Exercise Code
# Description: logistic regression with a single predictor
# Dataset: data/support.xlsx
# created: 10/22/2020
#
############################################################################################################

if (!require('readxl')) install.packages('readxl') 
if (!require('broom')) install.packages('broom') 
if (!require('epitools')) install.packages('epitools') 

library(readxl)
library(broom)
library(epitools)

# read in data ------------------------------------------------------------

zero <- read_excel("data/support.xlsx")

one <- as.data.frame(zero)


# 1) How many subjects die during the initial hospitalization? ---------------

table(one$hospdead)

# 2) Is death during the initial hospitalization associated with sex? --------

table(one$sex, one$hospdead)
prop.table(table(one$sex, one$hospdead))

# 3) What is the Pearson chi-square test statistic for this and the p-value? --------

chisq.test(one$sex, one$hospdead, correct = F)
oddsratio(one$sex, one$hospdead, method = "wald")

# 4) Which sex is at greater risk of dying during the initial hospitalization? --------

table(one$sex, one$hospdead)

# 5) What is the relative risk and 95% confidence interval? ---------------

riskratio(one$sex, one$hospdead, method = "wald")

# 6) What is the log odds ratio and its standard error for the association between sex and dying during initial hospitalization? --------

logreg1 <- glm(hospdead ~ sex, family = binomial(), data = one)
tidy(logreg1)

# 7) What is the odds ratio and its 95% confidence interval for sex and dying during the initial hospitalization? --------

tidy(logreg1, conf.int = T, exp = T)

# 8) What is the median and range for 3-day blood pressure? What is the mean and standard deviation? ----

c(median = median(one$meanbp), min = min(one$meanbp), max = max(one$meanbp))
c(mean = mean(one$meanbp), sd =  sd(one$meanbp))

#or 

summary(one$meanbp)

# 9) Is the 3-day blood pressure positively or negatively associated with dying in the initial hospitalization? 
#           Is it better to have high blood pressure or low blood pressure? --------

logreg2 <- glm(hospdead ~ meanbp, family = binomial(), data = one)

tidy(logreg2)

# 10) Is the association between 3-day blood pressure and death during the initial hospitalization statistically significant? --------

tidy(logreg2)

# 11) What is the odds ratio and 95% confidence interval for the association between 3-day blood pressure and dying during the initial hospitalization? --------

tidy(logreg2, conf.int = T, exp = T)[2,]

# 12) Does the residual plot for death in the hospital predicted by mean blood pressure show any potential problems? --------

plot(one$meanbp, residuals(logreg2, "pearson"), 
     col = "red",
     pch = 19,
     xlab = "Mean BP",
     ylab = "Residuals",
     main = "Pearson Residual Plot")


