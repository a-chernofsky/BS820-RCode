###########################################################################################################
#
# BS820 R Code: Lecture 2 Hands on Exercise Code 
# Description: maximum likelihood and multiple predictors
# Dataset: data/Arrests.csv
# created: 10/25/2020
#
############################################################################################################

if (!require('readxl')) install.packages('readxl') 
if (!require('broom')) install.packages('broom') 
if (!require('epitools')) install.packages('epitools') 
if (!require('car')) install.packages('car') 

library(readxl)
library(broom)
library(epitools)
library(car)

# read in data ------------------------------------------------------------

zero <- read.csv("data/Arrests.csv")

one <- zero

# 1)	What percentage of arrestees were not released? ----------------------

prop.table(table(one$released))*100

# 2)	What percentage of arrestees are black? ----------------------

prop.table(table(one$colour))*100

# 3)	What is the mean age for whites in these data? What is the mean age for blacks in these data? ----------------------

tapply(one$age, one$colour, mean)

# 4)	Is skin color associated with not being released? What is the odds ratio and Wald 95% confidence interval for this association? ----------------------

oddsratio(table(one$colour, one$released), method = "wald")

# 5)	What is the odds ratio for association of skin color with not being released when adjusted for age? What is the odds ratio and 95% confidence interval 
#             for the association between age and not being released when adjusted for skin color? ----------------------

logreg1 <- glm(released ~ colour + age, family = binomial(), data = one)
tidy(logreg1, conf.int = T, exp = T)

# 6)	Is there an interaction between skin color and age in predicting not being released? 
#             How would you describe the effect of skin color in this interaction model? 
#             How would you describe the effect of age in this interaction model? 
#             Make plots showing predicted values for not being released by skin color and age. ----------------------

logreg2 <- glm(released ~ colour + age + colour * age, family = binomial(), data = one)
tidy(logreg2)

plot(one$age, logreg2$fitted.values, 
     col = one$colour,
     xlab = "age",
     ylab = "fitted values",
     main = "fitted values by age and colour")
