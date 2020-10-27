###########################################################################################################
#
# BS820 R Code: Lecture2 Code
# Description: maximum likelihood and multiple predictors
# Dataset: data/LBWEIGHT.txt
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

zero <- read.table("data/LBWEIGHT.txt", 
                   header = F, 
                   col.names = c("idnum", "low", "age", "lwt", "race", "ftv"))

one <- zero


# descriptive and logistic regression -------------------------------------

#frequency and relative frequency of low birth weight
table(one$low, dnn = "low birth weight")
prop.table(table(one$low, dnn = "low birth weight"))

summary(one$lwt)
c(mean = mean(one$lwt), sd = sd(one$lwt))

#distribution of weight at last period
hist(one$lwt, probability =  T, col = "blue")
lines(density(one$lwt), col = "red", lwd = 2)

#logistic regression model of low birth weight with weight at last period as predictor
logreg1 <- glm(low ~ lwt, family = binomial(), data = one)
tidy(logreg1)
#odds ratio and 95% CI
tidy(logreg1, conf.int = T, exp = T)[2,]

#odds ratio and 95% CI with SD units
exp(sd(one$lwt)* c(tidy(logreg1, conf.int = T)$estimate[2], 
      tidy(logreg1, conf.int = T)$conf.low[2],
      tidy(logreg1, conf.int = T)$conf.high[2]))

one$race2 <- ifelse(one$race > 1, 1, 0)


# categorical predictors --------------------------------------------------

#rename race categories and convert to factor
one$race <- ifelse(one$race == 1, "white",
                   ifelse(one$race == 2, "black",
                          ifelse(one$race == 3, "other", NA)))
one$race <- factor(one$race, levels = c("white", "black", "other"))

#frequency tables
table(one$race)
table(one$race, one$low)
prop.table(table(one$race, one$low))

chisq.test(table(one$race, one$low))
oddsratio(table(one$race, one$low))

#logistic regression model with a categorical predictors
logreg2 <- glm(low ~ as.numeric(race), data = one, family = binomial() )
tidy(logreg2)

logreg3 <- glm(low ~ race, data = one, family = binomial() )
tidy(logreg3)
tidy(logreg3, conf.int = T, exp = T)[2:3,]
glance(logreg3)

#type 3 tests
Anova(logreg3, type = "III", test.statistic = "Wald")


# logistic regression with multiple predictors ----------------------------

logreg4 <- glm(low ~ age + lwt + race2 + ftv + race2*lwt, family = binomial(), data = one)
tidy(logreg4)
tidy(logreg4, conf.int = T, exp = T)
