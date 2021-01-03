###########################################################################################################
#
# BS820 R Code: Lecture 4 Code
# Description: Numerical Problems & Variable Selection
# Dataset: data/ICU.xlsx
# created: 11/10/2020
#
############################################################################################################


# install/load packages ---------------------------------------------------
if (!require('readxl')) install.packages('readxl') 
if (!require('broom')) install.packages('broom') 
if (!require('epitools')) install.packages('epitools') 
if (!require('logistf')) install.packages('logistf') 
if (!require('pwr')) install.packages('pwr') 
if (!require('survival')) install.packages('survival')

library(readxl)
library(broom)
library(epitools)
library(logistf)
library(pwr)
library(survival)

# read in data ------------------------------------------------------------

zero <- read_excel("data/ICU.xlsx")

one <- as.data.frame(zero)

names(one) <- tolower(names(one))

one$loc <- factor(one$loc)
one$sex <- factor(one$sex)
one$race <- factor(one$race)
one$ser <- factor(one$ser)
one$can <- factor(one$can)
one$crn <- factor(one$crn)
one$cpr <- factor(one$cpr)
one$inf <- factor(one$inf)
one$pre <- factor(one$pre)
one$typ <- factor(one$typ)
one$fra <- factor(one$fra)
one$po2 <- factor(one$po2)
one$ph <- factor(one$ph)
one$pco <- factor(one$pco)
one$bic <- factor(one$bic)
one$cre <- factor(one$cre)
one$loc <- factor(one$loc)


# Numerical problems in logistic regression -------------------------------

table(one$loc, one$sta)

logreg1 <- glm(sta ~ loc, family = binomial(), data = one)
tidy(logreg1)
tidy(logreg1, exp = T, conf.int = T)

logreg2 <- logistf(sta ~ loc, data = one)
summary(logreg2)

cbind(OR = exp(logreg2$coefficients), 
      lower = exp(logreg2$ci.lower), 
      upper = exp(logreg2$ci.upper))

fisher.test(table(one$loc, one$sta))

logreg3 <- glm(sta ~ loc, family = binomial(), data = one)


# selection methods -------------------------------------------------------

full <- glm(sta ~ sex + age + race + ser + can +
              crn + inf + cpr + pre + typ + fra + po2 +
              ph + pco + bic + cre, 
            family = binomial(), data = one)

fwd <- step(full, direction =  "forward")
fwd2 <- step(full, direction = "forward", steps = 2)


bw <- step(full, direction =  "backward")

step <- step(full, direction = "both")


# standardized beta -------------------------------------------------------


# conditional logistic regression -----------------------------------------

match1 <- read.table("data/match_11.txt")[, 1:9]
names(match1) <- c("pair", "low", "age", "lwt", "race", 
                   "smoke", "ptd", "ht", "ui")
match2 <- read.table("data/match_11.txt")[, 10:18]
names(match2) <- names(match1)
match_11 <- rbind(match1, match2)
match_11$race <- factor(match_11$race)

clogit(low ~ race + smoke + ht + ui + ptd + lwt + strata(pair), data = match_11)

# sample size calculation -------------------------------------------------

h = 2*asin(sqrt(0.15)) - 2*asin(sqrt(0.667*0.15))

pwr.2p.test(h,  n = NULL, sig.level = 0.05, power = 0.8)

plot(pwr.2p.test(h,  n = NULL, sig.level = 0.05, power = 0.8))
