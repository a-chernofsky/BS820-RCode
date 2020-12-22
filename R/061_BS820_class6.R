###########################################################################################################
#
# BS820 R Code: Lecture 6 Code
# Description: Poisson & negative binomial regression
# Dataset: 
# created: 11/22/2020
#
############################################################################################################

# install/load packages ---------------------------------------------------

if (!require('readxl')) install.packages('readxl') 
if (!require('broom')) install.packages('broom') 
if (!require('epitools')) install.packages('epitools') 
if (!require('survival')) install.packages('survival')
if (!require('MASS')) install.packages('MASS')
if (!require('car')) install.packages('car')
if (!require('geepack')) install.packages('geepack')
if (!require('logbin')) install.packages('logbin')

library(readxl)
library(broom)
library(epitools)
library(survival)
library(MASS)
library(car)
library(geepack)
library(logbin)

fap <- data.frame(idnum = 1:22, 
                  male = c(0, 0, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1), 
                  treat = c(1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 1),
                  base_n = c(7, 77, 7, 5, 23, 35, 11, 12, 7, 318, 160, 8, 20, 11, 24, 34, 54, 16, 30, 10, 20, 12),
                  age = c(17, 20, 16, 18, 22, 13, 23, 34, 50, 19, 17, 23, 22, 30, 27, 23, 22, 13, 34, 23, 22, 42),
                  r_n = c(6, 67, 4, 5, 16, 31, 6, 20, 7, 347, 142, 1, 16, 20, 26, 27, 45, 10, 30, 6, 5, 8))

sumstats <- function(x) c(min = min(x), 
                          q1 = quantile(x, probs = 0.25), 
                          median = median(x), 
                          mean = mean(x),
                          q3 = quantile(x, probs = 0.75),
                          max = max(x))

tapply(fap$r_n, fap$treat, sumstats)

preg1 <- glm(r_n ~ treat, family = poisson(), data = fap)
tidy(preg1)
glance(preg1)


pos_reg2 <- glm(r_n ~ treat + male + base_n + age, family = poisson(), data = fap)
tidy(pos_reg2)
glance(pos_reg2)


preg3 <- glm(r_n ~ treat + male + base_n + age, family = quasipoisson(), data = fap)
tidy(preg3)
glance(preg3)

preg4 <- geese(r_n ~ treat + male + base_n + age, 
                  id = idnum, 
                  family = poisson(), 
                  corstr = "exch",
                  data = fap)
summary(preg4)


nbreg <- glm.nb(r_n ~ treat + male + base_n + age, 
                 data = fap)
tidy(nbreg)
glance(nbreg)
summary(nbreg)


# Post-traumatic Stress Disorder Data -------------------------------------

ptsd01 <- read.table("data/ptsd.txt", 
                     col.names = c("subjid", "ptsd", "control", 
                                   "problems", "sevent", "cohes", 
                                   "time"))
ptsd01 <- ptsd01[ptsd01$time == 1,]

ptsd_logistic <- glm(ptsd ~ control + problems + sevent + cohes,
                     family = binomial(), data = ptsd01)
tidy(ptsd_logistic)
tidy(ptsd_logistic, exp = T, conf.int = T)

ptsd_logbin <- logbin(ptsd ~ control + problems + sevent + cohes,
                      data = ptsd01)
tidy(ptsd_logbin)


ptsd_gee <- geese(ptsd ~ control + problems + sevent + cohes, 
                  id = subjid, 
                  family = poisson(), 
                  corstr = "exch",
                  data = ptsd01)
summary(ptsd_gee)


# Estimating rates rather than counts -------------------------------------

one <- read.table("data/dccthypo.txt",
                  col.names = c("group", "nevents", 
                                "fuday", "iu", "duration", 
                                "female", "adult", "bcval5", 
                                "hbael", "hxcoma", "obweight"))
one$ievents <- ifelse(one$nevents > 0, 1, 0)
one$fuyears <- one$fuday/365.25
one$lnyears <- log(one$fuyears)
one$insulin <- one$iu/one$obweight
one$rate <- one$nevents/one$fuyears

c(n = length(one$nevents), mean = mean(one$nevents), sum = sum(one$nevents))
c(n = length(one$fuyears), mean = mean(one$fuyears), sum = sum(one$fuyears))

tapply(one$nevents, one$group, function(x)c(n = length(x), mean = mean(x), sum = sum(x)))
tapply(one$fuyears, one$group, function(x)c(n = length(x), mean = mean(x), sum = sum(x)))

rate_preg1 <- glm(nevents ~ 1, family = poisson(),
                  offset = lnyears,
                  data = one)
summary(rate_preg1)

rate_preg2 <- glm(nevents ~ group, family = poisson(),
                  offset = lnyears,
                  data = one)
summary(rate_preg2)

rate_preg3 <- glm(nevents ~ group + insulin + duration + 
                    female + adult + bcval5 + hbael + hxcoma , 
                  family = poisson(),
                  offset = lnyears,
                  data = one)
summary(rate_preg3)

rate_preg4 <- glm(nevents ~ group + insulin + duration + 
                    female + adult + bcval5 + hbael + hxcoma , 
                  family = quasipoisson(),
                  offset = lnyears,
                  data = one)
summary(rate_preg4)

two <- one
two$subnum <- 1:nrow(two)

rate_gee <- geeglm(nevents ~ group + insulin + duration + 
                    female + adult + bcval5 + hbael + hxcoma,  
                  id = subnum, 
                  family = poisson(), 
                  offset = lnyears,
                  corstr = "exchangeable",
                  data = two)
summary(rate_gee)

rate_nb <- glm.nb(nevents ~ group + insulin + duration + 
                    female + adult + bcval5 + hbael + hxcoma +
                    offset(lnyears),
                  data = two)
summary(rate_nb)
