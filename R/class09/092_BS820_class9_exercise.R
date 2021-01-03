###########################################################################################################
#
# BS820 R Code: Lecture 9 In-class Exercise Code
# Description: Parametric regression models
# Dataset: 
# created: 12/21/2020
#
############################################################################################################

# install/load packages ---------------------------------------------------

if (!require('haven')) install.packages('haven') 
if (!require('readxl')) install.packages('readxl') 
if (!require('broom')) install.packages('broom') 
if (!require('epitools')) install.packages('epitools') 
if (!require('survival')) install.packages('survival')
if (!require('MASS')) install.packages('MASS')
if (!require('car')) install.packages('car')
if (!require('geepack')) install.packages('geepack')
if (!require('muhaz')) install.packages('muhaz')
if (!require('survminer')) install.packages('survminer')
if (!require('flexsurv')) install.packages('flexsurv')

library(readxl)
library(haven)
library(broom)
library(epitools)
library(survival)
library(MASS)
library(car)
library(geepack)
library(muhaz)
library(survminer)
library(flexsurv)

# read in data ------------------------------------------------------------

one <- read_excel("data/support.xlsx")

#create id variable
one$id <- 1:nrow(one)

#create numeric sex variable
one$sexn <- ifelse(one$sex == "male", 0, 
                   ifelse(one$sex == "female", 1, NA))

two <- one[one$dzclass == "ARF/MOSF", ]


# Accelerate Failure Time Models ------------------------------------------

#1. exponential model
aft_exp <- survreg(Surv(d.time, death) ~ sexn + age + meanbp + hrt + crea + wblc + temp,
                   dist = "exponential",
                   data = two)
summary(aft_exp)

#2. weibull model
aft_wei <- survreg(Surv(d.time, death) ~ sexn + age + meanbp + hrt + crea + wblc + temp,
                   dist = "weibull",
                   data = two)
summary(aft_wei)

#3. log normal model
aft_logn <- survreg(Surv(d.time, death) ~ sexn + age + meanbp + hrt + crea + wblc + temp,
                   dist = "lognormal",
                   data = two)
summary(aft_logn)

#4. generalized gamma model
aft_ggam <- flexsurvreg(Surv(d.time, death) ~ sexn + age + meanbp + hrt + crea + wblc + temp,
                        dist = "gengamma",
                        data = two)
aft_ggam
plot(aft_ggam)
