###########################################################################################################
#
# BS820 R Code: Lecture 5 In-class exercise Code
# Description: Numerical Problems & Variable Selection
# Dataset: 
# created: 11/22/2020
#
############################################################################################################


# install/load packages ---------------------------------------------------

if (!require('readxl')) install.packages('readxl') 
if (!require('broom')) install.packages('broom') 
if (!require('epitools')) install.packages('epitools') 
if (!require('logistf')) install.packages('logistf') 
if (!require('pwr')) install.packages('pwr') 
if (!require('survival')) install.packages('survival')
if (!require('GDAtools')) install.packages('GDAtools')
if (!require('MASS')) install.packages('MASS')
if (!require('car')) install.packages('car')
if (!require('nnet')) install.packages('nnet')
if (!require('geepack')) install.packages('geepack')
if (!require('logbin')) install.packages('logbin')

library(readxl)
library(broom)
library(epitools)
library(logistf)
library(pwr)
library(survival)
library(GDAtools)
library(MASS)
library(car)
library(nnet)
library(geepack)
library(logbin)

zero <- read_excel("data/DebTrivedi.xlsx")

one <- zero

one$health <- ifelse(zero$poorhlth == 1, 0,
                     ifelse(zero$exclhlth == 1, 2, 1))
one$subid = seq(1, nrow(one))
one$health = factor(one$health, levels = c(1, 0, 2))


# Question 1 --------------------------------------------------------------

table(one$health)
tapply(one$ofp, one$health, 
       function(x) c(mean = mean(x), sd = sd(x)))

# Question 2 --------------------------------------------------------------

preg1 <- glm(ofp ~ health, family = "poisson", data = one)
tidy(preg1, exp = T, conf.int = T)

# Question 3 --------------------------------------------------------------

preg2 <- glm(ofp ~ health + male + numchron + school + privins,
             family = "poisson", data = one)
tidy(preg2, exp = T, conf.int = T)

# Question 4 --------------------------------------------------------------

nbreg <- glm.nb(ofp ~ health + male + numchron + school + privins,
                data = one)
tidy(nbreg, exp = T, conf.int = T)
glance(nbreg)
nbreg$theta
