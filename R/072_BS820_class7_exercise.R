###########################################################################################################
#
# BS820 R Code: Lecture 7 In-Class Exercise Code
# Description: Event Rates and Time-to-Event
# Dataset: support.xlsx
# created: 11/23/2020
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

library(readxl)
library(broom)
library(epitools)
library(survival)
library(MASS)
library(car)
library(geepack)

# Read in data ------------------------------------------------------------

one <- read_excel("data/support_cls7.xlsx")
one$idnum <- 1:nrow(one)
one$dzclassN <- ifelse(one$dzclass  == "ARF/MOSF", 1,
                       ifelse(one$dzclass == "COPD/CHF/Cirrhosis", 2,
                              ifelse(one$dzclass == "Cancer", 3,
                                     ifelse(one$dzclass == "Coma", 4, NA))))

# Question 1 --------------------------------------------------------------

c(n = length(one$death), mean = mean(one$death), sum = sum(one$death))
c(n = length(one$d.time), mean = mean(one$d.time), sum = sum(one$d.time))

# Question 2 --------------------------------------------------------------

tapply(one$death, one$dzclassN, function(x)c(n = length(x), mean = mean(x), sum = sum(x)))

tapply(one$d.time, one$dzclassN, function(x)c(n = length(x), mean = mean(x), sum = sum(x)))


# Question 3 - 5 ----------------------------------------------------------

two <- one
two$logtime <- log(two$d.time)

mod <- glm(death ~ dzclassN, 
           family = poisson(),
           offset = logtime,
           data = two) 
tidy(mod)
tidy(mod, exp = T, conf.int = T)
