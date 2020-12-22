###########################################################################################################
#
# BS820 R Code: Lecture 3 Hands on Exercise Code 
# Description: Non-linear predictors & Model fit
# Dataset: data/Arrests.csv
# created: 10/25/2020
#
############################################################################################################

if (!require('readxl')) install.packages('readxl') 
if (!require('broom')) install.packages('broom') 
if (!require('epitools')) install.packages('epitools') 
if (!require('car')) install.packages('car') 
if (!require('haven')) install.packages('haven') 

library(readxl)
library(broom)
library(epitools)
library(car)
library(haven)

# read in data ------------------------------------------------------------

zero <- read_sas("data/Arrests.sas7bdat")

one <- zero

one$releasedN <- ifelse(one$released == "Yes", 0,
                        ifelse(one$released == "No", 1, NA))
one$colorN <- ifelse(one$colour == "White", 0,
                     ifelse(one$colour == "Black", 1, NA))
one$sexN <- ifelse(one$sex == "Female", 0,
                   ifelse(one$sex == "Male", 1, NA))
one$employedN <- ifelse(one$employed == "Yes", 0,
                        ifelse(one$employed == "No", 1, NA))
one$citizenN <- ifelse(one$citizen == "Yes", 0, 
                       ifelse(one$citizen == "No", 1, NA))

one$checks <- factor(one$checks, 
                     levels = c("0", "1", "2", "3","4", "5", "6"))

# null model ----------------------------------------------------------

null <- glm(releasedN ~ 1, 
            family = binomial(),
            data = one)

# flexible model ----------------------------------------------------------

flex <- glm(releasedN ~ colorN + checks + colorN*checks, 
            family = binomial(),
            data = one)

# restricted model --------------------------------------------------------

rest <- glm(releasedN ~ colorN + checks, 
            family = binomial(),
            data = one)


# Question 2 --------------------------------------------------------------
anova(flex)

logLik(flex)
