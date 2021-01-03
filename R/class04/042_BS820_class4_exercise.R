###########################################################################################################
#
# BS820 R Code: Lecture 4 Code in class exercise
# Description: Numerical Problems & Variable Selection
# Dataset: data/compas2year.csv
# created: 11/11/2020
#
############################################################################################################

# install/load packages ---------------------------------------------------
if (!require('readxl')) install.packages('readxl') 
if (!require('broom')) install.packages('broom') 
if (!require('epitools')) install.packages('epitools') 
if (!require('logistf')) install.packages('logistf') 
if (!require('pwr')) install.packages('pwr') 

library(readxl)
library(broom)
library(epitools)
library(logistf)
library(pwr)


# read in data ------------------------------------------------------------

compas2yr <- read.csv("data/compas2year.csv")

compas2yr$sex <- factor(compas2yr$sex, levels = c("Male", "Female"))
compas2yr$age_cat <- factor(compas2yr$age_cat, 
                            levels = c("25 - 45", "Greater than 45", "Less than 25"))
compas2yr$c_charge_degree <- factor(compas2yr$c_charge_degree, levels = c("F", "M"))
# 1. combine Asians and Native Americans with other -----------------------

compas2yr$race2 <- ifelse(compas2yr$race == "Asian" | compas2yr$race == "Native American", 
                          "Other",
                          compas2yr$race)
compas2yr$race2 <- factor(compas2yr$race2, c("Caucasian", "Other", "African-American", "Hispanic"))

compas2yr$score_level <- ifelse(compas2yr$score_text == "High", 1, 0)
# 2. Recidivism in each race category ------------------------------------------

tapply(compas2yr$is_recid, compas2yr$race2, function(x)sum(x)/length(x))


# 3. Recidivism by COMPAS score ----------------------------------------------


tapply(compas2yr$is_recid, compas2yr$score_level, function(x)sum(x)/length(x))


# 4. Fit logistic regression model ----------------------------------------

logreg <- glm(score_level ~ sex + age_cat + race2 + priors_count + c_charge_degree + is_recid,
              family = binomial(),
              data = compas2yr)
tidy(logreg)
tidy(logreg, exp = T, conf.int = T)
