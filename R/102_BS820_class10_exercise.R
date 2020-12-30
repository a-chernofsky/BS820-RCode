###########################################################################################################
#
# BS820 R Code: Lecture 10 In-class exercise Code
# Description: Proportional Hazards in Semi-parametric (Cox) Regression Models
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

zero <- read_excel("data/Oscar01.xlsx")
names(zero) <- tolower(names(zero))

one <- zero[zero$identity != 1075 & zero$identity != 1430, ]
one$age_at_end <- one$final - one$birth
one$age_at_first_film <- one$film - one$birth
one$age_at_first_nom <- one$nom - one$birth
one$age_at_first_win <- one$win - one$birth

one$yrs_first_film <- one$final - one$film

one$group <- factor(ifelse(one$wins >= 1, 1,
                      ifelse(one$noms, 2, 3)))

one$born_usa_n <- ifelse(one$born_usa == "Yes", 1, 
                         ifelse(one$born_usa == "No", 0, NA))

one$white_n <- ifelse(one$white == "Yes", 1, 
                         ifelse(one$white == "No", 0, NA))

one$name_change_n <- ifelse(one$name_change == "Yes", 1, 
                         ifelse(one$name_change == "No", 0, NA))

one$drama_n <- ifelse(one$drama == "Yes", 1, 
                            ifelse(one$drama == "No", 0, NA))

one$dead <- 1 - one$alive


sfit <- survfit(Surv(age_at_end, dead) ~ group, data = one)

plot(sfit)
ggsurvplot(sfit)
survdiff(Surv(age_at_end, dead) ~ group, data = one, 
         rho = 0)

cph1 <- coxph(Surv(age_at_end, dead) ~ group, 
             ties = "exact",
             data = one)
summary(cph1)


cph2 <- coxph(Surv(age_at_end, dead) ~ group + birth + male + white_n, 
              ties = "exact",
              data = one)
summary(cph2)
plot(cph2)
