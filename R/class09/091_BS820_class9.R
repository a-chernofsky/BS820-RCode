###########################################################################################################
#
# BS820 R Code: Lecture 9 Code
# Description: Parametric regression models
# Dataset: 
# created: 11/26/2020
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

#I converted the .txt data into a SAS dataset
recid <- read_sas("data/recidiv.sas7bdat")
recid$educ_num <- recid$educ
recid$educ <- factor(recid$educ, levels = c(6, 2, 3, 4, 5))
recid$logweeks <- log(recid$week)


# The exponential AFT model -----------------------------------------------

aft_exp <- survreg(Surv(week,arrest) ~ fin + age + race + wexp + mar + 
                     paro + prio + educ,
                   dist = "exponential",
                   data = recid)
summary(aft_exp)


# Poisson model -----------------------------------------------------------

preg <- glm(arrest ~ fin + age + race + wexp + mar + 
              paro + prio + educ,
            family =  poisson(),
            offset = logweeks,
            data = recid)
summary(preg)

# The Weibull AFT model -----------------------------------------------

aft_wei <- survreg(Surv(week,arrest) ~ fin + age + race + wexp + mar + 
                     paro + prio + educ,
                   dist = "weibull",
                   data = recid)
summary(aft_wei)


# The log normal AFT model -----------------------------------------------

aft_logn <- survreg(Surv(week,arrest) ~ fin + age + race + wexp + mar + 
                     paro + prio + educ,
                   dist = "lognormal",
                   data = recid)
summary(aft_logn)

# The log normal AFT model -----------------------------------------------

aft_ggam <- flexsurvreg(Surv(week,arrest) ~ fin + age + race + wexp + mar + 
                      paro + prio + educ,
                    dist = "gengamma",
                    data = recid)
aft_ggam


# likelihood ratio test ---------------------------------------------------

#lrt test for whole set of independent variables

lrt <- -2*(aft_wei$loglik[1] - aft_wei$loglik[2])
pchisq(lrt, df = 11, lower.tail = F)

#lrt test for age coefficient 

m_age <- survreg(Surv(week,arrest) ~ fin + age + race + wexp + mar + 
                      paro + prio + educ,
                    dist = "weibull",
                    data = recid)
m_noage <- survreg(Surv(week,arrest) ~ fin + race + wexp + mar + 
                   paro + prio + educ,
                 dist = "weibull",
                 data = recid)

lrt_age <- -2*(m_noage$loglik[2] - m_age$loglik[2])
pchisq(lrt_age, 1, lower.tail = F)

#lrt for merging 2 education categories

recid$educ2 <- ifelse(recid$educ_num == 2 | recid$educ_num == 3, 1,
                      recid$educ_num - 2)
recid$educ2 <- factor(recid$educ2)

m_educ4 <- survreg(Surv(week,arrest) ~ fin + age + race + wexp + mar + 
                     paro + prio + educ2,
                   dist = "weibull",
                   data = recid)

lrt_educ <- -2*(m_educ4$loglik[2] - aft_wei$loglik[2])
pchisq(lrt_educ, df = 1, lower.tail = F)


# LRT for Distribution ----------------------------------------------------
#log likelihood for generalized gamma model
ll_ggam <- aft_ggam$loglik
#log likelihood for weibull model
ll_wei <- aft_wei$loglik[2]
#log likelihood for exponential model
ll_exp <- aft_exp$loglik[2]
#log likelihood for log normal model
ll_logn <- aft_logn$loglik[2]

#Weibull vs. Generalized Gamma
-2*(ll_wei - ll_ggam)
pchisq(-2*(ll_wei - ll_ggam), 1, lower.tail = F)

#Exponential vs. Generalized Gamma
-2*(ll_exp - ll_ggam)
pchisq(-2*(ll_exp - ll_ggam), 1, lower.tail = F)

#log normal vs. Generalized Gamma
-2*(ll_logn - ll_ggam)
pchisq(-2*(ll_logn - ll_ggam), 1, lower.tail = F)


# Problems with Maximum Likelihood Estimation for AFTs --------------------

recid2 <- recid

recid2$arrest <- ifelse(recid2$fin == 1, 0, recid2$arrest)

aft_exp2 <- survreg(Surv(week,arrest) ~ fin,
                   dist = "exponential",
                   data = recid2)
summary(aft_exp2)

