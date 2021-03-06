###########################################################################################################
#
# BS820 R Code: Lecture 8 Code
# Description: Nonparametric Comparisons of Survival in 2 or More Groups
# Dataset: 
# created: 11/24/2020
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


# read in data ------------------------------------------------------------

one <- read_excel("data/AMLChemo.xlsx")
names(one) <- tolower(names(one))

two <- one[which(one$chemo == 0),]

#survival fit object
sfit1 <- survfit(Surv(fupweeks, eventind) ~ 1, data = two)

#liftable
tab <- summary(sfit1, 
        times = c(0, 9, 19, 29, 39, 49, 59), censored = T)
tab

#survival function plot
plot(tab$time, tab$surv)
lines(tab$time, tab$surv)

#KM estimator
summary(sfit1)

# Kaplan-Meier Estimator --------------------------------------------------

#Kaplan Meier table
summary(sfit1)

#KM Curve with survival package
plot(sfit1, 
     conf.int = F,
     xlab = "FUpWeeks", 
     ylab = "Survival Probability", 
     main = "Product-Limit Survival Estimate")

#KM Curve with survminer package
ggsurvplot(sfit, 
           xlab = "FUpWeeks",
           conf.int = F)

#-log survival function
plot(sfit1, 
     conf.int = F,
     cumhaz = T,
     xlab = "FUpWeeks", 
     ylab = "-log(S(t))", 
     main = "Cumalive Hazard Function")

#c-log-log plot
plot(survfit(Surv(fupweeks, eventind) ~ 1, data = two), 
     conf.int = F,
     xlab = "FUpWeeks", 
     ylab = "Survival Probability", 
     main = "Product-Limit Survival Estimate",
     fun = "cloglog")

#smoothed hazard function
haz <- muhaz(times = two$fupweeks, delta = two$eventind, 
                 bw.smooth = 25,
                 bw.method = "global",
                 kern = "epanechnikov", max.time = 45)
plot(haz)


# Nonparametric Comparisons of Survival of 2 Groups --------------------

#survival fits by chemotherapy treatment group
sfit2 <- survfit(Surv(fupweeks, eventind) ~ chemo, data = one)
summary(sfit2)

#stratified KM curves
plot(sfit2)
ggsurvplot(sfit2)

#test differences in survival curves: 
#for log rank test set rho = 0
survdiff(Surv(fupweeks, eventind) ~ chemo,
         rho = 0,
         data = one)
#for Wilcoxan set rho = 1
survdiff(Surv(fupweeks, eventind) ~ chemo,
         rho = 1,
         data = one)

# Nonparametric Comparisons of Survival of > 2 Groups --------------------

support <- read_sas("data/support.sas7bdat")

#survival curves by disease class
sfit3 <- survfit(Surv(d_time, death) ~ dzclass, data = support)

#stratified survival curves by disease class
ggsurvplot(sfit3, risk.table = T)

#log rank test
survdiff(Surv(d_time, death) ~ dzclass, 
         rho = 0,
         data = support)
#Wilocoxon test
survdiff(Surv(d_time, death) ~ dzclass, 
         rho = 1,
         data = support)

#cumulative hazards plot
ggsurvplot(sfit3, fun = "cumhaz")
#clog log plots
ggsurvplot(sfit3, fun = "cloglog")

#survival curves by sex
sfit4 <- survfit(Surv(d_time, death) ~ sex, 
                 data = support,
                 subset = dzclass == 2)

ggsurvplot(sfit4, risk.table = T)

#log rank test
survdiff(Surv(d_time, death) ~ sex, 
         rho = 0,
         data = support)
#wilcoxon test
survdiff(Surv(d_time, death) ~ sex, 
         rho = 1,
         data = support)

#cumulative hazards plot by sex
ggsurvplot(sfit3, fun = "cumhaz")
#clog log plot by sex
ggsurvplot(sfit3, fun = "cloglog")
