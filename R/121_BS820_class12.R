###########################################################################################################
#
# BS820 R Code: Lecture 12 Code
# Description: Competing risks
# Dataset: 
# created: 12/19/2020
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
if (!require('cmprsk')) install.packages('cmprsk')

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
library(survAUC)
library(rms)
library(splines)
library(cmprsk)


# Explained variation ---------------------------------------------------------

support <- read_sas("data/support.sas7bdat")

support$dzclass <- factor(support$dzclass, levels = c(2, 1, 3, 4))

cph1 <- coxph(Surv(d_time, death) ~ dzclass, ties = "exact", data = support)

cph2 <- coxph(Surv(d_time, death) ~ dzclass + sex + race +age , ties = "exact", data = support)
cox.zph(cph2)

cph3 <- coxph(Surv(d_time, death) ~ dzclass + sex + race +age + ns(meanbp,3) , ties = "exact", data = support)
cph3

cph4 <- coxph(Surv(d_time, death) ~ strata(dzclass) + sex + race +age + ns(meanbp,3) , 
              ties = "exact", 
              data = support)
summary(cph4)


# Competing risks ---------------------------------------------------------

one <- data.frame(followup = c(1, 6, 8, 13, 15, 17, 30, 33, 34, 37),
                  event = c(1, 2, 2, 1, 2, 1, 1, 2, 0, 2))

one$firste <- ifelse(one$event > 0, 1, 0)
one$event1 <- ifelse(one$event == 1, 1, 0)
one$event2 <- ifelse(one$event == 2, 1, 0)

sfit_firste <- survfit(Surv(followup, firste)~ 1, data = one)
summary(sfit_firste)
plot(sfit_firste)

sfit_e1 <- survfit(Surv(followup, event1)~ 1, data = one)
summary(sfit_e1)
plot(sfit_e1)

sfit_e2 <- survfit(Surv(followup, event2)~ 1, data = one)
summary(sfit_e2)
plot(sfit_e2)


sfit_cr <- survfit(Surv(followup, factor(event))~ 1, data = one)
sfit_cr$transitions
summary(sfit_cr)

plot(sfit_cr, col=c(1,2), lty=c(1,1),
     mark.time=T, lwd=2, xscale=12,
     xlab="time", ylab="Probability in State")
legend("topleft", .6, c("event 1", "event 2"), col = c(1,2), lty = c(2,2))


leaders <- read.table("data/leaders.txt",
                      col.names = c("years", "lost", "manner", "start", "military", "age",
                                    "conflict", "loginc", "growth", "pop", "land",
                                    "literacy", "region"),
                      na.strings = ".")
leaders$lost2 <- ifelse(leaders$lost == 0 | leaders$lost == 2, 0, leaders$lost)

cmprsk <- cuminc(leaders$years, leaders$lost2, group =leaders$manner)
cmprsk$Tests

plot(cmprsk, color = c(1,1,2,2))

leaders$lostconst <- ifelse(leaders$lost == 1, 1, 0)

cph_lost1 <- coxph(Surv(years, lostconst) ~ manner + age + start + military + 
                   conflict + loginc + literacy + factor(region),
                 ties = "exact",
                 data = leaders)
summary(cph_lost1)

X <- model.matrix(cph_lost1)
cph_cr1 <- crr(leaders$years[-which(is.na(leaders$age))], 
               leaders$lost2[-which(is.na(leaders$age))], X, failcode=1, cencode=0)
summary(cph_cr1)

cph_cr3 <- crr(leaders$years[-which(is.na(leaders$age))], 
               leaders$lost2[-which(is.na(leaders$age))], X, failcode=3, cencode=0)
summary(cph_cr3)

