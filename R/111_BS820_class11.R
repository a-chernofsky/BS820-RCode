###########################################################################################################
#
# BS820 R Code: Lecture 11 Code
# Description: Functional form, time varying covariates
# Dataset: 
# created: 11/29/2020
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

# Functional form ---------------------------------------------------------

recid <- read_sas("data/recidiv_cls10.sas7bdat")

cph1 <- coxph(Surv(week, arrest) ~ fin + age + prio, 
              ties = "exact",
              data = recid,
              id = id)

ggcoxdiagnostics(fit = cph1,
         points.color = "blue",
         type = "schoenfeld",
         data = recid)

ggcoxfunctional(fit = cph1,
                points.color = "blue",
                point.shape = 1,
                data = recid)


# time varying covariates -------------------------------------------------

stan <- read.table("data/stan.txt",
                   col.names = c("dob", "doa", "dot", "dls",
                                 "id", "age", "dead", "dur", 
                                 "surg", "trans", "wtime", 
                                 "m1", "m2", "m3", "reject"),
                   na.strings = ".")

stan$dob <- as.Date(format(as.Date(stan$dob,format="%m/%d/%y"), "19%y%m%d"), "%Y%m%d")
stan$doa <- as.Date(format(as.Date(stan$doa,format="%m/%d/%y"), "19%y%m%d"), "%Y%m%d")
stan$dot <- as.Date(format(as.Date(stan$dot,format="%m/%d/%y"), "19%y%m%d"), "%Y%m%d")
stan$dls <- as.Date(format(as.Date(stan$dls,format="%m/%d/%y"), "19%y%m%d"), "%Y%m%d")

stan$surv1 <- stan$dls - stan$doa
stan$surv2 <- stan$dls - stan$dot
stan$ageaccpt <- (stan$doa-stan$dob)/365.25
stan$agetrans <- (stan$dot-stan$dob)/365.25
stan$wait <- stan$dot - stan$doa
stan$trans <- ifelse(is.na(stan$dot), 0, 1)

tapply(stan$surv1, stan$trans, function(x)c(N = length(x), sum =  sum(x)))
tapply(stan$dead, stan$trans, function(x)c(N = length(x), sum =  sum(x)))


cph2 <- coxph(Surv(surv1, dead) ~ trans + surg + ageaccpt, 
              ties = "exact",
              data = stan)

# stan$time0 <- 0
# stan$time1 <- ifelse(is.na(stan$dot), pmax(0, stan$dls - stan$doa),
#                      pmax(0, (stan$dot -1) - stan$doa))
# stan$time2 <- ifelse(is.na(stan$dot), NA, pmax(0, stan$dls - (stan$doa-1)))
# stan$futime <- ifelse(is.na(stan$dot), stan$time1,
#                       stan$time2)


tdata <- with(stan, data.frame(id = id,
                               futime = pmax(.5, dls - doa),
                               txtime = ifelse(dot == dls,
                                              (dot - doa) -.5,
                                              (dot - doa)),
                               dead = dead))

tdata <- with(stan, data.frame(id = id,
                               futime = pmax(.5, dls - doa),
                               txtime = ifelse(is.na(dot),
                                               (dls - doa) -.5,
                                               (dot - doa)),
                               dead = dead))

data.frame(subject = subject,
           futime= pmax(.5, fu.date - accept.dt),
           txtime= ifelse(tx.date == fu.date,
                          (tx.date -accept.dt) -.5,
                          (tx.date - accept.dt)),
           fustat = fustat
))

sdata <- tmerge(stan, tdata, id=id,
                death = event(futime, dead),
                plant = tdc(txtime),
                options= list(idname="id"))

attr(sdata, "tcount")

cph3 <- coxph(Surv(tstart, tstop, death) ~ plant + surg + ageaccpt, 
              ties = "exact",
              data = sdata)
cph3$coefficients

coxph(Surv(start, stop, event) ~ transplant + surgery + year, 
      ties = "exact",
      data = heart)
