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

cph3 <- coxph(Surv(surv1, dead) ~ surg + ageaccpt + tt(wait), 
              ties = "exact",
              iter.max = 500,
              data = stan, tt = function(x, t, ...) ifelse(x>t | is.na(x), 0, 1) )

coxph(Surv(surv1, dead) ~ surg + ageaccpt + tt(wait), 
      ties = "exact",
      iter.max = 500,
      data = stan, tt = function(x, t, ...) ifelse(x <= t, 1, 0) )

# stan$time0 <- 0
# stan$time1 <- ifelse(is.na(stan$dot), pmax(0, stan$dls - stan$doa),
#                      pmax(0, (stan$dot -1) - stan$doa))
# stan$time2 <- ifelse(is.na(stan$dot), NA, pmax(0, stan$dls - (stan$doa-1)))
# stan$futime <- ifelse(is.na(stan$dot), stan$time1,
#                       stan$time2)

stan2 <- stan
stan2$time0 <- 0
stan2$time1 <- ifelse(is.na(stan2$dot), pmax(0, stan2$dls - stan2$doa), 
                      pmax(0, (stan2$dot - 1) - stan2$doa))
stan2$plant <- 0
stan2$dead1 <- ifelse(is.na(stan2$dot), stan2$dead, 0)

last <- stan2[!is.na(stan2$dot),]
last$time0 <- pmax(0, (last$dot - 1) - last$doa)
last$time1 <- pmax(0, last$dls - (last$doa - 1))
last$plant <- 1
last$dead1 <- last$dead

stan2 <- rbind(stan2, last)
stan2 <- stan2[order(stan2$id),]
stan2$fudurat <- stan2$time1 - stan2$time0

tapply(stan2$fudurat, stan2$plant, function(x) c(n = length(x), sum = sum(x)))
tapply(stan2$dead1, stan2$plant, function(x) c(n = length(x), sum = sum(x)))

cph3 <- coxph(Surv(time0, time1, dead1) ~ plant + surg + ageaccpt, 
              ties = "exact",
              cluster = id,
              data = stan2)
cph3$coefficients

coxph(Surv(start, stop, event) ~ transplant + surgery + year, 
      ties = "exact",
      data = heart)


# recidivism data ---------------------------------------------------------

recid <- read_sas("data/recidiv_cls10.sas7bdat")

recid <- as.data.frame(recid)

recid_long <- reshape(data = recid,
                      varying = paste0("emp", 1:52),
                      v.names = "employed",
                      timevar = "time",
                      idvar = "id",
                      direction = "long")
recid_long <- recid_long[order(recid_long$id),]


recid_long2 <- recid_long[!is.na(recid_long$employed),]
recid_long2$time0 <- c(NA, recid_long2$time[-nrow(recid_long2)])
recid_long2$time0[which(!duplicated(recid_long2$id))] <- NA
recid_long2$time0[is.na(recid_long2$time0)] <- 0
recid_long2$time1 <- recid_long2$time

recid_long2$event[cumsum(recid$week)] <- recid$arrest
recid_long2$event[-cumsum(recid$week)] <- 0


recid_cph <-  coxph(formula = Surv(time0, time1, event) ~ fin + age + race + wexp + mar + paro + prio + employed,
                    data = recid_long2,
                    id = id,
                    ties    = "efron")
summary(recid_cph)



# Detecting non-PH --------------------------------------------------------

support <- read_excel("data/support.xlsx")

#create id variable
support$id <- 1:nrow(support)

#create numeric sex variable
support$sexn <- ifelse(support$sex == "male", 0, 
                   ifelse(support$sex == "female", 1, NA))

support$dzclassn <- ifelse(support$dzclass == 'ARF/MOSF', 1,
                           ifelse(support$dzclass == 'COPD/CHF/Cirrhosis', 2,
                                  ifelse(support$dzclass == 'Cancer', 3,
                                         ifelse(support$dzclass == 'Coma', 4, NA))))
support$dzclassn <- factor(support$dzclassn)
dzclass_dummy <- model.matrix(~-1 + dzclassn, data = support)
support <- cbind(support, dzclass_dummy)

survdiff(Surv(d.time, death) ~ dzclass, data = support)
plot(survfit(Surv(d.time, death) ~ dzclass, data = support), fun = "cloglog")

cph_not <- coxph(Surv(d.time, death) ~  dzclassn2+ dzclassn3 + dzclassn4 + 
                  sexn + age + meanbp + hrt + sod +
                  temp + crea + wblc + num.co, 
                data = support)
cph_t <- coxph(Surv(d.time, death) ~  dzclassn2+ dzclassn3 + dzclassn4 + 
                 tt(dzclassn2)+ tt(dzclassn3) + tt(dzclassn4) + 
                  sexn + age + meanbp + hrt + sod +
                  temp + crea + wblc + num.co, 
                data = support, 
                tt = function(x, t, ...)x*log(t))
anova(cph_t1, cph_t)
cph_not$linear.predictors

df <- length(cph_t$coefficients) - length(cph_not$coefficients)
pchisq(2*(cph_t$loglik - cph_not$loglik)[2], df = df, lower= F)

cph_str <- coxph(Surv(d.time, death) ~  strata(dzclassn) + 
                   sexn + age + meanbp + hrt + sod +
                   temp + crea + wblc + num.co,
                 data = support)
