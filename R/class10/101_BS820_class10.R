###########################################################################################################
#
# BS820 R Code: Lecture 10 Code
# Description: Proportional Hazards in Semi-parametric (Cox) Regression Models
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


# read in data ------------------------------------------------------------

brscan <- read.table("data/brscan.txt",
                     col.names = c("id", "age", "durat", "status"))


# fit cox models  -------------------------------------------------------------

cph1 <- coxph(Surv(durat, status) ~ age,
              data = brscan)
survfit(cph1)$surv

brscan$survprb1 <- predict(cph1, newdata= brscan, type = "survival")
brscan <- brscan[order(brscan$age), ]

survprb_age45 <- data.frame(age = 45, 
                            durat = c(0, brscan$durat[brscan$status == 1]),
                            surfun = c(1, predict(cph1, 
                                             newdata = data.frame(brscan[, -2], age = 45), 
                                             type = "survival")[brscan$status == 1]))
survprb_age45 <- survprb_age45[order(survprb_age45$durat),]

survprb_age60 <- data.frame(age = 60, 
                            durat = c(0, brscan$durat[brscan$status == 1]),
                            surfun = c(1, predict(cph1, 
                                                  newdata = data.frame(brscan[, -2], age = 60), 
                                                  type = "survival")[brscan$status == 1]))
survprb_age60 <- survprb_age60[order(survprb_age60$durat),]
survprb <- cbind(survprb_age45, survprb_age60)

plot(stepfun(survprb_age45$durat[-1], survprb_age45$surfun, right = T), 
     col = "red", ylim = c(0,1), xlim = c(0, 6277),
     main = "Survival functions for ages 45 and 60",
     xlab = "Duration",
     ylab = "Survival probability")
lines(stepfun(survprb_age60$durat[-1], survprb_age60$surfun, right = T), 
      col = "blue", lty = 2)
legend("bottomleft", legend = c("age = 45", "age = 60"), col = c("red", "blue"), lty = 1:2)

# read in data ------------------------------------------------------------

chemo <- read_excel("data/AMLChemo.xlsx")
names(chemo) <- tolower(names(chemo))


ggsurvplot(survfit(Surv(fupweeks, eventind) ~ chemo, data = chemo))

survdiff(Surv(fupweeks, eventind) ~ chemo, data = chemo)

# PHreg models ------------------------------------------------------------

cph2 <- coxph(Surv(fupweeks, eventind) ~ chemo, 
              ties = "exact",
              data = chemo)

summary(cph2)
tidy(cph2)

surv_adjustedcurves(cph1, data = chemo, variable = "chemo")
save <- survfit(cph1)
plot(survfit(cph1))


survprb_chemo0 <- data.frame(chemo = 0, 
                            fupweeks = c(0, chemo$fupweeks[chemo$eventind == 1]),
                            surfun = c(1, predict(cph2, 
                                                  newdata = data.frame(chemo[, -2], chemo = 0), 
                                                  type = "survival")[chemo$eventind == 1]))
survprb_chemo0 <- survprb_chemo0[order(survprb_chemo0$fupweeks),]

survprb_chemo1 <- data.frame(chemo = 1, 
                             fupweeks = c(0, chemo$fupweeks[chemo$eventind == 1]),
                             surfun = c(1, predict(cph2, 
                                                   newdata = data.frame(chemo[, -2], chemo = 1), 
                                                   type = "survival")[chemo$eventind == 1]))
survprb_chemo1 <- survprb_chemo1[order(survprb_chemo1$fupweeks),]

plot(stepfun(unique(survprb_chemo1$fupweeks)[-1], unique(survprb_chemo1$surfun), right = T), 
     col = "blue", 
     ylim = c(0,1),
     xlim = c(0, 50),
     main = "Survival functions by chemotherapy status",
     xlab = "Follow-up (weeks)",
     ylab = "Survival probability", 
     lty = 2)
lines(stepfun(unique(survprb_chemo0$fupweeks)[-1], unique(survprb_chemo0$surfun), right = T), 
      col = "red")
legend("bottomleft", legend = c("chemo = 1", "chemo = 0"), col = c("blue", "red"), lty = 2:1)

# read in data ------------------------------------------------------------

recid <- read_sas("data/recidiv_cls10.sas7bdat")
names(recid) <- tolower(names(recid))


# cox models --------------------------------------------------------------

cph3 <- coxph(Surv(week, arrest) ~ fin + age + prio, 
              ties = "exact",
              data = recid,
              id = id)

summary(cph3)
tidy(cph3)

recid$dresid <- residuals(cph3, type = "deviance")

boxplot(dresid ~ fin, boxwex = 0.1, data = recid,
        main = "Deviance Residual by Financial Aid",
        ylab = "Deviance residual",
        xlab = "Financial aid", 
        col = "red",
        border = "blue")

palette(c("blue", "red"))
plot(recid$age, recid$dresid, 
     col = factor(recid$arrest),
     main = "Deviance residual by age and recidivism",
     xlab = "Age in years",
     ylab = "Deviance residual")
legend("top", legend = c("Arrest = 1", "Arrest = 0"), col = c("red", "blue"), pch = 1)

palette(c("blue", "red"))
plot(recid$prio, recid$dresid, 
     col = factor(recid$arrest),
     main = "Deviance residual by number of prior arrests and recidivism",
     xlab = "Number of prior arrests",
     ylab = "Deviance residual")
legend("topright", legend = c("Arrest = 1", "Arrest = 0"), col = c("red", "blue"), pch = 1)

