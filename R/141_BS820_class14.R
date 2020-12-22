###########################################################################################################
#
# BS820 R Code: Lecture 14 Code
# Description: Repeated Events
# Dataset: 
# created: 12/20/2020
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


library(readxl)
library(haven)
library(broom)
library(epitools)
library(survival)
library(MASS)
library(car)
library(geepack)


# GEE ---------------------------------------------------------------------

ptsd01 <- read.table("data/ptsd.txt", 
                     col.names = c("subjid", "ptsd", "control", 
                                   "problems", "sevent", "cohes", "time"))
#outcome by time
table(ptsd01$ptsd, ptsd01$time)

#logistic regression model without repeated events
logreg <- glm(ptsd ~ control + problems + sevent + cohes,
              family = binomial(),
              data = ptsd01,
              subset = which(time == 1))
summary(logreg)

#GEE model with time interaction
gee1 <- geeglm(ptsd ~ factor(time) + control + problems + sevent + cohes + factor(time)*problems,
               family = binomial(),
               data = ptsd01,
               id = subjid,
               corstr = "unstructured",
               waves = time)
summary(gee1)

#GEE model without interaction
gee2 <- geeglm(ptsd ~ factor(time) + control + problems + sevent + cohes,
               family = binomial(),
               data = ptsd01,
               id = subjid,
               corstr = "unstructured",
               waves = time)
summary(gee2)

# repeated events with a survival outcome ---------------------------------

#read in data
hyevents <- read_sas("data/hyevents.sas7bdat")
names(hyevents) <- tolower(names(hyevents))

### Data wrangling for repeated measure survival model ###

two <- hyevents
#recode control group as 0
two$intgroup <- ifelse(two$intgroup == 2, 0, two$intgroup) 

#remove observations with missing evnum and ftime
two <- two[!is.na(two$evnum),]

#set event to 0 if event number is 0
two$event[two$evnum == 0] <- 0

#create startday variable as the lag of the event time
two$startday <-  c(NA, two$etime[-nrow(two)])
two$startday[which(!duplicated(two$patient))] <- NA
two$startday[is.na(two$startday)] <- 0

#create stopday variable as modified event time (if event = 0 then use follow up time)
two$stopday <- two$etime
two$stopday[is.na(two$stopday)] <- ifelse(two$evnum[is.na(two$stopday)] == 0, two$ftime[is.na(two$stopday)],
                                          two$etime[is.na(two$stopday)])

#sort data
two <- two[order(two$patient, two$evnum),]

#add an additional row for time after event
last <- two[cumsum(table(two$patient)), ]
last <- last[!is.na(last$etime),]
last$event <- 0
last$startday <- last$etime
last$stopday <- last$ftime

two <- rbind(two, last)
two <- two[order(two$patient, two$evnum),]

#create gaptime and priorgap variables using lag
two$gaptime <- two$stopday - two$startday
two$priorgap <- c(NA, two$gaptime[-nrow(two)])
two$priorgap[which(!duplicated(two$patient))] <- NA

###Cox Models###

#model first event
cph1 <- coxph(Surv(stopday, event) ~ intgroup, 
              ties = "breslow",
              data = two, 
              subset = which(!duplicated(patient)))
summary(cph1)

#model second event
cph2 <- coxph(Surv(gaptime, event) ~ intgroup, 
              data = two, 
              subset = which(((evnum == 1) & (event == 0))| ((evnum == 2) & (event == 1))),
              ties = "breslow")
summary(cph2)

cph3 <- coxph(Surv(gaptime, event) ~ intgroup + priorgap, 
              data = two, 
              subset = which(((evnum == 1) & (event == 0))| ((evnum == 2) & (event == 1))),
              ties = "breslow")
summary(cph3)

#model with repeated measures, robust = T gives sandwich estimator for score statistics
cph4 <- coxph(Surv(gaptime, event) ~ intgroup, 
              robust = T,
              data = two, 
              id = patient,
              ties = "breslow")
summary(cph4)
