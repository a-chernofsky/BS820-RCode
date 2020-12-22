###########################################################################################################
#
# BS820 R Code: Lecture 7 Code
# Description: Event Rates and Time-to-Event
# Dataset: 
# created: 11/22/2020
#
############################################################################################################

# install/load packages ---------------------------------------------------

if (!require('haven')) install.packages('haven') 
if (!require('broom')) install.packages('broom') 
if (!require('epitools')) install.packages('epitools') 
if (!require('survival')) install.packages('survival')
if (!require('MASS')) install.packages('MASS')
if (!require('car')) install.packages('car')
if (!require('geepack')) install.packages('geepack')
if (!require('muhaz')) install.packages('muhaz')
if (!require('survminer')) install.packages('survminer')

library(haven)
library(broom)
library(epitools)
library(survival)
library(MASS)
library(car)
library(geepack)
library(muhaz)
library(survminer)

# Read in data ------------------------------------------------------------

first <- read_sas("data/hyfirst.sas7bdat")
names(first) <- tolower(names(first))
first$logdays <- log(first$stopday)

all_day <- c(n = sum(!is.na(first$stopday)), mean = mean(first$stopday), sum = sum(first$stopday))
all_event <- c(n = sum(!is.na(first$event)), mean = mean(first$event), sum = sum(first$event))

preg1 <- glm(event ~ 1, family = poisson(), offset = logdays, data = first)
tidy(preg1, conf.int = T)
glance(preg1)

group_day <- tapply(first$stopday, first$intgroup, 
                    function(x)c(n = sum(!is.na(x)), mean = mean(x), sum = sum(x)))
group_event <- tapply(first$event, first$intgroup, function(x)c(n = sum(!is.na(x)), mean = mean(x), sum = sum(x)))

groupevents <- c(all_event[3], group_event$`0`[3], group_event$`1`[3])
grouptime <- c(all_day[3], group_day$`0`[3], group_day$`1`[3])
lambda <- groupevents/grouptime
meansrv <- 1/lambda
neg2lgl <- -2*(groupevents*log(lambda) - lambda*grouptime)

group_lik <- data.frame(intgroup = c(-1, 0, 1), 
                        groupevents,
                        grouptime, 
                        lambda,
                        meansrv,
                        neg2lgl)
group_lik

wholen2ll <- group_lik$neg2lgl[is.na(group_lik$intgroup)]
groupn2ll <- sum(group_lik$neg2lgl[!is.na(group_lik$intgroup)])
dif_n2ll <- wholen2ll - groupn2ll
p_value <- pchisq(dif_n2ll, 3, lower.tail = F)
c(wholen2ll = wholen2ll, groupn2ll = groupn2ll, dif_n2ll = dif_n2ll, p_value = p_value)

preg2 <- glm(event ~ intgroup, family = poisson(), offset = logdays, data = first)
tidy(preg2, conf.int = T)
glance(preg2)
Anova(preg2)

muhaz_0 <- muhaz(first$stopday, first$event, 
                 subset = first$intgroup == 0, 
                 bw.smooth = 200,
                 bw.method = "global",
                 kern = "epanechnikov")
muhaz_1 <- muhaz(first$stopday, first$event, 
                 subset = first$intgroup == 1, 
                 bw.smooth = 200,
                 bw.method = "global",
                 kern = "epanechnikov")
plot(muhaz_1, lty = 2)
lines(muhaz_0)
legend("topright", legend = c("treatment subjects", "control subjects"))


