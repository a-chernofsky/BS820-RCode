###########################################################################################################
#
# BS820 R Code: Lecture 11 In-class exercise Code
# Description: Time varying covariates & PH assumption
# Dataset: 
# created: 12/27/2020
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

one$win_time <- 0
last_win_time <- one[!is.na(one$age_at_first_win),]

save <- coxph(Surv(age_at_end, alive == 1) ~ tt(age_at_first_win) + male + born_usa_n + 
      white_n + name_change_n + drama_n + birth, iter.max = 100, data = one, 
      tt = function(age_at_first_win, age_at_first_nom, t, ...) {
          win_time <- ifelse(is.na(age_at_first_win), 0,
                            ifelse(age_at_first_win > t, 0, 1))
          nom_time <- ifelse(is.na(age_at_first_nom), 0,
                            ifelse(age_at_first_nom > t, 0, 1))
          cbind(win_time = win_time, nom_time = nom_time)
        })

coxph(Surv(age_at_end, alive == 1) ~ tt(age_at_first_nom) + male + born_usa_n + 
        white_n + name_change_n + drama_n + birth, data = one, 
      tt = function(age_at_first_win, age_at_first_nom, t, ...) {
        win_time <- ifelse(is.na(age_at_first_win) | age_at_first_win > t, 0, 1)
        nom_time <- ifelse(is.na(age_at_first_nom) | age_at_first_nom > t, 0, 1)
        cbind(win_time = win_time, nom_time = nom_time)
      })

coxph(Surv(age_at_end, alive == 1) ~ tt(age_at_first_win) + tt(age_at_first_nom) + male + born_usa_n + 
        white_n + name_change_n + drama_n + birth, data = one, 
      iter.max = 200,
      tt = function(x, t, ...) {
        ifelse(is.na(x) | x > t, 0, 1)
      })

two <- one
two$time0 <- 0
two$time1 <- ifelse(is.na(two$age_at_first_win) & is.na(two$age_at_first_nom), 
                    two$age_at_end, 
                    pmax(two$age_at_first_nom, two$age_at_first_win, na.rm = T))
two$alive1 <- ifelse(is.na(two$age_at_first_win) & is.na(two$age_at_first_nom), 
                    two$alive, 
                    1)
two$nom1 <- 0
two$win1 <- 0

last <- two[!is.na(two$age_at_first_win) | !is.na(two$age_at_first_nom),]
last$time0 <- pmax(last$age_at_first_nom, last$age_at_first_win, na.rm = T)
last$time1 <- last$age_at_end
last$alive1 <- last$alive
last$nom1 <- ifelse(is.na(last$age_at_first_nom), 0, 1)
last$win1 <- ifelse(is.na(last$age_at_first_win), 0, 1)


two <- rbind(two, last)
two <- two[order(two$identity),]

one$alive.0 <- ifelse(is.na(one$age_at_first_nom), one$alive, 1)
one$alive.1 <- ifelse(is.na(one$age_at_first_nom), NA, pmax(one$alive, 1))
one$alive.2 <- ifelse(is.na(one$age_at_first_win), NA, one$alive)

one <- as.data.frame(one)
three <- reshape(one,varying = c("alive.0", "alive.1", "alive.2"), direction = "long")
three <- three[order(three$id),]

coxph(Surv(time0, time1, alive1 == 1) ~ nom1 + win1 + male + born_usa_n + 
        white_n + name_change_n + drama_n + birth, data = two)
cut.points <- unique(wins$age_at_end[wins$alive == 1])
SURV2 <- survSplit(data = wins,  end = "age_at_end", cut = cut.points,
                    start = "time0", event = "alive")
