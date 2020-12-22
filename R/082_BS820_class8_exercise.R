###########################################################################################################
#
# BS820 R Code: Lecture 8 Code
# Description: Nonparametric Comparisons of Survival in 2 or More Groups
# Dataset: 
# created: 11/25/2020
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

cnames <- c("week", "arrest", "fin", "age", "race", 
            "wexp", "mar", "paro", "prio", "educ", 
            paste0("emp", 1:52))
one <- read.table("data/recidiv.txt", na.strings = ".",
                  col.names = cnames)
