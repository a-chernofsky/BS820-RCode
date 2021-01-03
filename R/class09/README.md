Parametric regression models
================

``` r
# install/load packages ---------------------------------------------------

if (!require('haven')) install.packages('haven') 
```

    ## Loading required package: haven

``` r
if (!require('readxl')) install.packages('readxl') 
```

    ## Loading required package: readxl

``` r
if (!require('broom')) install.packages('broom') 
```

    ## Loading required package: broom

``` r
if (!require('epitools')) install.packages('epitools') 
```

    ## Loading required package: epitools

``` r
if (!require('survival')) install.packages('survival')
```

    ## Loading required package: survival

    ## 
    ## Attaching package: 'survival'

    ## The following object is masked from 'package:epitools':
    ## 
    ##     ratetable

``` r
if (!require('MASS')) install.packages('MASS')
```

    ## Loading required package: MASS

``` r
if (!require('car')) install.packages('car')
```

    ## Loading required package: car

    ## Loading required package: carData

``` r
if (!require('geepack')) install.packages('geepack')
```

    ## Loading required package: geepack

``` r
if (!require('muhaz')) install.packages('muhaz')
```

    ## Loading required package: muhaz

``` r
if (!require('survminer')) install.packages('survminer')
```

    ## Loading required package: survminer

    ## Loading required package: ggplot2

    ## Loading required package: ggpubr

``` r
if (!require('flexsurv')) install.packages('flexsurv')
```

    ## Loading required package: flexsurv

``` r
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
```

``` r
# read in data ------------------------------------------------------------

#I converted the .txt data into a SAS dataset
recid <- read_sas("data/recidiv.sas7bdat")
recid$educ_num <- recid$educ
recid$educ <- factor(recid$educ, levels = c(6, 2, 3, 4, 5))
recid$logweeks <- log(recid$week)
```

``` r
# The exponential AFT model -----------------------------------------------

aft_exp <- survreg(Surv(week,arrest) ~ fin + age + race + wexp + mar + 
                     paro + prio + educ,
                   dist = "exponential",
                   data = recid)
summary(aft_exp)
```

    ## 
    ## Call:
    ## survreg(formula = Surv(week, arrest) ~ fin + age + race + wexp + 
    ##     mar + paro + prio + educ, data = recid, dist = "exponential")
    ##               Value Std. Error     z       p
    ## (Intercept)  5.0258     1.1809  4.26 2.1e-05
    ## fin          0.3888     0.1927  2.02   0.044
    ## age          0.0496     0.0221  2.25   0.024
    ## race        -0.3544     0.3123 -1.13   0.256
    ## wexp         0.1153     0.2130  0.54   0.588
    ## mar          0.4169     0.3817  1.09   0.275
    ## paro         0.0953     0.1957  0.49   0.626
    ## prio        -0.0743     0.0290 -2.56   0.010
    ## educ2       -0.4117     1.1232 -0.37   0.714
    ## educ3       -0.9856     1.0113 -0.97   0.330
    ## educ4       -0.7161     1.0231 -0.70   0.484
    ## educ5       -0.2860     1.0991 -0.26   0.795
    ## 
    ## Scale fixed at 1 
    ## 
    ## Exponential distribution
    ## Loglik(model)= -683.8   Loglik(intercept only)= -702
    ##  Chisq= 36.45 on 11 degrees of freedom, p= 0.00014 
    ## Number of Newton-Raphson Iterations: 5 
    ## n= 432

``` r
# Poisson model -----------------------------------------------------------

preg <- glm(arrest ~ fin + age + race + wexp + mar + 
              paro + prio + educ,
            family =  poisson(),
            offset = logweeks,
            data = recid)
summary(preg)
```

    ## 
    ## Call:
    ## glm(formula = arrest ~ fin + age + race + wexp + mar + paro + 
    ##     prio + educ, family = poisson(), data = recid, offset = logweeks)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.3454  -0.7835  -0.6243   0.6130   3.1433  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -5.02582    1.18086  -4.256 2.08e-05 ***
    ## fin         -0.38883    0.19269  -2.018   0.0436 *  
    ## age         -0.04962    0.02206  -2.250   0.0245 *  
    ## race         0.35440    0.31230   1.135   0.2564    
    ## wexp        -0.11530    0.21296  -0.541   0.5882    
    ## mar         -0.41694    0.38165  -1.092   0.2746    
    ## paro        -0.09530    0.19571  -0.487   0.6263    
    ## prio         0.07435    0.02899   2.564   0.0103 *  
    ## educ2        0.41175    1.12319   0.367   0.7139    
    ## educ3        0.98565    1.01126   0.975   0.3297    
    ## educ4        0.71615    1.02311   0.700   0.4839    
    ## educ5        0.28600    1.09911   0.260   0.7947    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance: 454.87  on 431  degrees of freedom
    ## Residual deviance: 418.42  on 420  degrees of freedom
    ## AIC: 670.42
    ## 
    ## Number of Fisher Scoring iterations: 7

``` r
# The Weibull AFT model -----------------------------------------------

aft_wei <- survreg(Surv(week,arrest) ~ fin + age + race + wexp + mar + 
                     paro + prio + educ,
                   dist = "weibull",
                   data = recid)
summary(aft_wei)
```

    ## 
    ## Call:
    ## survreg(formula = Surv(week, arrest) ~ fin + age + race + wexp + 
    ##     mar + paro + prio + educ, data = recid, dist = "weibull")
    ##               Value Std. Error     z       p
    ## (Intercept)  4.6904     0.8434  5.56 2.7e-08
    ## fin          0.2879     0.1390  2.07 0.03833
    ## age          0.0363     0.0160  2.27 0.02348
    ## race        -0.2579     0.2230 -1.16 0.24745
    ## wexp         0.0856     0.1520  0.56 0.57327
    ## mar          0.3034     0.2729  1.11 0.26626
    ## paro         0.0683     0.1395  0.49 0.62422
    ## prio        -0.0571     0.0213 -2.69 0.00725
    ## educ2       -0.2903     0.7991 -0.36 0.71639
    ## educ3       -0.7117     0.7215 -0.99 0.32395
    ## educ4       -0.5251     0.7285 -0.72 0.47109
    ## educ5       -0.2053     0.7817 -0.26 0.79283
    ## Log(scale)  -0.3412     0.0890 -3.83 0.00013
    ## 
    ## Scale= 0.711 
    ## 
    ## Weibull distribution
    ## Loglik(model)= -677.2   Loglik(intercept only)= -696.6
    ##  Chisq= 38.8 on 11 degrees of freedom, p= 5.7e-05 
    ## Number of Newton-Raphson Iterations: 6 
    ## n= 432

``` r
# The log normal AFT model -----------------------------------------------

aft_logn <- survreg(Surv(week,arrest) ~ fin + age + race + wexp + mar + 
                     paro + prio + educ,
                   dist = "lognormal",
                   data = recid)
summary(aft_logn)
```

    ## 
    ## Call:
    ## survreg(formula = Surv(week, arrest) ~ fin + age + race + wexp + 
    ##     mar + paro + prio + educ, data = recid, dist = "lognormal")
    ##               Value Std. Error     z       p
    ## (Intercept)  5.0747     0.8327  6.09 1.1e-09
    ## fin          0.3576     0.1655  2.16 0.03071
    ## age          0.0204     0.0163  1.25 0.21000
    ## race        -0.4060     0.2691 -1.51 0.13143
    ## wexp         0.2504     0.1812  1.38 0.16691
    ## mar          0.4453     0.2951  1.51 0.13127
    ## paro         0.0499     0.1691  0.29 0.76801
    ## prio        -0.0596     0.0271 -2.20 0.02759
    ## educ2       -0.1591     0.7786 -0.20 0.83812
    ## educ3       -0.7367     0.6767 -1.09 0.27633
    ## educ4       -0.5440     0.6860 -0.79 0.42777
    ## educ5       -0.4658     0.7310 -0.64 0.52401
    ## Log(scale)   0.2527     0.0764  3.31 0.00095
    ## 
    ## Scale= 1.29 
    ## 
    ## Log Normal distribution
    ## Loglik(model)= -681.3   Loglik(intercept only)= -697.9
    ##  Chisq= 33.16 on 11 degrees of freedom, p= 0.00049 
    ## Number of Newton-Raphson Iterations: 4 
    ## n= 432

``` r
# The log normal AFT model -----------------------------------------------

aft_ggam <- flexsurvreg(Surv(week,arrest) ~ fin + age + race + wexp + mar + 
                      paro + prio + educ,
                    dist = "gengamma",
                    data = recid)
aft_ggam
```

    ## Call:
    ## flexsurvreg(formula = Surv(week, arrest) ~ fin + age + race + 
    ##     wexp + mar + paro + prio + educ, data = recid, dist = "gengamma")
    ## 
    ## Estimates: 
    ##        data mean  est       L95%      U95%      se        exp(est)  L95%    
    ## mu           NA    4.62235   2.91844   6.32627   0.86936        NA        NA
    ## sigma        NA    0.60031   0.22310   1.61528   0.30317        NA        NA
    ## Q            NA    1.25845  -0.25672   2.77361   0.77306        NA        NA
    ## fin     0.50000    0.27633   0.00630   0.54635   0.13777   1.31828   1.00632
    ## age    24.59722    0.03893   0.00511   0.07274   0.01725   1.03970   1.00513
    ## race    0.87731   -0.23139  -0.67372   0.21093   0.22568   0.79343   0.50981
    ## wexp    0.57176    0.04986  -0.29360   0.39332   0.17524   1.05112   0.74558
    ## mar     0.12269    0.28337  -0.25532   0.82207   0.27485   1.32760   0.77467
    ## paro    0.61806    0.07381  -0.19035   0.33796   0.13478   1.07660   0.82667
    ## prio    2.98380   -0.05459  -0.09725  -0.01194   0.02177   0.94687   0.90733
    ## educ2   0.05556   -0.29675  -1.88891   1.29541   0.81234   0.74323   0.15124
    ## educ3   0.55324   -0.71250  -2.15876   0.73376   0.73790   0.49042   0.11547
    ## educ4   0.27546   -0.52450  -1.98351   0.93452   0.74441   0.59185   0.13759
    ## educ5   0.09028   -0.18667  -1.75305   1.37971   0.79919   0.82972   0.17325
    ##        U95%    
    ## mu           NA
    ## sigma        NA
    ## Q            NA
    ## fin     1.72694
    ## age     1.07545
    ## race    1.23483
    ## wexp    1.48189
    ## mar     2.27520
    ## paro    1.40209
    ## prio    0.98814
    ## educ2   3.65249
    ## educ3   2.08291
    ## educ4   2.54599
    ## educ5   3.97373
    ## 
    ## N = 432,  Events: 114,  Censored: 318
    ## Total time at risk: 19809
    ## Log-likelihood = -677.1401, df = 14
    ## AIC = 1382.28

``` r
# likelihood ratio test ---------------------------------------------------

#lrt test for whole set of independent variables

lrt <- -2*(aft_wei$loglik[1] - aft_wei$loglik[2])
pchisq(lrt, df = 11, lower.tail = F)
```

    ## [1] 5.730423e-05

``` r
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
```

    ## [1] 0.01387888

``` r
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
```

    ## [1] 0.2138304

``` r
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
```

    ## [1] 0.1695143

``` r
pchisq(-2*(ll_wei - ll_ggam), 1, lower.tail = F)
```

    ## [1] 0.6805439

``` r
#Exponential vs. Generalized Gamma
-2*(ll_exp - ll_ggam)
```

    ## [1] 13.22341

``` r
pchisq(-2*(ll_exp - ll_ggam), 1, lower.tail = F)
```

    ## [1] 0.0002764748

``` r
#log normal vs. Generalized Gamma
-2*(ll_logn - ll_ggam)
```

    ## [1] 8.375902

``` r
pchisq(-2*(ll_logn - ll_ggam), 1, lower.tail = F)
```

    ## [1] 0.003802287

``` r
# Problems with Maximum Likelihood Estimation for AFTs --------------------

recid2 <- recid

recid2$arrest <- ifelse(recid2$fin == 1, 0, recid2$arrest)

aft_exp2 <- survreg(Surv(week,arrest) ~ fin,
                   dist = "exponential",
                   data = recid2)
summary(aft_exp2)
```

    ## 
    ## Call:
    ## survreg(formula = Surv(week, arrest) ~ fin, data = recid2, dist = "exponential")
    ##                Value Std. Error     z      p
    ## (Intercept)    4.989      0.123 40.53 <2e-16
    ## fin           20.798   3951.691  0.01      1
    ## 
    ## Scale fixed at 1 
    ## 
    ## Exponential distribution
    ## Loglik(model)= -395.2   Loglik(intercept only)= -442.5
    ##  Chisq= 94.47 on 1 degrees of freedom, p= 2.5e-22 
    ## Number of Newton-Raphson Iterations: 21 
    ## n= 432
