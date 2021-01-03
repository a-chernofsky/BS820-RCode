Lecture 5: Count regression
================

``` r
# install/load packages ---------------------------------------------------

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
if (!require('logistf')) install.packages('logistf') 
```

    ## Loading required package: logistf

``` r
if (!require('pwr')) install.packages('pwr') 
```

    ## Loading required package: pwr

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
if (!require('GDAtools')) install.packages('GDAtools')
```

    ## Loading required package: GDAtools

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
if (!require('nnet')) install.packages('nnet')
```

    ## Loading required package: nnet

``` r
if (!require('geepack')) install.packages('geepack')
```

    ## Loading required package: geepack

``` r
if (!require('logbin')) install.packages('logbin')
```

    ## Loading required package: logbin

``` r
library(readxl)
library(broom)
library(epitools)
library(logistf)
library(pwr)
library(survival)
library(GDAtools)
library(MASS)
library(car)
library(nnet)
library(geepack)
library(logbin)
```

``` r
# read in data ------------------------------------------------------------

arthrit <- data.frame(sex = rep(c("female", "female", "male", "male"),3), 
                      treat = rep(c("active", "placebo"), 6), 
                      cimprove = c(rep("marked", 4), rep("some", 4), rep("none", 4)), 
                      count = c(16, 6, 5, 1, 5, 7, 2, 0, 6, 19, 7, 10))

arthrit$treat <- factor(arthrit$treat, levels = c("placebo", "active"))

arthrit$improve <- ifelse(arthrit$cimprove == "none", 0,
                          ifelse(arthrit$cimprove == "some", 1,
                                 ifelse(arthrit$cimprove == "marked", 2, NA)))

arthrit$improv1 <- ifelse(arthrit$improve == 2, 1, 0)
arthrit$improv2 <- ifelse(arthrit$improve >= 1, 1, 0)
```

``` r
wtable(arthrit$sex, arthrit$improv1, w = arthrit$count, na = F)
```

    ##         0  1 tot
    ## female 37 22  59
    ## male   19  6  25
    ## tot    56 28  84

``` r
wtable(arthrit$treat, arthrit$improv1, w = arthrit$count, na = F)
```

    ##          0  1 tot
    ## placebo 36  7  43
    ## active  20 21  41
    ## tot     56 28  84

``` r
wtable(arthrit$sex, arthrit$improv2, w = arthrit$count, na = F)
```

    ##         0  1 tot
    ## female 25 34  59
    ## male   17  8  25
    ## tot    42 42  84

``` r
wtable(arthrit$treat, arthrit$improv2, w = arthrit$count, na = F)
```

    ##          0  1 tot
    ## placebo 29 14  43
    ## active  13 28  41
    ## tot     42 42  84

``` r
arthrit$improve <- factor(arthrit$improve)
```

``` r
# Proportional odds model -------------------------------------------------

pomodel1 <- polr(improve ~ sex + treat + sex*treat, weights = count,  data = arthrit)
poTest(pomodel1)
```

    ## 
    ## Tests for Proportional Odds
    ## polr(formula = improve ~ sex + treat + sex * treat, data = arthrit, 
    ##     weights = count)
    ## 
    ##                       b[polr]     b[>0]     b[>1] Chisquare df Pr(>Chisq)
    ## Overall                                                   0  3          1
    ## sexmale             -1.81e+00 -1.36e-16 -8.16e-16         0  1          1
    ## treatactive          1.67e+00 -1.36e-16 -5.44e-16         0  1          1
    ## sexmale:treatactive  6.84e-01  1.09e-15  1.09e-15         0  1          1

``` r
pomodel2 <- polr(improve ~ sex + treat, weights = count,  data = arthrit)
poTest(pomodel2)
```

    ## 
    ## Tests for Proportional Odds
    ## polr(formula = improve ~ sex + treat, data = arthrit, weights = count)
    ## 
    ##               b[polr]     b[>0]     b[>1] Chisquare df Pr(>Chisq)
    ## Overall                                           0  2          1
    ## sexmale     -1.32e+00 -2.72e-16 -8.16e-16         0  1          1
    ## treatactive  1.80e+00 -2.72e-16 -8.16e-16         0  1          1

``` r
# Multinomial logistic regression ---------------------------------------------


mlogit1 <- multinom(improve ~ sex + treat + treat*sex , weights = count,  data = arthrit)
```

    ## # weights:  15 (8 variable)
    ## initial  value 92.283432 
    ## iter  10 value 73.803938
    ## iter  20 value 73.659586
    ## final  value 73.659487 
    ## converged

``` r
summary(mlogit1)
```

    ## Call:
    ## multinom(formula = improve ~ sex + treat + treat * sex, data = arthrit, 
    ##     weights = count)
    ## 
    ## Coefficients:
    ##   (Intercept)   sexmale treatactive sexmale:treatactive
    ## 1  -0.9983682 -8.398306   0.8165115           7.3262054
    ## 2  -1.1522022 -1.151074   2.1331679          -0.1669321
    ## 
    ## Std. Errors:
    ##   (Intercept)   sexmale treatactive sexmale:treatactive
    ## 1   0.4421427 34.715057   0.7497500           34.729599
    ## 2   0.4682372  1.148891   0.6696585            1.375496
    ## 
    ## Residual Deviance: 147.319 
    ## AIC: 163.319

``` r
mlogit2 <- multinom(improve ~ sex + treat, weights = count,  data = arthrit)
```

    ## # weights:  12 (6 variable)
    ## initial  value 92.283432 
    ## iter  10 value 74.510408
    ## final  value 74.510390 
    ## converged

``` r
summary(mlogit2)
```

    ## Call:
    ## multinom(formula = improve ~ sex + treat, data = arthrit, weights = count)
    ## 
    ## Coefficients:
    ##   (Intercept)   sexmale treatactive
    ## 1   -1.108385 -1.661426    1.105479
    ## 2   -1.136237 -1.378473    2.168690
    ## 
    ## Std. Errors:
    ##   (Intercept)   sexmale treatactive
    ## 1   0.4405685 0.8602610   0.6737636
    ## 2   0.4376503 0.6384877   0.5943035
    ## 
    ## Residual Deviance: 149.0208 
    ## AIC: 161.0208

``` r
# Recurrent Events --------------------------------------------------------

fap <- data.frame(idnum = 1:22, 
                  male = c(0, 0, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1), 
                  treat = c(1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 1),
                  base_n = c(7, 77, 7, 5, 23, 35, 11, 12, 7, 318, 160, 8, 20, 11, 24, 34, 54, 16, 30, 10, 20, 12),
                  age = c(17, 20, 16, 18, 22, 13, 23, 34, 50, 19, 17, 23, 22, 30, 27, 23, 22, 13, 34, 23, 22, 42),
                  r_n = c(6, 67, 4, 5, 16, 31, 6, 20, 7, 347, 142, 1, 16, 20, 26, 27, 45, 10, 30, 6, 5, 8))

sumstats <- function(x) c(min = min(x), 
                          q1 = quantile(x, probs = 0.25), 
                          median = median(x), 
                          mean = mean(x),
                          q3 = quantile(x, probs = 0.75),
                          max = max(x))

tapply(fap$r_n, fap$treat, sumstats)
```

    ## $`0`
    ##       min    q1.25%    median      mean    q3.75%       max 
    ##   5.00000  18.00000  26.00000  55.81818  38.00000 347.00000 
    ## 
    ## $`1`
    ##    min q1.25% median   mean q3.75%    max 
    ##    1.0    5.5    6.0   21.0   13.0  142.0

``` r
pos_reg1 <- glm(r_n ~ treat, family = poisson(), data = fap)
tidy(pos_reg1)
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic  p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)    4.02     0.0404      99.7 0.      
    ## 2 treat         -0.978    0.0772     -12.7 9.22e-37

``` r
glance(pos_reg1)
```

    ## # A tibble: 1 x 8
    ##   null.deviance df.null logLik   AIC   BIC deviance df.residual  nobs
    ##           <dbl>   <int>  <dbl> <dbl> <dbl>    <dbl>       <int> <int>
    ## 1         1603.      21  -762. 1528. 1530.    1423.          20    22

``` r
pos_reg2 <- glm(r_n ~ treat + male + base_n + age, family = poisson(), data = fap)
tidy(pos_reg2)
```

    ## # A tibble: 5 x 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)  3.36     0.188        17.9  2.28e- 71
    ## 2 treat       -0.318    0.0984       -3.23 1.22e-  3
    ## 3 male         0.281    0.111         2.53 1.13e-  2
    ## 4 base_n       0.00890  0.000406     21.9  3.00e-106
    ## 5 age         -0.0264   0.00734      -3.60 3.20e-  4

``` r
glance(pos_reg2)
```

    ## # A tibble: 1 x 8
    ##   null.deviance df.null logLik   AIC   BIC deviance df.residual  nobs
    ##           <dbl>   <int>  <dbl> <dbl> <dbl>    <dbl>       <int> <int>
    ## 1         1603.      21  -144.  298.  303.     187.          17    22

``` r
pos_reg3 <- glm(r_n ~ treat + male + base_n + age, family = quasipoisson(), data = fap)
tidy(pos_reg3)
```

    ## # A tibble: 5 x 5
    ##   term        estimate std.error statistic    p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>      <dbl>
    ## 1 (Intercept)  3.36      0.622       5.40  0.0000479 
    ## 2 treat       -0.318     0.326      -0.978 0.342     
    ## 3 male         0.281     0.367       0.766 0.454     
    ## 4 base_n       0.00890   0.00134     6.62  0.00000436
    ## 5 age         -0.0264    0.0243     -1.09  0.292

``` r
glance(pos_reg3)
```

    ## # A tibble: 1 x 8
    ##   null.deviance df.null logLik   AIC   BIC deviance df.residual  nobs
    ##           <dbl>   <int>  <dbl> <dbl> <dbl>    <dbl>       <int> <int>
    ## 1         1603.      21     NA    NA    NA     187.          17    22

``` r
fap$idfact <- as.factor(fap$idnum)

pos_reg4 <- geese(r_n ~ treat + male + base_n + age, 
                  id = idnum, 
                  family = poisson(), 
                  corstr = "exch",
                  data = fap)
summary(pos_reg4)
```

    ## 
    ## Call:
    ## geese(formula = r_n ~ treat + male + base_n + age, id = idnum, 
    ##     data = fap, family = poisson(), corstr = "exch")
    ## 
    ## Mean Model:
    ##  Mean Link:                 log 
    ##  Variance to Mean Relation: poisson 
    ## 
    ##  Coefficients:
    ##                 estimate       san.se        wald            p
    ## (Intercept)  3.361005981 0.4115788742  66.6856549 3.330669e-16
    ## treat       -0.318306861 0.3820566258   0.6941231 4.047655e-01
    ## male         0.281418574 0.3403868038   0.6835339 4.083725e-01
    ## base_n       0.008895975 0.0008586727 107.3326516 0.000000e+00
    ## age         -0.026407689 0.0143834829   3.3707976 6.636162e-02
    ## 
    ## Scale Model:
    ##  Scale Link:                identity 
    ## 
    ##  Estimated Scale Parameters:
    ##             estimate   san.se     wald           p
    ## (Intercept) 8.458199 2.950819 8.216191 0.004151825
    ## 
    ## Correlation Model:
    ##  Correlation Structure:     exch 
    ##  Correlation Link:          identity 
    ## 
    ##  Estimated Correlation Parameters:
    ##       estimate san.se wald   p
    ## alpha        0      0  NaN NaN
    ## 
    ## Returned Error Value:    0 
    ## Number of clusters:   22   Maximum cluster size: 1

``` r
pos_reg5 <- glm.nb(r_n ~ treat + male + base_n + age, data = fap)
tidy(pos_reg5)
```

    ## # A tibble: 5 x 5
    ##   term        estimate std.error statistic  p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)   3.08     0.462        6.68 2.38e-11
    ## 2 treat        -0.788    0.262       -3.01 2.60e- 3
    ## 3 male          0.448    0.284        1.58 1.15e- 1
    ## 4 base_n        0.0126   0.00181      6.97 3.22e-12
    ## 5 age          -0.0226   0.0168      -1.35 1.77e- 1

``` r
glance(pos_reg5)
```

    ## # A tibble: 1 x 8
    ##   null.deviance df.null logLik   AIC   BIC deviance df.residual  nobs
    ##           <dbl>   <int>  <dbl> <dbl> <dbl>    <dbl>       <int> <int>
    ## 1          135.      21  -80.5  173.  180.     21.8          17    22

``` r
summary(pos_reg5)
```

    ## 
    ## Call:
    ## glm.nb(formula = r_n ~ treat + male + base_n + age, data = fap, 
    ##     init.theta = 3.944284964, link = log)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -2.06183  -0.79102  -0.08186   0.42006   1.29114  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  3.083803   0.461616   6.680 2.38e-11 ***
    ## treat       -0.788197   0.261783  -3.011   0.0026 ** 
    ## male         0.448341   0.284154   1.578   0.1146    
    ## base_n       0.012591   0.001807   6.968 3.22e-12 ***
    ## age         -0.022596   0.016753  -1.349   0.1774    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for Negative Binomial(3.9443) family taken to be 1)
    ## 
    ##     Null deviance: 134.579  on 21  degrees of freedom
    ## Residual deviance:  21.843  on 17  degrees of freedom
    ## AIC: 173
    ## 
    ## Number of Fisher Scoring iterations: 1
    ## 
    ## 
    ##               Theta:  3.94 
    ##           Std. Err.:  1.42 
    ## 
    ##  2 x log-likelihood:  -161.002

``` r
#to convert dispersion parameter between SAS and R: SASdisp = 1/Rdisp = 1/3.94 = 0.2538
```

``` r
# log binomial model ------------------------------------------------------

help <- read_excel("data/HELPJsat.xlsx")
help$drinks10 = help$i1/10

table(help$homeless)
```

    ## 
    ##   0   1 
    ## 250 215

``` r
logreg1 <- glm(homeless ~ drinks10, family =  binomial(), data = help)
tidy(logreg1)
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic     p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>       <dbl>
    ## 1 (Intercept)   -0.653    0.136      -4.81 0.00000153 
    ## 2 drinks10       0.278    0.0557      5.00 0.000000576

``` r
tidy(logreg1, exp = T, conf.int = T)
```

    ## # A tibble: 2 x 7
    ##   term        estimate std.error statistic     p.value conf.low conf.high
    ##   <chr>          <dbl>     <dbl>     <dbl>       <dbl>    <dbl>     <dbl>
    ## 1 (Intercept)    0.520    0.136      -4.81 0.00000153     0.397     0.677
    ## 2 drinks10       1.32     0.0557      5.00 0.000000576    1.19      1.48

``` r
start.p <- mean(help$homeless)
```

``` r
#binreg1 <- glm(homeless ~ drinks10, family = binomial(log), 
#               data = help)
```

``` r
binreg2 <- geese(homeless ~ drinks10, 
                  id = id, 
                  family = poisson(), 
                  corstr = "exch",
                  data = help)
summary(binreg2)
```

    ## 
    ## Call:
    ## geese(formula = homeless ~ drinks10, id = id, data = help, family = poisson(), 
    ##     corstr = "exch")
    ## 
    ## Mean Model:
    ##  Mean Link:                 log 
    ##  Variance to Mean Relation: poisson 
    ## 
    ##  Coefficients:
    ##               estimate     san.se      wald            p
    ## (Intercept) -0.9873806 0.06757407 213.50548 0.000000e+00
    ## drinks10     0.1034842 0.01490919  48.17701 3.894218e-12
    ## 
    ## Scale Model:
    ##  Scale Link:                identity 
    ## 
    ##  Estimated Scale Parameters:
    ##             estimate     san.se     wald p
    ## (Intercept) 0.533323 0.03717249 205.8436 0
    ## 
    ## Correlation Model:
    ##  Correlation Structure:     exch 
    ##  Correlation Link:          identity 
    ## 
    ##  Estimated Correlation Parameters:
    ##       estimate san.se wald   p
    ## alpha        0      0  NaN NaN
    ## 
    ## Returned Error Value:    0 
    ## Number of clusters:   465   Maximum cluster size: 1
