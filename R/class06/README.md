Untitled
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
if (!require('logbin')) install.packages('logbin')
```

    ## Loading required package: logbin

``` r
library(readxl)
library(broom)
library(epitools)
library(survival)
library(MASS)
library(car)
library(geepack)
library(logbin)
```

``` r
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
preg1 <- glm(r_n ~ treat, family = poisson(), data = fap)
tidy(preg1)
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic  p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)    4.02     0.0404      99.7 0.      
    ## 2 treat         -0.978    0.0772     -12.7 9.22e-37

``` r
glance(preg1)
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
preg3 <- glm(r_n ~ treat + male + base_n + age, family = quasipoisson(), data = fap)
tidy(preg3)
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
glance(preg3)
```

    ## # A tibble: 1 x 8
    ##   null.deviance df.null logLik   AIC   BIC deviance df.residual  nobs
    ##           <dbl>   <int>  <dbl> <dbl> <dbl>    <dbl>       <int> <int>
    ## 1         1603.      21     NA    NA    NA     187.          17    22

``` r
preg4 <- geese(r_n ~ treat + male + base_n + age, 
                  id = idnum, 
                  family = poisson(), 
                  corstr = "exch",
                  data = fap)
summary(preg4)
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
nbreg <- glm.nb(r_n ~ treat + male + base_n + age, 
                 data = fap)
tidy(nbreg)
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
glance(nbreg)
```

    ## # A tibble: 1 x 8
    ##   null.deviance df.null logLik   AIC   BIC deviance df.residual  nobs
    ##           <dbl>   <int>  <dbl> <dbl> <dbl>    <dbl>       <int> <int>
    ## 1          135.      21  -80.5  173.  180.     21.8          17    22

``` r
summary(nbreg)
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
# Post-traumatic Stress Disorder Data -------------------------------------

ptsd01 <- read.table("data/ptsd.txt", 
                     col.names = c("subjid", "ptsd", "control", 
                                   "problems", "sevent", "cohes", 
                                   "time"))
ptsd01 <- ptsd01[ptsd01$time == 1,]
```

``` r
ptsd_logistic <- glm(ptsd ~ control + problems + sevent + cohes,
                     family = binomial(), data = ptsd01)
tidy(ptsd_logistic)
```

    ## # A tibble: 5 x 5
    ##   term        estimate std.error statistic  p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)    2.30     1.48        1.55 0.121   
    ## 2 control       -1.20     0.347      -3.45 0.000567
    ## 3 problems       0.363    0.108       3.37 0.000751
    ## 4 sevent         0.259    0.135       1.92 0.0544  
    ## 5 cohes         -0.195    0.0673     -2.90 0.00371

``` r
tidy(ptsd_logistic, exp = T, conf.int = T)
```

    ## # A tibble: 5 x 7
    ##   term        estimate std.error statistic  p.value conf.low conf.high
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
    ## 1 (Intercept)    9.96     1.48        1.55 0.121       0.553   189.   
    ## 2 control        0.302    0.347      -3.45 0.000567    0.151     0.590
    ## 3 problems       1.44     0.108       3.37 0.000751    1.17      1.79 
    ## 4 sevent         1.30     0.135       1.92 0.0544      0.997     1.69 
    ## 5 cohes          0.823    0.0673     -2.90 0.00371     0.718     0.936

``` r
ptsd_logbin <- logbin(ptsd ~ control + problems + sevent + cohes,
                      data = ptsd01)
```

    ## Loading required package: doParallel

    ## Loading required package: foreach

    ## Loading required package: iterators

    ## Loading required package: parallel

    ## Loading required package: numDeriv

    ## Loading required package: quantreg

    ## Loading required package: SparseM

    ## 
    ## Attaching package: 'SparseM'

    ## The following object is masked from 'package:base':
    ## 
    ##     backsolve

    ## 
    ## Attaching package: 'quantreg'

    ## The following object is masked from 'package:survival':
    ## 
    ##     untangle.specials

    ## 
    ## Attaching package: 'turboEM'

    ## The following objects are masked from 'package:numDeriv':
    ## 
    ##     grad, hessian

    ## Warning: nplbin: fitted probabilities numerically 1 occurred

``` r
tidy(ptsd_logbin)
```

    ## Warning: MLE on boundary of parameter space, cannot use asymptotic covariance
    ## matrix

    ## # A tibble: 5 x 5
    ##   term         estimate std.error statistic p.value
    ##   <chr>           <dbl>     <dbl>     <dbl>   <dbl>
    ## 1 (Intercept) -6.32e- 1       NaN       NaN     NaN
    ## 2 control     -2.85e- 1       NaN       NaN     NaN
    ## 3 problems     1.18e- 1       NaN       NaN     NaN
    ## 4 sevent      -2.22e-16       NaN       NaN     NaN
    ## 5 cohes       -2.18e- 2       NaN       NaN     NaN

``` r
ptsd_gee <- geese(ptsd ~ control + problems + sevent + cohes, 
                  id = subjid, 
                  family = poisson(), 
                  corstr = "exch",
                  data = ptsd01)
summary(ptsd_gee)
```

    ## 
    ## Call:
    ## geese(formula = ptsd ~ control + problems + sevent + cohes, id = subjid, 
    ##     data = ptsd01, family = poisson(), corstr = "exch")
    ## 
    ## Mean Model:
    ##  Mean Link:                 log 
    ##  Variance to Mean Relation: poisson 
    ## 
    ##  Coefficients:
    ##                estimate     san.se       wald            p
    ## (Intercept) -0.19209234 0.62517287  0.0944104 7.586434e-01
    ## control     -0.55845207 0.13453818 17.2298178 3.311971e-05
    ## problems     0.21650978 0.05456590 15.7439023 7.252132e-05
    ## sevent       0.10990334 0.05472510  4.0331899 4.461351e-02
    ## cohes       -0.07163456 0.02634123  7.3955921 6.538390e-03
    ## 
    ## Scale Model:
    ##  Scale Link:                identity 
    ## 
    ##  Estimated Scale Parameters:
    ##              estimate     san.se     wald p
    ## (Intercept) 0.5418271 0.05694274 90.54089 0
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
    ## Number of clusters:   316   Maximum cluster size: 1

``` r
# Estimating rates rather than counts -------------------------------------

one <- read.table("data/dccthypo.txt",
                  col.names = c("group", "nevents", 
                                "fuday", "iu", "duration", 
                                "female", "adult", "bcval5", 
                                "hbael", "hxcoma", "obweight"))
one$ievents <- ifelse(one$nevents > 0, 1, 0)
one$fuyears <- one$fuday/365.25
one$lnyears <- log(one$fuyears)
one$insulin <- one$iu/one$obweight
one$rate <- one$nevents/one$fuyears
```

``` r
c(n = length(one$nevents), mean = mean(one$nevents), sum = sum(one$nevents))
```

    ##           n        mean         sum 
    ##  715.000000    3.169231 2266.000000

``` r
c(n = length(one$fuyears), mean = mean(one$fuyears), sum = sum(one$fuyears))
```

    ##           n        mean         sum 
    ##  715.000000    7.103099 5078.715948

``` r
tapply(one$nevents, one$group, function(x)c(n = length(x), mean = mean(x), sum = sum(x)))
```

    ## $`0`
    ##          n       mean        sum 
    ## 352.000000   1.542614 543.000000 
    ## 
    ## $`1`
    ##           n        mean         sum 
    ##  363.000000    4.746556 1723.000000

``` r
tapply(one$fuyears, one$group, function(x)c(n = length(x), mean = mean(x), sum = sum(x)))
```

    ## $`0`
    ##           n        mean         sum 
    ##  352.000000    7.046123 2480.235455 
    ## 
    ## $`1`
    ##           n        mean         sum 
    ##  363.000000    7.158348 2598.480493

``` r
rate_preg1 <- glm(nevents ~ 1, family = poisson(),
                  offset = lnyears,
                  data = one)
summary(rate_preg1)
```

    ## 
    ## Call:
    ## glm(formula = nevents ~ 1, family = poisson(), data = one, offset = lnyears)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.9504  -2.3695  -1.3753   0.3861  11.7842  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -0.80704    0.02101  -38.42   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance: 4520.8  on 714  degrees of freedom
    ## Residual deviance: 4520.8  on 714  degrees of freedom
    ## AIC: 5785.9
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
rate_preg2 <- glm(nevents ~ group, family = poisson(),
                  offset = lnyears,
                  data = one)
summary(rate_preg2)
```

    ## 
    ## Call:
    ## glm(formula = nevents ~ group, family = poisson(), data = one, 
    ##     offset = lnyears)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.5932  -1.9641  -1.5148   0.4555  10.4822  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -1.51900    0.04291  -35.40   <2e-16 ***
    ## group        1.10814    0.04921   22.52   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance: 4520.8  on 714  degrees of freedom
    ## Residual deviance: 3928.8  on 713  degrees of freedom
    ## AIC: 5195.8
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
rate_preg3 <- glm(nevents ~ group + insulin + duration + 
                    female + adult + bcval5 + hbael + hxcoma , 
                  family = poisson(),
                  offset = lnyears,
                  data = one)
summary(rate_preg3)
```

    ## 
    ## Call:
    ## glm(formula = nevents ~ group + insulin + duration + female + 
    ##     adult + bcval5 + hbael + hxcoma, family = poisson(), data = one, 
    ##     offset = lnyears)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -5.5380  -1.8734  -1.3642   0.5198  10.5883  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -0.9568309  0.2173689  -4.402 1.07e-05 ***
    ## group        1.0845299  0.0493015  21.998  < 2e-16 ***
    ## insulin      0.0050737  0.0994503   0.051  0.95931    
    ## duration     0.0014638  0.0005618   2.605  0.00918 ** 
    ## female       0.1793789  0.0423653   4.234 2.29e-05 ***
    ## adult       -0.5979589  0.0655828  -9.118  < 2e-16 ***
    ## bcval5      -0.5282798  0.3630353  -1.455  0.14562    
    ## hbael       -0.0334576  0.0151350  -2.211  0.02706 *  
    ## hxcoma       0.6010155  0.0684512   8.780  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance: 4520.8  on 714  degrees of freedom
    ## Residual deviance: 3707.7  on 706  degrees of freedom
    ## AIC: 4988.7
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
rate_preg4 <- glm(nevents ~ group + insulin + duration + 
                    female + adult + bcval5 + hbael + hxcoma , 
                  family = quasipoisson(),
                  offset = lnyears,
                  data = one)
summary(rate_preg4)
```

    ## 
    ## Call:
    ## glm(formula = nevents ~ group + insulin + duration + female + 
    ##     adult + bcval5 + hbael + hxcoma, family = quasipoisson(), 
    ##     data = one, offset = lnyears)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -5.5380  -1.8734  -1.3642   0.5198  10.5883  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.956831   0.566356  -1.689 0.091574 .  
    ## group        1.084530   0.128455   8.443  < 2e-16 ***
    ## insulin      0.005074   0.259118   0.020 0.984383    
    ## duration     0.001464   0.001464   1.000 0.317668    
    ## female       0.179379   0.110383   1.625 0.104596    
    ## adult       -0.597959   0.170876  -3.499 0.000496 ***
    ## bcval5      -0.528280   0.945890  -0.559 0.576680    
    ## hbael       -0.033458   0.039434  -0.848 0.396481    
    ## hxcoma       0.601015   0.178350   3.370 0.000793 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for quasipoisson family taken to be 6.788654)
    ## 
    ##     Null deviance: 4520.8  on 714  degrees of freedom
    ## Residual deviance: 3707.7  on 706  degrees of freedom
    ## AIC: NA
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
two <- one
two$subnum <- 1:nrow(two)

rate_gee <- geeglm(nevents ~ group + insulin + duration + 
                    female + adult + bcval5 + hbael + hxcoma,  
                  id = subnum, 
                  family = poisson(), 
                  offset = lnyears,
                  corstr = "exchangeable",
                  data = two)
summary(rate_gee)
```

    ## 
    ## Call:
    ## geeglm(formula = nevents ~ group + insulin + duration + female + 
    ##     adult + bcval5 + hbael + hxcoma, family = poisson(), data = two, 
    ##     offset = lnyears, id = subnum, corstr = "exchangeable")
    ## 
    ##  Coefficients:
    ##              Estimate   Std.err   Wald Pr(>|W|)    
    ## (Intercept) -0.956831  0.572475  2.794 0.094644 .  
    ## group        1.084530  0.121562 79.595  < 2e-16 ***
    ## insulin      0.005074  0.305007  0.000 0.986728    
    ## duration     0.001464  0.001399  1.095 0.295411    
    ## female       0.179379  0.121114  2.194 0.138587    
    ## adult       -0.597959  0.210591  8.062 0.004519 ** 
    ## bcval5      -0.528280  0.982634  0.289 0.590842    
    ## hbael       -0.033458  0.041107  0.662 0.415695    
    ## hxcoma       0.601015  0.177046 11.524 0.000687 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation structure = exchangeable 
    ## Estimated Scale Parameters:
    ## 
    ##             Estimate Std.err
    ## (Intercept)    6.703  0.7132
    ##   Link = identity 
    ## 
    ## Estimated Correlation Parameters:
    ##       Estimate Std.err
    ## alpha        0       0
    ## Number of clusters:   715  Maximum cluster size: 1

``` r
rate_nb <- glm.nb(nevents ~ group + insulin + duration + 
                    female + adult + bcval5 + hbael + hxcoma +
                    offset(lnyears),
                  data = two)
summary(rate_nb)
```

    ## 
    ## Call:
    ## glm.nb(formula = nevents ~ group + insulin + duration + female + 
    ##     adult + bcval5 + hbael + hxcoma + offset(lnyears), data = two, 
    ##     init.theta = 0.4926285658, link = log)
    ## 
    ## Deviance Residuals: 
    ##    Min      1Q  Median      3Q     Max  
    ## -1.861  -1.133  -0.660   0.159   2.660  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -5.09e-01   6.29e-01   -0.81   0.4183    
    ## group        1.20e+00   1.20e-01   10.01   <2e-16 ***
    ## insulin      2.14e-01   2.91e-01    0.73   0.4624    
    ## duration     3.42e-05   1.58e-03    0.02   0.9828    
    ## female       2.47e-01   1.20e-01    2.06   0.0397 *  
    ## adult       -6.54e-01   2.23e-01   -2.94   0.0033 ** 
    ## bcval5      -1.63e+00   9.62e-01   -1.69   0.0904 .  
    ## hbael       -8.17e-02   4.18e-02   -1.95   0.0509 .  
    ## hxcoma       4.80e-01   2.58e-01    1.86   0.0631 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for Negative Binomial(0.4926) family taken to be 1)
    ## 
    ##     Null deviance: 829.49  on 714  degrees of freedom
    ## Residual deviance: 707.69  on 706  degrees of freedom
    ## AIC: 2949
    ## 
    ## Number of Fisher Scoring iterations: 1
    ## 
    ## 
    ##               Theta:  0.4926 
    ##           Std. Err.:  0.0378 
    ## 
    ##  2 x log-likelihood:  -2928.5750
