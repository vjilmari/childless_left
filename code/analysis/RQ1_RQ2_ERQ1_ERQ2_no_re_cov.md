---
title: "RQ1, RQ2, ERQ1, ERQ2"
output: 
  html_document: 
    toc: yes
    keep_md: yes
date: '2022-05-02'
---




RQ1. Is childlessness associated with the three Chapel Hill rated political ideology (Left-Right, Economic Left-Right, GAL-TAN) positions of the party for which one has voted? 

H1. Regarding GAL-TAN, we expect childless people to have voted closer to the GAL pole of the dimension.

RQ2. If, as expected (H1), childlessness is associated with the GAL-TAN ideology position of the party for which one has voted, are those associations stronger if this dimension is salient in the party’s public stance (i.e., is there a boundary condition to these associations, such that the ideology needs to be salient in order for the association to exist)?

H2. We expect the association between childlessness and the GAL-TAN political ideology of the party for which one has voted to be stronger if this dimension is salient in the party’s public stance.

Exploratory research questions

ERQ1. If childlessness is associated with the economic left-right position of the party for which one has voted, are those associations stronger if this dimension is salient in the party’s public stance (i.e., is there a boundary condition to these associations, such that the ideology needs to be salient in order for the association to exist)?

ERQ2. Is childlessness associated with any of the 15 Chapel Hill rated policy positions of the party for which one has voted, or with the salience of anti-elitism or anti-corruption in the party agenda?


# Preparations

## Packages


```r
library(lme4)
```

```
## Loading required package: Matrix
```

```r
library(emmeans)
library(rio)
```

```
## 
## Attaching package: 'rio'
```

```
## The following object is masked from 'package:lme4':
## 
##     factorize
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(vjihelpers)
```

## Custom functions


```r
source("../custom_functions.R")
source("../modglmer_logit.R")
```

## Data


```r
fdat<-import("../../data/processed/fdat.xlsx")
```

## Data exclusions


```r
names(fdat)
```

```
##  [1] "idno"                     "cntry"                   
##  [3] "dweight"                  "pspwght"                 
##  [5] "pweight"                  "pt.nmbr"                 
##  [7] "pt.name"                  "gndr.f"                  
##  [9] "gndr.c"                   "agea"                    
## [11] "age10.c"                  "income"                  
## [13] "income.f"                 "income.fr"               
## [15] "edu"                      "edu.f"                   
## [17] "edu.c"                    "vote"                    
## [19] "vote.c"                   "con"                     
## [21] "con.c"                    "tra"                     
## [23] "tra.c"                    "ben"                     
## [25] "ben.c"                    "uni"                     
## [27] "uni.c"                    "sdi"                     
## [29] "sdi.c"                    "sti"                     
## [31] "sti.c"                    "hed"                     
## [33] "hed.c"                    "ach"                     
## [35] "ach.c"                    "pow"                     
## [37] "pow.c"                    "sec"                     
## [39] "sec.c"                    "resp.style"              
## [41] "lrgen"                    "lrecon"                  
## [43] "galtan"                   "spendvtax"               
## [45] "deregulation"             "redistribution"          
## [47] "econ_interven"            "civlib_laworder"         
## [49] "sociallifestyle"          "religious_principle"     
## [51] "immigrate_policy"         "multiculturalism"        
## [53] "urban_rural"              "environment"             
## [55] "regions"                  "international_security"  
## [57] "ethnic_minorities"        "nationalism"             
## [59] "lrecon_salience"          "galtan_salience"         
## [61] "antielite_salience"       "corrupt_salience"        
## [63] "lrgen.z"                  "lrecon.z"                
## [65] "galtan.z"                 "spendvtax.z"             
## [67] "deregulation.z"           "redistribution.z"        
## [69] "econ_interven.z"          "civlib_laworder.z"       
## [71] "sociallifestyle.z"        "religious_principle.z"   
## [73] "immigrate_policy.z"       "multiculturalism.z"      
## [75] "urban_rural.z"            "environment.z"           
## [77] "regions.z"                "international_security.z"
## [79] "ethnic_minorities.z"      "nationalism.z"           
## [81] "lrecon_salience.z"        "galtan_salience.z"       
## [83] "antielite_salience.z"     "corrupt_salience.z"      
## [85] "childlessness"            "minority"                
## [87] "minority.c"               "anweight"
```

```r
exdat<-fdat %>%
  dplyr::select(childlessness,
                gndr.c,age10.c,
                minority.c,lrgen.z,
                lrecon.z,galtan.z,
                lrecon_salience.z,galtan_salience.z,
                cntry,anweight) %>%
  na.omit()
```

## Variable centering


```r
exdat<-
  group_mean_center(
    data=exdat,group.var="cntry",
    vars=c("lrgen.z","lrecon.z","galtan.z",
           "lrecon_salience.z","galtan_salience.z"),
    grand.init = F)
```

# Descriptives


```r
table(exdat$cntry,exdat$childlessness)
```

```
##     
##         0    1
##   AT  690  367
##   BE  881  354
##   CH  427  213
##   CZ  791  218
##   DE 1438  605
##   DK  909  241
##   EE  778  175
##   ES  716  274
##   FI  958  364
##   FR  757  176
##   GB 1022  309
##   HU  624  221
##   IE  883  384
##   LT  831  117
##   NL  972  367
##   NO  791  263
##   PL  605  149
##   PT  436  118
##   SE 1034  375
##   SI  431  110
```

```r
round(100*prop.table(table(exdat$cntry,
                           exdat$childlessness),1),1)
```

```
##     
##         0    1
##   AT 65.3 34.7
##   BE 71.3 28.7
##   CH 66.7 33.3
##   CZ 78.4 21.6
##   DE 70.4 29.6
##   DK 79.0 21.0
##   EE 81.6 18.4
##   ES 72.3 27.7
##   FI 72.5 27.5
##   FR 81.1 18.9
##   GB 76.8 23.2
##   HU 73.8 26.2
##   IE 69.7 30.3
##   LT 87.7 12.3
##   NL 72.6 27.4
##   NO 75.0 25.0
##   PL 80.2 19.8
##   PT 78.7 21.3
##   SE 73.4 26.6
##   SI 79.7 20.3
```

```r
prop.table(table(exdat$childlessness))
```

```
## 
##         0         1 
## 0.7473566 0.2526434
```

# RQ1 Analysis 


RQ1. Is childlessness associated with the three Chapel Hill rated political ideology (Left-Right, Economic Left-Right, GAL-TAN) positions of the party for which one has voted? 

H1. Regarding GAL-TAN, we expect childless people to have voted closer to the GAL pole of the dimension.

## Baserate only



```r
mod0<-
  glmer(childlessness~(1|cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight)
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```r
summary(mod0)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  17219.9  17235.9  -8608.0  17215.9    21372 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -1.9067 -0.4931 -0.3113  0.3660  7.8186 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1046   0.3234  
## Number of obs: 21374, groups:  cntry, 20
## 
## Fixed effects:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.07113    0.08228  -13.02   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## Covariates


```r
mod1<-
  glmer(childlessness~gndr.c+age10.c+minority.c+(1|cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight)
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```r
summary(mod1)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15072.4  15112.2  -7531.2  15062.4    21369 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7345 -0.4386 -0.2243  0.2395 12.9768 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.116    0.3406  
## Number of obs: 21374, groups:  cntry, 20
## 
## Fixed effects:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.28639    0.10000 -12.865  < 2e-16 ***
## gndr.c      -0.30943    0.04045  -7.650 2.01e-14 ***
## age10.c     -0.55489    0.01346 -41.234  < 2e-16 ***
## minority.c  -0.28000    0.10387  -2.696  0.00703 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##            (Intr) gndr.c ag10.c
## gndr.c     0.015               
## age10.c    0.106  0.010        
## minority.c 0.489  0.002  0.093
```

## lrgen

### Fixed


```r
mod2.lrgen<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          lrgen.z.gmc+
          (1|cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight)
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```r
summary(mod2.lrgen)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + lrgen.z.gmc +  
##     (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15074.2  15122.0  -7531.1  15062.2    21368 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7200 -0.4387 -0.2245  0.2403 12.9067 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1158   0.3404  
## Number of obs: 21374, groups:  cntry, 20
## 
## Fixed effects:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.28811    0.10002 -12.878  < 2e-16 ***
## gndr.c      -0.30981    0.04046  -7.657  1.9e-14 ***
## age10.c     -0.55477    0.01346 -41.215  < 2e-16 ***
## minority.c  -0.28373    0.10420  -2.723  0.00647 ** 
## lrgen.z.gmc -0.01075    0.02345  -0.458  0.64671    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.016                     
## age10.c      0.106  0.009              
## minority.c   0.490  0.003  0.091       
## lrgen.z.gmc  0.038  0.020 -0.018  0.078
```

### Random


```r
mod3.lrgen<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          lrgen.z.gmc+
          (lrgen.z.gmc||cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight,
        control = glmerControl(optimizer="bobyqa"))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```
## boundary (singular) fit: see help('isSingular')
```

```r
summary(mod3.lrgen)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + lrgen.z.gmc +  
##     (lrgen.z.gmc || cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa")
## 
##      AIC      BIC   logLik deviance df.resid 
##  15076.2  15131.9  -7531.1  15062.2    21367 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7200 -0.4387 -0.2245  0.2403 12.9067 
## 
## Random effects:
##  Groups  Name        Variance Std.Dev.
##  cntry   (Intercept) 0.1158   0.3404  
##  cntry.1 lrgen.z.gmc 0.0000   0.0000  
## Number of obs: 21374, groups:  cntry, 20
## 
## Fixed effects:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.28810    0.10003 -12.877  < 2e-16 ***
## gndr.c      -0.30981    0.04046  -7.657  1.9e-14 ***
## age10.c     -0.55477    0.01346 -41.215  < 2e-16 ***
## minority.c  -0.28372    0.10421  -2.723  0.00647 ** 
## lrgen.z.gmc -0.01075    0.02345  -0.458  0.64673    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.016                     
## age10.c      0.106  0.009              
## minority.c   0.490  0.003  0.091       
## lrgen.z.gmc  0.038  0.020 -0.018  0.078
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.lrgen,mod3.lrgen)
```

```
## Data: exdat
## Models:
## mod2.lrgen: childlessness ~ gndr.c + age10.c + minority.c + lrgen.z.gmc + (1 | cntry)
## mod3.lrgen: childlessness ~ gndr.c + age10.c + minority.c + lrgen.z.gmc + (lrgen.z.gmc || cntry)
##            npar   AIC   BIC  logLik deviance Chisq Df Pr(>Chisq)
## mod2.lrgen    6 15074 15122 -7531.1    15062                    
## mod3.lrgen    7 15076 15132 -7531.1    15062     0  1     0.9999
```


## lrecon

### Fixed


```r
mod2.lrecon<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          lrecon.z.gmc+
          (1|cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight)
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```r
summary(mod2.lrecon)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + lrecon.z.gmc +  
##     (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15074.0  15121.8  -7531.0  15062.0    21368 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7079 -0.4382 -0.2246  0.2410 12.9028 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1158   0.3403  
## Number of obs: 21374, groups:  cntry, 20
## 
## Fixed effects:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  -1.28878    0.10002 -12.886  < 2e-16 ***
## gndr.c       -0.30994    0.04046  -7.661 1.85e-14 ***
## age10.c      -0.55466    0.01346 -41.200  < 2e-16 ***
## minority.c   -0.28506    0.10420  -2.736  0.00622 ** 
## lrecon.z.gmc -0.01474    0.02332  -0.632  0.52736    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.016                     
## age10.c      0.105  0.009              
## minority.c   0.490  0.003  0.090       
## lrecn.z.gmc  0.039  0.020 -0.025  0.077
```

### Random


```r
mod3.lrecon<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          lrecon.z.gmc+
          (lrecon.z.gmc||cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight,
        control = glmerControl(optimizer="bobyqa"))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```
## boundary (singular) fit: see help('isSingular')
```

```r
summary(mod3.lrecon)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + lrecon.z.gmc +  
##     (lrecon.z.gmc || cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa")
## 
##      AIC      BIC   logLik deviance df.resid 
##  15076.0  15131.8  -7531.0  15062.0    21367 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7080 -0.4382 -0.2246  0.2410 12.9028 
## 
## Random effects:
##  Groups  Name         Variance  Std.Dev. 
##  cntry   (Intercept)  1.158e-01 3.403e-01
##  cntry.1 lrecon.z.gmc 1.198e-09 3.461e-05
## Number of obs: 21374, groups:  cntry, 20
## 
## Fixed effects:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  -1.28880    0.10002 -12.885  < 2e-16 ***
## gndr.c       -0.30994    0.04046  -7.661 1.85e-14 ***
## age10.c      -0.55466    0.01346 -41.200  < 2e-16 ***
## minority.c   -0.28507    0.10420  -2.736  0.00622 ** 
## lrecon.z.gmc -0.01473    0.02332  -0.632  0.52743    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.016                     
## age10.c      0.105  0.009              
## minority.c   0.490  0.003  0.090       
## lrecn.z.gmc  0.039  0.020 -0.025  0.077
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.lrecon,mod3.lrecon)
```

```
## Data: exdat
## Models:
## mod2.lrecon: childlessness ~ gndr.c + age10.c + minority.c + lrecon.z.gmc + (1 | cntry)
## mod3.lrecon: childlessness ~ gndr.c + age10.c + minority.c + lrecon.z.gmc + (lrecon.z.gmc || cntry)
##             npar   AIC   BIC logLik deviance Chisq Df Pr(>Chisq)
## mod2.lrecon    6 15074 15122  -7531    15062                    
## mod3.lrecon    7 15076 15132  -7531    15062     0  1     0.9998
```



## galtan

### Fixed


```r
mod2.galtan<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          galtan.z.gmc+
          (1|cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight)
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```r
summary(mod2.galtan)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + galtan.z.gmc +  
##     (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15072.0  15119.8  -7530.0  15060.0    21368 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7183 -0.4386 -0.2245  0.2419 12.7554 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1151   0.3393  
## Number of obs: 21374, groups:  cntry, 20
## 
## Fixed effects:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  -1.29044    0.09981 -12.929  < 2e-16 ***
## gndr.c       -0.31056    0.04046  -7.676 1.65e-14 ***
## age10.c      -0.55355    0.01348 -41.050  < 2e-16 ***
## minority.c   -0.28770    0.10399  -2.767  0.00566 ** 
## galtan.z.gmc -0.03717    0.02421  -1.535  0.12471    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.015                     
## age10.c      0.105  0.008              
## minority.c   0.491  0.003  0.089       
## galtn.z.gmc  0.028  0.019 -0.061  0.048
```

### Random


```r
mod3.galtan<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          galtan.z.gmc+
          (galtan.z.gmc||cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight,
        control = glmerControl(optimizer="bobyqa"))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```
## boundary (singular) fit: see help('isSingular')
```

```r
summary(mod3.galtan)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + galtan.z.gmc +  
##     (galtan.z.gmc || cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa")
## 
##      AIC      BIC   logLik deviance df.resid 
##  15074.0  15129.8  -7530.0  15060.0    21367 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7183 -0.4386 -0.2245  0.2419 12.7554 
## 
## Random effects:
##  Groups  Name         Variance Std.Dev.
##  cntry   (Intercept)  0.1151   0.3393  
##  cntry.1 galtan.z.gmc 0.0000   0.0000  
## Number of obs: 21374, groups:  cntry, 20
## 
## Fixed effects:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  -1.29044    0.09981 -12.929  < 2e-16 ***
## gndr.c       -0.31057    0.04046  -7.676 1.64e-14 ***
## age10.c      -0.55355    0.01348 -41.050  < 2e-16 ***
## minority.c   -0.28770    0.10399  -2.767  0.00566 ** 
## galtan.z.gmc -0.03717    0.02421  -1.535  0.12468    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.015                     
## age10.c      0.105  0.008              
## minority.c   0.490  0.003  0.089       
## galtn.z.gmc  0.028  0.019 -0.061  0.048
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.galtan,mod3.galtan)
```

```
## Data: exdat
## Models:
## mod2.galtan: childlessness ~ gndr.c + age10.c + minority.c + galtan.z.gmc + (1 | cntry)
## mod3.galtan: childlessness ~ gndr.c + age10.c + minority.c + galtan.z.gmc + (galtan.z.gmc || cntry)
##             npar   AIC   BIC logLik deviance Chisq Df Pr(>Chisq)
## mod2.galtan    6 15072 15120  -7530    15060                    
## mod3.galtan    7 15074 15130  -7530    15060     0  1     0.9998
```

# RQ2 Analysis 

RQ2. If, as expected (H1), childlessness is associated with the GAL-TAN ideology position of the party for which one has voted, are those associations stronger if this dimension is salient in the party’s public stance (i.e., is there a boundary condition to these associations, such that the ideology needs to be salient in order for the association to exist)?

H2. We expect the association between childlessness and the GAL-TAN political ideology of the party for which one has voted to be stronger if this dimension is salient in the party’s public stance.

## galtan salience fixed main effect


```r
mod4.galtan<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          galtan.z.gmc+galtan_salience.z.gmc+
          (galtan.z.gmc||cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight,
        control = glmerControl(optimizer="bobyqa"))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```
## boundary (singular) fit: see help('isSingular')
```

```r
summary(mod4.galtan)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + galtan.z.gmc +  
##     galtan_salience.z.gmc + (galtan.z.gmc || cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa")
## 
##      AIC      BIC   logLik deviance df.resid 
##  15075.2  15138.9  -7529.6  15059.2    21366 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7931 -0.4381 -0.2244  0.2411 12.7627 
## 
## Random effects:
##  Groups  Name         Variance Std.Dev.
##  cntry   (Intercept)  0.1146   0.3385  
##  cntry.1 galtan.z.gmc 0.0000   0.0000  
## Number of obs: 21374, groups:  cntry, 20
## 
## Fixed effects:
##                       Estimate Std. Error z value Pr(>|z|)    
## (Intercept)           -1.28945    0.09967 -12.937  < 2e-16 ***
## gndr.c                -0.31103    0.04047  -7.686 1.52e-14 ***
## age10.c               -0.55274    0.01351 -40.909  < 2e-16 ***
## minority.c            -0.28428    0.10406  -2.732   0.0063 ** 
## galtan.z.gmc          -0.04012    0.02437  -1.646   0.0997 .  
## galtan_salience.z.gmc  0.03033    0.03317   0.914   0.3606    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. gltn..
## gndr.c       0.015                            
## age10.c      0.105  0.007                     
## minority.c   0.491  0.002  0.091              
## galtn.z.gmc  0.026  0.021 -0.069  0.043       
## gltn_slnc..  0.010 -0.013  0.063  0.036 -0.134
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

## fixed interaction between galtan and galtan salience


```r
mod5.galtan<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          galtan.z.gmc+galtan_salience.z.gmc+
          galtan.z.gmc:galtan_salience.z.gmc+
          (galtan.z.gmc||cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight,
        control = glmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e6)))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```r
summary(mod5.galtan)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + galtan.z.gmc +  
##     galtan_salience.z.gmc + galtan.z.gmc:galtan_salience.z.gmc +  
##     (galtan.z.gmc || cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15067.7  15139.5  -7524.9  15049.7    21365 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.9616 -0.4390 -0.2247  0.2398 13.0026 
## 
## Random effects:
##  Groups  Name         Variance  Std.Dev.
##  cntry   (Intercept)  0.1082233 0.32897 
##  cntry.1 galtan.z.gmc 0.0003914 0.01978 
## Number of obs: 21374, groups:  cntry, 20
## 
## Fixed effects:
##                                    Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                        -1.27824    0.09807 -13.034  < 2e-16 ***
## gndr.c                             -0.31033    0.04049  -7.664 1.81e-14 ***
## age10.c                            -0.55121    0.01352 -40.764  < 2e-16 ***
## minority.c                         -0.27924    0.10424  -2.679  0.00739 ** 
## galtan.z.gmc                        0.01179    0.03223   0.366  0.71451    
## galtan_salience.z.gmc               0.02493    0.03428   0.727  0.46712    
## galtan.z.gmc:galtan_salience.z.gmc -0.11345    0.03862  -2.937  0.00331 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. gltn.. glt_..
## gndr.c       0.016                                   
## age10.c      0.108  0.008                            
## minority.c   0.501  0.003  0.092                     
## galtn.z.gmc  0.046  0.025 -0.031  0.051              
## gltn_slnc..  0.005 -0.016  0.058  0.028 -0.176       
## gltn.z.:_.. -0.040 -0.010 -0.032 -0.025 -0.586  0.115
```

### marginal effects


```r
emtrends(mod5.galtan,
         var="galtan.z.gmc",
         specs="galtan_salience.z.gmc",
         at=list(galtan_salience.z.gmc=c(-1,0,1)),infer=c(T,T))
```

```
##  galtan_salience.z.gmc galtan.z.gmc.trend     SE  df asymp.LCL asymp.UCL
##                     -1             0.1252 0.0632 Inf   0.00143    0.2490
##                      0             0.0118 0.0322 Inf  -0.05138    0.0750
##                      1            -0.1017 0.0327 Inf  -0.16578   -0.0375
##  z.ratio p.value
##    1.983  0.0474
##    0.366  0.7145
##   -3.107  0.0019
## 
## Results are averaged over the levels of: gndr.c, minority.c 
## Confidence level used: 0.95
```



## random interaction between galtan and galtan salience


```r
mod6.galtan<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          galtan.z.gmc+galtan_salience.z.gmc+
          galtan.z.gmc:galtan_salience.z.gmc+
          (galtan.z.gmc+galtan_salience.z.gmc+
             galtan.z.gmc:galtan_salience.z.gmc||cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight,
        control = glmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e6)))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```
## boundary (singular) fit: see help('isSingular')
```

```r
summary(mod6.galtan)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + galtan.z.gmc +  
##     galtan_salience.z.gmc + galtan.z.gmc:galtan_salience.z.gmc +  
##     (galtan.z.gmc + galtan_salience.z.gmc + galtan.z.gmc:galtan_salience.z.gmc ||  
##         cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15071.4  15159.1  -7524.7  15049.4    21363 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.9003 -0.4389 -0.2245  0.2403 12.9834 
## 
## Random effects:
##  Groups  Name                               Variance  Std.Dev. 
##  cntry   (Intercept)                        1.081e-01 3.288e-01
##  cntry.1 galtan.z.gmc                       1.511e-04 1.229e-02
##  cntry.2 galtan_salience.z.gmc              1.809e-03 4.253e-02
##  cntry.3 galtan.z.gmc:galtan_salience.z.gmc 2.550e-14 1.597e-07
## Number of obs: 21374, groups:  cntry, 20
## 
## Fixed effects:
##                                    Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                        -1.27917    0.09809 -13.041  < 2e-16 ***
## gndr.c                             -0.30883    0.04058  -7.611 2.72e-14 ***
## age10.c                            -0.55152    0.01354 -40.748  < 2e-16 ***
## minority.c                         -0.28066    0.10433  -2.690  0.00714 ** 
## galtan.z.gmc                        0.00731    0.03267   0.224  0.82297    
## galtan_salience.z.gmc               0.02943    0.03853   0.764  0.44494    
## galtan.z.gmc:galtan_salience.z.gmc -0.11655    0.04009  -2.907  0.00365 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. gltn.. glt_..
## gndr.c       0.015                                   
## age10.c      0.109  0.005                            
## minority.c   0.501  0.001  0.093                     
## galtn.z.gmc  0.050  0.010 -0.023  0.058              
## gltn_slnc.. -0.002 -0.001  0.043  0.017 -0.185       
## gltn.z.:_.. -0.039 -0.019 -0.023 -0.025 -0.538  0.055
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

### marginal effects


```r
emtrends(mod6.galtan,
         var="galtan.z.gmc",
         specs="galtan_salience.z.gmc",
         at=list(galtan_salience.z.gmc=c(-1,0,1)),infer=c(T,T))
```

```
##  galtan_salience.z.gmc galtan.z.gmc.trend     SE  df asymp.LCL asymp.UCL
##                     -1            0.12386 0.0639 Inf  -0.00138    0.2491
##                      0            0.00731 0.0327 Inf  -0.05673    0.0713
##                      1           -0.10924 0.0356 Inf  -0.17899   -0.0395
##  z.ratio p.value
##    1.938  0.0526
##    0.224  0.8230
##   -3.070  0.0021
## 
## Results are averaged over the levels of: gndr.c, minority.c 
## Confidence level used: 0.95
```

# ERQ1 Analysis 

ERQ1. If childlessness is associated with the economic left-right position of the party for which one has voted, are those associations stronger if this dimension is salient in the party’s public stance (i.e., is there a boundary condition to these associations, such that the ideology needs to be salient in order for the association to exist)?


## lrecon salience fixed main effect


```r
mod4.lrecon<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          lrecon.z.gmc+lrecon_salience.z.gmc+
          (lrecon.z.gmc||cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight,
        control = glmerControl(optimizer="bobyqa"))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```
## boundary (singular) fit: see help('isSingular')
```

```r
summary(mod4.lrecon)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + lrecon.z.gmc +  
##     lrecon_salience.z.gmc + (lrecon.z.gmc || cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa")
## 
##      AIC      BIC   logLik deviance df.resid 
##  15077.7  15141.4  -7530.8  15061.7    21366 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7291 -0.4382 -0.2246  0.2410 12.8622 
## 
## Random effects:
##  Groups  Name         Variance Std.Dev.
##  cntry   (Intercept)  0.1159   0.3404  
##  cntry.1 lrecon.z.gmc 0.0000   0.0000  
## Number of obs: 21374, groups:  cntry, 20
## 
## Fixed effects:
##                       Estimate Std. Error z value Pr(>|z|)    
## (Intercept)           -1.28921    0.10004 -12.886  < 2e-16 ***
## gndr.c                -0.31017    0.04046  -7.666 1.78e-14 ***
## age10.c               -0.55518    0.01350 -41.127  < 2e-16 ***
## minority.c            -0.28606    0.10420  -2.745  0.00605 ** 
## lrecon.z.gmc          -0.01847    0.02437  -0.758  0.44852    
## lrecon_salience.z.gmc  0.01774    0.03369   0.527  0.59839    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. lrcn..
## gndr.c       0.016                            
## age10.c      0.106  0.010                     
## minority.c   0.490  0.003  0.092              
## lrecn.z.gmc  0.040  0.022 -0.002  0.079       
## lrcn_slnc.. -0.008 -0.011 -0.074 -0.018 -0.292
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

## fixed interaction between lrecon and lrecon salience


```r
mod5.lrecon<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          lrecon.z.gmc+lrecon_salience.z.gmc+
          lrecon.z.gmc:lrecon_salience.z.gmc+
          (lrecon.z.gmc||cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight,
        control = glmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e6)))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```r
summary(mod5.lrecon)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + lrecon.z.gmc +  
##     lrecon_salience.z.gmc + lrecon.z.gmc:lrecon_salience.z.gmc +  
##     (lrecon.z.gmc || cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15071.8  15143.5  -7526.9  15053.8    21365 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.8049 -0.4402 -0.2246  0.2408 12.9459 
## 
## Random effects:
##  Groups  Name         Variance  Std.Dev.
##  cntry   (Intercept)  0.1158766 0.34041 
##  cntry.1 lrecon.z.gmc 0.0001863 0.01365 
## Number of obs: 21374, groups:  cntry, 20
## 
## Fixed effects:
##                                    Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                        -1.30106    0.10020 -12.985  < 2e-16 ***
## gndr.c                             -0.30729    0.04050  -7.588 3.25e-14 ***
## age10.c                            -0.55290    0.01352 -40.887  < 2e-16 ***
## minority.c                         -0.28235    0.10462  -2.699  0.00696 ** 
## lrecon.z.gmc                       -0.01891    0.02512  -0.753  0.45155    
## lrecon_salience.z.gmc               0.02266    0.03446   0.658  0.51082    
## lrecon.z.gmc:lrecon_salience.z.gmc  0.09939    0.03564   2.788  0.00530 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. lrcn.. lrc_..
## gndr.c       0.015                                   
## age10.c      0.103  0.011                            
## minority.c   0.490  0.005  0.093                     
## lrecn.z.gmc  0.038  0.022 -0.001  0.074              
## lrcn_slnc.. -0.014 -0.011 -0.075 -0.024 -0.304       
## lrcn.z.:_.. -0.043  0.024  0.053  0.017 -0.018  0.063
```

### marginal effects


```r
emtrends(mod5.lrecon,
         var="lrecon.z.gmc",
         specs="lrecon_salience.z.gmc",
         at=list(lrecon_salience.z.gmc=c(-1,0,1)),infer=c(T,T))
```

```
##  lrecon_salience.z.gmc lrecon.z.gmc.trend     SE  df asymp.LCL asymp.UCL
##                     -1            -0.1183 0.0440 Inf  -0.20449   -0.0321
##                      0            -0.0189 0.0251 Inf  -0.06815    0.0303
##                      1             0.0805 0.0432 Inf  -0.00427    0.1652
##  z.ratio p.value
##   -2.690  0.0071
##   -0.753  0.4515
##    1.861  0.0627
## 
## Results are averaged over the levels of: gndr.c, minority.c 
## Confidence level used: 0.95
```



## random interaction between lrecon and lrecon salience


```r
mod6.lrecon<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          lrecon.z.gmc+lrecon_salience.z.gmc+
          lrecon.z.gmc:lrecon_salience.z.gmc+
          (lrecon.z.gmc+lrecon_salience.z.gmc+
             lrecon.z.gmc:lrecon_salience.z.gmc||cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight,
        control = glmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e6)))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```
## boundary (singular) fit: see help('isSingular')
```

```r
summary(mod6.lrecon)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + lrecon.z.gmc +  
##     lrecon_salience.z.gmc + lrecon.z.gmc:lrecon_salience.z.gmc +  
##     (lrecon.z.gmc + lrecon_salience.z.gmc + lrecon.z.gmc:lrecon_salience.z.gmc ||  
##         cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15075.5  15163.2  -7526.8  15053.5    21363 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.8242 -0.4404 -0.2240  0.2408 12.9256 
## 
## Random effects:
##  Groups  Name                               Variance  Std.Dev.
##  cntry   (Intercept)                        0.1158171 0.34032 
##  cntry.1 lrecon.z.gmc                       0.0001315 0.01147 
##  cntry.2 lrecon_salience.z.gmc              0.0019123 0.04373 
##  cntry.3 lrecon.z.gmc:lrecon_salience.z.gmc 0.0000000 0.00000 
## Number of obs: 21374, groups:  cntry, 20
## 
## Fixed effects:
##                                    Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                        -1.30063    0.10020 -12.981  < 2e-16 ***
## gndr.c                             -0.30632    0.04054  -7.555 4.19e-14 ***
## age10.c                            -0.55353    0.01358 -40.752  < 2e-16 ***
## minority.c                         -0.28081    0.10468  -2.683  0.00731 ** 
## lrecon.z.gmc                       -0.01867    0.02509  -0.744  0.45686    
## lrecon_salience.z.gmc               0.01445    0.04104   0.352  0.72474    
## lrecon.z.gmc:lrecon_salience.z.gmc  0.09707    0.03647   2.661  0.00778 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. lrcn.. lrc_..
## gndr.c       0.015                                   
## age10.c      0.102  0.007                            
## minority.c   0.490  0.006  0.090                     
## lrecn.z.gmc  0.040  0.024 -0.001  0.080              
## lrcn_slnc.. -0.015 -0.027 -0.030 -0.032 -0.261       
## lrcn.z.:_.. -0.043  0.019  0.061  0.015 -0.012  0.110
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

### marginal effects


```r
emtrends(mod6.lrecon,
         var="lrecon.z.gmc",
         specs="lrecon_salience.z.gmc",
         at=list(lrecon_salience.z.gmc=c(-1,0,1)),infer=c(T,T))
```

```
##  lrecon_salience.z.gmc lrecon.z.gmc.trend     SE  df asymp.LCL asymp.UCL
##                     -1            -0.1157 0.0445 Inf  -0.20299   -0.0285
##                      0            -0.0187 0.0251 Inf  -0.06785    0.0305
##                      1             0.0784 0.0440 Inf  -0.00789    0.1647
##  z.ratio p.value
##   -2.600  0.0093
##   -0.744  0.4569
##    1.781  0.0749
## 
## Results are averaged over the levels of: gndr.c, minority.c 
## Confidence level used: 0.95
```

# ERQ2 Analysis

## Data exclusions


```r
PP.vars<-
  names(fdat)[which(names(fdat)=="spendvtax.z"):
                which(names(fdat)=="corrupt_salience.z")]

exdat<-fdat %>%
  dplyr::select(childlessness,
                gndr.c,age10.c,
                minority.c,all_of(PP.vars),
                cntry,anweight) %>%
  na.omit()
```

## Variable centering


```r
exdat<-
  group_mean_center(data=exdat,
                    group.var="cntry",
                    vars=PP.vars,
                    grand.init = F)
names(exdat)
```

```
##  [1] "childlessness"                "gndr.c"                      
##  [3] "age10.c"                      "minority.c"                  
##  [5] "spendvtax.z"                  "deregulation.z"              
##  [7] "redistribution.z"             "econ_interven.z"             
##  [9] "civlib_laworder.z"            "sociallifestyle.z"           
## [11] "religious_principle.z"        "immigrate_policy.z"          
## [13] "multiculturalism.z"           "urban_rural.z"               
## [15] "environment.z"                "regions.z"                   
## [17] "international_security.z"     "ethnic_minorities.z"         
## [19] "nationalism.z"                "lrecon_salience.z"           
## [21] "galtan_salience.z"            "antielite_salience.z"        
## [23] "corrupt_salience.z"           "cntry"                       
## [25] "anweight"                     "spendvtax.z.gm"              
## [27] "deregulation.z.gm"            "redistribution.z.gm"         
## [29] "econ_interven.z.gm"           "civlib_laworder.z.gm"        
## [31] "sociallifestyle.z.gm"         "religious_principle.z.gm"    
## [33] "immigrate_policy.z.gm"        "multiculturalism.z.gm"       
## [35] "urban_rural.z.gm"             "environment.z.gm"            
## [37] "regions.z.gm"                 "international_security.z.gm" 
## [39] "ethnic_minorities.z.gm"       "nationalism.z.gm"            
## [41] "lrecon_salience.z.gm"         "galtan_salience.z.gm"        
## [43] "antielite_salience.z.gm"      "corrupt_salience.z.gm"       
## [45] "spendvtax.z.gmc"              "deregulation.z.gmc"          
## [47] "redistribution.z.gmc"         "econ_interven.z.gmc"         
## [49] "civlib_laworder.z.gmc"        "sociallifestyle.z.gmc"       
## [51] "religious_principle.z.gmc"    "immigrate_policy.z.gmc"      
## [53] "multiculturalism.z.gmc"       "urban_rural.z.gmc"           
## [55] "environment.z.gmc"            "regions.z.gmc"               
## [57] "international_security.z.gmc" "ethnic_minorities.z.gmc"     
## [59] "nationalism.z.gmc"            "lrecon_salience.z.gmc"       
## [61] "galtan_salience.z.gmc"        "antielite_salience.z.gmc"    
## [63] "corrupt_salience.z.gmc"
```

## spendvtax

### Fixed


```r
mod2.spendvtax<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          spendvtax.z.gmc+
          (1|cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight)
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```r
summary(mod2.spendvtax)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + spendvtax.z.gmc +  
##     (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15056.7  15104.5  -7522.4  15044.7    21328 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7104 -0.4386 -0.2245  0.2393 12.9138 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.117    0.3421  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                 Estimate Std. Error z value Pr(>|z|)    
## (Intercept)     -1.28891    0.10035 -12.845  < 2e-16 ***
## gndr.c          -0.30833    0.04048  -7.617 2.59e-14 ***
## age10.c         -0.55399    0.01347 -41.142  < 2e-16 ***
## minority.c      -0.28267    0.10411  -2.715  0.00663 ** 
## spendvtax.z.gmc -0.00859    0.02060  -0.417  0.67677    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.015                     
## age10.c      0.105  0.008              
## minority.c   0.488  0.003  0.090       
## spndvtx.z.g  0.033  0.021 -0.035  0.068
```

### Random


```r
mod3.spendvtax<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          spendvtax.z.gmc+
          (spendvtax.z.gmc||cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight,
        control = glmerControl(optimizer="bobyqa"))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```
## boundary (singular) fit: see help('isSingular')
```

```r
summary(mod3.spendvtax)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + spendvtax.z.gmc +  
##     (spendvtax.z.gmc || cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa")
## 
##      AIC      BIC   logLik deviance df.resid 
##  15058.7  15114.5  -7522.4  15044.7    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7104 -0.4386 -0.2245  0.2393 12.9137 
## 
## Random effects:
##  Groups  Name            Variance Std.Dev.
##  cntry   (Intercept)     0.117    0.3421  
##  cntry.1 spendvtax.z.gmc 0.000    0.0000  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                 Estimate Std. Error z value Pr(>|z|)    
## (Intercept)     -1.28890    0.10035 -12.844  < 2e-16 ***
## gndr.c          -0.30833    0.04048  -7.617 2.59e-14 ***
## age10.c         -0.55399    0.01347 -41.142  < 2e-16 ***
## minority.c      -0.28267    0.10411  -2.715  0.00663 ** 
## spendvtax.z.gmc -0.00859    0.02060  -0.417  0.67677    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.015                     
## age10.c      0.105  0.008              
## minority.c   0.488  0.003  0.090       
## spndvtx.z.g  0.033  0.021 -0.035  0.068
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.spendvtax,mod3.spendvtax)
```

```
## Data: exdat
## Models:
## mod2.spendvtax: childlessness ~ gndr.c + age10.c + minority.c + spendvtax.z.gmc + (1 | cntry)
## mod3.spendvtax: childlessness ~ gndr.c + age10.c + minority.c + spendvtax.z.gmc + (spendvtax.z.gmc || cntry)
##                npar   AIC   BIC  logLik deviance Chisq Df Pr(>Chisq)
## mod2.spendvtax    6 15057 15104 -7522.4    15045                    
## mod3.spendvtax    7 15059 15114 -7522.4    15045     0  1     0.9999
```

## deregulation

### Fixed


```r
mod2.deregulation<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          deregulation.z.gmc+
          (1|cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight)
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```r
summary(mod2.deregulation)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + deregulation.z.gmc +  
##     (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15056.9  15104.7  -7522.4  15044.9    21328 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7291 -0.4390 -0.2243  0.2383 12.9556 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1171   0.3423  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                      Estimate Std. Error z value Pr(>|z|)    
## (Intercept)        -1.2875431  0.1003931 -12.825  < 2e-16 ***
## gndr.c             -0.3079811  0.0404771  -7.609 2.77e-14 ***
## age10.c            -0.5541985  0.0134683 -41.148  < 2e-16 ***
## minority.c         -0.2797154  0.1041356  -2.686  0.00723 ** 
## deregulation.z.gmc -0.0000191  0.0237880  -0.001  0.99936    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.015                     
## age10.c      0.104  0.008              
## minority.c   0.488  0.003  0.089       
## drgltn.z.gm  0.036  0.019 -0.043  0.072
```

### Random


```r
mod3.deregulation<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          deregulation.z.gmc+
          (deregulation.z.gmc||cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight,
        control = glmerControl(optimizer="bobyqa"))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```r
summary(mod3.deregulation)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + deregulation.z.gmc +  
##     (deregulation.z.gmc || cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa")
## 
##      AIC      BIC   logLik deviance df.resid 
##  15057.4  15113.2  -7521.7  15043.4    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7596 -0.4388 -0.2246  0.2384 13.1460 
## 
## Random effects:
##  Groups  Name               Variance Std.Dev.
##  cntry   (Intercept)        0.117018 0.34208 
##  cntry.1 deregulation.z.gmc 0.002408 0.04907 
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                     Estimate Std. Error z value Pr(>|z|)    
## (Intercept)        -1.285339   0.100403 -12.802  < 2e-16 ***
## gndr.c             -0.307400   0.040496  -7.591 3.18e-14 ***
## age10.c            -0.553960   0.013481 -41.093  < 2e-16 ***
## minority.c         -0.273708   0.104364  -2.623  0.00873 ** 
## deregulation.z.gmc  0.004092   0.029663   0.138  0.89029    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.016                     
## age10.c      0.104  0.009              
## minority.c   0.489  0.004  0.088       
## drgltn.z.gm  0.030  0.022 -0.026  0.059
```

```r
anova(mod2.deregulation,mod3.deregulation)
```

```
## Data: exdat
## Models:
## mod2.deregulation: childlessness ~ gndr.c + age10.c + minority.c + deregulation.z.gmc + (1 | cntry)
## mod3.deregulation: childlessness ~ gndr.c + age10.c + minority.c + deregulation.z.gmc + (deregulation.z.gmc || cntry)
##                   npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)
## mod2.deregulation    6 15057 15105 -7522.4    15045                     
## mod3.deregulation    7 15057 15113 -7521.7    15043 1.4859  1     0.2229
```

## redistribution

### Fixed


```r
mod2.redistribution<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          redistribution.z.gmc+
          (1|cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight)
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```r
summary(mod2.redistribution)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## childlessness ~ gndr.c + age10.c + minority.c + redistribution.z.gmc +  
##     (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15056.8  15104.7  -7522.4  15044.8    21328 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7211 -0.4386 -0.2243  0.2388 12.9273 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1171   0.3422  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                      Estimate Std. Error z value Pr(>|z|)    
## (Intercept)          -1.28826    0.10036 -12.836  < 2e-16 ***
## gndr.c               -0.30810    0.04047  -7.612 2.69e-14 ***
## age10.c              -0.55411    0.01346 -41.161  < 2e-16 ***
## minority.c           -0.28120    0.10408  -2.702   0.0069 ** 
## redistribution.z.gmc -0.00475    0.02149  -0.221   0.8250    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.015                     
## age10.c      0.105  0.009              
## minority.c   0.488  0.002  0.091       
## rdstrbtn.z.  0.032  0.014 -0.028  0.065
```

### Random


```r
mod3.redistribution<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          redistribution.z.gmc+
          (redistribution.z.gmc||cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight,
        control = glmerControl(optimizer="bobyqa"))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```r
summary(mod3.redistribution)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## childlessness ~ gndr.c + age10.c + minority.c + redistribution.z.gmc +  
##     (redistribution.z.gmc || cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa")
## 
##      AIC      BIC   logLik deviance df.resid 
##  15058.5  15114.2  -7522.2  15044.5    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7344 -0.4385 -0.2244  0.2387 13.1064 
## 
## Random effects:
##  Groups  Name                 Variance  Std.Dev.
##  cntry   (Intercept)          0.1171093 0.34221 
##  cntry.1 redistribution.z.gmc 0.0008946 0.02991 
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                       Estimate Std. Error z value Pr(>|z|)    
## (Intercept)          -1.287556   0.100392 -12.825  < 2e-16 ***
## gndr.c               -0.307627   0.040486  -7.598    3e-14 ***
## age10.c              -0.553868   0.013472 -41.112  < 2e-16 ***
## minority.c           -0.279380   0.104197  -2.681  0.00733 ** 
## redistribution.z.gmc -0.000807   0.025007  -0.032  0.97426    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.015                     
## age10.c      0.105  0.009              
## minority.c   0.488  0.003  0.091       
## rdstrbtn.z.  0.030  0.019 -0.015  0.061
```

```r
anova(mod2.redistribution,mod3.redistribution)
```

```
## Data: exdat
## Models:
## mod2.redistribution: childlessness ~ gndr.c + age10.c + minority.c + redistribution.z.gmc + (1 | cntry)
## mod3.redistribution: childlessness ~ gndr.c + age10.c + minority.c + redistribution.z.gmc + (redistribution.z.gmc || cntry)
##                     npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)
## mod2.redistribution    6 15057 15105 -7522.4    15045                     
## mod3.redistribution    7 15058 15114 -7522.2    15044 0.3757  1     0.5399
```

## econ_interven

### Fixed


```r
mod2.econ_interven<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          econ_interven.z.gmc+
          (1|cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight)
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```r
summary(mod2.econ_interven)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + econ_interven.z.gmc +  
##     (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15056.3  15104.2  -7522.2  15044.3    21328 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6989 -0.4381 -0.2246  0.2397 12.9022 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.117    0.342   
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                     Estimate Std. Error z value Pr(>|z|)    
## (Intercept)         -1.29047    0.10036 -12.859  < 2e-16 ***
## gndr.c              -0.30854    0.04048  -7.623 2.49e-14 ***
## age10.c             -0.55387    0.01346 -41.137  < 2e-16 ***
## minority.c          -0.28575    0.10419  -2.743   0.0061 ** 
## econ_interven.z.gmc -0.01733    0.02342  -0.740   0.4594    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.015                     
## age10.c      0.105  0.009              
## minority.c   0.489  0.003  0.090       
## ecn_ntrvn..  0.040  0.019 -0.030  0.078
```

### Random


```r
mod3.econ_interven<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          econ_interven.z.gmc+
          (econ_interven.z.gmc||cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight,
        control = glmerControl(optimizer="bobyqa"))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```r
summary(mod3.econ_interven)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + econ_interven.z.gmc +  
##     (econ_interven.z.gmc || cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa")
## 
##      AIC      BIC   logLik deviance df.resid 
##  15058.3  15114.1  -7522.2  15044.3    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7012 -0.4381 -0.2246  0.2397 12.9087 
## 
## Random effects:
##  Groups  Name                Variance  Std.Dev.
##  cntry   (Intercept)         1.169e-01 0.341968
##  cntry.1 econ_interven.z.gmc 9.224e-05 0.009604
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                     Estimate Std. Error z value Pr(>|z|)    
## (Intercept)         -1.29032    0.10040 -12.852  < 2e-16 ***
## gndr.c              -0.30850    0.04049  -7.620 2.54e-14 ***
## age10.c             -0.55385    0.01347 -41.109  < 2e-16 ***
## minority.c          -0.28538    0.10443  -2.733  0.00628 ** 
## econ_interven.z.gmc -0.01707    0.02421  -0.705  0.48066    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.016                     
## age10.c      0.105  0.009              
## minority.c   0.489  0.005  0.092       
## ecn_ntrvn..  0.044  0.023 -0.022  0.089
```

```r
anova(mod2.econ_interven,mod3.econ_interven)
```

```
## Data: exdat
## Models:
## mod2.econ_interven: childlessness ~ gndr.c + age10.c + minority.c + econ_interven.z.gmc + (1 | cntry)
## mod3.econ_interven: childlessness ~ gndr.c + age10.c + minority.c + econ_interven.z.gmc + (econ_interven.z.gmc || cntry)
##                    npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)
## mod2.econ_interven    6 15056 15104 -7522.2    15044                     
## mod3.econ_interven    7 15058 15114 -7522.2    15044 0.0027  1     0.9585
```



## civlib_laworder

### Fixed


```r
mod2.civlib_laworder<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          civlib_laworder.z.gmc+
          (1|cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight)
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```r
summary(mod2.civlib_laworder)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## childlessness ~ gndr.c + age10.c + minority.c + civlib_laworder.z.gmc +  
##     (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15055.5  15103.3  -7521.8  15043.5    21328 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6948 -0.4384 -0.2244  0.2390 12.7996 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1166   0.3415  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                       Estimate Std. Error z value Pr(>|z|)    
## (Intercept)           -1.29046    0.10023 -12.875  < 2e-16 ***
## gndr.c                -0.30900    0.04048  -7.633 2.29e-14 ***
## age10.c               -0.55326    0.01348 -41.044  < 2e-16 ***
## minority.c            -0.28540    0.10398  -2.745  0.00606 ** 
## civlib_laworder.z.gmc -0.02637    0.02244  -1.175  0.23987    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.015                     
## age10.c      0.104  0.008              
## minority.c   0.488  0.003  0.090       
## cvlb_lwrd..  0.026  0.022 -0.056  0.047
```

### Random


```r
mod3.civlib_laworder<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          civlib_laworder.z.gmc+
          (civlib_laworder.z.gmc||cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight,
        control = glmerControl(optimizer="bobyqa"))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```
## boundary (singular) fit: see help('isSingular')
```

```r
summary(mod3.civlib_laworder)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## childlessness ~ gndr.c + age10.c + minority.c + civlib_laworder.z.gmc +  
##     (civlib_laworder.z.gmc || cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa")
## 
##      AIC      BIC   logLik deviance df.resid 
##  15057.5  15113.3  -7521.8  15043.5    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6948 -0.4384 -0.2244  0.2390 12.7996 
## 
## Random effects:
##  Groups  Name                  Variance Std.Dev.
##  cntry   (Intercept)           0.1166   0.3415  
##  cntry.1 civlib_laworder.z.gmc 0.0000   0.0000  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                       Estimate Std. Error z value Pr(>|z|)    
## (Intercept)           -1.29046    0.10023 -12.875  < 2e-16 ***
## gndr.c                -0.30900    0.04048  -7.633 2.29e-14 ***
## age10.c               -0.55326    0.01348 -41.044  < 2e-16 ***
## minority.c            -0.28540    0.10398  -2.745  0.00606 ** 
## civlib_laworder.z.gmc -0.02637    0.02244  -1.175  0.23986    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.015                     
## age10.c      0.104  0.008              
## minority.c   0.488  0.003  0.090       
## cvlb_lwrd..  0.026  0.022 -0.056  0.047
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.civlib_laworder,mod3.civlib_laworder)
```

```
## Data: exdat
## Models:
## mod2.civlib_laworder: childlessness ~ gndr.c + age10.c + minority.c + civlib_laworder.z.gmc + (1 | cntry)
## mod3.civlib_laworder: childlessness ~ gndr.c + age10.c + minority.c + civlib_laworder.z.gmc + (civlib_laworder.z.gmc || cntry)
##                      npar   AIC   BIC  logLik deviance Chisq Df Pr(>Chisq)
## mod2.civlib_laworder    6 15056 15103 -7521.8    15044                    
## mod3.civlib_laworder    7 15058 15113 -7521.8    15044     0  1          1
```

## sociallifestyle

### Fixed


```r
mod2.sociallifestyle<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          sociallifestyle.z.gmc+
          (1|cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight)
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```r
summary(mod2.sociallifestyle)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## childlessness ~ gndr.c + age10.c + minority.c + sociallifestyle.z.gmc +  
##     (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15055.5  15103.3  -7521.8  15043.5    21328 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6940 -0.4388 -0.2246  0.2373 12.7554 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1165   0.3413  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                       Estimate Std. Error z value Pr(>|z|)    
## (Intercept)           -1.29062    0.10019 -12.882  < 2e-16 ***
## gndr.c                -0.30868    0.04048  -7.626 2.41e-14 ***
## age10.c               -0.55332    0.01348 -41.056  < 2e-16 ***
## minority.c            -0.28565    0.10398  -2.747  0.00601 ** 
## sociallifestyle.z.gmc -0.02990    0.02569  -1.164  0.24449    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.015                     
## age10.c      0.104  0.008              
## minority.c   0.489  0.002  0.090       
## scllfstyl..  0.027  0.016 -0.053  0.049
```

### Random


```r
mod3.sociallifestyle<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          sociallifestyle.z.gmc+
          (sociallifestyle.z.gmc||cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight,
        control = glmerControl(optimizer="bobyqa"))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```
## boundary (singular) fit: see help('isSingular')
```

```r
summary(mod3.sociallifestyle)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## childlessness ~ gndr.c + age10.c + minority.c + sociallifestyle.z.gmc +  
##     (sociallifestyle.z.gmc || cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa")
## 
##      AIC      BIC   logLik deviance df.resid 
##  15057.5  15113.3  -7521.8  15043.5    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6940 -0.4388 -0.2246  0.2373 12.7554 
## 
## Random effects:
##  Groups  Name                  Variance Std.Dev.
##  cntry   (Intercept)           0.1165   0.3413  
##  cntry.1 sociallifestyle.z.gmc 0.0000   0.0000  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                       Estimate Std. Error z value Pr(>|z|)    
## (Intercept)           -1.29062    0.10019 -12.881  < 2e-16 ***
## gndr.c                -0.30869    0.04048  -7.626 2.41e-14 ***
## age10.c               -0.55332    0.01348 -41.056  < 2e-16 ***
## minority.c            -0.28566    0.10399  -2.747  0.00601 ** 
## sociallifestyle.z.gmc -0.02990    0.02569  -1.164  0.24449    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.015                     
## age10.c      0.104  0.008              
## minority.c   0.489  0.002  0.090       
## scllfstyl..  0.027  0.016 -0.053  0.049
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.sociallifestyle,mod3.sociallifestyle)
```

```
## Data: exdat
## Models:
## mod2.sociallifestyle: childlessness ~ gndr.c + age10.c + minority.c + sociallifestyle.z.gmc + (1 | cntry)
## mod3.sociallifestyle: childlessness ~ gndr.c + age10.c + minority.c + sociallifestyle.z.gmc + (sociallifestyle.z.gmc || cntry)
##                      npar   AIC   BIC  logLik deviance Chisq Df Pr(>Chisq)
## mod2.sociallifestyle    6 15056 15103 -7521.8    15044                    
## mod3.sociallifestyle    7 15058 15113 -7521.8    15044     0  1     0.9999
```

## religious_principle

### Fixed


```r
mod2.religious_principle<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          religious_principle.z.gmc+
          (1|cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight)
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```r
summary(mod2.religious_principle)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## childlessness ~ gndr.c + age10.c + minority.c + religious_principle.z.gmc +  
##     (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15056.5  15104.3  -7522.3  15044.5    21328 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7126 -0.4381 -0.2244  0.2391 12.8751 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1169   0.342   
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                           Estimate Std. Error z value Pr(>|z|)    
## (Intercept)               -1.28867    0.10029 -12.849  < 2e-16 ***
## gndr.c                    -0.30796    0.04047  -7.610 2.75e-14 ***
## age10.c                   -0.55367    0.01348 -41.059  < 2e-16 ***
## minority.c                -0.28181    0.10392  -2.712  0.00669 ** 
## religious_principle.z.gmc -0.01505    0.02494  -0.603  0.54632    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.015                     
## age10.c      0.104  0.009              
## minority.c   0.488  0.002  0.090       
## rlgs_prnc..  0.019  0.000 -0.064  0.034
```

### Random


```r
mod3.religious_principle<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          religious_principle.z.gmc+
          (religious_principle.z.gmc||cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight,
        control = glmerControl(optimizer="bobyqa"))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```r
summary(mod3.religious_principle)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## childlessness ~ gndr.c + age10.c + minority.c + religious_principle.z.gmc +  
##     (religious_principle.z.gmc || cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa")
## 
##      AIC      BIC   logLik deviance df.resid 
##  15058.5  15114.2  -7522.2  15044.5    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7099 -0.4384 -0.2243  0.2391 12.9333 
## 
## Random effects:
##  Groups  Name                      Variance  Std.Dev.
##  cntry   (Intercept)               0.1168567 0.34184 
##  cntry.1 religious_principle.z.gmc 0.0004964 0.02228 
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                           Estimate Std. Error z value Pr(>|z|)    
## (Intercept)               -1.28858    0.10027 -12.851  < 2e-16 ***
## gndr.c                    -0.30782    0.04048  -7.605 2.85e-14 ***
## age10.c                   -0.55356    0.01349 -41.031  < 2e-16 ***
## minority.c                -0.28131    0.10393  -2.707   0.0068 ** 
## religious_principle.z.gmc -0.01405    0.02719  -0.517   0.6053    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.015                     
## age10.c      0.104  0.010              
## minority.c   0.488  0.002  0.091       
## rlgs_prnc..  0.019  0.003 -0.053  0.034
```

```r
anova(mod2.religious_principle,mod3.religious_principle)
```

```
## Data: exdat
## Models:
## mod2.religious_principle: childlessness ~ gndr.c + age10.c + minority.c + religious_principle.z.gmc + (1 | cntry)
## mod3.religious_principle: childlessness ~ gndr.c + age10.c + minority.c + religious_principle.z.gmc + (religious_principle.z.gmc || cntry)
##                          npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)
## mod2.religious_principle    6 15056 15104 -7522.3    15044                     
## mod3.religious_principle    7 15058 15114 -7522.2    15044 0.0605  1     0.8057
```

## immigrate_policy

### Fixed


```r
mod2.immigrate_policy<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          immigrate_policy.z.gmc+
          (1|cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight)
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```r
summary(mod2.immigrate_policy)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## childlessness ~ gndr.c + age10.c + minority.c + immigrate_policy.z.gmc +  
##     (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15054.8  15102.6  -7521.4  15042.8    21328 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7120 -0.4381 -0.2246  0.2388 12.7798 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1163   0.3411  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                        Estimate Std. Error z value Pr(>|z|)    
## (Intercept)            -1.29099    0.10015 -12.891  < 2e-16 ***
## gndr.c                 -0.30947    0.04049  -7.644 2.11e-14 ***
## age10.c                -0.55316    0.01348 -41.049  < 2e-16 ***
## minority.c             -0.28673    0.10398  -2.757  0.00583 ** 
## immigrate_policy.z.gmc -0.03476    0.02395  -1.451  0.14671    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.015                     
## age10.c      0.105  0.008              
## minority.c   0.489  0.003  0.090       
## immgrt_pl..  0.025  0.026 -0.050  0.046
```

### Random


```r
mod3.immigrate_policy<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          immigrate_policy.z.gmc+
          (immigrate_policy.z.gmc||cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight,
        control = glmerControl(optimizer="bobyqa"))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```
## boundary (singular) fit: see help('isSingular')
```

```r
summary(mod3.immigrate_policy)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## childlessness ~ gndr.c + age10.c + minority.c + immigrate_policy.z.gmc +  
##     (immigrate_policy.z.gmc || cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa")
## 
##      AIC      BIC   logLik deviance df.resid 
##  15056.8  15112.6  -7521.4  15042.8    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7120 -0.4381 -0.2246  0.2388 12.7798 
## 
## Random effects:
##  Groups  Name                   Variance Std.Dev.
##  cntry   (Intercept)            0.1163   0.3411  
##  cntry.1 immigrate_policy.z.gmc 0.0000   0.0000  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                        Estimate Std. Error z value Pr(>|z|)    
## (Intercept)            -1.29099    0.10015 -12.891  < 2e-16 ***
## gndr.c                 -0.30947    0.04049  -7.644 2.11e-14 ***
## age10.c                -0.55316    0.01348 -41.049  < 2e-16 ***
## minority.c             -0.28673    0.10399  -2.757  0.00583 ** 
## immigrate_policy.z.gmc -0.03476    0.02395  -1.451  0.14672    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.015                     
## age10.c      0.105  0.008              
## minority.c   0.489  0.003  0.090       
## immgrt_pl..  0.025  0.026 -0.050  0.047
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.immigrate_policy,mod3.immigrate_policy)
```

```
## Data: exdat
## Models:
## mod2.immigrate_policy: childlessness ~ gndr.c + age10.c + minority.c + immigrate_policy.z.gmc + (1 | cntry)
## mod3.immigrate_policy: childlessness ~ gndr.c + age10.c + minority.c + immigrate_policy.z.gmc + (immigrate_policy.z.gmc || cntry)
##                       npar   AIC   BIC  logLik deviance Chisq Df Pr(>Chisq)
## mod2.immigrate_policy    6 15055 15103 -7521.4    15043                    
## mod3.immigrate_policy    7 15057 15113 -7521.4    15043     0  1          1
```

## multiculturalism

### Fixed


```r
mod2.multiculturalism<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          multiculturalism.z.gmc+
          (1|cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight)
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```r
summary(mod2.multiculturalism)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## childlessness ~ gndr.c + age10.c + minority.c + multiculturalism.z.gmc +  
##     (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15056.3  15104.1  -7522.2  15044.3    21328 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7032 -0.4385 -0.2245  0.2394 12.8617 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1167   0.3417  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                        Estimate Std. Error z value Pr(>|z|)    
## (Intercept)            -1.28980    0.10027 -12.864  < 2e-16 ***
## gndr.c                 -0.30871    0.04048  -7.626 2.42e-14 ***
## age10.c                -0.55364    0.01348 -41.084  < 2e-16 ***
## minority.c             -0.28432    0.10403  -2.733  0.00628 ** 
## multiculturalism.z.gmc -0.01675    0.02199  -0.762  0.44614    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.015                     
## age10.c      0.104  0.008              
## minority.c   0.488  0.003  0.089       
## mltcltrls..  0.030  0.024 -0.052  0.058
```

### Random


```r
mod3.multiculturalism<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          multiculturalism.z.gmc+
          (multiculturalism.z.gmc||cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight,
        control = glmerControl(optimizer="bobyqa"))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```
## boundary (singular) fit: see help('isSingular')
```

```r
summary(mod3.multiculturalism)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## childlessness ~ gndr.c + age10.c + minority.c + multiculturalism.z.gmc +  
##     (multiculturalism.z.gmc || cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa")
## 
##      AIC      BIC   logLik deviance df.resid 
##  15058.3  15114.1  -7522.2  15044.3    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7032 -0.4385 -0.2245  0.2394 12.8617 
## 
## Random effects:
##  Groups  Name                   Variance Std.Dev.
##  cntry   (Intercept)            0.1167   0.3417  
##  cntry.1 multiculturalism.z.gmc 0.0000   0.0000  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                        Estimate Std. Error z value Pr(>|z|)    
## (Intercept)            -1.28979    0.10026 -12.864  < 2e-16 ***
## gndr.c                 -0.30871    0.04048  -7.626 2.43e-14 ***
## age10.c                -0.55364    0.01348 -41.084  < 2e-16 ***
## minority.c             -0.28432    0.10403  -2.733  0.00628 ** 
## multiculturalism.z.gmc -0.01675    0.02199  -0.762  0.44610    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.015                     
## age10.c      0.104  0.008              
## minority.c   0.488  0.003  0.089       
## mltcltrls..  0.030  0.024 -0.052  0.058
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.multiculturalism,mod3.multiculturalism)
```

```
## Data: exdat
## Models:
## mod2.multiculturalism: childlessness ~ gndr.c + age10.c + minority.c + multiculturalism.z.gmc + (1 | cntry)
## mod3.multiculturalism: childlessness ~ gndr.c + age10.c + minority.c + multiculturalism.z.gmc + (multiculturalism.z.gmc || cntry)
##                       npar   AIC   BIC  logLik deviance Chisq Df Pr(>Chisq)
## mod2.multiculturalism    6 15056 15104 -7522.2    15044                    
## mod3.multiculturalism    7 15058 15114 -7522.2    15044     0  1     0.9999
```

## urban_rural

### Fixed


```r
mod2.urban_rural<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          urban_rural.z.gmc+
          (1|cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight)
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```r
summary(mod2.urban_rural)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + urban_rural.z.gmc +  
##     (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15056.7  15104.5  -7522.4  15044.7    21328 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7446 -0.4391 -0.2244  0.2381 13.0245 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1173   0.3425  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                   Estimate Std. Error z value Pr(>|z|)    
## (Intercept)       -1.28609    0.10043 -12.806  < 2e-16 ***
## gndr.c            -0.30797    0.04047  -7.610 2.74e-14 ***
## age10.c           -0.55447    0.01347 -41.160  < 2e-16 ***
## minority.c        -0.27694    0.10408  -2.661  0.00779 ** 
## urban_rural.z.gmc  0.01065    0.02470   0.431  0.66635    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.015                     
## age10.c      0.104  0.009              
## minority.c   0.488  0.002  0.090       
## urbn_rrl.z.  0.033  0.000 -0.048  0.062
```

### Random


```r
mod3.urban_rural<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          urban_rural.z.gmc+
          (urban_rural.z.gmc||cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight,
        control = glmerControl(optimizer="bobyqa"))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```
## boundary (singular) fit: see help('isSingular')
```

```r
summary(mod3.urban_rural)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + urban_rural.z.gmc +  
##     (urban_rural.z.gmc || cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa")
## 
##      AIC      BIC   logLik deviance df.resid 
##  15058.7  15114.5  -7522.4  15044.7    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7446 -0.4391 -0.2244  0.2381 13.0245 
## 
## Random effects:
##  Groups  Name              Variance Std.Dev.
##  cntry   (Intercept)       0.1173   0.3425  
##  cntry.1 urban_rural.z.gmc 0.0000   0.0000  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                   Estimate Std. Error z value Pr(>|z|)    
## (Intercept)       -1.28609    0.10043 -12.806  < 2e-16 ***
## gndr.c            -0.30797    0.04047  -7.610 2.74e-14 ***
## age10.c           -0.55447    0.01347 -41.160  < 2e-16 ***
## minority.c        -0.27694    0.10408  -2.661   0.0078 ** 
## urban_rural.z.gmc  0.01065    0.02470   0.431   0.6664    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.015                     
## age10.c      0.104  0.009              
## minority.c   0.488  0.002  0.090       
## urbn_rrl.z.  0.033  0.000 -0.048  0.062
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.urban_rural,mod3.urban_rural)
```

```
## Data: exdat
## Models:
## mod2.urban_rural: childlessness ~ gndr.c + age10.c + minority.c + urban_rural.z.gmc + (1 | cntry)
## mod3.urban_rural: childlessness ~ gndr.c + age10.c + minority.c + urban_rural.z.gmc + (urban_rural.z.gmc || cntry)
##                  npar   AIC   BIC  logLik deviance Chisq Df Pr(>Chisq)
## mod2.urban_rural    6 15057 15104 -7522.4    15045                    
## mod3.urban_rural    7 15059 15114 -7522.4    15045     0  1          1
```

## environment

### Fixed


```r
mod2.environment<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          environment.z.gmc+
          (1|cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight)
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```r
summary(mod2.environment)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + environment.z.gmc +  
##     (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15054.5  15102.3  -7521.3  15042.5    21328 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7455 -0.4389 -0.2244  0.2395 12.8351 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1161   0.3408  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                   Estimate Std. Error z value Pr(>|z|)    
## (Intercept)       -1.29177    0.10010 -12.905  < 2e-16 ***
## gndr.c            -0.31003    0.04050  -7.656 1.92e-14 ***
## age10.c           -0.55245    0.01350 -40.914  < 2e-16 ***
## minority.c        -0.28723    0.10400  -2.762  0.00575 ** 
## environment.z.gmc -0.03790    0.02453  -1.545  0.12238    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.016                     
## age10.c      0.103  0.006              
## minority.c   0.489  0.003  0.088       
## envrnmnt.z.  0.029  0.034 -0.080  0.047
```

### Random


```r
mod3.environment<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          environment.z.gmc+
          (environment.z.gmc||cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight,
        control = glmerControl(optimizer="bobyqa"))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```
## boundary (singular) fit: see help('isSingular')
```

```r
summary(mod3.environment)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + environment.z.gmc +  
##     (environment.z.gmc || cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa")
## 
##      AIC      BIC   logLik deviance df.resid 
##  15056.5  15112.3  -7521.3  15042.5    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7455 -0.4389 -0.2244  0.2395 12.8351 
## 
## Random effects:
##  Groups  Name              Variance Std.Dev.
##  cntry   (Intercept)       0.1161   0.3408  
##  cntry.1 environment.z.gmc 0.0000   0.0000  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                   Estimate Std. Error z value Pr(>|z|)    
## (Intercept)       -1.29177    0.10010 -12.905  < 2e-16 ***
## gndr.c            -0.31003    0.04050  -7.656 1.92e-14 ***
## age10.c           -0.55245    0.01350 -40.914  < 2e-16 ***
## minority.c        -0.28723    0.10399  -2.762  0.00574 ** 
## environment.z.gmc -0.03790    0.02453  -1.545  0.12239    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.016                     
## age10.c      0.103  0.006              
## minority.c   0.489  0.003  0.088       
## envrnmnt.z.  0.029  0.034 -0.080  0.047
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.environment,mod3.environment)
```

```
## Data: exdat
## Models:
## mod2.environment: childlessness ~ gndr.c + age10.c + minority.c + environment.z.gmc + (1 | cntry)
## mod3.environment: childlessness ~ gndr.c + age10.c + minority.c + environment.z.gmc + (environment.z.gmc || cntry)
##                  npar   AIC   BIC  logLik deviance Chisq Df Pr(>Chisq)
## mod2.environment    6 15054 15102 -7521.3    15042                    
## mod3.environment    7 15056 15112 -7521.3    15042     0  1     0.9999
```

## regions

### Fixed


```r
mod2.regions<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          regions.z.gmc+
          (1|cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight)
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```r
summary(mod2.regions)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + regions.z.gmc +  
##     (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15053.7  15101.5  -7520.8  15041.7    21328 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6842 -0.4396 -0.2245  0.2393 12.6991 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1173   0.3425  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)   -1.28769    0.10038 -12.828  < 2e-16 ***
## gndr.c        -0.30831    0.04048  -7.617 2.59e-14 ***
## age10.c       -0.55322    0.01347 -41.081  < 2e-16 ***
## minority.c    -0.27967    0.10391  -2.691  0.00711 ** 
## regions.z.gmc -0.04299    0.02392  -1.798  0.07223 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.015                     
## age10.c      0.106  0.009              
## minority.c   0.487  0.001  0.092       
## regns.z.gmc  0.002  0.005 -0.036  0.000
```

### Random


```r
mod3.regions<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          regions.z.gmc+
          (regions.z.gmc||cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight,
        control = glmerControl(optimizer="bobyqa"))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```
## boundary (singular) fit: see help('isSingular')
```

```r
summary(mod3.regions)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + regions.z.gmc +  
##     (regions.z.gmc || cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa")
## 
##      AIC      BIC   logLik deviance df.resid 
##  15055.7  15111.4  -7520.8  15041.7    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6842 -0.4396 -0.2245  0.2393 12.6991 
## 
## Random effects:
##  Groups  Name          Variance Std.Dev.
##  cntry   (Intercept)   0.1173   0.3425  
##  cntry.1 regions.z.gmc 0.0000   0.0000  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)   -1.28770    0.10038 -12.828  < 2e-16 ***
## gndr.c        -0.30831    0.04048  -7.617 2.59e-14 ***
## age10.c       -0.55322    0.01347 -41.081  < 2e-16 ***
## minority.c    -0.27968    0.10391  -2.692  0.00711 ** 
## regions.z.gmc -0.04299    0.02392  -1.798  0.07222 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.015                     
## age10.c      0.106  0.009              
## minority.c   0.487  0.001  0.092       
## regns.z.gmc  0.002  0.005 -0.036  0.000
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.regions,mod3.regions)
```

```
## Data: exdat
## Models:
## mod2.regions: childlessness ~ gndr.c + age10.c + minority.c + regions.z.gmc + (1 | cntry)
## mod3.regions: childlessness ~ gndr.c + age10.c + minority.c + regions.z.gmc + (regions.z.gmc || cntry)
##              npar   AIC   BIC  logLik deviance Chisq Df Pr(>Chisq)
## mod2.regions    6 15054 15102 -7520.8    15042                    
## mod3.regions    7 15056 15111 -7520.8    15042     0  1     0.9999
```

## international_security

### Fixed


```r
mod2.international_security<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          international_security.z.gmc+
          (1|cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight)
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```r
summary(mod2.international_security)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## childlessness ~ gndr.c + age10.c + minority.c + international_security.z.gmc +  
##     (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15055.1  15102.9  -7521.6  15043.1    21328 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6960 -0.4387 -0.2243  0.2403 12.9747 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1175   0.3428  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                  -1.28935    0.10043 -12.838  < 2e-16 ***
## gndr.c                       -0.30636    0.04049  -7.566 3.84e-14 ***
## age10.c                      -0.55283    0.01349 -40.970  < 2e-16 ***
## minority.c                   -0.28205    0.10389  -2.715  0.00663 ** 
## international_security.z.gmc  0.03885    0.02903   1.338  0.18078    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.014                     
## age10.c      0.104  0.011              
## minority.c   0.487  0.001  0.091       
## intrntnl_.. -0.015  0.029  0.072 -0.017
```

### Random


```r
mod3.international_security<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          international_security.z.gmc+
          (international_security.z.gmc||cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight,
        control = glmerControl(optimizer="bobyqa"))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```r
summary(mod3.international_security)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## childlessness ~ gndr.c + age10.c + minority.c + international_security.z.gmc +  
##     (international_security.z.gmc || cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa")
## 
##      AIC      BIC   logLik deviance df.resid 
##  15051.8  15107.6  -7518.9  15037.8    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6839 -0.4391 -0.2240  0.2385 12.8155 
## 
## Random effects:
##  Groups  Name                         Variance Std.Dev.
##  cntry   (Intercept)                  0.11705  0.34213 
##  cntry.1 international_security.z.gmc 0.00889  0.09429 
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                  -1.28870    0.10034 -12.844  < 2e-16 ***
## gndr.c                       -0.30363    0.04057  -7.485 7.16e-14 ***
## age10.c                      -0.55204    0.01350 -40.884  < 2e-16 ***
## minority.c                   -0.27398    0.10399  -2.635  0.00842 ** 
## international_security.z.gmc  0.06206    0.04321   1.436  0.15094    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.014                     
## age10.c      0.105  0.012              
## minority.c   0.487  0.003  0.092       
## intrntnl_.. -0.014  0.017  0.049 -0.007
```

```r
anova(mod2.international_security,mod3.international_security)
```

```
## Data: exdat
## Models:
## mod2.international_security: childlessness ~ gndr.c + age10.c + minority.c + international_security.z.gmc + (1 | cntry)
## mod3.international_security: childlessness ~ gndr.c + age10.c + minority.c + international_security.z.gmc + (international_security.z.gmc || cntry)
##                             npar   AIC   BIC  logLik deviance  Chisq Df
## mod2.international_security    6 15055 15103 -7521.6    15043          
## mod3.international_security    7 15052 15108 -7518.9    15038 5.3183  1
##                             Pr(>Chisq)  
## mod2.international_security             
## mod3.international_security     0.0211 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## ethnic_minorities

### Fixed


```r
mod2.ethnic_minorities<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          ethnic_minorities.z.gmc+
          (1|cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight)
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```r
summary(mod2.ethnic_minorities)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## childlessness ~ gndr.c + age10.c + minority.c + ethnic_minorities.z.gmc +  
##     (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15055.3  15103.1  -7521.6  15043.3    21328 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6937 -0.4388 -0.2244  0.2403 12.7617 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1164   0.3412  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                         Estimate Std. Error z value Pr(>|z|)    
## (Intercept)             -1.29128    0.10019 -12.889  < 2e-16 ***
## gndr.c                  -0.30900    0.04048  -7.633 2.29e-14 ***
## age10.c                 -0.55342    0.01347 -41.083  < 2e-16 ***
## minority.c              -0.28697    0.10403  -2.759   0.0058 ** 
## ethnic_minorities.z.gmc -0.03374    0.02645  -1.276   0.2020    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.015                     
## age10.c      0.105  0.008              
## minority.c   0.489  0.003  0.090       
## ethnc_mnr..  0.030  0.020 -0.042  0.055
```

### Random


```r
mod3.ethnic_minorities<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          ethnic_minorities.z.gmc+
          (ethnic_minorities.z.gmc||cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight,
        control = glmerControl(optimizer="bobyqa"))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```
## boundary (singular) fit: see help('isSingular')
```

```r
summary(mod3.ethnic_minorities)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## childlessness ~ gndr.c + age10.c + minority.c + ethnic_minorities.z.gmc +  
##     (ethnic_minorities.z.gmc || cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa")
## 
##      AIC      BIC   logLik deviance df.resid 
##  15057.3  15113.0  -7521.6  15043.3    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6937 -0.4388 -0.2244  0.2403 12.7617 
## 
## Random effects:
##  Groups  Name                    Variance  Std.Dev. 
##  cntry   (Intercept)             1.164e-01 3.412e-01
##  cntry.1 ethnic_minorities.z.gmc 8.077e-15 8.987e-08
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                         Estimate Std. Error z value Pr(>|z|)    
## (Intercept)             -1.29128    0.10018 -12.889  < 2e-16 ***
## gndr.c                  -0.30900    0.04048  -7.634 2.28e-14 ***
## age10.c                 -0.55342    0.01347 -41.083  < 2e-16 ***
## minority.c              -0.28696    0.10402  -2.759  0.00581 ** 
## ethnic_minorities.z.gmc -0.03374    0.02645  -1.276  0.20201    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.015                     
## age10.c      0.105  0.008              
## minority.c   0.489  0.003  0.090       
## ethnc_mnr..  0.030  0.020 -0.042  0.055
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.ethnic_minorities,mod3.ethnic_minorities)
```

```
## Data: exdat
## Models:
## mod2.ethnic_minorities: childlessness ~ gndr.c + age10.c + minority.c + ethnic_minorities.z.gmc + (1 | cntry)
## mod3.ethnic_minorities: childlessness ~ gndr.c + age10.c + minority.c + ethnic_minorities.z.gmc + (ethnic_minorities.z.gmc || cntry)
##                        npar   AIC   BIC  logLik deviance Chisq Df Pr(>Chisq)
## mod2.ethnic_minorities    6 15055 15103 -7521.6    15043                    
## mod3.ethnic_minorities    7 15057 15113 -7521.6    15043     0  1     0.9999
```

## nationalism

### Fixed


```r
mod2.nationalism<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          nationalism.z.gmc+
          (1|cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight)
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```r
summary(mod2.nationalism)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + nationalism.z.gmc +  
##     (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15056.8  15104.6  -7522.4  15044.8    21328 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7197 -0.4388 -0.2244  0.2387 12.9261 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.117    0.3421  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                    Estimate Std. Error z value Pr(>|z|)    
## (Intercept)       -1.288330   0.100350 -12.838  < 2e-16 ***
## gndr.c            -0.308250   0.040484  -7.614 2.65e-14 ***
## age10.c           -0.554035   0.013472 -41.125  < 2e-16 ***
## minority.c        -0.281301   0.104060  -2.703  0.00687 ** 
## nationalism.z.gmc -0.006398   0.025516  -0.251  0.80200    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.015                     
## age10.c      0.104  0.008              
## minority.c   0.488  0.003  0.089       
## ntnlsm.z.gm  0.032  0.027 -0.048  0.061
```

### Random


```r
mod3.nationalism<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          nationalism.z.gmc+
          (nationalism.z.gmc||cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight,
        control = glmerControl(optimizer="bobyqa"))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```
## boundary (singular) fit: see help('isSingular')
```

```r
summary(mod3.nationalism)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + nationalism.z.gmc +  
##     (nationalism.z.gmc || cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa")
## 
##      AIC      BIC   logLik deviance df.resid 
##  15058.8  15114.6  -7522.4  15044.8    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7197 -0.4388 -0.2244  0.2387 12.9261 
## 
## Random effects:
##  Groups  Name              Variance Std.Dev.
##  cntry   (Intercept)       0.117    0.3421  
##  cntry.1 nationalism.z.gmc 0.000    0.0000  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                    Estimate Std. Error z value Pr(>|z|)    
## (Intercept)       -1.288337   0.100355 -12.838  < 2e-16 ***
## gndr.c            -0.308250   0.040484  -7.614 2.66e-14 ***
## age10.c           -0.554034   0.013472 -41.125  < 2e-16 ***
## minority.c        -0.281312   0.104062  -2.703  0.00687 ** 
## nationalism.z.gmc -0.006399   0.025516  -0.251  0.80197    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.015                     
## age10.c      0.104  0.008              
## minority.c   0.488  0.003  0.089       
## ntnlsm.z.gm  0.032  0.027 -0.048  0.061
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.nationalism,mod3.nationalism)
```

```
## Data: exdat
## Models:
## mod2.nationalism: childlessness ~ gndr.c + age10.c + minority.c + nationalism.z.gmc + (1 | cntry)
## mod3.nationalism: childlessness ~ gndr.c + age10.c + minority.c + nationalism.z.gmc + (nationalism.z.gmc || cntry)
##                  npar   AIC   BIC  logLik deviance Chisq Df Pr(>Chisq)
## mod2.nationalism    6 15057 15105 -7522.4    15045                    
## mod3.nationalism    7 15059 15115 -7522.4    15045     0  1     0.9999
```
## antielite_salience

### Fixed


```r
mod2.antielite_salience<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          antielite_salience.z.gmc+
          (1|cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight)
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```r
summary(mod2.antielite_salience)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## childlessness ~ gndr.c + age10.c + minority.c + antielite_salience.z.gmc +  
##     (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15056.9  15104.7  -7522.4  15044.9    21328 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7285 -0.4389 -0.2243  0.2383 12.9565 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1171   0.3423  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                            Estimate Std. Error z value Pr(>|z|)    
## (Intercept)              -1.2875513  0.1003295 -12.833  < 2e-16 ***
## gndr.c                   -0.3079600  0.0404904  -7.606 2.83e-14 ***
## age10.c                  -0.5541838  0.0134927 -41.073  < 2e-16 ***
## minority.c               -0.2797120  0.1038651  -2.693  0.00708 ** 
## antielite_salience.z.gmc  0.0003864  0.0251921   0.015  0.98776    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.014                     
## age10.c      0.105  0.012              
## minority.c   0.487  0.002  0.093       
## antlt_sln.. -0.005  0.032  0.074  0.003
```

### Random


```r
mod3.antielite_salience<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          antielite_salience.z.gmc+
          (antielite_salience.z.gmc||cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight,
        control = glmerControl(optimizer="bobyqa"))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```r
summary(mod3.antielite_salience)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## childlessness ~ gndr.c + age10.c + minority.c + antielite_salience.z.gmc +  
##     (antielite_salience.z.gmc || cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa")
## 
##      AIC      BIC   logLik deviance df.resid 
##  15053.5  15109.3  -7519.8  15039.5    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.8068 -0.4388 -0.2244  0.2390 12.6553 
## 
## Random effects:
##  Groups  Name                     Variance Std.Dev.
##  cntry   (Intercept)              0.116698 0.34161 
##  cntry.1 antielite_salience.z.gmc 0.005722 0.07565 
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                           Estimate Std. Error z value Pr(>|z|)    
## (Intercept)              -1.284675   0.100226 -12.818  < 2e-16 ***
## gndr.c                   -0.306053   0.040530  -7.551 4.31e-14 ***
## age10.c                  -0.553961   0.013506 -41.017  < 2e-16 ***
## minority.c               -0.270097   0.103964  -2.598  0.00938 ** 
## antielite_salience.z.gmc  0.009808   0.036107   0.272  0.78590    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.014                     
## age10.c      0.105  0.012              
## minority.c   0.488  0.002  0.093       
## antlt_sln.. -0.007  0.022  0.057  0.008
```

```r
anova(mod2.antielite_salience,mod3.antielite_salience)
```

```
## Data: exdat
## Models:
## mod2.antielite_salience: childlessness ~ gndr.c + age10.c + minority.c + antielite_salience.z.gmc + (1 | cntry)
## mod3.antielite_salience: childlessness ~ gndr.c + age10.c + minority.c + antielite_salience.z.gmc + (antielite_salience.z.gmc || cntry)
##                         npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)
## mod2.antielite_salience    6 15057 15105 -7522.4    15045                     
## mod3.antielite_salience    7 15054 15109 -7519.8    15040 5.3836  1    0.02033
##                          
## mod2.antielite_salience  
## mod3.antielite_salience *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```



## corrupt_salience

### Fixed


```r
mod2.corrupt_salience<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          corrupt_salience.z.gmc+
          (1|cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight)
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```r
summary(mod2.corrupt_salience)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## childlessness ~ gndr.c + age10.c + minority.c + corrupt_salience.z.gmc +  
##     (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15056.8  15104.6  -7522.4  15044.8    21328 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7200 -0.4389 -0.2242  0.2382 12.9759 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.117    0.342   
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                        Estimate Std. Error z value Pr(>|z|)    
## (Intercept)            -1.28753    0.10029 -12.839  < 2e-16 ***
## gndr.c                 -0.30768    0.04048  -7.601 2.94e-14 ***
## age10.c                -0.55376    0.01351 -40.992  < 2e-16 ***
## minority.c             -0.27913    0.10387  -2.687   0.0072 ** 
## corrupt_salience.z.gmc  0.01299    0.03544   0.366   0.7140    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c      0.015                      
## age10.c     0.106  0.011               
## minority.c  0.487  0.002  0.094        
## crrpt_sln.. 0.000  0.020  0.088  0.015
```

### Random


```r
mod3.corrupt_salience<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          corrupt_salience.z.gmc+
          (corrupt_salience.z.gmc||cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight,
        control = glmerControl(optimizer="bobyqa"))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```r
summary(mod3.corrupt_salience)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## childlessness ~ gndr.c + age10.c + minority.c + corrupt_salience.z.gmc +  
##     (corrupt_salience.z.gmc || cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa")
## 
##      AIC      BIC   logLik deviance df.resid 
##  15055.4  15111.2  -7520.7  15041.4    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7862 -0.4392 -0.2246  0.2385 12.6790 
## 
## Random effects:
##  Groups  Name                   Variance Std.Dev.
##  cntry   (Intercept)            0.116663 0.34156 
##  cntry.1 corrupt_salience.z.gmc 0.009893 0.09946 
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                          Estimate Std. Error z value Pr(>|z|)    
## (Intercept)            -1.2864643  0.1002093 -12.838  < 2e-16 ***
## gndr.c                 -0.3072379  0.0405054  -7.585 3.32e-14 ***
## age10.c                -0.5538747  0.0135313 -40.933  < 2e-16 ***
## minority.c             -0.2762648  0.1039251  -2.658  0.00785 ** 
## corrupt_salience.z.gmc -0.0002616  0.0503417  -0.005  0.99585    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.014                     
## age10.c      0.105  0.010              
## minority.c   0.488  0.002  0.093       
## crrpt_sln.. -0.003  0.009  0.069  0.012
```

```r
anova(mod2.corrupt_salience,mod3.corrupt_salience)
```

```
## Data: exdat
## Models:
## mod2.corrupt_salience: childlessness ~ gndr.c + age10.c + minority.c + corrupt_salience.z.gmc + (1 | cntry)
## mod3.corrupt_salience: childlessness ~ gndr.c + age10.c + minority.c + corrupt_salience.z.gmc + (corrupt_salience.z.gmc || cntry)
##                       npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)  
## mod2.corrupt_salience    6 15057 15105 -7522.4    15045                       
## mod3.corrupt_salience    7 15055 15111 -7520.7    15041 3.3561  1    0.06696 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


```r
sinf<-sessionInfo()
print(sinf,locale=F)
```

```
## R version 4.2.0 (2022-04-22 ucrt)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 10 x64 (build 19043)
## 
## Matrix products: default
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] vjihelpers_0.0.0.9000 dplyr_1.0.9           rio_0.5.29           
## [4] emmeans_1.7.3         lme4_1.1-29           Matrix_1.4-1         
## 
## loaded via a namespace (and not attached):
##  [1] tidyselect_1.1.2  xfun_0.30         bslib_0.3.1       purrr_0.3.4      
##  [5] splines_4.2.0     haven_2.5.0       lattice_0.20-45   generics_0.1.2   
##  [9] vctrs_0.4.1       htmltools_0.5.2   yaml_2.3.5        utf8_1.2.2       
## [13] rlang_1.0.2       jquerylib_0.1.4   nloptr_2.0.0      pillar_1.7.0     
## [17] foreign_0.8-82    glue_1.6.2        readxl_1.4.0      lifecycle_1.0.1  
## [21] stringr_1.4.0     cellranger_1.1.0  zip_2.2.0         mvtnorm_1.1-3    
## [25] evaluate_0.15     knitr_1.39        forcats_0.5.1     fastmap_1.1.0    
## [29] curl_4.3.2        fansi_1.0.3       Rcpp_1.0.8.3      xtable_1.8-4     
## [33] jsonlite_1.8.0    hms_1.1.1         digest_0.6.29     stringi_1.7.6    
## [37] openxlsx_4.2.5    grid_4.2.0        cli_3.3.0         tools_4.2.0      
## [41] magrittr_2.0.3    sass_0.4.1        tibble_3.1.6      crayon_1.5.1     
## [45] pkgconfig_2.0.3   MASS_7.3-56       ellipsis_0.3.2    data.table_1.14.2
## [49] estimability_1.3  minqa_1.2.4       rmarkdown_2.14    rstudioapi_0.13  
## [53] R6_2.5.1          boot_1.3-28       nlme_3.1-157      compiler_4.2.0
```
