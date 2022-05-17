---
title: "ERQ3 Gender moderations"
output: 
  html_document: 
    toc: yes
    keep_md: yes
date: '2022-05-02'
---






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
PP.vars<-
  names(fdat)[which(names(fdat)=="lrgen.z"):
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
  group_mean_center(
    data=exdat,group.var="cntry",
    vars=PP.vars,
    grand.init = F)
```


# ERQ3 Analysis 

## lrgen

### Fixed


```r
mod2.lrgen<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          lrgen.z.gmc+
          lrgen.z.gmc:gndr.c+
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
##     lrgen.z.gmc:gndr.c + (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15058.7  15114.5  -7522.4  15044.7    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7123 -0.4385 -0.2246  0.2390 12.9132 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.117    0.342   
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                     Estimate Std. Error z value Pr(>|z|)    
## (Intercept)        -1.288924   0.100363 -12.843  < 2e-16 ***
## gndr.c             -0.308276   0.040478  -7.616 2.62e-14 ***
## age10.c            -0.554099   0.013459 -41.168  < 2e-16 ***
## minority.c         -0.283067   0.104192  -2.717  0.00659 ** 
## lrgen.z.gmc        -0.009361   0.023557  -0.397  0.69109    
## gndr.c:lrgen.z.gmc  0.006583   0.046948   0.140  0.88849    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. lrgn..
## gndr.c       0.015                            
## age10.c      0.105  0.009                     
## minority.c   0.488  0.003  0.091              
## lrgen.z.gmc  0.039  0.020 -0.018  0.078       
## gndr.c:lr..  0.012  0.007 -0.004  0.000  0.096
```

### Random


```r
mod3.lrgen<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          lrgen.z.gmc+
          lrgen.z.gmc:gndr.c+
          (lrgen.z.gmc+gndr.c+lrgen.z.gmc:gndr.c|cntry),
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
summary(mod3.lrgen)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + lrgen.z.gmc +  
##     lrgen.z.gmc:gndr.c + (lrgen.z.gmc + gndr.c + lrgen.z.gmc:gndr.c |  
##     cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15061.5  15189.0  -7514.8  15029.5    21318 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.8679 -0.4383 -0.2245  0.2361 13.5489 
## 
## Random effects:
##  Groups Name               Variance Std.Dev. Corr             
##  cntry  (Intercept)        0.115760 0.34024                   
##         lrgen.z.gmc        0.001216 0.03487  -0.57            
##         gndr.c             0.015347 0.12388   0.29 -0.95      
##         lrgen.z.gmc:gndr.c 0.018688 0.13670  -0.38 -0.54  0.77
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                    Estimate Std. Error z value Pr(>|z|)    
## (Intercept)        -1.28343    0.10002 -12.832  < 2e-16 ***
## gndr.c             -0.27877    0.05909  -4.718 2.38e-06 ***
## age10.c            -0.55461    0.01348 -41.129  < 2e-16 ***
## minority.c         -0.27261    0.10438  -2.612  0.00901 ** 
## lrgen.z.gmc        -0.01372    0.02613  -0.525  0.59948    
## gndr.c:lrgen.z.gmc  0.03781    0.06421   0.589  0.55598    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. lrgn..
## gndr.c       0.142                            
## age10.c      0.104 -0.001                     
## minority.c   0.490  0.004  0.090              
## lrgen.z.gmc -0.127 -0.260 -0.015  0.065       
## gndr.c:lr.. -0.160  0.404 -0.013 -0.002 -0.136
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.lrgen,mod3.lrgen)
```

```
## Data: exdat
## Models:
## mod2.lrgen: childlessness ~ gndr.c + age10.c + minority.c + lrgen.z.gmc + lrgen.z.gmc:gndr.c + (1 | cntry)
## mod3.lrgen: childlessness ~ gndr.c + age10.c + minority.c + lrgen.z.gmc + lrgen.z.gmc:gndr.c + (lrgen.z.gmc + gndr.c + lrgen.z.gmc:gndr.c | cntry)
##            npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)  
## mod2.lrgen    7 15059 15114 -7522.4    15045                       
## mod3.lrgen   16 15062 15189 -7514.8    15030 15.201  9    0.08556 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


## lrecon

### Fixed


```r
mod2.lrecon<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          lrecon.z.gmc+
          lrecon.z.gmc:gndr.c+
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
##     lrecon.z.gmc:gndr.c + (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15057.2  15113.0  -7521.6  15043.2    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6728 -0.4394 -0.2248  0.2378 13.0204 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1167   0.3416  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                     Estimate Std. Error z value Pr(>|z|)    
## (Intercept)         -1.28928    0.10030 -12.854  < 2e-16 ***
## gndr.c              -0.30802    0.04048  -7.610 2.74e-14 ***
## age10.c             -0.55401    0.01346 -41.155  < 2e-16 ***
## minority.c          -0.28604    0.10425  -2.744  0.00607 ** 
## lrecon.z.gmc        -0.01139    0.02341  -0.487  0.62639    
## gndr.c:lrecon.z.gmc  0.05434    0.04663   1.165  0.24384    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. lrcn..
## gndr.c       0.015                            
## age10.c      0.105  0.009                     
## minority.c   0.489  0.003  0.090              
## lrecn.z.gmc  0.038  0.012 -0.025  0.075       
## gndr.c:lr..  0.003  0.008 -0.004 -0.013  0.088
```

### Random


```r
mod3.lrecon<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          lrecon.z.gmc+
          lrecon.z.gmc:gndr.c+
          (lrecon.z.gmc+gndr.c+lrecon.z.gmc:gndr.c|cntry),
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
summary(mod3.lrecon)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + lrecon.z.gmc +  
##     lrecon.z.gmc:gndr.c + (lrecon.z.gmc + gndr.c + lrecon.z.gmc:gndr.c |  
##     cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15059.0  15186.5  -7513.5  15027.0    21318 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.8344 -0.4395 -0.2254  0.2366 14.1204 
## 
## Random effects:
##  Groups Name                Variance Std.Dev. Corr             
##  cntry  (Intercept)         0.112800 0.33586                   
##         lrecon.z.gmc        0.002725 0.05221  -0.92            
##         gndr.c              0.013734 0.11719   0.35 -0.70      
##         lrecon.z.gmc:gndr.c 0.015452 0.12431  -0.78  0.47  0.30
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                      Estimate Std. Error z value Pr(>|z|)    
## (Intercept)         -1.283009   0.099071 -12.950  < 2e-16 ***
## gndr.c              -0.271551   0.059156  -4.590 4.42e-06 ***
## age10.c             -0.553856   0.013481 -41.083  < 2e-16 ***
## minority.c          -0.282771   0.104377  -2.709  0.00675 ** 
## lrecon.z.gmc        -0.009704   0.028020  -0.346  0.72909    
## gndr.c:lrecon.z.gmc  0.099237   0.060007   1.654  0.09818 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. lrcn..
## gndr.c       0.160                            
## age10.c      0.105  0.001                     
## minority.c   0.495  0.006  0.090              
## lrecn.z.gmc -0.323 -0.244 -0.020  0.052       
## gndr.c:lr.. -0.332  0.225 -0.016 -0.012  0.106
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.lrecon,mod3.lrecon)
```

```
## Data: exdat
## Models:
## mod2.lrecon: childlessness ~ gndr.c + age10.c + minority.c + lrecon.z.gmc + lrecon.z.gmc:gndr.c + (1 | cntry)
## mod3.lrecon: childlessness ~ gndr.c + age10.c + minority.c + lrecon.z.gmc + lrecon.z.gmc:gndr.c + (lrecon.z.gmc + gndr.c + lrecon.z.gmc:gndr.c | cntry)
##             npar   AIC   BIC  logLik deviance Chisq Df Pr(>Chisq)  
## mod2.lrecon    7 15057 15113 -7521.6    15043                      
## mod3.lrecon   16 15059 15186 -7513.5    15027 16.22  9    0.06242 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


## galtan

### Fixed


```r
mod2.galtan<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          galtan.z.gmc+
          galtan.z.gmc:gndr.c+
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
##     galtan.z.gmc:gndr.c + (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15056.5  15112.3  -7521.3  15042.5    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6925 -0.4386 -0.2244  0.2375 12.6991 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1163   0.341   
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                     Estimate Std. Error z value Pr(>|z|)    
## (Intercept)         -1.29163    0.10015 -12.897  < 2e-16 ***
## gndr.c              -0.30981    0.04053  -7.644 2.11e-14 ***
## age10.c             -0.55292    0.01348 -41.006  < 2e-16 ***
## minority.c          -0.28674    0.10397  -2.758  0.00582 ** 
## galtan.z.gmc        -0.03674    0.02431  -1.511  0.13070    
## gndr.c:galtan.z.gmc -0.01840    0.04843  -0.380  0.70392    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. gltn..
## gndr.c       0.015                            
## age10.c      0.104  0.008                     
## minority.c   0.489  0.002  0.089              
## galtn.z.gmc  0.028  0.026 -0.060  0.047       
## gndr.c:gl..  0.005  0.049  0.002 -0.010  0.087
```

### Random


```r
mod3.galtan<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          galtan.z.gmc+
          galtan.z.gmc:gndr.c+
          (galtan.z.gmc+gndr.c+galtan.z.gmc:gndr.c|cntry),
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
summary(mod3.galtan)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + galtan.z.gmc +  
##     galtan.z.gmc:gndr.c + (galtan.z.gmc + gndr.c + galtan.z.gmc:gndr.c |  
##     cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15058.0  15185.5  -7513.0  15026.0    21318 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.8996 -0.4389 -0.2242  0.2380 13.1268 
## 
## Random effects:
##  Groups Name                Variance  Std.Dev. Corr             
##  cntry  (Intercept)         0.1165683 0.34142                   
##         galtan.z.gmc        0.0006636 0.02576  -0.31            
##         gndr.c              0.0158557 0.12592   0.18 -0.99      
##         galtan.z.gmc:gndr.c 0.0268738 0.16393  -0.42 -0.73  0.82
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                      Estimate Std. Error z value Pr(>|z|)    
## (Intercept)         -1.287297   0.100173 -12.851  < 2e-16 ***
## gndr.c              -0.283850   0.060028  -4.729 2.26e-06 ***
## age10.c             -0.553881   0.013513 -40.988  < 2e-16 ***
## minority.c          -0.275718   0.104059  -2.650  0.00806 ** 
## galtan.z.gmc        -0.037633   0.026218  -1.435  0.15119    
## gndr.c:galtan.z.gmc  0.004225   0.072396   0.058  0.95346    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. gltn..
## gndr.c       0.094                            
## age10.c      0.103  0.000                     
## minority.c   0.488  0.005  0.088              
## galtn.z.gmc -0.040 -0.218 -0.051  0.041       
## gndr.c:gl.. -0.191  0.477 -0.006 -0.005 -0.145
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.galtan,mod3.galtan)
```

```
## Data: exdat
## Models:
## mod2.galtan: childlessness ~ gndr.c + age10.c + minority.c + galtan.z.gmc + galtan.z.gmc:gndr.c + (1 | cntry)
## mod3.galtan: childlessness ~ gndr.c + age10.c + minority.c + galtan.z.gmc + galtan.z.gmc:gndr.c + (galtan.z.gmc + gndr.c + galtan.z.gmc:gndr.c | cntry)
##             npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)  
## mod2.galtan    7 15056 15112 -7521.3    15042                       
## mod3.galtan   16 15058 15186 -7513.0    15026 16.513  9    0.05691 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


## spendvtax

### Fixed


```r
mod2.spendvtax<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          spendvtax.z.gmc+
          spendvtax.z.gmc:gndr.c+
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
##     spendvtax.z.gmc:gndr.c + (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15057.7  15113.4  -7521.8  15043.7    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6773 -0.4384 -0.2244  0.2377 13.0104 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1168   0.3418  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                         Estimate Std. Error z value Pr(>|z|)    
## (Intercept)            -1.288758   0.100305 -12.848  < 2e-16 ***
## gndr.c                 -0.307907   0.040476  -7.607  2.8e-14 ***
## age10.c                -0.554026   0.013465 -41.145  < 2e-16 ***
## minority.c             -0.284437   0.104177  -2.730  0.00633 ** 
## spendvtax.z.gmc        -0.006871   0.020672  -0.332  0.73961    
## gndr.c:spendvtax.z.gmc  0.042463   0.041193   1.031  0.30262    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. spnd..
## gndr.c       0.015                            
## age10.c      0.105  0.008                     
## minority.c   0.489  0.003  0.090              
## spndvtx.z.g  0.033  0.014 -0.035  0.067       
## gndr.c:sp..  0.000  0.009 -0.005 -0.017  0.081
```

### Random


```r
mod3.spendvtax<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          spendvtax.z.gmc+
          spendvtax.z.gmc:gndr.c+
          (spendvtax.z.gmc+gndr.c+spendvtax.z.gmc:gndr.c|cntry),
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
summary(mod3.spendvtax)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + spendvtax.z.gmc +  
##     spendvtax.z.gmc:gndr.c + (spendvtax.z.gmc + gndr.c + spendvtax.z.gmc:gndr.c |  
##     cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15059.5  15186.9  -7513.7  15027.5    21318 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.8423 -0.4399 -0.2252  0.2377 13.9227 
## 
## Random effects:
##  Groups Name                   Variance Std.Dev. Corr             
##  cntry  (Intercept)            0.115064 0.33921                   
##         spendvtax.z.gmc        0.001178 0.03432  -1.00            
##         gndr.c                 0.015051 0.12268   0.35 -0.35      
##         spendvtax.z.gmc:gndr.c 0.015585 0.12484  -0.70  0.70  0.42
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                         Estimate Std. Error z value Pr(>|z|)    
## (Intercept)            -1.284843   0.099712 -12.886  < 2e-16 ***
## gndr.c                 -0.273247   0.059981  -4.556 5.23e-06 ***
## age10.c                -0.554469   0.013480 -41.132  < 2e-16 ***
## minority.c             -0.283865   0.104212  -2.724  0.00645 ** 
## spendvtax.z.gmc         0.001943   0.023406   0.083  0.93383    
## gndr.c:spendvtax.z.gmc  0.090640   0.057281   1.582  0.11356    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. spnd..
## gndr.c       0.169                            
## age10.c      0.104  0.001                     
## minority.c   0.491  0.005  0.089              
## spndvtx.z.g -0.274 -0.072 -0.029  0.054       
## gndr.c:sp.. -0.318  0.288 -0.017 -0.018  0.188
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.spendvtax,mod3.spendvtax)
```

```
## Data: exdat
## Models:
## mod2.spendvtax: childlessness ~ gndr.c + age10.c + minority.c + spendvtax.z.gmc + spendvtax.z.gmc:gndr.c + (1 | cntry)
## mod3.spendvtax: childlessness ~ gndr.c + age10.c + minority.c + spendvtax.z.gmc + spendvtax.z.gmc:gndr.c + (spendvtax.z.gmc + gndr.c + spendvtax.z.gmc:gndr.c | cntry)
##                npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)  
## mod2.spendvtax    7 15058 15113 -7521.8    15044                       
## mod3.spendvtax   16 15060 15187 -7513.7    15028 16.203  9    0.06277 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## deregulation

### Fixed


```r
mod2.deregulation<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          deregulation.z.gmc+
          deregulation.z.gmc:gndr.c+
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
##     deregulation.z.gmc:gndr.c + (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15055.9  15111.7  -7521.0  15041.9    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6783 -0.4387 -0.2248  0.2378 13.0841 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1168   0.3417  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                            Estimate Std. Error z value Pr(>|z|)    
## (Intercept)               -1.287752   0.100309 -12.838  < 2e-16 ***
## gndr.c                    -0.307153   0.040476  -7.589 3.24e-14 ***
## age10.c                   -0.554318   0.013468 -41.159  < 2e-16 ***
## minority.c                -0.283389   0.104229  -2.719  0.00655 ** 
## deregulation.z.gmc         0.003449   0.023881   0.144  0.88516    
## gndr.c:deregulation.z.gmc  0.082309   0.047562   1.731  0.08353 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. drgl..
## gndr.c       0.015                            
## age10.c      0.104  0.008                     
## minority.c   0.489  0.003  0.089              
## drgltn.z.gm  0.035  0.008 -0.043  0.070       
## gndr.c:dr.. -0.003  0.010 -0.010 -0.021  0.084
```

### Random


```r
mod3.deregulation<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          deregulation.z.gmc+
          deregulation.z.gmc:gndr.c+
          (deregulation.z.gmc+gndr.c+deregulation.z.gmc:gndr.c|cntry),
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
summary(mod3.deregulation)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + deregulation.z.gmc +  
##     deregulation.z.gmc:gndr.c + (deregulation.z.gmc + gndr.c +  
##     deregulation.z.gmc:gndr.c | cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15055.5  15183.0  -7511.8  15023.5    21318 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.8574 -0.4393 -0.2261  0.2362 13.8253 
## 
## Random effects:
##  Groups Name                      Variance Std.Dev. Corr             
##  cntry  (Intercept)               0.112595 0.33555                   
##         deregulation.z.gmc        0.004922 0.07016  -0.93            
##         gndr.c                    0.013487 0.11613   0.38 -0.70      
##         deregulation.z.gmc:gndr.c 0.012591 0.11221  -0.66  0.34  0.44
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                            Estimate Std. Error z value Pr(>|z|)    
## (Intercept)               -1.277686   0.099036 -12.901  < 2e-16 ***
## gndr.c                    -0.260090   0.059678  -4.358 1.31e-05 ***
## age10.c                   -0.553627   0.013482 -41.065  < 2e-16 ***
## minority.c                -0.277289   0.104416  -2.656  0.00792 ** 
## deregulation.z.gmc         0.009638   0.030529   0.316  0.75223    
## gndr.c:deregulation.z.gmc  0.111032   0.059370   1.870  0.06146 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. drgl..
## gndr.c       0.166                            
## age10.c      0.104 -0.002                     
## minority.c   0.495  0.008  0.089              
## drgltn.z.gm -0.414 -0.287 -0.032  0.049       
## gndr.c:dr.. -0.263  0.261 -0.017 -0.016  0.067
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.deregulation,mod3.deregulation)
```

```
## Data: exdat
## Models:
## mod2.deregulation: childlessness ~ gndr.c + age10.c + minority.c + deregulation.z.gmc + deregulation.z.gmc:gndr.c + (1 | cntry)
## mod3.deregulation: childlessness ~ gndr.c + age10.c + minority.c + deregulation.z.gmc + deregulation.z.gmc:gndr.c + (deregulation.z.gmc + gndr.c + deregulation.z.gmc:gndr.c | cntry)
##                   npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)  
## mod2.deregulation    7 15056 15112 -7521.0    15042                       
## mod3.deregulation   16 15056 15183 -7511.8    15024 18.369  9    0.03112 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


## redistribution

### Fixed


```r
mod2.redistribution<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          redistribution.z.gmc+
          redistribution.z.gmc:gndr.c+
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
##     redistribution.z.gmc:gndr.c + (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15057.2  15112.9  -7521.6  15043.2    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6816 -0.4389 -0.2249  0.2366 13.0947 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1168   0.3417  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                 -1.288013   0.100298 -12.842  < 2e-16 ***
## gndr.c                      -0.307756   0.040472  -7.604 2.87e-14 ***
## age10.c                     -0.554114   0.013462 -41.163  < 2e-16 ***
## minority.c                  -0.282572   0.104142  -2.713  0.00666 ** 
## redistribution.z.gmc        -0.002213   0.021574  -0.103  0.91831    
## gndr.c:redistribution.z.gmc  0.055890   0.043020   1.299  0.19389    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. rdst..
## gndr.c       0.015                            
## age10.c      0.105  0.009                     
## minority.c   0.489  0.002  0.090              
## rdstrbtn.z.  0.032  0.005 -0.028  0.063       
## gndr.c:rd..  0.001  0.005 -0.004 -0.010  0.090
```

### Random


```r
mod3.redistribution<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          redistribution.z.gmc+
          redistribution.z.gmc:gndr.c+
          (redistribution.z.gmc+gndr.c+redistribution.z.gmc:gndr.c|cntry),
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
summary(mod3.redistribution)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## childlessness ~ gndr.c + age10.c + minority.c + redistribution.z.gmc +  
##     redistribution.z.gmc:gndr.c + (redistribution.z.gmc + gndr.c +  
##     redistribution.z.gmc:gndr.c | cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15057.0  15184.5  -7512.5  15025.0    21318 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.8222 -0.4396 -0.2255  0.2362 14.4874 
## 
## Random effects:
##  Groups Name                        Variance Std.Dev. Corr             
##  cntry  (Intercept)                 0.113472 0.33686                   
##         redistribution.z.gmc        0.002473 0.04973  -1.00            
##         gndr.c                      0.013921 0.11799   0.37 -0.37      
##         redistribution.z.gmc:gndr.c 0.012226 0.11057  -0.69  0.69  0.42
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                 -1.28405    0.09928 -12.933  < 2e-16 ***
## gndr.c                      -0.27462    0.05947  -4.618 3.88e-06 ***
## age10.c                     -0.55420    0.01347 -41.140  < 2e-16 ***
## minority.c                  -0.28127    0.10430  -2.697    0.007 ** 
## redistribution.z.gmc         0.01023    0.02549   0.401    0.688    
## gndr.c:redistribution.z.gmc  0.09185    0.05584   1.645    0.100 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. rdst..
## gndr.c       0.168                            
## age10.c      0.105  0.001                     
## minority.c   0.494  0.003  0.090              
## rdstrbtn.z. -0.379 -0.100 -0.025  0.052       
## gndr.c:rd.. -0.283  0.264 -0.014 -0.011  0.218
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.redistribution,mod3.redistribution)
```

```
## Data: exdat
## Models:
## mod2.redistribution: childlessness ~ gndr.c + age10.c + minority.c + redistribution.z.gmc + redistribution.z.gmc:gndr.c + (1 | cntry)
## mod3.redistribution: childlessness ~ gndr.c + age10.c + minority.c + redistribution.z.gmc + redistribution.z.gmc:gndr.c + (redistribution.z.gmc + gndr.c + redistribution.z.gmc:gndr.c | cntry)
##                     npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)  
## mod2.redistribution    7 15057 15113 -7521.6    15043                       
## mod3.redistribution   16 15057 15184 -7512.5    15025 18.125  9    0.03376 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


## econ_interven

### Fixed


```r
mod2.econ_interven<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          econ_interven.z.gmc+
          econ_interven.z.gmc:gndr.c+
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
##     econ_interven.z.gmc:gndr.c + (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15056.8  15112.6  -7521.4  15042.8    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6724 -0.4387 -0.2247  0.2379 12.9881 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1168   0.3417  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                            Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                -1.29022    0.10033 -12.860  < 2e-16 ***
## gndr.c                     -0.30779    0.04048  -7.604 2.87e-14 ***
## age10.c                    -0.55392    0.01346 -41.141  < 2e-16 ***
## minority.c                 -0.28764    0.10425  -2.759   0.0058 ** 
## econ_interven.z.gmc        -0.01462    0.02353  -0.621   0.5343    
## gndr.c:econ_interven.z.gmc  0.05748    0.04685   1.227   0.2199    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. ecn_..
## gndr.c       0.015                            
## age10.c      0.104  0.008                     
## minority.c   0.489  0.003  0.090              
## ecn_ntrvn..  0.040  0.012 -0.030  0.077       
## gndr.c:c_..  0.001  0.014 -0.006 -0.015  0.094
```

### Random


```r
mod3.econ_interven<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          econ_interven.z.gmc+
          econ_interven.z.gmc:gndr.c+
          (econ_interven.z.gmc+gndr.c+econ_interven.z.gmc:gndr.c|cntry),
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
summary(mod3.econ_interven)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + econ_interven.z.gmc +  
##     econ_interven.z.gmc:gndr.c + (econ_interven.z.gmc + gndr.c +  
##     econ_interven.z.gmc:gndr.c | cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15058.8  15186.3  -7513.4  15026.8    21318 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.8093 -0.4401 -0.2257  0.2358 13.6843 
## 
## Random effects:
##  Groups Name                       Variance Std.Dev. Corr             
##  cntry  (Intercept)                0.113664 0.33714                   
##         econ_interven.z.gmc        0.003473 0.05893  -1.00            
##         gndr.c                     0.014977 0.12238   0.36 -0.36      
##         econ_interven.z.gmc:gndr.c 0.014665 0.12110  -0.71  0.71  0.39
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                -1.2838909  0.0993768 -12.919  < 2e-16 ***
## gndr.c                     -0.2700574  0.0604489  -4.468 7.91e-06 ***
## age10.c                    -0.5540077  0.0134765 -41.109  < 2e-16 ***
## minority.c                 -0.2879028  0.1044286  -2.757  0.00583 ** 
## econ_interven.z.gmc        -0.0001854  0.0285025  -0.007  0.99481    
## gndr.c:econ_interven.z.gmc  0.1070518  0.0611593   1.750  0.08005 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. ecn_..
## gndr.c       0.169                            
## age10.c      0.105  0.000                     
## minority.c   0.494  0.005  0.089              
## ecn_ntrvn.. -0.393 -0.099 -0.024  0.058       
## gndr.c:c_.. -0.294  0.261 -0.016 -0.018  0.232
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.econ_interven,mod3.econ_interven)
```

```
## Data: exdat
## Models:
## mod2.econ_interven: childlessness ~ gndr.c + age10.c + minority.c + econ_interven.z.gmc + econ_interven.z.gmc:gndr.c + (1 | cntry)
## mod3.econ_interven: childlessness ~ gndr.c + age10.c + minority.c + econ_interven.z.gmc + econ_interven.z.gmc:gndr.c + (econ_interven.z.gmc + gndr.c + econ_interven.z.gmc:gndr.c | cntry)
##                    npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)  
## mod2.econ_interven    7 15057 15113 -7521.4    15043                       
## mod3.econ_interven   16 15059 15186 -7513.4    15027 15.998  9    0.06693 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```



## civlib_laworder

### Fixed


```r
mod2.civlib_laworder<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          civlib_laworder.z.gmc+
          civlib_laworder.z.gmc:gndr.c+
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
##     civlib_laworder.z.gmc:gndr.c + (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15057.5  15113.3  -7521.8  15043.5    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6944 -0.4384 -0.2244  0.2390 12.7988 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1167   0.3415  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                  -1.2904685  0.1002304 -12.875  < 2e-16 ***
## gndr.c                       -0.3090135  0.0405049  -7.629 2.36e-14 ***
## age10.c                      -0.5532645  0.0134799 -41.044  < 2e-16 ***
## minority.c                   -0.2853940  0.1039843  -2.745  0.00606 ** 
## civlib_laworder.z.gmc        -0.0263865  0.0225280  -1.171  0.24149    
## gndr.c:civlib_laworder.z.gmc -0.0003552  0.0448961  -0.008  0.99369    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. cvl_..
## gndr.c       0.015                            
## age10.c      0.104  0.008                     
## minority.c   0.488  0.002  0.090              
## cvlb_lwrd..  0.026  0.025 -0.056  0.045       
## gndr.c:c_..  0.005  0.034 -0.004 -0.013  0.090
```

### Random


```r
mod3.civlib_laworder<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          civlib_laworder.z.gmc+
          civlib_laworder.z.gmc:gndr.c+
          (civlib_laworder.z.gmc+gndr.c+civlib_laworder.z.gmc:gndr.c|cntry),
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
summary(mod3.civlib_laworder)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## childlessness ~ gndr.c + age10.c + minority.c + civlib_laworder.z.gmc +  
##     civlib_laworder.z.gmc:gndr.c + (civlib_laworder.z.gmc + gndr.c +  
##     civlib_laworder.z.gmc:gndr.c | cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15057.9  15185.4  -7513.0  15025.9    21318 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.8759 -0.4388 -0.2240  0.2387 13.1464 
## 
## Random effects:
##  Groups Name                         Variance  Std.Dev. Corr             
##  cntry  (Intercept)                  0.1173518 0.34257                   
##         civlib_laworder.z.gmc        0.0005976 0.02445   0.03            
##         gndr.c                       0.0159829 0.12642   0.24 -0.96      
##         civlib_laworder.z.gmc:gndr.c 0.0237516 0.15412  -0.44 -0.91  0.77
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                  -1.28593    0.10035 -12.814  < 2e-16 ***
## gndr.c                       -0.27900    0.06001  -4.650 3.33e-06 ***
## age10.c                      -0.55450    0.01351 -41.045  < 2e-16 ***
## minority.c                   -0.27364    0.10397  -2.632  0.00849 ** 
## civlib_laworder.z.gmc        -0.02919    0.02429  -1.202  0.22954    
## gndr.c:civlib_laworder.z.gmc  0.02249    0.06695   0.336  0.73694    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. cvl_..
## gndr.c       0.123                            
## age10.c      0.103  0.000                     
## minority.c   0.487  0.005  0.088              
## cvlb_lwrd..  0.030 -0.220 -0.046  0.040       
## gndr.c:c_.. -0.204  0.458 -0.011 -0.008 -0.177
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.civlib_laworder,mod3.civlib_laworder)
```

```
## Data: exdat
## Models:
## mod2.civlib_laworder: childlessness ~ gndr.c + age10.c + minority.c + civlib_laworder.z.gmc + civlib_laworder.z.gmc:gndr.c + (1 | cntry)
## mod3.civlib_laworder: childlessness ~ gndr.c + age10.c + minority.c + civlib_laworder.z.gmc + civlib_laworder.z.gmc:gndr.c + (civlib_laworder.z.gmc + gndr.c + civlib_laworder.z.gmc:gndr.c | cntry)
##                      npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)  
## mod2.civlib_laworder    7 15058 15113 -7521.8    15044                       
## mod3.civlib_laworder   16 15058 15185 -7513.0    15026 17.573  9    0.04047 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```





## sociallifestyle

### Fixed


```r
mod2.sociallifestyle<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          sociallifestyle.z.gmc+
          sociallifestyle.z.gmc:gndr.c+
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
##     sociallifestyle.z.gmc:gndr.c + (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15057.4  15113.2  -7521.7  15043.4    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7015 -0.4387 -0.2247  0.2370 12.7089 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1165   0.3413  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                  -1.29076    0.10019 -12.883  < 2e-16 ***
## gndr.c                       -0.30922    0.04051  -7.632  2.3e-14 ***
## age10.c                      -0.55334    0.01348 -41.057  < 2e-16 ***
## minority.c                   -0.28535    0.10398  -2.744  0.00607 ** 
## sociallifestyle.z.gmc        -0.03066    0.02581  -1.188  0.23472    
## gndr.c:sociallifestyle.z.gmc -0.01633    0.05144  -0.317  0.75094    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. scll..
## gndr.c       0.015                            
## age10.c      0.104  0.008                     
## minority.c   0.488  0.002  0.090              
## scllfstyl..  0.028  0.022 -0.052  0.048       
## gndr.c:sc..  0.004  0.042  0.005 -0.009  0.094
```

### Random


```r
mod3.sociallifestyle<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          sociallifestyle.z.gmc+
          sociallifestyle.z.gmc:gndr.c+
          (sociallifestyle.z.gmc+gndr.c+sociallifestyle.z.gmc:gndr.c|cntry),
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
summary(mod3.sociallifestyle)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## childlessness ~ gndr.c + age10.c + minority.c + sociallifestyle.z.gmc +  
##     sociallifestyle.z.gmc:gndr.c + (sociallifestyle.z.gmc + gndr.c +  
##     sociallifestyle.z.gmc:gndr.c | cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15059.7  15187.1  -7513.8  15027.7    21318 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.8776 -0.4387 -0.2243  0.2384 13.4112 
## 
## Random effects:
##  Groups Name                         Variance  Std.Dev. Corr             
##  cntry  (Intercept)                  0.1168387 0.34182                   
##         sociallifestyle.z.gmc        0.0001866 0.01366  -1.00            
##         gndr.c                       0.0150725 0.12277   0.22 -0.22      
##         sociallifestyle.z.gmc:gndr.c 0.0278015 0.16674  -0.35  0.35  0.84
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                  -1.288141   0.100258 -12.848  < 2e-16 ***
## gndr.c                       -0.292311   0.059206  -4.937 7.93e-07 ***
## age10.c                      -0.554000   0.013496 -41.049  < 2e-16 ***
## minority.c                   -0.278786   0.104058  -2.679  0.00738 ** 
## sociallifestyle.z.gmc        -0.031873   0.026352  -1.210  0.22647    
## gndr.c:sociallifestyle.z.gmc -0.005435   0.076030  -0.071  0.94302    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. scll..
## gndr.c       0.110                            
## age10.c      0.104  0.002                     
## minority.c   0.488  0.002  0.089              
## scllfstyl.. -0.082 -0.004 -0.051  0.044       
## gndr.c:sc.. -0.154  0.480 -0.002 -0.008  0.094
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.sociallifestyle,mod3.sociallifestyle)
```

```
## Data: exdat
## Models:
## mod2.sociallifestyle: childlessness ~ gndr.c + age10.c + minority.c + sociallifestyle.z.gmc + sociallifestyle.z.gmc:gndr.c + (1 | cntry)
## mod3.sociallifestyle: childlessness ~ gndr.c + age10.c + minority.c + sociallifestyle.z.gmc + sociallifestyle.z.gmc:gndr.c + (sociallifestyle.z.gmc + gndr.c + sociallifestyle.z.gmc:gndr.c | cntry)
##                      npar   AIC   BIC  logLik deviance Chisq Df Pr(>Chisq)  
## mod2.sociallifestyle    7 15057 15113 -7521.7    15043                      
## mod3.sociallifestyle   16 15060 15187 -7513.8    15028 15.79  9    0.07141 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## religious_principle

### Fixed


```r
mod2.religious_principle<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          religious_principle.z.gmc+
          religious_principle.z.gmc:gndr.c+
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
##     religious_principle.z.gmc:gndr.c + (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15058.5  15114.3  -7522.3  15044.5    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7158 -0.4384 -0.2245  0.2390 12.8563 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1169   0.342   
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                   Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                      -1.288625   0.100290 -12.849  < 2e-16 ***
## gndr.c                           -0.308189   0.040494  -7.611 2.73e-14 ***
## age10.c                          -0.553672   0.013485 -41.059  < 2e-16 ***
## minority.c                       -0.281520   0.103926  -2.709  0.00675 ** 
## religious_principle.z.gmc        -0.015469   0.025071  -0.617  0.53722    
## gndr.c:religious_principle.z.gmc -0.008339   0.049972  -0.167  0.86746    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. rlg_..
## gndr.c       0.015                            
## age10.c      0.104  0.009                     
## minority.c   0.488  0.001  0.090              
## rlgs_prnc..  0.019  0.004 -0.063  0.032       
## gndr.c:r_.. -0.002  0.034  0.003 -0.016  0.101
```

### Random


```r
mod3.religious_principle<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          religious_principle.z.gmc+
          religious_principle.z.gmc:gndr.c+
          (religious_principle.z.gmc+gndr.c+religious_principle.z.gmc:gndr.c|cntry),
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
summary(mod3.religious_principle)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## childlessness ~ gndr.c + age10.c + minority.c + religious_principle.z.gmc +  
##     religious_principle.z.gmc:gndr.c + (religious_principle.z.gmc +  
##     gndr.c + religious_principle.z.gmc:gndr.c | cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15064.7  15192.2  -7516.4  15032.7    21318 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.8783 -0.4393 -0.2248  0.2371 13.5408 
## 
## Random effects:
##  Groups Name                             Variance  Std.Dev. Corr             
##  cntry  (Intercept)                      0.1169693 0.34201                   
##         religious_principle.z.gmc        0.0005642 0.02375  -0.86            
##         gndr.c                           0.0146512 0.12104   0.23 -0.70      
##         religious_principle.z.gmc:gndr.c 0.0192055 0.13858  -0.58  0.08  0.66
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                   Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                      -1.285919   0.100217 -12.831  < 2e-16 ***
## gndr.c                           -0.282868   0.059838  -4.727 2.28e-06 ***
## age10.c                          -0.554048   0.013494 -41.059  < 2e-16 ***
## minority.c                       -0.278275   0.103934  -2.677  0.00742 ** 
## religious_principle.z.gmc        -0.006982   0.028601  -0.244  0.80714    
## gndr.c:religious_principle.z.gmc  0.039285   0.070411   0.558  0.57688    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. rlg_..
## gndr.c       0.112                            
## age10.c      0.104  0.003                     
## minority.c   0.488  0.002  0.090              
## rlgs_prnc.. -0.129 -0.134 -0.054  0.027       
## gndr.c:r_.. -0.239  0.367  0.000 -0.011  0.038
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.religious_principle,mod3.religious_principle)
```

```
## Data: exdat
## Models:
## mod2.religious_principle: childlessness ~ gndr.c + age10.c + minority.c + religious_principle.z.gmc + religious_principle.z.gmc:gndr.c + (1 | cntry)
## mod3.religious_principle: childlessness ~ gndr.c + age10.c + minority.c + religious_principle.z.gmc + religious_principle.z.gmc:gndr.c + (religious_principle.z.gmc + gndr.c + religious_principle.z.gmc:gndr.c | cntry)
##                          npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)
## mod2.religious_principle    7 15058 15114 -7522.3    15044                     
## mod3.religious_principle   16 15065 15192 -7516.4    15033 11.759  9     0.2273
```

## immigrate_policy

### Fixed


```r
mod2.immigrate_policy<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          immigrate_policy.z.gmc+
          immigrate_policy.z.gmc:gndr.c+
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
##     immigrate_policy.z.gmc:gndr.c + (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15056.8  15112.6  -7521.4  15042.8    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7178 -0.4380 -0.2247  0.2389 12.7913 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1163   0.3411  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                   -1.290921   0.100144 -12.891  < 2e-16 ***
## gndr.c                        -0.309291   0.040513  -7.634 2.27e-14 ***
## age10.c                       -0.553165   0.013476 -41.049  < 2e-16 ***
## minority.c                    -0.286900   0.103994  -2.759   0.0058 ** 
## immigrate_policy.z.gmc        -0.034495   0.024062  -1.434   0.1517    
## gndr.c:immigrate_policy.z.gmc  0.005613   0.047964   0.117   0.9068    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. imm_..
## gndr.c       0.015                            
## age10.c      0.105  0.008                     
## minority.c   0.489  0.002  0.090              
## immgrt_pl..  0.025  0.029 -0.050  0.045       
## gndr.c:m_..  0.005  0.037 -0.006 -0.014  0.095
```

### Random


```r
mod3.immigrate_policy<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          immigrate_policy.z.gmc+
          immigrate_policy.z.gmc:gndr.c+
          (immigrate_policy.z.gmc+gndr.c+immigrate_policy.z.gmc:gndr.c|cntry),
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
summary(mod3.immigrate_policy)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## childlessness ~ gndr.c + age10.c + minority.c + immigrate_policy.z.gmc +  
##     immigrate_policy.z.gmc:gndr.c + (immigrate_policy.z.gmc +  
##     gndr.c + immigrate_policy.z.gmc:gndr.c | cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15059.5  15187.0  -7513.8  15027.5    21318 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.8552 -0.4378 -0.2244  0.2387 12.9941 
## 
## Random effects:
##  Groups Name                          Variance  Std.Dev. Corr             
##  cntry  (Intercept)                   0.1168548 0.3418                    
##         immigrate_policy.z.gmc        0.0009675 0.0311   -0.10            
##         gndr.c                        0.0157298 0.1254    0.20 -0.99      
##         immigrate_policy.z.gmc:gndr.c 0.0254363 0.1595   -0.34 -0.90  0.85
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                   -1.28577    0.10025 -12.826  < 2e-16 ***
## gndr.c                        -0.28379    0.05928  -4.788 1.69e-06 ***
## age10.c                       -0.55400    0.01350 -41.027  < 2e-16 ***
## minority.c                    -0.27420    0.10404  -2.636   0.0084 ** 
## immigrate_policy.z.gmc        -0.03549    0.02650  -1.339   0.1805    
## gndr.c:immigrate_policy.z.gmc  0.02130    0.07035   0.303   0.7621    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. imm_..
## gndr.c       0.105                            
## age10.c      0.104  0.000                     
## minority.c   0.487  0.005  0.088              
## immgrt_pl.. -0.002 -0.246 -0.038  0.039       
## gndr.c:m_.. -0.151  0.470 -0.013 -0.009 -0.189
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.immigrate_policy,mod3.immigrate_policy)
```

```
## Data: exdat
## Models:
## mod2.immigrate_policy: childlessness ~ gndr.c + age10.c + minority.c + immigrate_policy.z.gmc + immigrate_policy.z.gmc:gndr.c + (1 | cntry)
## mod3.immigrate_policy: childlessness ~ gndr.c + age10.c + minority.c + immigrate_policy.z.gmc + immigrate_policy.z.gmc:gndr.c + (immigrate_policy.z.gmc + gndr.c + immigrate_policy.z.gmc:gndr.c | cntry)
##                       npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)  
## mod2.immigrate_policy    7 15057 15113 -7521.4    15043                       
## mod3.immigrate_policy   16 15060 15187 -7513.8    15028 15.269  9    0.08381 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


## multiculturalism

### Fixed


```r
mod2.multiculturalism<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          multiculturalism.z.gmc+
          multiculturalism.z.gmc:gndr.c+
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
##     multiculturalism.z.gmc:gndr.c + (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15057.9  15113.7  -7521.9  15043.9    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7209 -0.4389 -0.2244  0.2385 12.7985 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1168   0.3417  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                   -1.29033    0.10027 -12.868  < 2e-16 ***
## gndr.c                        -0.30962    0.04051  -7.642 2.13e-14 ***
## age10.c                       -0.55364    0.01348 -41.083  < 2e-16 ***
## minority.c                    -0.28388    0.10402  -2.729  0.00635 ** 
## multiculturalism.z.gmc        -0.01788    0.02206  -0.810  0.41771    
## gndr.c:multiculturalism.z.gmc -0.02837    0.04395  -0.646  0.51856    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. mltc..
## gndr.c       0.016                            
## age10.c      0.104  0.008                     
## minority.c   0.488  0.003  0.089              
## mltcltrls..  0.031  0.031 -0.052  0.057       
## gndr.c:ml..  0.009  0.036  0.002 -0.007  0.079
```

### Random


```r
mod3.multiculturalism<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          multiculturalism.z.gmc+
          multiculturalism.z.gmc:gndr.c+
          (multiculturalism.z.gmc+gndr.c+multiculturalism.z.gmc:gndr.c|cntry),
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
summary(mod3.multiculturalism)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## childlessness ~ gndr.c + age10.c + minority.c + multiculturalism.z.gmc +  
##     multiculturalism.z.gmc:gndr.c + (multiculturalism.z.gmc +  
##     gndr.c + multiculturalism.z.gmc:gndr.c | cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15060.3  15187.8  -7514.2  15028.3    21318 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.8896 -0.4390 -0.2244  0.2375 13.2146 
## 
## Random effects:
##  Groups Name                          Variance  Std.Dev. Corr             
##  cntry  (Intercept)                   0.1163845 0.34115                   
##         multiculturalism.z.gmc        0.0008205 0.02864  -0.48            
##         gndr.c                        0.0151381 0.12304   0.20 -0.95      
##         multiculturalism.z.gmc:gndr.c 0.0208021 0.14423  -0.38 -0.63  0.83
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                   -1.28413    0.10012 -12.826  < 2e-16 ***
## gndr.c                        -0.28614    0.05904  -4.846 1.26e-06 ***
## age10.c                       -0.55411    0.01350 -41.042  < 2e-16 ***
## minority.c                    -0.27396    0.10411  -2.631   0.0085 ** 
## multiculturalism.z.gmc        -0.02167    0.02496  -0.868   0.3853    
## gndr.c:multiculturalism.z.gmc  0.02096    0.06522   0.321   0.7479    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. mltc..
## gndr.c       0.101                            
## age10.c      0.104  0.000                     
## minority.c   0.489  0.004  0.088              
## mltcltrls.. -0.090 -0.241 -0.041  0.046       
## gndr.c:ml.. -0.165  0.459 -0.010 -0.003 -0.155
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.multiculturalism,mod3.multiculturalism)
```

```
## Data: exdat
## Models:
## mod2.multiculturalism: childlessness ~ gndr.c + age10.c + minority.c + multiculturalism.z.gmc + multiculturalism.z.gmc:gndr.c + (1 | cntry)
## mod3.multiculturalism: childlessness ~ gndr.c + age10.c + minority.c + multiculturalism.z.gmc + multiculturalism.z.gmc:gndr.c + (multiculturalism.z.gmc + gndr.c + multiculturalism.z.gmc:gndr.c | cntry)
##                       npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)  
## mod2.multiculturalism    7 15058 15114 -7521.9    15044                       
## mod3.multiculturalism   16 15060 15188 -7514.2    15028 15.581  9    0.07616 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


## urban_rural

### Fixed


```r
mod2.urban_rural<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          urban_rural.z.gmc+
          urban_rural.z.gmc:gndr.c+
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
##     urban_rural.z.gmc:gndr.c + (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15058.2  15114.0  -7522.1  15044.2    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7668 -0.4385 -0.2243  0.2380 12.9265 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1174   0.3426  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                           Estimate Std. Error z value Pr(>|z|)    
## (Intercept)              -1.286936   0.100443 -12.813  < 2e-16 ***
## gndr.c                   -0.308481   0.040479  -7.621 2.52e-14 ***
## age10.c                  -0.554549   0.013472 -41.162  < 2e-16 ***
## minority.c               -0.277685   0.104055  -2.669  0.00762 ** 
## urban_rural.z.gmc         0.009223   0.024784   0.372  0.70979    
## gndr.c:urban_rural.z.gmc -0.035788   0.049383  -0.725  0.46864    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. urb_..
## gndr.c       0.015                            
## age10.c      0.104  0.010                     
## minority.c   0.488  0.002  0.090              
## urbn_rrl.z.  0.034  0.007 -0.047  0.062       
## gndr.c:r_..  0.013  0.018  0.011  0.010  0.080
```

### Random


```r
mod3.urban_rural<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          urban_rural.z.gmc+
          urban_rural.z.gmc:gndr.c+
          (urban_rural.z.gmc+gndr.c+urban_rural.z.gmc:gndr.c|cntry),
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
summary(mod3.urban_rural)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + urban_rural.z.gmc +  
##     urban_rural.z.gmc:gndr.c + (urban_rural.z.gmc + gndr.c +  
##     urban_rural.z.gmc:gndr.c | cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15067.4  15194.9  -7517.7  15035.4    21318 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.8571 -0.4389 -0.2241  0.2378 13.6587 
## 
## Random effects:
##  Groups Name                     Variance  Std.Dev. Corr             
##  cntry  (Intercept)              0.1163635 0.34112                   
##         urban_rural.z.gmc        0.0003329 0.01825  -1.00            
##         gndr.c                   0.0125850 0.11218   0.29 -0.29      
##         urban_rural.z.gmc:gndr.c 0.0093831 0.09687  -0.80  0.80  0.34
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                           Estimate Std. Error z value Pr(>|z|)    
## (Intercept)              -1.282628   0.100142 -12.808  < 2e-16 ***
## gndr.c                   -0.292753   0.058763  -4.982 6.29e-07 ***
## age10.c                  -0.554980   0.013482 -41.163  < 2e-16 ***
## minority.c               -0.271818   0.104222  -2.608  0.00911 ** 
## urban_rural.z.gmc         0.011836   0.025366   0.467  0.64077    
## gndr.c:urban_rural.z.gmc  0.001309   0.061523   0.021  0.98302    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. urb_..
## gndr.c       0.128                            
## age10.c      0.104  0.004                     
## minority.c   0.490  0.000  0.089              
## urbn_rrl.z. -0.115 -0.018 -0.047  0.061       
## gndr.c:r_.. -0.245  0.172  0.000  0.012  0.117
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.urban_rural,mod3.urban_rural)
```

```
## Data: exdat
## Models:
## mod2.urban_rural: childlessness ~ gndr.c + age10.c + minority.c + urban_rural.z.gmc + urban_rural.z.gmc:gndr.c + (1 | cntry)
## mod3.urban_rural: childlessness ~ gndr.c + age10.c + minority.c + urban_rural.z.gmc + urban_rural.z.gmc:gndr.c + (urban_rural.z.gmc + gndr.c + urban_rural.z.gmc:gndr.c | cntry)
##                  npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)
## mod2.urban_rural    7 15058 15114 -7522.1    15044                     
## mod3.urban_rural   16 15067 15195 -7517.7    15035 8.7365  9     0.4619
```



## environment

### Fixed


```r
mod2.environment<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          environment.z.gmc+
          environment.z.gmc:gndr.c+
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
##     environment.z.gmc:gndr.c + (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15055.7  15111.5  -7520.9  15041.7    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6916 -0.4391 -0.2240  0.2390 12.7999 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1162   0.3409  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                          Estimate Std. Error z value Pr(>|z|)    
## (Intercept)              -1.29193    0.10011 -12.905  < 2e-16 ***
## gndr.c                   -0.31202    0.04057  -7.691 1.46e-14 ***
## age10.c                  -0.55249    0.01350 -40.914  < 2e-16 ***
## minority.c               -0.28495    0.10399  -2.740  0.00614 ** 
## environment.z.gmc        -0.03887    0.02455  -1.583  0.11335    
## gndr.c:environment.z.gmc -0.04385    0.04887  -0.897  0.36960    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. envr..
## gndr.c       0.016                            
## age10.c      0.103  0.007                     
## minority.c   0.489  0.002  0.088              
## envrnmnt.z.  0.029  0.042 -0.080  0.045       
## gndr.c:nv..  0.002  0.056  0.005 -0.025  0.043
```

### Random


```r
mod3.environment<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          environment.z.gmc+
          environment.z.gmc:gndr.c+
          (environment.z.gmc+gndr.c+environment.z.gmc:gndr.c|cntry),
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
summary(mod3.environment)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + environment.z.gmc +  
##     environment.z.gmc:gndr.c + (environment.z.gmc + gndr.c +  
##     environment.z.gmc:gndr.c | cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15063.4  15190.9  -7515.7  15031.4    21318 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.8590 -0.4384 -0.2240  0.2401 12.8619 
## 
## Random effects:
##  Groups Name                     Variance  Std.Dev. Corr             
##  cntry  (Intercept)              0.1160686 0.34069                   
##         environment.z.gmc        0.0001545 0.01243  -0.43            
##         gndr.c                   0.0145262 0.12052   0.26 -0.98      
##         environment.z.gmc:gndr.c 0.0132241 0.11500  -0.70 -0.34  0.51
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                           Estimate Std. Error z value Pr(>|z|)    
## (Intercept)              -1.288358   0.100006 -12.883  < 2e-16 ***
## gndr.c                   -0.283122   0.060645  -4.668 3.03e-06 ***
## age10.c                  -0.553547   0.013531 -40.911  < 2e-16 ***
## minority.c               -0.278341   0.104005  -2.676  0.00745 ** 
## environment.z.gmc        -0.038980   0.025759  -1.513  0.13021    
## gndr.c:environment.z.gmc -0.007859   0.061244  -0.128  0.89789    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. envr..
## gndr.c       0.125                            
## age10.c      0.103 -0.001                     
## minority.c   0.489  0.006  0.087              
## envrnmnt.z. -0.014 -0.105 -0.069  0.040       
## gndr.c:nv.. -0.271  0.305 -0.008 -0.021 -0.002
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.environment,mod3.environment)
```

```
## Data: exdat
## Models:
## mod2.environment: childlessness ~ gndr.c + age10.c + minority.c + environment.z.gmc + environment.z.gmc:gndr.c + (1 | cntry)
## mod3.environment: childlessness ~ gndr.c + age10.c + minority.c + environment.z.gmc + environment.z.gmc:gndr.c + (environment.z.gmc + gndr.c + environment.z.gmc:gndr.c | cntry)
##                  npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)
## mod2.environment    7 15056 15112 -7520.9    15042                     
## mod3.environment   16 15063 15191 -7515.7    15031 10.337  9     0.3239
```

## regions

### Fixed


```r
mod2.regions<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          regions.z.gmc+
          regions.z.gmc:gndr.c+
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
##     regions.z.gmc:gndr.c + (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15054.3  15110.1  -7520.2  15040.3    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6640 -0.4394 -0.2244  0.2381 12.8475 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1169   0.342   
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                      Estimate Std. Error z value Pr(>|z|)    
## (Intercept)          -1.28844    0.10030 -12.846  < 2e-16 ***
## gndr.c               -0.30688    0.04049  -7.579 3.49e-14 ***
## age10.c              -0.55333    0.01347 -41.085  < 2e-16 ***
## minority.c           -0.28146    0.10397  -2.707  0.00679 ** 
## regions.z.gmc        -0.03973    0.02409  -1.649  0.09912 .  
## gndr.c:regions.z.gmc  0.05537    0.04813   1.150  0.25004    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. rgns..
## gndr.c       0.014                            
## age10.c      0.106  0.009                     
## minority.c   0.488  0.001  0.092              
## regns.z.gmc  0.001  0.001 -0.037 -0.001       
## gndr.c:rg.. -0.007  0.029 -0.010 -0.015  0.118
```

### Random


```r
mod3.regions<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          regions.z.gmc+
          regions.z.gmc:gndr.c+
          (regions.z.gmc+gndr.c+regions.z.gmc:gndr.c|cntry),
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
summary(mod3.regions)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + regions.z.gmc +  
##     regions.z.gmc:gndr.c + (regions.z.gmc + gndr.c + regions.z.gmc:gndr.c |  
##     cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15061.8  15189.3  -7514.9  15029.8    21318 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.8370 -0.4399 -0.2246  0.2390 12.8777 
## 
## Random effects:
##  Groups Name                 Variance  Std.Dev. Corr             
##  cntry  (Intercept)          1.181e-01 0.343700                  
##         regions.z.gmc        4.551e-05 0.006746 -0.80            
##         gndr.c               1.653e-02 0.128574  0.29 -0.81      
##         regions.z.gmc:gndr.c 1.331e-02 0.115371 -0.07 -0.54  0.93
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                      Estimate Std. Error z value Pr(>|z|)    
## (Intercept)          -1.28525    0.10064 -12.771  < 2e-16 ***
## gndr.c               -0.28879    0.06099  -4.735 2.19e-06 ***
## age10.c              -0.55410    0.01348 -41.094  < 2e-16 ***
## minority.c           -0.27595    0.10410  -2.651  0.00803 ** 
## regions.z.gmc        -0.03877    0.02531  -1.532  0.12558    
## gndr.c:regions.z.gmc  0.01467    0.06556   0.224  0.82295    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. rgns..
## gndr.c       0.135                            
## age10.c      0.105  0.004                     
## minority.c   0.486  0.000  0.091              
## regns.z.gmc -0.042 -0.050 -0.040  0.002       
## gndr.c:rg.. -0.034  0.426 -0.004 -0.017  0.043
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.regions,mod3.regions)
```

```
## Data: exdat
## Models:
## mod2.regions: childlessness ~ gndr.c + age10.c + minority.c + regions.z.gmc + regions.z.gmc:gndr.c + (1 | cntry)
## mod3.regions: childlessness ~ gndr.c + age10.c + minority.c + regions.z.gmc + regions.z.gmc:gndr.c + (regions.z.gmc + gndr.c + regions.z.gmc:gndr.c | cntry)
##              npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)
## mod2.regions    7 15054 15110 -7520.2    15040                     
## mod3.regions   16 15062 15189 -7514.9    15030 10.567  9     0.3066
```


## international_security

### Fixed


```r
mod2.international_security<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          international_security.z.gmc+
          international_security.z.gmc:gndr.c+
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
##     international_security.z.gmc:gndr.c + (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15057.0  15112.7  -7521.5  15043.0    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6909 -0.4386 -0.2242  0.2404 12.9596 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1173   0.3425  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                     Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                         -1.28949    0.10039 -12.845  < 2e-16 ***
## gndr.c                              -0.30619    0.04049  -7.561 3.99e-14 ***
## age10.c                             -0.55290    0.01349 -40.973  < 2e-16 ***
## minority.c                          -0.28247    0.10391  -2.719  0.00656 ** 
## international_security.z.gmc         0.03694    0.02947   1.254  0.20996    
## gndr.c:international_security.z.gmc -0.02268    0.05871  -0.386  0.69929    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. int_..
## gndr.c       0.014                            
## age10.c      0.104  0.011                     
## minority.c   0.487  0.001  0.091              
## intrntnl_.. -0.014  0.029  0.073 -0.015       
## gndr.c:n_..  0.005 -0.011  0.013  0.011  0.169
```

### Random


```r
mod3.international_security<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          international_security.z.gmc+
          international_security.z.gmc:gndr.c+
          (international_security.z.gmc+gndr.c+international_security.z.gmc:gndr.c|cntry),
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
summary(mod3.international_security)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## childlessness ~ gndr.c + age10.c + minority.c + international_security.z.gmc +  
##     international_security.z.gmc:gndr.c + (international_security.z.gmc +  
##     gndr.c + international_security.z.gmc:gndr.c | cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15060.9  15188.4  -7514.5  15028.9    21318 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7094 -0.4384 -0.2239  0.2390 12.6428 
## 
## Random effects:
##  Groups Name                                Variance Std.Dev. Corr             
##  cntry  (Intercept)                         0.115387 0.33969                   
##         international_security.z.gmc        0.006706 0.08189  -0.04            
##         gndr.c                              0.010893 0.10437   0.26 -0.43      
##         international_security.z.gmc:gndr.c 0.017681 0.13297   0.95 -0.31  0.48
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                     Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                         -1.29183    0.09982 -12.942  < 2e-16 ***
## gndr.c                              -0.29161    0.05889  -4.952 7.35e-07 ***
## age10.c                             -0.55204    0.01351 -40.864  < 2e-16 ***
## minority.c                          -0.27702    0.10401  -2.663  0.00774 ** 
## international_security.z.gmc         0.05688    0.04150   1.370  0.17054    
## gndr.c:international_security.z.gmc -0.07102    0.07463  -0.952  0.34134    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. int_..
## gndr.c       0.103                            
## age10.c      0.104  0.004                     
## minority.c   0.489 -0.002  0.091              
## intrntnl_.. -0.030 -0.125  0.051 -0.005       
## gndr.c:n_..  0.354  0.095  0.010  0.018  0.018
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.international_security,mod3.international_security)
```

```
## Data: exdat
## Models:
## mod2.international_security: childlessness ~ gndr.c + age10.c + minority.c + international_security.z.gmc + international_security.z.gmc:gndr.c + (1 | cntry)
## mod3.international_security: childlessness ~ gndr.c + age10.c + minority.c + international_security.z.gmc + international_security.z.gmc:gndr.c + (international_security.z.gmc + gndr.c + international_security.z.gmc:gndr.c | cntry)
##                             npar   AIC   BIC  logLik deviance  Chisq Df
## mod2.international_security    7 15057 15113 -7521.5    15043          
## mod3.international_security   16 15061 15188 -7514.5    15029 14.062  9
##                             Pr(>Chisq)
## mod2.international_security           
## mod3.international_security     0.1202
```



## ethnic_minorities

### Fixed


```r
mod2.ethnic_minorities<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          ethnic_minorities.z.gmc+
          ethnic_minorities.z.gmc:gndr.c+
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
##     ethnic_minorities.z.gmc:gndr.c + (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15057.0  15112.8  -7521.5  15043.0    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7044 -0.4382 -0.2243  0.2399 12.6970 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1165   0.3413  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                    -1.29174    0.10020 -12.892  < 2e-16 ***
## gndr.c                         -0.30974    0.04051  -7.646 2.08e-14 ***
## age10.c                        -0.55341    0.01347 -41.083  < 2e-16 ***
## minority.c                     -0.28668    0.10401  -2.756  0.00584 ** 
## ethnic_minorities.z.gmc        -0.03508    0.02659  -1.319  0.18713    
## gndr.c:ethnic_minorities.z.gmc -0.02634    0.05303  -0.497  0.61934    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. eth_..
## gndr.c       0.016                            
## age10.c      0.105  0.008                     
## minority.c   0.489  0.002  0.090              
## ethnc_mnr..  0.031  0.028 -0.042  0.054       
## gndr.c:t_..  0.009  0.037 -0.001 -0.005  0.102
```

### Random


```r
mod3.ethnic_minorities<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          ethnic_minorities.z.gmc+
          ethnic_minorities.z.gmc:gndr.c+
          (ethnic_minorities.z.gmc+gndr.c+ethnic_minorities.z.gmc:gndr.c|cntry),
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
summary(mod3.ethnic_minorities)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## childlessness ~ gndr.c + age10.c + minority.c + ethnic_minorities.z.gmc +  
##     ethnic_minorities.z.gmc:gndr.c + (ethnic_minorities.z.gmc +  
##     gndr.c + ethnic_minorities.z.gmc:gndr.c | cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15059.3  15186.8  -7513.7  15027.3    21318 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.8656 -0.4384 -0.2244  0.2391 13.1271 
## 
## Random effects:
##  Groups Name                           Variance Std.Dev. Corr             
##  cntry  (Intercept)                    0.115711 0.34016                   
##         ethnic_minorities.z.gmc        0.001101 0.03318  -0.50            
##         gndr.c                         0.015670 0.12518   0.20 -0.95      
##         ethnic_minorities.z.gmc:gndr.c 0.027089 0.16459  -0.32 -0.66  0.86
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                 Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                    -1.287685   0.099954 -12.883  < 2e-16 ***
## gndr.c                         -0.286872   0.059379  -4.831 1.36e-06 ***
## age10.c                        -0.554029   0.013499 -41.043  < 2e-16 ***
## minority.c                     -0.276077   0.104157  -2.651  0.00804 ** 
## ethnic_minorities.z.gmc        -0.036929   0.028640  -1.289  0.19724    
## gndr.c:ethnic_minorities.z.gmc -0.008584   0.075350  -0.114  0.90930    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. eth_..
## gndr.c       0.103                            
## age10.c      0.104  0.000                     
## minority.c   0.489  0.004  0.088              
## ethnc_mnr.. -0.096 -0.230 -0.038  0.047       
## gndr.c:t_.. -0.137  0.471 -0.009 -0.005 -0.135
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.ethnic_minorities,mod3.ethnic_minorities)
```

```
## Data: exdat
## Models:
## mod2.ethnic_minorities: childlessness ~ gndr.c + age10.c + minority.c + ethnic_minorities.z.gmc + ethnic_minorities.z.gmc:gndr.c + (1 | cntry)
## mod3.ethnic_minorities: childlessness ~ gndr.c + age10.c + minority.c + ethnic_minorities.z.gmc + ethnic_minorities.z.gmc:gndr.c + (ethnic_minorities.z.gmc + gndr.c + ethnic_minorities.z.gmc:gndr.c | cntry)
##                        npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)  
## mod2.ethnic_minorities    7 15057 15113 -7521.5    15043                       
## mod3.ethnic_minorities   16 15059 15187 -7513.7    15027 15.674  9      0.074 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


## nationalism

### Fixed


```r
mod2.nationalism<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          nationalism.z.gmc+
          nationalism.z.gmc:gndr.c+
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
##     nationalism.z.gmc:gndr.c + (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15057.9  15113.7  -7522.0  15043.9    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7483 -0.4390 -0.2243  0.2375 12.8374 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1169   0.342   
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                           Estimate Std. Error z value Pr(>|z|)    
## (Intercept)              -1.288998   0.100327 -12.848  < 2e-16 ***
## gndr.c                   -0.309537   0.040518  -7.639 2.18e-14 ***
## age10.c                  -0.554071   0.013473 -41.126  < 2e-16 ***
## minority.c               -0.280449   0.104031  -2.696  0.00702 ** 
## nationalism.z.gmc        -0.008334   0.025608  -0.325  0.74484    
## gndr.c:nationalism.z.gmc -0.048531   0.051000  -0.952  0.34130    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. ntnl..
## gndr.c       0.016                            
## age10.c      0.104  0.008                     
## minority.c   0.488  0.003  0.090              
## ntnlsm.z.gm  0.032  0.036 -0.048  0.060       
## gndr.c:nt..  0.008  0.034  0.005 -0.009  0.080
```

### Random


```r
mod3.nationalism<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          nationalism.z.gmc+
          nationalism.z.gmc:gndr.c+
          (nationalism.z.gmc+gndr.c+nationalism.z.gmc:gndr.c|cntry),
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
summary(mod3.nationalism)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + nationalism.z.gmc +  
##     nationalism.z.gmc:gndr.c + (nationalism.z.gmc + gndr.c +  
##     nationalism.z.gmc:gndr.c | cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15062.5  15190.0  -7515.2  15030.5    21318 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.9240 -0.4395 -0.2241  0.2381 13.2113 
## 
## Random effects:
##  Groups Name                     Variance  Std.Dev. Corr             
##  cntry  (Intercept)              1.183e-01 0.344008                  
##         nationalism.z.gmc        1.534e-05 0.003917 -1.00            
##         gndr.c                   1.490e-02 0.122081  0.24 -0.24      
##         nationalism.z.gmc:gndr.c 2.482e-02 0.157550 -0.36  0.36  0.82
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                           Estimate Std. Error z value Pr(>|z|)    
## (Intercept)              -1.284487   0.100664 -12.760  < 2e-16 ***
## gndr.c                   -0.291660   0.058900  -4.952 7.35e-07 ***
## age10.c                  -0.555022   0.013491 -41.139  < 2e-16 ***
## minority.c               -0.272389   0.104080  -2.617  0.00887 ** 
## nationalism.z.gmc        -0.009708   0.025815  -0.376  0.70687    
## gndr.c:nationalism.z.gmc  0.005279   0.072313   0.073  0.94180    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. ntnl..
## gndr.c       0.124                            
## age10.c      0.103  0.001                     
## minority.c   0.486  0.004  0.088              
## ntnlsm.z.gm  0.002  0.017 -0.048  0.059       
## gndr.c:nt.. -0.153  0.424 -0.007 -0.005  0.072
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.nationalism,mod3.nationalism)
```

```
## Data: exdat
## Models:
## mod2.nationalism: childlessness ~ gndr.c + age10.c + minority.c + nationalism.z.gmc + nationalism.z.gmc:gndr.c + (1 | cntry)
## mod3.nationalism: childlessness ~ gndr.c + age10.c + minority.c + nationalism.z.gmc + nationalism.z.gmc:gndr.c + (nationalism.z.gmc + gndr.c + nationalism.z.gmc:gndr.c | cntry)
##                  npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)
## mod2.nationalism    7 15058 15114 -7522.0    15044                     
## mod3.nationalism   16 15062 15190 -7515.2    15030 13.438  9     0.1437
```


## antielite_salience

### Fixed


```r
mod2.antielite_salience<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          antielite_salience.z.gmc+
          antielite_salience.z.gmc:gndr.c+
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
##     antielite_salience.z.gmc:gndr.c + (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15057.5  15113.3  -7521.7  15043.5    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6991 -0.4395 -0.2243  0.2380 12.8735 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1171   0.3422  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                  Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                     -1.289942   0.100345 -12.855  < 2e-16 ***
## gndr.c                          -0.307543   0.040505  -7.593 3.13e-14 ***
## age10.c                         -0.554235   0.013490 -41.085  < 2e-16 ***
## minority.c                      -0.283069   0.103924  -2.724  0.00645 ** 
## antielite_salience.z.gmc        -0.003469   0.025454  -0.136  0.89161    
## gndr.c:antielite_salience.z.gmc -0.060024   0.050783  -1.182  0.23722    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. ant_..
## gndr.c       0.014                            
## age10.c      0.105  0.011                     
## minority.c   0.488  0.001  0.093              
## antlt_sln.. -0.002  0.039  0.073  0.006       
## gndr.c:n_..  0.021 -0.007  0.007  0.027  0.134
```

### Random


```r
mod3.antielite_salience<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          antielite_salience.z.gmc+
          antielite_salience.z.gmc:gndr.c+
          (antielite_salience.z.gmc+gndr.c+antielite_salience.z.gmc:gndr.c|cntry),
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
summary(mod3.antielite_salience)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## childlessness ~ gndr.c + age10.c + minority.c + antielite_salience.z.gmc +  
##     antielite_salience.z.gmc:gndr.c + (antielite_salience.z.gmc +  
##     gndr.c + antielite_salience.z.gmc:gndr.c | cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15060.1  15187.6  -7514.1  15028.1    21318 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.8207 -0.4386 -0.2237  0.2378 12.6343 
## 
## Random effects:
##  Groups Name                            Variance Std.Dev. Corr          
##  cntry  (Intercept)                     0.117003 0.34206                
##         antielite_salience.z.gmc        0.005815 0.07626  0.28          
##         gndr.c                          0.011468 0.10709  0.28 0.13     
##         antielite_salience.z.gmc:gndr.c 0.015113 0.12294  0.67 0.18 0.90
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                  Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                     -1.287759   0.100358 -12.832  < 2e-16 ***
## gndr.c                          -0.296053   0.057307  -5.166 2.39e-07 ***
## age10.c                         -0.553462   0.013522 -40.929  < 2e-16 ***
## minority.c                      -0.273262   0.104181  -2.623  0.00872 ** 
## antielite_salience.z.gmc         0.008664   0.037201   0.233  0.81583    
## gndr.c:antielite_salience.z.gmc -0.028125   0.063873  -0.440  0.65970    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. ant_..
## gndr.c       0.111                            
## age10.c      0.105  0.009                     
## minority.c   0.488 -0.003  0.093              
## antlt_sln..  0.112  0.097  0.063  0.010       
## gndr.c:n_..  0.279  0.307  0.007  0.016  0.108
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.antielite_salience,mod3.antielite_salience)
```

```
## Data: exdat
## Models:
## mod2.antielite_salience: childlessness ~ gndr.c + age10.c + minority.c + antielite_salience.z.gmc + antielite_salience.z.gmc:gndr.c + (1 | cntry)
## mod3.antielite_salience: childlessness ~ gndr.c + age10.c + minority.c + antielite_salience.z.gmc + antielite_salience.z.gmc:gndr.c + (antielite_salience.z.gmc + gndr.c + antielite_salience.z.gmc:gndr.c | cntry)
##                         npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)
## mod2.antielite_salience    7 15058 15113 -7521.7    15044                     
## mod3.antielite_salience   16 15060 15188 -7514.1    15028 15.385  9    0.08089
##                          
## mod2.antielite_salience  
## mod3.antielite_salience .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## corrupt_salience

### Fixed


```r
mod2.corrupt_salience<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          corrupt_salience.z.gmc+
          corrupt_salience.z.gmc:gndr.c+
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
##     corrupt_salience.z.gmc:gndr.c + (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15057.6  15113.4  -7521.8  15043.6    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7051 -0.4387 -0.2245  0.2376 12.8929 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1171   0.3422  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                   -1.289704   0.100351 -12.852  < 2e-16 ***
## gndr.c                        -0.306113   0.040511  -7.556 4.15e-14 ***
## age10.c                       -0.553774   0.013507 -40.998  < 2e-16 ***
## minority.c                    -0.282715   0.103924  -2.720  0.00652 ** 
## corrupt_salience.z.gmc         0.008585   0.035722   0.240  0.81007    
## gndr.c:corrupt_salience.z.gmc -0.075074   0.071208  -1.054  0.29175    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. crr_..
## gndr.c       0.014                            
## age10.c      0.106  0.011                     
## minority.c   0.487  0.001  0.094              
## crrpt_sln..  0.003  0.023  0.088  0.019       
## gndr.c:c_..  0.022 -0.035  0.004  0.033  0.120
```

### Random


```r
mod3.corrupt_salience<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          corrupt_salience.z.gmc+
          corrupt_salience.z.gmc:gndr.c+
          (corrupt_salience.z.gmc+gndr.c+corrupt_salience.z.gmc:gndr.c|cntry),
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
summary(mod3.corrupt_salience)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## childlessness ~ gndr.c + age10.c + minority.c + corrupt_salience.z.gmc +  
##     corrupt_salience.z.gmc:gndr.c + (corrupt_salience.z.gmc +  
##     gndr.c + corrupt_salience.z.gmc:gndr.c | cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15064.9  15192.4  -7516.4  15032.9    21318 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.8459 -0.4388 -0.2242  0.2380 12.7108 
## 
## Random effects:
##  Groups Name                          Variance Std.Dev. Corr             
##  cntry  (Intercept)                   0.117459 0.34272                   
##         corrupt_salience.z.gmc        0.009043 0.09509   0.23            
##         gndr.c                        0.011414 0.10684   0.30  0.19      
##         corrupt_salience.z.gmc:gndr.c 0.016001 0.12650   0.81 -0.06  0.71
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                   -1.287211   0.100480 -12.811  < 2e-16 ***
## gndr.c                        -0.291494   0.058571  -4.977 6.47e-07 ***
## age10.c                       -0.553844   0.013552 -40.867  < 2e-16 ***
## minority.c                    -0.276994   0.104158  -2.659  0.00783 ** 
## corrupt_salience.z.gmc        -0.004154   0.049955  -0.083  0.93372    
## gndr.c:corrupt_salience.z.gmc -0.059269   0.082493  -0.718  0.47247    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. crr_..
## gndr.c       0.115                            
## age10.c      0.105  0.006                     
## minority.c   0.488 -0.006  0.092              
## crrpt_sln..  0.090  0.100  0.071  0.012       
## gndr.c:c_..  0.278  0.162  0.002  0.030  0.021
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.corrupt_salience,mod3.corrupt_salience)
```

```
## Data: exdat
## Models:
## mod2.corrupt_salience: childlessness ~ gndr.c + age10.c + minority.c + corrupt_salience.z.gmc + corrupt_salience.z.gmc:gndr.c + (1 | cntry)
## mod3.corrupt_salience: childlessness ~ gndr.c + age10.c + minority.c + corrupt_salience.z.gmc + corrupt_salience.z.gmc:gndr.c + (corrupt_salience.z.gmc + gndr.c + corrupt_salience.z.gmc:gndr.c | cntry)
##                       npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)
## mod2.corrupt_salience    7 15058 15113 -7521.8    15044                     
## mod3.corrupt_salience   16 15065 15192 -7516.4    15033 10.777  9     0.2913
```

# Session Information


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


