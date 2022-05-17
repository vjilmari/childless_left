---
title: "ERQ4 Minority moderations"
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
          lrgen.z.gmc:minority.c+
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
##     lrgen.z.gmc:minority.c + (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15056.8  15112.6  -7521.4  15042.8    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7223 -0.4390 -0.2245  0.2374 12.9173 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1168   0.3418  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                        Estimate Std. Error z value Pr(>|z|)    
## (Intercept)            -1.31954    0.10313 -12.795  < 2e-16 ***
## gndr.c                 -0.30826    0.04048  -7.615 2.64e-14 ***
## age10.c                -0.55421    0.01346 -41.178  < 2e-16 ***
## minority.c             -0.34409    0.11467  -3.001  0.00269 ** 
## lrgen.z.gmc            -0.09192    0.06477  -1.419  0.15586    
## minority.c:lrgen.z.gmc -0.17684    0.12956  -1.365  0.17227    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. lrgn..
## gndr.c       0.014                            
## age10.c      0.104  0.009                     
## minority.c   0.529  0.002  0.085              
## lrgen.z.gmc  0.228  0.006  0.002  0.413       
## mnrty.c:l..  0.229 -0.001  0.009  0.412  0.932
```

### Random


```r
mod3.lrgen<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          lrgen.z.gmc+
          lrgen.z.gmc:minority.c+
          (lrgen.z.gmc+minority.c+lrgen.z.gmc:minority.c|cntry),
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
##     lrgen.z.gmc:minority.c + (lrgen.z.gmc + minority.c + lrgen.z.gmc:minority.c |  
##     cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15052.5  15180.0  -7510.3  15020.5    21318 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7242 -0.4395 -0.2251  0.2290 13.4949 
## 
## Random effects:
##  Groups Name                   Variance Std.Dev. Corr          
##  cntry  (Intercept)            0.136046 0.36884                
##         lrgen.z.gmc            0.152127 0.39003  0.77          
##         minority.c             0.008038 0.08966  0.65 0.99     
##         lrgen.z.gmc:minority.c 0.780412 0.88341  0.75 1.00 0.99
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                        Estimate Std. Error z value Pr(>|z|)    
## (Intercept)            -1.32490    0.11136 -11.897  < 2e-16 ***
## gndr.c                 -0.31071    0.04055  -7.662 1.82e-14 ***
## age10.c                -0.55473    0.01350 -41.104  < 2e-16 ***
## minority.c             -0.36351    0.12915  -2.815  0.00488 ** 
## lrgen.z.gmc            -0.10128    0.14100  -0.718  0.47256    
## minority.c:lrgen.z.gmc -0.19106    0.30677  -0.623  0.53340    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. lrgn..
## gndr.c       0.009                            
## age10.c      0.092  0.011                     
## minority.c   0.621 -0.006  0.069              
## lrgen.z.gmc  0.590 -0.006  0.002  0.424       
## mnrty.c:l..  0.588 -0.009  0.005  0.409  0.984
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.lrgen,mod3.lrgen)
```

```
## Data: exdat
## Models:
## mod2.lrgen: childlessness ~ gndr.c + age10.c + minority.c + lrgen.z.gmc + lrgen.z.gmc:minority.c + (1 | cntry)
## mod3.lrgen: childlessness ~ gndr.c + age10.c + minority.c + lrgen.z.gmc + lrgen.z.gmc:minority.c + (lrgen.z.gmc + minority.c + lrgen.z.gmc:minority.c | cntry)
##            npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)   
## mod2.lrgen    7 15057 15113 -7521.4    15043                        
## mod3.lrgen   16 15052 15180 -7510.3    15020 22.303  9   0.007968 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


## lrecon

### Fixed


```r
mod2.lrecon<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          lrecon.z.gmc+
          lrecon.z.gmc:minority.c+
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
##     lrecon.z.gmc:minority.c + (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15058.5  15114.3  -7522.2  15044.5    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7033 -0.4382 -0.2244  0.2396 12.8840 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.117    0.342   
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                           Estimate Std. Error z value Pr(>|z|)    
## (Intercept)             -1.2850602  0.1023687 -12.553  < 2e-16 ***
## gndr.c                  -0.3085827  0.0404819  -7.623 2.48e-14 ***
## age10.c                 -0.5539922  0.0134621 -41.152  < 2e-16 ***
## minority.c              -0.2749481  0.1117612  -2.460   0.0139 *  
## lrecon.z.gmc            -0.0004254  0.0621001  -0.007   0.9945    
## minority.c:lrecon.z.gmc  0.0288158  0.1242093   0.232   0.8165    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. lrcn..
## gndr.c       0.012                            
## age10.c      0.102  0.009                     
## minority.c   0.518 -0.002  0.084              
## lrecn.z.gmc  0.197 -0.006 -0.011  0.363       
## mnrty.c:l..  0.197 -0.014 -0.002  0.363  0.927
```

### Random


```r
mod3.lrecon<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          lrecon.z.gmc+
          lrecon.z.gmc:minority.c+
          (lrecon.z.gmc+minority.c+lrecon.z.gmc:minority.c|cntry),
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
##     lrecon.z.gmc:minority.c + (lrecon.z.gmc + minority.c + lrecon.z.gmc:minority.c |  
##     cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15056.7  15184.2  -7512.4  15024.7    21318 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7046 -0.4388 -0.2254  0.2282 13.9468 
## 
## Random effects:
##  Groups Name                    Variance Std.Dev. Corr          
##  cntry  (Intercept)             0.11258  0.3355                 
##         lrecon.z.gmc            0.10018  0.3165   0.74          
##         minority.c              0.01112  0.1054   0.02 0.68     
##         lrecon.z.gmc:minority.c 0.59849  0.7736   0.78 1.00 0.65
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                          Estimate Std. Error z value Pr(>|z|)    
## (Intercept)             -1.274464   0.102275 -12.461  < 2e-16 ***
## gndr.c                  -0.308452   0.040561  -7.605 2.86e-14 ***
## age10.c                 -0.553725   0.013487 -41.057  < 2e-16 ***
## minority.c              -0.267352   0.121873  -2.194   0.0283 *  
## lrecon.z.gmc            -0.001647   0.122840  -0.013   0.9893    
## minority.c:lrecon.z.gmc  0.011742   0.275166   0.043   0.9660    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. lrcn..
## gndr.c       0.014                            
## age10.c      0.097  0.011                     
## minority.c   0.533 -0.001  0.067              
## lrecn.z.gmc  0.534 -0.006 -0.006  0.381       
## mnrty.c:l..  0.574 -0.010 -0.001  0.364  0.978
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.lrecon,mod3.lrecon)
```

```
## Data: exdat
## Models:
## mod2.lrecon: childlessness ~ gndr.c + age10.c + minority.c + lrecon.z.gmc + lrecon.z.gmc:minority.c + (1 | cntry)
## mod3.lrecon: childlessness ~ gndr.c + age10.c + minority.c + lrecon.z.gmc + lrecon.z.gmc:minority.c + (lrecon.z.gmc + minority.c + lrecon.z.gmc:minority.c | cntry)
##             npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)  
## mod2.lrecon    7 15058 15114 -7522.2    15044                       
## mod3.lrecon   16 15057 15184 -7512.4    15025 19.762  9    0.01944 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


## galtan

### Fixed


```r
mod2.galtan<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          galtan.z.gmc+
          galtan.z.gmc:minority.c+
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
##     galtan.z.gmc:minority.c + (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15053.9  15109.7  -7520.0  15039.9    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6974 -0.4387 -0.2245  0.2362 12.7674 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1161   0.3407  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                         Estimate Std. Error z value Pr(>|z|)    
## (Intercept)             -1.32364    0.10241 -12.925  < 2e-16 ***
## gndr.c                  -0.30835    0.04049  -7.616 2.61e-14 ***
## age10.c                 -0.55334    0.01349 -41.028  < 2e-16 ***
## minority.c              -0.35287    0.11288  -3.126  0.00177 ** 
## galtan.z.gmc            -0.15198    0.07489  -2.029  0.04242 *  
## minority.c:galtan.z.gmc -0.24588    0.14980  -1.641  0.10072    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. gltn..
## gndr.c       0.012                            
## age10.c      0.105  0.008                     
## minority.c   0.523 -0.002  0.089              
## galtn.z.gmc  0.206 -0.004  0.001  0.379       
## mnrty.c:g..  0.207 -0.011  0.022  0.382  0.946
```

### Random


```r
mod3.galtan<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          galtan.z.gmc+
          galtan.z.gmc:minority.c+
          (galtan.z.gmc+minority.c+galtan.z.gmc:minority.c|cntry),
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
##     galtan.z.gmc:minority.c + (galtan.z.gmc + minority.c + galtan.z.gmc:minority.c |  
##     cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15045.2  15172.7  -7506.6  15013.2    21318 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7164 -0.4393 -0.2250  0.2312 13.2352 
## 
## Random effects:
##  Groups Name                    Variance Std.Dev. Corr          
##  cntry  (Intercept)             0.18099  0.4254                 
##         galtan.z.gmc            0.18715  0.4326   0.88          
##         minority.c              0.03549  0.1884   0.93 0.99     
##         galtan.z.gmc:minority.c 0.89239  0.9447   0.86 1.00 0.99
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                         Estimate Std. Error z value Pr(>|z|)    
## (Intercept)             -1.36478    0.12394 -11.011  < 2e-16 ***
## gndr.c                  -0.31122    0.04056  -7.674 1.67e-14 ***
## age10.c                 -0.55446    0.01352 -41.018  < 2e-16 ***
## minority.c              -0.44671    0.14258  -3.133  0.00173 ** 
## galtan.z.gmc            -0.20115    0.15228  -1.321  0.18652    
## minority.c:galtan.z.gmc -0.35915    0.32360  -1.110  0.26706    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. gltn..
## gndr.c       0.007                            
## age10.c      0.088  0.009                     
## minority.c   0.733 -0.008  0.073              
## galtn.z.gmc  0.702 -0.009  0.006  0.581       
## mnrty.c:g..  0.698 -0.011  0.015  0.572  0.986
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.galtan,mod3.galtan)
```

```
## Data: exdat
## Models:
## mod2.galtan: childlessness ~ gndr.c + age10.c + minority.c + galtan.z.gmc + galtan.z.gmc:minority.c + (1 | cntry)
## mod3.galtan: childlessness ~ gndr.c + age10.c + minority.c + galtan.z.gmc + galtan.z.gmc:minority.c + (galtan.z.gmc + minority.c + galtan.z.gmc:minority.c | cntry)
##             npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)   
## mod2.galtan    7 15054 15110 -7520.0    15040                        
## mod3.galtan   16 15045 15173 -7506.6    15013 26.718  9   0.001557 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


## spendvtax

### Fixed


```r
mod2.spendvtax<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          spendvtax.z.gmc+
          spendvtax.z.gmc:minority.c+
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
##     spendvtax.z.gmc:minority.c + (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15058.7  15114.5  -7522.4  15044.7    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7107 -0.4385 -0.2245  0.2392 12.9143 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.117    0.342   
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                -1.290034   0.102151 -12.629  < 2e-16 ***
## gndr.c                     -0.308293   0.040484  -7.615 2.63e-14 ***
## age10.c                    -0.553995   0.013466 -41.142  < 2e-16 ***
## minority.c                 -0.284952   0.111013  -2.567   0.0103 *  
## spendvtax.z.gmc            -0.011674   0.055510  -0.210   0.8334    
## minority.c:spendvtax.z.gmc -0.006643   0.111017  -0.060   0.9523    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. spnd..
## gndr.c       0.012                            
## age10.c      0.103  0.008                     
## minority.c   0.515 -0.003  0.085              
## spndvtx.z.g  0.186 -0.008 -0.011  0.346       
## mnrty.c:s..  0.187 -0.017  0.003  0.347  0.929
```

### Random


```r
mod3.spendvtax<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          spendvtax.z.gmc+
          spendvtax.z.gmc:minority.c+
          (spendvtax.z.gmc+minority.c+spendvtax.z.gmc:minority.c|cntry),
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
##     spendvtax.z.gmc:minority.c + (spendvtax.z.gmc + minority.c +  
##     spendvtax.z.gmc:minority.c | cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15052.1  15179.6  -7510.0  15020.1    21318 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7335 -0.4386 -0.2253  0.2279 13.8007 
## 
## Random effects:
##  Groups Name                       Variance Std.Dev. Corr          
##  cntry  (Intercept)                0.13098  0.3619                 
##         spendvtax.z.gmc            0.10140  0.3184   0.81          
##         minority.c                 0.01524  0.1235   0.41 0.87     
##         spendvtax.z.gmc:minority.c 0.57196  0.7563   0.82 1.00 0.86
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                            Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                -1.28630    0.10809 -11.900  < 2e-16 ***
## gndr.c                     -0.30826    0.04057  -7.598 3.01e-14 ***
## age10.c                    -0.55444    0.01349 -41.087  < 2e-16 ***
## minority.c                 -0.28985    0.12561  -2.308    0.021 *  
## spendvtax.z.gmc            -0.01171    0.11818  -0.099    0.921    
## minority.c:spendvtax.z.gmc -0.03071    0.26310  -0.117    0.907    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. spnd..
## gndr.c       0.011                            
## age10.c      0.091  0.011                     
## minority.c   0.590 -0.006  0.064              
## spndvtx.z.g  0.597 -0.010 -0.010  0.443       
## mnrty.c:s..  0.622 -0.014 -0.005  0.433  0.981
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.spendvtax,mod3.spendvtax)
```

```
## Data: exdat
## Models:
## mod2.spendvtax: childlessness ~ gndr.c + age10.c + minority.c + spendvtax.z.gmc + spendvtax.z.gmc:minority.c + (1 | cntry)
## mod3.spendvtax: childlessness ~ gndr.c + age10.c + minority.c + spendvtax.z.gmc + spendvtax.z.gmc:minority.c + (spendvtax.z.gmc + minority.c + spendvtax.z.gmc:minority.c | cntry)
##                npar   AIC   BIC  logLik deviance Chisq Df Pr(>Chisq)   
## mod2.spendvtax    7 15059 15114 -7522.4    15045                       
## mod3.spendvtax   16 15052 15180 -7510.0    15020 24.62  9   0.003422 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## deregulation

### Fixed


```r
mod2.deregulation<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          deregulation.z.gmc+
          deregulation.z.gmc:minority.c+
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
##     deregulation.z.gmc:minority.c + (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15058.8  15114.6  -7522.4  15044.8    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7282 -0.4388 -0.2243  0.2383 12.9542 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1172   0.3423  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                   -1.28229    0.10246 -12.515  < 2e-16 ***
## gndr.c                        -0.30821    0.04049  -7.612 2.69e-14 ***
## age10.c                       -0.55420    0.01347 -41.147  < 2e-16 ***
## minority.c                    -0.26915    0.11196  -2.404   0.0162 *  
## deregulation.z.gmc             0.01569    0.06626   0.237   0.8128    
## minority.c:deregulation.z.gmc  0.03366    0.13254   0.254   0.7995    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. drgl..
## gndr.c       0.010                            
## age10.c      0.102  0.008                     
## minority.c   0.518 -0.006  0.083              
## drgltn.z.gm  0.199 -0.014 -0.016  0.367       
## mnrty.c:d..  0.200 -0.022 -0.001  0.368  0.933
```

### Random


```r
mod3.deregulation<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          deregulation.z.gmc+
          deregulation.z.gmc:minority.c+
          (deregulation.z.gmc+minority.c+deregulation.z.gmc:minority.c|cntry),
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
##     deregulation.z.gmc:minority.c + (deregulation.z.gmc + minority.c +  
##     deregulation.z.gmc:minority.c | cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15057.1  15184.6  -7512.6  15025.1    21318 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7461 -0.4393 -0.2257  0.2310 13.6841 
## 
## Random effects:
##  Groups Name                          Variance Std.Dev. Corr          
##  cntry  (Intercept)                   0.11543  0.3397                 
##         deregulation.z.gmc            0.05428  0.2330   0.62          
##         minority.c                    0.01314  0.1146   0.09 0.84     
##         deregulation.z.gmc:minority.c 0.38360  0.6194   0.71 0.99 0.77
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                   -1.27077    0.10444 -12.168  < 2e-16 ***
## gndr.c                        -0.30540    0.04057  -7.528 5.16e-14 ***
## age10.c                       -0.55353    0.01348 -41.051  < 2e-16 ***
## minority.c                    -0.26109    0.12698  -2.056   0.0398 *  
## deregulation.z.gmc             0.05102    0.10712   0.476   0.6339    
## minority.c:deregulation.z.gmc  0.07690    0.24312   0.316   0.7518    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. drgl..
## gndr.c       0.009                            
## age10.c      0.093  0.010                     
## minority.c   0.555 -0.008  0.061              
## drgltn.z.gm  0.452 -0.016 -0.018  0.433       
## mnrty.c:d..  0.528 -0.019 -0.009  0.418  0.965
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.deregulation,mod3.deregulation)
```

```
## Data: exdat
## Models:
## mod2.deregulation: childlessness ~ gndr.c + age10.c + minority.c + deregulation.z.gmc + deregulation.z.gmc:minority.c + (1 | cntry)
## mod3.deregulation: childlessness ~ gndr.c + age10.c + minority.c + deregulation.z.gmc + deregulation.z.gmc:minority.c + (deregulation.z.gmc + minority.c + deregulation.z.gmc:minority.c | cntry)
##                   npar   AIC   BIC  logLik deviance Chisq Df Pr(>Chisq)  
## mod2.deregulation    7 15059 15115 -7522.4    15045                      
## mod3.deregulation   16 15057 15185 -7512.6    15025  19.7  9    0.01986 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


## redistribution

### Fixed


```r
mod2.redistribution<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          redistribution.z.gmc+
          redistribution.z.gmc:minority.c+
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
##     redistribution.z.gmc:minority.c + (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15058.7  15114.5  -7522.4  15044.7    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7198 -0.4387 -0.2244  0.2389 12.9225 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1171   0.3422  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                 Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                     -1.28244    0.10203 -12.570  < 2e-16 ***
## gndr.c                          -0.30826    0.04048  -7.616 2.62e-14 ***
## age10.c                         -0.55410    0.01346 -41.159  < 2e-16 ***
## minority.c                      -0.26949    0.11040  -2.441   0.0146 *  
## redistribution.z.gmc             0.01325    0.06129   0.216   0.8288    
## minority.c:redistribution.z.gmc  0.03844    0.12260   0.314   0.7539    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. rdst..
## gndr.c       0.012                            
## age10.c      0.104  0.009                     
## minority.c   0.512 -0.002  0.086              
## rdstrbtn.z.  0.179 -0.008 -0.008  0.333       
## mnrty.c:r..  0.179 -0.013  0.002  0.334  0.937
```

### Random


```r
mod3.redistribution<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          redistribution.z.gmc+
          redistribution.z.gmc:minority.c+
          (redistribution.z.gmc+minority.c+redistribution.z.gmc:minority.c|cntry),
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
##     redistribution.z.gmc:minority.c + (redistribution.z.gmc +  
##     minority.c + redistribution.z.gmc:minority.c | cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15052.4  15179.9  -7510.2  15020.4    21318 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7336 -0.4388 -0.2250  0.2291 14.1322 
## 
## Random effects:
##  Groups Name                            Variance Std.Dev. Corr          
##  cntry  (Intercept)                     0.11944  0.3456                 
##         redistribution.z.gmc            0.07245  0.2692   0.70          
##         minority.c                      0.01303  0.1142   0.21 0.85     
##         redistribution.z.gmc:minority.c 0.43497  0.6595   0.75 1.00 0.80
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                  Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                     -1.284733   0.104293 -12.318  < 2e-16 ***
## gndr.c                          -0.308512   0.040566  -7.605 2.85e-14 ***
## age10.c                         -0.554201   0.013483 -41.103  < 2e-16 ***
## minority.c                      -0.282127   0.122012  -2.312   0.0208 *  
## redistribution.z.gmc             0.001302   0.109476   0.012   0.9905    
## minority.c:redistribution.z.gmc -0.015122   0.243035  -0.062   0.9504    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. rdst..
## gndr.c       0.013                            
## age10.c      0.096  0.011                     
## minority.c   0.556 -0.002  0.070              
## rdstrbtn.z.  0.514 -0.012 -0.007  0.436       
## mnrty.c:r..  0.568 -0.015 -0.002  0.420  0.975
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.redistribution,mod3.redistribution)
```

```
## Data: exdat
## Models:
## mod2.redistribution: childlessness ~ gndr.c + age10.c + minority.c + redistribution.z.gmc + redistribution.z.gmc:minority.c + (1 | cntry)
## mod3.redistribution: childlessness ~ gndr.c + age10.c + minority.c + redistribution.z.gmc + redistribution.z.gmc:minority.c + (redistribution.z.gmc + minority.c + redistribution.z.gmc:minority.c | cntry)
##                     npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)   
## mod2.redistribution    7 15059 15114 -7522.4    15045                        
## mod3.redistribution   16 15052 15180 -7510.2    15020 24.328  9   0.003812 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


## econ_interven

### Fixed


```r
mod2.econ_interven<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          econ_interven.z.gmc+
          econ_interven.z.gmc:minority.c+
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
##     econ_interven.z.gmc:minority.c + (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15057.7  15113.5  -7521.8  15043.7    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6954 -0.4380 -0.2247  0.2400 12.8991 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1171   0.3422  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                    -1.27357    0.10237 -12.441  < 2e-16 ***
## gndr.c                         -0.30911    0.04049  -7.635 2.26e-14 ***
## age10.c                        -0.55389    0.01347 -41.135  < 2e-16 ***
## minority.c                     -0.25183    0.11168  -2.255   0.0241 *  
## econ_interven.z.gmc             0.02991    0.06256   0.478   0.6326    
## minority.c:econ_interven.z.gmc  0.10181    0.12512   0.814   0.4158    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. ecn_..
## gndr.c       0.012                            
## age10.c      0.102  0.009                     
## minority.c   0.517 -0.004  0.083              
## ecn_ntrvn..  0.196 -0.009 -0.014  0.361       
## mnrty.c:_..  0.196 -0.018 -0.003  0.362  0.927
```

### Random


```r
mod3.econ_interven<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          econ_interven.z.gmc+
          econ_interven.z.gmc:minority.c+
          (econ_interven.z.gmc+minority.c+econ_interven.z.gmc:minority.c|cntry),
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
##     econ_interven.z.gmc:minority.c + (econ_interven.z.gmc + minority.c +  
##     econ_interven.z.gmc:minority.c | cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15058.3  15185.8  -7513.2  15026.3    21318 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7078 -0.4395 -0.2257  0.2297 13.6191 
## 
## Random effects:
##  Groups Name                           Variance Std.Dev. Corr             
##  cntry  (Intercept)                    0.111328 0.33366                   
##         econ_interven.z.gmc            0.073685 0.27145   0.68            
##         minority.c                     0.009163 0.09572  -0.03  0.71      
##         econ_interven.z.gmc:minority.c 0.480706 0.69333   0.75  0.99  0.63
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                    -1.26945    0.10191 -12.457  < 2e-16 ***
## gndr.c                         -0.30602    0.04056  -7.544 4.54e-14 ***
## age10.c                        -0.55318    0.01348 -41.031  < 2e-16 ***
## minority.c                     -0.25943    0.12109  -2.142   0.0322 *  
## econ_interven.z.gmc             0.01378    0.11350   0.121   0.9034    
## minority.c:econ_interven.z.gmc  0.03204    0.25620   0.125   0.9005    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. ecn_..
## gndr.c       0.013                            
## age10.c      0.097  0.010                     
## minority.c   0.528 -0.002  0.069              
## ecn_ntrvn..  0.495 -0.011 -0.008  0.396       
## mnrty.c:_..  0.560 -0.015 -0.003  0.372  0.970
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.econ_interven,mod3.econ_interven)
```

```
## Data: exdat
## Models:
## mod2.econ_interven: childlessness ~ gndr.c + age10.c + minority.c + econ_interven.z.gmc + econ_interven.z.gmc:minority.c + (1 | cntry)
## mod3.econ_interven: childlessness ~ gndr.c + age10.c + minority.c + econ_interven.z.gmc + econ_interven.z.gmc:minority.c + (econ_interven.z.gmc + minority.c + econ_interven.z.gmc:minority.c | cntry)
##                    npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)  
## mod2.econ_interven    7 15058 15114 -7521.8    15044                       
## mod3.econ_interven   16 15058 15186 -7513.2    15026 17.352  9    0.04348 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```



## civlib_laworder

### Fixed


```r
mod2.civlib_laworder<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          civlib_laworder.z.gmc+
          civlib_laworder.z.gmc:minority.c+
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
##     civlib_laworder.z.gmc:minority.c + (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15051.2  15107.0  -7518.6  15037.2    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7064 -0.4386 -0.2245  0.2377 12.8357 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1164   0.3412  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                  Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                      -1.33660    0.10247 -13.043  < 2e-16 ***
## gndr.c                           -0.30770    0.04049  -7.599    3e-14 ***
## age10.c                          -0.55376    0.01348 -41.073  < 2e-16 ***
## minority.c                       -0.37819    0.11264  -3.357 0.000787 ***
## civlib_laworder.z.gmc            -0.18298    0.06737  -2.716 0.006608 ** 
## minority.c:civlib_laworder.z.gmc -0.33328    0.13479  -2.473 0.013414 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. cvl_..
## gndr.c       0.012                            
## age10.c      0.104  0.008                     
## minority.c   0.522 -0.004  0.087              
## cvlb_lwrd..  0.201 -0.005  0.000  0.365       
## mnrty.c:_..  0.202 -0.013  0.020  0.368  0.943
```

### Random


```r
mod3.civlib_laworder<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          civlib_laworder.z.gmc+
          civlib_laworder.z.gmc:minority.c+
          (civlib_laworder.z.gmc+minority.c+
             civlib_laworder.z.gmc:minority.c|cntry),
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
##     civlib_laworder.z.gmc:minority.c + (civlib_laworder.z.gmc +  
##     minority.c + civlib_laworder.z.gmc:minority.c | cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15045.2  15172.6  -7506.6  15013.2    21318 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7225 -0.4393 -0.2253  0.2352 13.1640 
## 
## Random effects:
##  Groups Name                             Variance Std.Dev. Corr          
##  cntry  (Intercept)                      0.17825  0.4222                 
##         civlib_laworder.z.gmc            0.15230  0.3903   0.88          
##         minority.c                       0.03623  0.1903   0.87 1.00     
##         civlib_laworder.z.gmc:minority.c 0.71595  0.8461   0.85 1.00 1.00
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                  Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                      -1.36511    0.12419 -10.992  < 2e-16 ***
## gndr.c                           -0.30964    0.04056  -7.634 2.28e-14 ***
## age10.c                          -0.55498    0.01351 -41.066  < 2e-16 ***
## minority.c                       -0.44385    0.14432  -3.075   0.0021 ** 
## civlib_laworder.z.gmc            -0.18655    0.13993  -1.333   0.1825    
## minority.c:civlib_laworder.z.gmc -0.34556    0.29751  -1.162   0.2454    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. cvl_..
## gndr.c       0.008                            
## age10.c      0.087  0.010                     
## minority.c   0.724 -0.006  0.069              
## cvlb_lwrd..  0.700 -0.008  0.002  0.585       
## mnrty.c:_..  0.686 -0.010  0.009  0.581  0.985
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.civlib_laworder,mod3.civlib_laworder)
```

```
## Data: exdat
## Models:
## mod2.civlib_laworder: childlessness ~ gndr.c + age10.c + minority.c + civlib_laworder.z.gmc + civlib_laworder.z.gmc:minority.c + (1 | cntry)
## mod3.civlib_laworder: childlessness ~ gndr.c + age10.c + minority.c + civlib_laworder.z.gmc + civlib_laworder.z.gmc:minority.c + (civlib_laworder.z.gmc + minority.c + civlib_laworder.z.gmc:minority.c | cntry)
##                      npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)   
## mod2.civlib_laworder    7 15051 15107 -7518.6    15037                        
## mod3.civlib_laworder   16 15045 15173 -7506.6    15013 24.039  9    0.00424 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
mod4.civlib_laworder<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          civlib_laworder.z.gmc+
          civlib_laworder.z.gmc:minority.c+
          (civlib_laworder.z.gmc+minority.c+
             civlib_laworder.z.gmc:minority.c||cntry),
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
summary(mod4.civlib_laworder)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## childlessness ~ gndr.c + age10.c + minority.c + civlib_laworder.z.gmc +  
##     civlib_laworder.z.gmc:minority.c + (civlib_laworder.z.gmc +  
##     minority.c + civlib_laworder.z.gmc:minority.c || cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15056.0  15135.7  -7518.0  15036.0    21324 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6855 -0.4381 -0.2243  0.2378 13.0256 
## 
## Random effects:
##  Groups  Name                             Variance Std.Dev.
##  cntry   (Intercept)                      0.116111 0.34075 
##  cntry.1 civlib_laworder.z.gmc            0.000000 0.00000 
##  cntry.2 minority.c                       0.000000 0.00000 
##  cntry.3 civlib_laworder.z.gmc:minority.c 0.007058 0.08401 
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                  Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                      -1.33612    0.10241 -13.047  < 2e-16 ***
## gndr.c                           -0.30732    0.04051  -7.587 3.27e-14 ***
## age10.c                          -0.55335    0.01349 -41.022  < 2e-16 ***
## minority.c                       -0.37631    0.11271  -3.339 0.000842 ***
## civlib_laworder.z.gmc            -0.18146    0.06755  -2.686 0.007223 ** 
## minority.c:civlib_laworder.z.gmc -0.33393    0.13851  -2.411 0.015918 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. cvl_..
## gndr.c       0.011                            
## age10.c      0.104  0.008                     
## minority.c   0.523 -0.004  0.087              
## cvlb_lwrd..  0.201 -0.004  0.000  0.365       
## mnrty.c:_..  0.197 -0.013  0.017  0.359  0.918
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.civlib_laworder,mod4.civlib_laworder)
```

```
## Data: exdat
## Models:
## mod2.civlib_laworder: childlessness ~ gndr.c + age10.c + minority.c + civlib_laworder.z.gmc + civlib_laworder.z.gmc:minority.c + (1 | cntry)
## mod4.civlib_laworder: childlessness ~ gndr.c + age10.c + minority.c + civlib_laworder.z.gmc + civlib_laworder.z.gmc:minority.c + (civlib_laworder.z.gmc + minority.c + civlib_laworder.z.gmc:minority.c || cntry)
##                      npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)
## mod2.civlib_laworder    7 15051 15107 -7518.6    15037                     
## mod4.civlib_laworder   10 15056 15136 -7518.0    15036 1.2126  3       0.75
```

```r
anova(mod4.civlib_laworder,mod3.civlib_laworder)
```

```
## Data: exdat
## Models:
## mod4.civlib_laworder: childlessness ~ gndr.c + age10.c + minority.c + civlib_laworder.z.gmc + civlib_laworder.z.gmc:minority.c + (civlib_laworder.z.gmc + minority.c + civlib_laworder.z.gmc:minority.c || cntry)
## mod3.civlib_laworder: childlessness ~ gndr.c + age10.c + minority.c + civlib_laworder.z.gmc + civlib_laworder.z.gmc:minority.c + (civlib_laworder.z.gmc + minority.c + civlib_laworder.z.gmc:minority.c | cntry)
##                      npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)    
## mod4.civlib_laworder   10 15056 15136 -7518.0    15036                         
## mod3.civlib_laworder   16 15045 15173 -7506.6    15013 22.826  6  0.0008568 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
mod5.civlib_laworder<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          civlib_laworder.z.gmc+
          civlib_laworder.z.gmc:minority.c+
          (civlib_laworder.z.gmc:minority.c|cntry),
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
summary(mod5.civlib_laworder)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## childlessness ~ gndr.c + age10.c + minority.c + civlib_laworder.z.gmc +  
##     civlib_laworder.z.gmc:minority.c + (civlib_laworder.z.gmc:minority.c |  
##     cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15053.0  15124.7  -7517.5  15035.0    21325 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6945 -0.4386 -0.2247  0.2372 13.1947 
## 
## Random effects:
##  Groups Name                             Variance Std.Dev. Corr
##  cntry  (Intercept)                      0.114666 0.33862      
##         civlib_laworder.z.gmc:minority.c 0.006603 0.08126  0.58
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                  Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                      -1.33379    0.10202 -13.074  < 2e-16 ***
## gndr.c                           -0.30741    0.04051  -7.589 3.21e-14 ***
## age10.c                          -0.55311    0.01349 -41.013  < 2e-16 ***
## minority.c                       -0.37598    0.11276  -3.334 0.000855 ***
## civlib_laworder.z.gmc            -0.18118    0.06756  -2.682 0.007327 ** 
## minority.c:civlib_laworder.z.gmc -0.33811    0.13787  -2.452 0.014189 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. cvl_..
## gndr.c       0.011                            
## age10.c      0.105  0.008                     
## minority.c   0.525 -0.004  0.087              
## cvlb_lwrd..  0.204 -0.005  0.000  0.368       
## mnrty.c:_..  0.269 -0.014  0.018  0.364  0.923
```

```r
anova(mod2.civlib_laworder,mod5.civlib_laworder)
```

```
## Data: exdat
## Models:
## mod2.civlib_laworder: childlessness ~ gndr.c + age10.c + minority.c + civlib_laworder.z.gmc + civlib_laworder.z.gmc:minority.c + (1 | cntry)
## mod5.civlib_laworder: childlessness ~ gndr.c + age10.c + minority.c + civlib_laworder.z.gmc + civlib_laworder.z.gmc:minority.c + (civlib_laworder.z.gmc:minority.c | cntry)
##                      npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)
## mod2.civlib_laworder    7 15051 15107 -7518.6    15037                     
## mod5.civlib_laworder    9 15053 15125 -7517.5    15035 2.1629  2     0.3391
```

```r
anova(mod5.civlib_laworder,mod4.civlib_laworder)
```

```
## Data: exdat
## Models:
## mod5.civlib_laworder: childlessness ~ gndr.c + age10.c + minority.c + civlib_laworder.z.gmc + civlib_laworder.z.gmc:minority.c + (civlib_laworder.z.gmc:minority.c | cntry)
## mod4.civlib_laworder: childlessness ~ gndr.c + age10.c + minority.c + civlib_laworder.z.gmc + civlib_laworder.z.gmc:minority.c + (civlib_laworder.z.gmc + minority.c + civlib_laworder.z.gmc:minority.c || cntry)
##                      npar   AIC   BIC  logLik deviance Chisq Df Pr(>Chisq)
## mod5.civlib_laworder    9 15053 15125 -7517.5    15035                    
## mod4.civlib_laworder   10 15056 15136 -7518.0    15036     0  1          1
```


### Marginal effects


```r
emtrends(mod5.civlib_laworder,
         var="civlib_laworder.z.gmc",
         specs="minority.c",
         at=list(minority.c=c(-0.5,0.5)),
         infer=c(T,T))
```

```
##  minority.c civlib_laworder.z.gmc.trend     SE  df asymp.LCL asymp.UCL z.ratio
##        -0.5                     -0.0121 0.0268 Inf   -0.0647    0.0405  -0.452
##         0.5                     -0.3502 0.1338 Inf   -0.6125   -0.0879  -2.617
##  p.value
##   0.6514
##   0.0089
## 
## Results are averaged over the levels of: gndr.c 
## Confidence level used: 0.95
```


## sociallifestyle

### Fixed


```r
mod2.sociallifestyle<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          sociallifestyle.z.gmc+
          sociallifestyle.z.gmc:minority.c+
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
##     sociallifestyle.z.gmc:minority.c + (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15054.6  15110.4  -7520.3  15040.6    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7012 -0.4387 -0.2247  0.2363 12.7912 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1162   0.3408  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                  Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                      -1.32320    0.10240 -12.921  < 2e-16 ***
## gndr.c                           -0.30796    0.04048  -7.607  2.8e-14 ***
## age10.c                          -0.55375    0.01348 -41.078  < 2e-16 ***
## minority.c                       -0.35253    0.11286  -3.123  0.00179 ** 
## sociallifestyle.z.gmc            -0.15633    0.07943  -1.968  0.04906 *  
## minority.c:sociallifestyle.z.gmc -0.26803    0.15887  -1.687  0.09159 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. scll..
## gndr.c       0.012                            
## age10.c      0.106  0.008                     
## minority.c   0.522 -0.002  0.089              
## scllfstyl..  0.205 -0.005  0.003  0.379       
## mnrty.c:s..  0.206 -0.011  0.022  0.381  0.946
```

### Random


```r
mod3.sociallifestyle<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          sociallifestyle.z.gmc+
          sociallifestyle.z.gmc:minority.c+
          (sociallifestyle.z.gmc+minority.c+sociallifestyle.z.gmc:minority.c|cntry),
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
##     sociallifestyle.z.gmc:minority.c + (sociallifestyle.z.gmc +  
##     minority.c + sociallifestyle.z.gmc:minority.c | cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15052.6  15180.1  -7510.3  15020.6    21318 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7292 -0.4398 -0.2249  0.2332 13.4067 
## 
## Random effects:
##  Groups Name                             Variance Std.Dev. Corr          
##  cntry  (Intercept)                      0.16391  0.4049                 
##         sociallifestyle.z.gmc            0.09995  0.3161   1.00          
##         minority.c                       0.02625  0.1620   0.81 0.81     
##         sociallifestyle.z.gmc:minority.c 0.49413  0.7029   0.99 0.99 0.87
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                  Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                      -1.34679    0.12018 -11.207  < 2e-16 ***
## gndr.c                           -0.31064    0.04053  -7.664  1.8e-14 ***
## age10.c                          -0.55423    0.01350 -41.058  < 2e-16 ***
## minority.c                       -0.40467    0.14192  -2.851  0.00435 ** 
## sociallifestyle.z.gmc            -0.18734    0.11902  -1.574  0.11548    
## minority.c:sociallifestyle.z.gmc -0.33622    0.25271  -1.330  0.18336    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. scll..
## gndr.c       0.006                            
## age10.c      0.087  0.010                     
## minority.c   0.696 -0.011  0.066              
## scllfstyl..  0.773 -0.015  0.013  0.551       
## mnrty.c:s..  0.791 -0.018  0.023  0.566  0.967
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.sociallifestyle,mod3.sociallifestyle)
```

```
## Data: exdat
## Models:
## mod2.sociallifestyle: childlessness ~ gndr.c + age10.c + minority.c + sociallifestyle.z.gmc + sociallifestyle.z.gmc:minority.c + (1 | cntry)
## mod3.sociallifestyle: childlessness ~ gndr.c + age10.c + minority.c + sociallifestyle.z.gmc + sociallifestyle.z.gmc:minority.c + (sociallifestyle.z.gmc + minority.c + sociallifestyle.z.gmc:minority.c | cntry)
##                      npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)  
## mod2.sociallifestyle    7 15055 15110 -7520.3    15041                       
## mod3.sociallifestyle   16 15053 15180 -7510.3    15021 19.956  9    0.01818 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## religious_principle

### Fixed


```r
mod2.religious_principle<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          religious_principle.z.gmc+
          religious_principle.z.gmc:minority.c+
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
##     religious_principle.z.gmc:minority.c + (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15055.9  15111.7  -7521.0  15041.9    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7228 -0.4383 -0.2244  0.2384 12.9037 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1166   0.3414  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                      Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                          -1.31339    0.10165 -12.920  < 2e-16 ***
## gndr.c                               -0.30677    0.04048  -7.578  3.5e-14 ***
## age10.c                              -0.55406    0.01349 -41.079  < 2e-16 ***
## minority.c                           -0.33251    0.10956  -3.035   0.0024 ** 
## religious_principle.z.gmc            -0.14147    0.08262  -1.712   0.0868 .  
## minority.c:religious_principle.z.gmc -0.26556    0.16527  -1.607   0.1081    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. rlg_..
## gndr.c       0.012                            
## age10.c      0.106  0.009                     
## minority.c   0.510 -0.004  0.091              
## rlgs_prnc..  0.163 -0.016  0.002  0.307       
## mnrty.c:_..  0.164 -0.017  0.022  0.309  0.953
```

### Random


```r
mod3.religious_principle<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          religious_principle.z.gmc+
          religious_principle.z.gmc:minority.c+
          (religious_principle.z.gmc+minority.c+religious_principle.z.gmc:minority.c|cntry),
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
##     religious_principle.z.gmc:minority.c + (religious_principle.z.gmc +  
##     minority.c + religious_principle.z.gmc:minority.c | cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15051.8  15179.3  -7509.9  15019.8    21318 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7490 -0.4402 -0.2253  0.2348 13.5475 
## 
## Random effects:
##  Groups Name                                 Variance Std.Dev. Corr          
##  cntry  (Intercept)                          0.17345  0.4165                 
##         religious_principle.z.gmc            0.21898  0.4680   0.90          
##         minority.c                           0.03395  0.1843   0.85 1.00     
##         religious_principle.z.gmc:minority.c 1.09192  1.0450   0.89 1.00 1.00
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                      Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                          -1.34917    0.12213 -11.047  < 2e-16 ***
## gndr.c                               -0.30926    0.04055  -7.627  2.4e-14 ***
## age10.c                              -0.55445    0.01351 -41.050  < 2e-16 ***
## minority.c                           -0.41862    0.14189  -2.950  0.00318 ** 
## religious_principle.z.gmc            -0.24007    0.17225  -1.394  0.16340    
## minority.c:religious_principle.z.gmc -0.48892    0.36761  -1.330  0.18352    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. rlg_..
## gndr.c       0.013                            
## age10.c      0.090  0.011                     
## minority.c   0.714  0.002  0.075              
## rlgs_prnc..  0.682 -0.010  0.006  0.555       
## mnrty.c:_..  0.690 -0.009  0.014  0.553  0.986
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.religious_principle,mod3.religious_principle)
```

```
## Data: exdat
## Models:
## mod2.religious_principle: childlessness ~ gndr.c + age10.c + minority.c + religious_principle.z.gmc + religious_principle.z.gmc:minority.c + (1 | cntry)
## mod3.religious_principle: childlessness ~ gndr.c + age10.c + minority.c + religious_principle.z.gmc + religious_principle.z.gmc:minority.c + (religious_principle.z.gmc + minority.c + religious_principle.z.gmc:minority.c | cntry)
##                          npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)
## mod2.religious_principle    7 15056 15112 -7521.0    15042                     
## mod3.religious_principle   16 15052 15179 -7509.9    15020 22.121  9   0.008503
##                            
## mod2.religious_principle   
## mod3.religious_principle **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## immigrate_policy

### Fixed


```r
mod2.immigrate_policy<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          immigrate_policy.z.gmc+
          immigrate_policy.z.gmc:minority.c+
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
##     immigrate_policy.z.gmc:minority.c + (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15051.6  15107.4  -7518.8  15037.6    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6919 -0.4382 -0.2246  0.2386 12.8046 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1162   0.3409  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                   Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                       -1.33271    0.10243 -13.011  < 2e-16 ***
## gndr.c                            -0.30808    0.04050  -7.607  2.8e-14 ***
## age10.c                           -0.55356    0.01348 -41.074  < 2e-16 ***
## minority.c                        -0.37059    0.11268  -3.289  0.00101 ** 
## immigrate_policy.z.gmc            -0.18751    0.07294  -2.571  0.01015 *  
## minority.c:immigrate_policy.z.gmc -0.32472    0.14593  -2.225  0.02606 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. imm_..
## gndr.c       0.011                            
## age10.c      0.104  0.008                     
## minority.c   0.522 -0.004  0.087              
## immgrt_pl..  0.202 -0.006  0.000  0.368       
## mnrty.c:_..  0.203 -0.015  0.017  0.370  0.944
```

### Random


```r
mod3.immigrate_policy<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          immigrate_policy.z.gmc+
          immigrate_policy.z.gmc:minority.c+
          (immigrate_policy.z.gmc+minority.c+immigrate_policy.z.gmc:minority.c|cntry),
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
##     immigrate_policy.z.gmc:minority.c + (immigrate_policy.z.gmc +  
##     minority.c + immigrate_policy.z.gmc:minority.c | cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15046.9  15174.4  -7507.4  15014.9    21318 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7073 -0.4386 -0.2250  0.2369 13.1523 
## 
## Random effects:
##  Groups Name                              Variance Std.Dev. Corr          
##  cntry  (Intercept)                       0.15362  0.3919                 
##         immigrate_policy.z.gmc            0.19113  0.4372   0.80          
##         minority.c                        0.01451  0.1205   0.86 0.99     
##         immigrate_policy.z.gmc:minority.c 0.92670  0.9627   0.77 1.00 0.99
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                   Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                       -1.35444    0.11635 -11.641  < 2e-16 ***
## gndr.c                            -0.30951    0.04057  -7.630 2.35e-14 ***
## age10.c                           -0.55455    0.01351 -41.046  < 2e-16 ***
## minority.c                        -0.42368    0.13163  -3.219  0.00129 ** 
## immigrate_policy.z.gmc            -0.17406    0.15583  -1.117  0.26401    
## minority.c:immigrate_policy.z.gmc -0.30557    0.33505  -0.912  0.36176    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. imm_..
## gndr.c       0.008                            
## age10.c      0.091  0.010                     
## minority.c   0.669 -0.008  0.073              
## immgrt_pl..  0.606 -0.008  0.001  0.437       
## mnrty.c:_..  0.591 -0.011  0.008  0.429  0.986
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.immigrate_policy,mod3.immigrate_policy)
```

```
## Data: exdat
## Models:
## mod2.immigrate_policy: childlessness ~ gndr.c + age10.c + minority.c + immigrate_policy.z.gmc + immigrate_policy.z.gmc:minority.c + (1 | cntry)
## mod3.immigrate_policy: childlessness ~ gndr.c + age10.c + minority.c + immigrate_policy.z.gmc + immigrate_policy.z.gmc:minority.c + (immigrate_policy.z.gmc + minority.c + immigrate_policy.z.gmc:minority.c | cntry)
##                       npar   AIC   BIC  logLik deviance Chisq Df Pr(>Chisq)   
## mod2.immigrate_policy    7 15052 15107 -7518.8    15038                       
## mod3.immigrate_policy   16 15047 15174 -7507.4    15015 22.76  9   0.006759 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
mod4.immigrate_policy<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          immigrate_policy.z.gmc+
          immigrate_policy.z.gmc:minority.c+
          (immigrate_policy.z.gmc+minority.c+
             immigrate_policy.z.gmc:minority.c||cntry),
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
summary(mod4.immigrate_policy)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## childlessness ~ gndr.c + age10.c + minority.c + immigrate_policy.z.gmc +  
##     immigrate_policy.z.gmc:minority.c + (immigrate_policy.z.gmc +  
##     minority.c + immigrate_policy.z.gmc:minority.c || cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15057.1  15136.8  -7518.5  15037.1    21324 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6722 -0.4381 -0.2245  0.2388 12.9308 
## 
## Random effects:
##  Groups  Name                              Variance  Std.Dev. 
##  cntry   (Intercept)                       1.160e-01 3.406e-01
##  cntry.1 immigrate_policy.z.gmc            7.049e-15 8.396e-08
##  cntry.2 minority.c                        0.000e+00 0.000e+00
##  cntry.3 immigrate_policy.z.gmc:minority.c 6.188e-03 7.866e-02
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                   Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                       -1.33229    0.10239 -13.012  < 2e-16 ***
## gndr.c                            -0.30755    0.04051  -7.592 3.15e-14 ***
## age10.c                           -0.55312    0.01349 -40.999  < 2e-16 ***
## minority.c                        -0.36904    0.11274  -3.273  0.00106 ** 
## immigrate_policy.z.gmc            -0.18692    0.07306  -2.559  0.01051 *  
## minority.c:immigrate_policy.z.gmc -0.32889    0.14920  -2.204  0.02750 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. imm_..
## gndr.c       0.011                            
## age10.c      0.104  0.009                     
## minority.c   0.523 -0.004  0.087              
## immgrt_pl..  0.202 -0.006  0.000  0.367       
## mnrty.c:_..  0.199 -0.016  0.014  0.361  0.924
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.immigrate_policy,mod4.immigrate_policy)
```

```
## Data: exdat
## Models:
## mod2.immigrate_policy: childlessness ~ gndr.c + age10.c + minority.c + immigrate_policy.z.gmc + immigrate_policy.z.gmc:minority.c + (1 | cntry)
## mod4.immigrate_policy: childlessness ~ gndr.c + age10.c + minority.c + immigrate_policy.z.gmc + immigrate_policy.z.gmc:minority.c + (immigrate_policy.z.gmc + minority.c + immigrate_policy.z.gmc:minority.c || cntry)
##                       npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)
## mod2.immigrate_policy    7 15052 15107 -7518.8    15038                     
## mod4.immigrate_policy   10 15057 15137 -7518.5    15037 0.5473  3     0.9084
```

```r
anova(mod4.immigrate_policy,mod3.immigrate_policy)
```

```
## Data: exdat
## Models:
## mod4.immigrate_policy: childlessness ~ gndr.c + age10.c + minority.c + immigrate_policy.z.gmc + immigrate_policy.z.gmc:minority.c + (immigrate_policy.z.gmc + minority.c + immigrate_policy.z.gmc:minority.c || cntry)
## mod3.immigrate_policy: childlessness ~ gndr.c + age10.c + minority.c + immigrate_policy.z.gmc + immigrate_policy.z.gmc:minority.c + (immigrate_policy.z.gmc + minority.c + immigrate_policy.z.gmc:minority.c | cntry)
##                       npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)   
## mod4.immigrate_policy   10 15057 15137 -7518.5    15037                        
## mod3.immigrate_policy   16 15047 15174 -7507.4    15015 22.212  6   0.001108 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
mod5.immigrate_policy<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          immigrate_policy.z.gmc+
          immigrate_policy.z.gmc:minority.c+
          (immigrate_policy.z.gmc:minority.c|cntry),
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
summary(mod5.immigrate_policy)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## childlessness ~ gndr.c + age10.c + minority.c + immigrate_policy.z.gmc +  
##     immigrate_policy.z.gmc:minority.c + (immigrate_policy.z.gmc:minority.c |  
##     cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15053.8  15125.5  -7517.9  15035.8    21325 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7191 -0.4389 -0.2249  0.2376 13.1210 
## 
## Random effects:
##  Groups Name                              Variance Std.Dev. Corr
##  cntry  (Intercept)                       0.11370  0.33719      
##         immigrate_policy.z.gmc:minority.c 0.00598  0.07733  0.72
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                   Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                       -1.32885    0.10178 -13.056  < 2e-16 ***
## gndr.c                            -0.30761    0.04051  -7.594 3.11e-14 ***
## age10.c                           -0.55290    0.01348 -41.005  < 2e-16 ***
## minority.c                        -0.36802    0.11277  -3.263   0.0011 ** 
## immigrate_policy.z.gmc            -0.18479    0.07310  -2.528   0.0115 *  
## minority.c:immigrate_policy.z.gmc -0.33217    0.14834  -2.239   0.0251 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. imm_..
## gndr.c       0.011                            
## age10.c      0.105  0.008                     
## minority.c   0.526 -0.004  0.087              
## immgrt_pl..  0.206 -0.007  0.001  0.371       
## mnrty.c:_..  0.280 -0.017  0.016  0.368  0.929
```

```r
anova(mod2.immigrate_policy,mod5.immigrate_policy)
```

```
## Data: exdat
## Models:
## mod2.immigrate_policy: childlessness ~ gndr.c + age10.c + minority.c + immigrate_policy.z.gmc + immigrate_policy.z.gmc:minority.c + (1 | cntry)
## mod5.immigrate_policy: childlessness ~ gndr.c + age10.c + minority.c + immigrate_policy.z.gmc + immigrate_policy.z.gmc:minority.c + (immigrate_policy.z.gmc:minority.c | cntry)
##                       npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)
## mod2.immigrate_policy    7 15052 15107 -7518.8    15038                     
## mod5.immigrate_policy    9 15054 15126 -7517.9    15036 1.8774  2     0.3911
```

```r
anova(mod5.immigrate_policy,mod4.immigrate_policy)
```

```
## Data: exdat
## Models:
## mod5.immigrate_policy: childlessness ~ gndr.c + age10.c + minority.c + immigrate_policy.z.gmc + immigrate_policy.z.gmc:minority.c + (immigrate_policy.z.gmc:minority.c | cntry)
## mod4.immigrate_policy: childlessness ~ gndr.c + age10.c + minority.c + immigrate_policy.z.gmc + immigrate_policy.z.gmc:minority.c + (immigrate_policy.z.gmc + minority.c + immigrate_policy.z.gmc:minority.c || cntry)
##                       npar   AIC   BIC  logLik deviance Chisq Df Pr(>Chisq)
## mod5.immigrate_policy    9 15054 15126 -7517.9    15036                    
## mod4.immigrate_policy   10 15057 15137 -7518.5    15037     0  1          1
```

### Marginal effects


```r
emtrends(mod5.immigrate_policy,
         var="immigrate_policy.z.gmc",
         specs="minority.c",
         at=list(minority.c=c(-0.5,0.5)),
         infer=c(T,T))
```

```
##  minority.c immigrate_policy.z.gmc.trend     SE  df asymp.LCL asymp.UCL z.ratio
##        -0.5                      -0.0187 0.0277 Inf    -0.073    0.0356  -0.675
##         0.5                      -0.3509 0.1446 Inf    -0.634   -0.0674  -2.426
##  p.value
##   0.4994
##   0.0153
## 
## Results are averaged over the levels of: gndr.c 
## Confidence level used: 0.95
```

## multiculturalism

### Fixed


```r
mod2.multiculturalism<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          multiculturalism.z.gmc+
          multiculturalism.z.gmc:minority.c+
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
##     multiculturalism.z.gmc:minority.c + (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15056.1  15111.9  -7521.1  15042.1    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7118 -0.4384 -0.2245  0.2349 12.8824 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1167   0.3416  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                   Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                       -1.32086    0.10285 -12.843  < 2e-16 ***
## gndr.c                            -0.30833    0.04049  -7.616 2.62e-14 ***
## age10.c                           -0.55401    0.01348 -41.102  < 2e-16 ***
## minority.c                        -0.34634    0.11368  -3.047  0.00231 ** 
## multiculturalism.z.gmc            -0.10762    0.06621  -1.625  0.10409    
## minority.c:multiculturalism.z.gmc -0.19308    0.13246  -1.458  0.14493    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. mltc..
## gndr.c       0.013                            
## age10.c      0.105  0.008                     
## minority.c   0.526  0.000  0.089              
## mltcltrls..  0.219  0.002  0.002  0.396       
## mnrty.c:m..  0.221 -0.006  0.021  0.398  0.943
```

### Random


```r
mod3.multiculturalism<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          multiculturalism.z.gmc+
          multiculturalism.z.gmc:minority.c+
          (multiculturalism.z.gmc+minority.c+multiculturalism.z.gmc:minority.c|cntry),
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
##     multiculturalism.z.gmc:minority.c + (multiculturalism.z.gmc +  
##     minority.c + multiculturalism.z.gmc:minority.c | cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15047.0  15174.5  -7507.5  15015.0    21318 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7103 -0.4391 -0.2256  0.2281 13.3731 
## 
## Random effects:
##  Groups Name                              Variance Std.Dev. Corr          
##  cntry  (Intercept)                       0.17258  0.4154                 
##         multiculturalism.z.gmc            0.16477  0.4059   0.83          
##         minority.c                        0.03439  0.1855   0.86 1.00     
##         multiculturalism.z.gmc:minority.c 0.83569  0.9142   0.82 1.00 1.00
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                   Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                       -1.34969    0.12267 -11.003  < 2e-16 ***
## gndr.c                            -0.31021    0.04055  -7.649 2.02e-14 ***
## age10.c                           -0.55498    0.01351 -41.071  < 2e-16 ***
## minority.c                        -0.41684    0.14093  -2.958   0.0031 ** 
## multiculturalism.z.gmc            -0.13286    0.14620  -0.909   0.3635    
## minority.c:multiculturalism.z.gmc -0.23704    0.31791  -0.746   0.4559    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. mltc..
## gndr.c       0.007                            
## age10.c      0.088  0.009                     
## minority.c   0.722 -0.008  0.071              
## mltcltrls..  0.671 -0.006  0.005  0.578       
## mnrty.c:m..  0.669 -0.009  0.011  0.567  0.986
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.multiculturalism,mod3.multiculturalism)
```

```
## Data: exdat
## Models:
## mod2.multiculturalism: childlessness ~ gndr.c + age10.c + minority.c + multiculturalism.z.gmc + multiculturalism.z.gmc:minority.c + (1 | cntry)
## mod3.multiculturalism: childlessness ~ gndr.c + age10.c + minority.c + multiculturalism.z.gmc + multiculturalism.z.gmc:minority.c + (multiculturalism.z.gmc + minority.c + multiculturalism.z.gmc:minority.c | cntry)
##                       npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)   
## mod2.multiculturalism    7 15056 15112 -7521.1    15042                        
## mod3.multiculturalism   16 15047 15174 -7507.5    15015 27.108  9   0.001342 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


## urban_rural

### Fixed


```r
mod2.urban_rural<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          urban_rural.z.gmc+
          urban_rural.z.gmc:minority.c+
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
##     urban_rural.z.gmc:minority.c + (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15058.7  15114.5  -7522.3  15044.7    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7437 -0.4391 -0.2244  0.2382 13.0218 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1173   0.3425  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                  -1.28404    0.10210 -12.576  < 2e-16 ***
## gndr.c                       -0.30791    0.04047  -7.608 2.79e-14 ***
## age10.c                      -0.55443    0.01347 -41.147  < 2e-16 ***
## minority.c                   -0.27276    0.11066  -2.465   0.0137 *  
## urban_rural.z.gmc             0.01806    0.07155   0.252   0.8007    
## minority.c:urban_rural.z.gmc  0.01579    0.14307   0.110   0.9121    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. urb_..
## gndr.c      0.017                             
## age10.c     0.106  0.010                      
## minority.c  0.512  0.006  0.092               
## urbn_rrl.z. 0.180  0.012  0.005  0.339        
## mnrty.c:_.. 0.180  0.013  0.023  0.340  0.939
```

### Random


```r
mod3.urban_rural<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          urban_rural.z.gmc+
          urban_rural.z.gmc:minority.c+
          (urban_rural.z.gmc+minority.c+urban_rural.z.gmc:minority.c|cntry),
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
##     urban_rural.z.gmc:minority.c + (urban_rural.z.gmc + minority.c +  
##     urban_rural.z.gmc:minority.c | cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15054.1  15181.6  -7511.1  15022.1    21318 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7594 -0.4394 -0.2245  0.2378 13.5951 
## 
## Random effects:
##  Groups Name                         Variance Std.Dev. Corr          
##  cntry  (Intercept)                  0.125388 0.3541                 
##         urban_rural.z.gmc            0.110511 0.3324   0.73          
##         minority.c                   0.003238 0.0569   0.43 0.93     
##         urban_rural.z.gmc:minority.c 0.527491 0.7263   0.75 1.00 0.92
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                  -1.29780    0.10918 -11.887  < 2e-16 ***
## gndr.c                       -0.31077    0.04053  -7.667 1.76e-14 ***
## age10.c                      -0.55607    0.01350 -41.184  < 2e-16 ***
## minority.c                   -0.30094    0.13225  -2.275   0.0229 *  
## urban_rural.z.gmc            -0.05176    0.13423  -0.386   0.6998    
## minority.c:urban_rural.z.gmc -0.11927    0.28200  -0.423   0.6723    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. urb_..
## gndr.c      0.015                             
## age10.c     0.095  0.011                      
## minority.c  0.598  0.003  0.071               
## urbn_rrl.z. 0.540  0.007  0.011  0.397        
## mnrty.c:_.. 0.554  0.006  0.022  0.384  0.980 
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.urban_rural,mod3.urban_rural)
```

```
## Data: exdat
## Models:
## mod2.urban_rural: childlessness ~ gndr.c + age10.c + minority.c + urban_rural.z.gmc + urban_rural.z.gmc:minority.c + (1 | cntry)
## mod3.urban_rural: childlessness ~ gndr.c + age10.c + minority.c + urban_rural.z.gmc + urban_rural.z.gmc:minority.c + (urban_rural.z.gmc + minority.c + urban_rural.z.gmc:minority.c | cntry)
##                  npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)   
## mod2.urban_rural    7 15059 15114 -7522.3    15045                        
## mod3.urban_rural   16 15054 15182 -7511.1    15022 22.582  9   0.007207 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```



## environment

### Fixed


```r
mod2.environment<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          environment.z.gmc+
          environment.z.gmc:minority.c+
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
##     environment.z.gmc:minority.c + (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15049.7  15105.5  -7517.9  15035.7    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7081 -0.4387 -0.2244  0.2384 12.8521 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1164   0.3411  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                  -1.33866    0.10237 -13.077  < 2e-16 ***
## gndr.c                       -0.30773    0.04052  -7.595 3.07e-14 ***
## age10.c                      -0.55300    0.01350 -40.949  < 2e-16 ***
## minority.c                   -0.38165    0.11227  -3.399 0.000675 ***
## environment.z.gmc            -0.19261    0.06512  -2.958 0.003100 ** 
## minority.c:environment.z.gmc -0.33521    0.13031  -2.572 0.010102 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. envr..
## gndr.c       0.011                            
## age10.c      0.104  0.006                     
## minority.c   0.521 -0.004  0.087              
## envrnmnt.z.  0.193 -0.007 -0.009  0.350       
## mnrty.c:n..  0.194 -0.021  0.023  0.355  0.926
```

### Random


```r
mod3.environment<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          environment.z.gmc+
          environment.z.gmc:minority.c+
          (environment.z.gmc+minority.c+environment.z.gmc:minority.c|cntry),
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
##     environment.z.gmc:minority.c + (environment.z.gmc + minority.c +  
##     environment.z.gmc:minority.c | cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15049.6  15177.1  -7508.8  15017.6    21318 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7307 -0.4393 -0.2249  0.2363 13.0968 
## 
## Random effects:
##  Groups Name                         Variance  Std.Dev. Corr          
##  cntry  (Intercept)                  0.1236917 0.35170                
##         environment.z.gmc            0.0643604 0.25369  0.93          
##         minority.c                   0.0007146 0.02673  0.98 0.98     
##         environment.z.gmc:minority.c 0.3140648 0.56041  0.92 1.00 0.98
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                  -1.33920    0.10548 -12.696  < 2e-16 ***
## gndr.c                       -0.30743    0.04056  -7.580 3.44e-14 ***
## age10.c                      -0.55412    0.01353 -40.942  < 2e-16 ***
## minority.c                   -0.38950    0.11655  -3.342 0.000832 ***
## environment.z.gmc            -0.18284    0.10239  -1.786 0.074153 .  
## minority.c:environment.z.gmc -0.32570    0.21705  -1.501 0.133463    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. envr..
## gndr.c       0.009                            
## age10.c      0.099  0.007                     
## minority.c   0.569 -0.008  0.081              
## envrnmnt.z.  0.603 -0.005  0.005  0.282       
## mnrty.c:n..  0.609 -0.013  0.023  0.274  0.969
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.environment,mod3.environment)
```

```
## Data: exdat
## Models:
## mod2.environment: childlessness ~ gndr.c + age10.c + minority.c + environment.z.gmc + environment.z.gmc:minority.c + (1 | cntry)
## mod3.environment: childlessness ~ gndr.c + age10.c + minority.c + environment.z.gmc + environment.z.gmc:minority.c + (environment.z.gmc + minority.c + environment.z.gmc:minority.c | cntry)
##                  npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)  
## mod2.environment    7 15050 15106 -7517.9    15036                       
## mod3.environment   16 15050 15177 -7508.8    15018 18.081  9    0.03424 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
mod4.environment<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          environment.z.gmc+
          environment.z.gmc:minority.c+
          (environment.z.gmc+minority.c+
             environment.z.gmc:minority.c||cntry),
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
summary(mod4.environment)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + environment.z.gmc +  
##     environment.z.gmc:minority.c + (environment.z.gmc + minority.c +  
##     environment.z.gmc:minority.c || cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15055.7  15135.4  -7517.9  15035.7    21324 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7080 -0.4387 -0.2244  0.2384 12.8520 
## 
## Random effects:
##  Groups  Name                         Variance  Std.Dev. 
##  cntry   (Intercept)                  1.164e-01 3.411e-01
##  cntry.1 environment.z.gmc            0.000e+00 0.000e+00
##  cntry.2 minority.c                   5.699e-10 2.387e-05
##  cntry.3 environment.z.gmc:minority.c 7.426e-13 8.618e-07
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                  -1.33867    0.10237 -13.077  < 2e-16 ***
## gndr.c                       -0.30773    0.04052  -7.595 3.07e-14 ***
## age10.c                      -0.55300    0.01350 -40.949  < 2e-16 ***
## minority.c                   -0.38164    0.11227  -3.399 0.000675 ***
## environment.z.gmc            -0.19262    0.06512  -2.958 0.003099 ** 
## minority.c:environment.z.gmc -0.33522    0.13031  -2.572 0.010099 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. envr..
## gndr.c       0.011                            
## age10.c      0.104  0.006                     
## minority.c   0.521 -0.004  0.087              
## envrnmnt.z.  0.193 -0.007 -0.009  0.350       
## mnrty.c:n..  0.194 -0.021  0.023  0.355  0.926
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.environment,mod4.environment)
```

```
## Data: exdat
## Models:
## mod2.environment: childlessness ~ gndr.c + age10.c + minority.c + environment.z.gmc + environment.z.gmc:minority.c + (1 | cntry)
## mod4.environment: childlessness ~ gndr.c + age10.c + minority.c + environment.z.gmc + environment.z.gmc:minority.c + (environment.z.gmc + minority.c + environment.z.gmc:minority.c || cntry)
##                  npar   AIC   BIC  logLik deviance Chisq Df Pr(>Chisq)
## mod2.environment    7 15050 15106 -7517.9    15036                    
## mod4.environment   10 15056 15135 -7517.9    15036     0  3          1
```

```r
anova(mod4.environment,mod3.environment)
```

```
## Data: exdat
## Models:
## mod4.environment: childlessness ~ gndr.c + age10.c + minority.c + environment.z.gmc + environment.z.gmc:minority.c + (environment.z.gmc + minority.c + environment.z.gmc:minority.c || cntry)
## mod3.environment: childlessness ~ gndr.c + age10.c + minority.c + environment.z.gmc + environment.z.gmc:minority.c + (environment.z.gmc + minority.c + environment.z.gmc:minority.c | cntry)
##                  npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)   
## mod4.environment   10 15056 15135 -7517.9    15036                        
## mod3.environment   16 15050 15177 -7508.8    15018 18.081  6   0.006033 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
mod5.environment<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          environment.z.gmc+
          environment.z.gmc:minority.c+
          (environment.z.gmc:minority.c|cntry),
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
summary(mod5.environment)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + environment.z.gmc +  
##     environment.z.gmc:minority.c + (environment.z.gmc:minority.c |      cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15051.1  15122.8  -7516.5  15033.1    21325 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.8229 -0.4391 -0.2249  0.2368 13.0630 
## 
## Random effects:
##  Groups Name                         Variance Std.Dev. Corr
##  cntry  (Intercept)                  0.113032 0.33620      
##         environment.z.gmc:minority.c 0.005286 0.07271  1.00
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                  -1.33349    0.10142 -13.148  < 2e-16 ***
## gndr.c                       -0.30787    0.04053  -7.597 3.04e-14 ***
## age10.c                      -0.55226    0.01351 -40.889  < 2e-16 ***
## minority.c                   -0.37974    0.11224  -3.383 0.000716 ***
## environment.z.gmc            -0.18876    0.06487  -2.910 0.003618 ** 
## minority.c:environment.z.gmc -0.34442    0.13106  -2.628 0.008592 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. envr..
## gndr.c       0.011                            
## age10.c      0.105  0.006                     
## minority.c   0.525 -0.005  0.087              
## envrnmnt.z.  0.198 -0.008 -0.007  0.354       
## mnrty.c:n..  0.310 -0.022  0.024  0.357  0.913
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.environment,mod5.environment)
```

```
## Data: exdat
## Models:
## mod2.environment: childlessness ~ gndr.c + age10.c + minority.c + environment.z.gmc + environment.z.gmc:minority.c + (1 | cntry)
## mod5.environment: childlessness ~ gndr.c + age10.c + minority.c + environment.z.gmc + environment.z.gmc:minority.c + (environment.z.gmc:minority.c | cntry)
##                  npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)
## mod2.environment    7 15050 15106 -7517.9    15036                     
## mod5.environment    9 15051 15123 -7516.5    15033 2.6623  2     0.2642
```

```r
anova(mod5.environment,mod4.environment)
```

```
## Data: exdat
## Models:
## mod5.environment: childlessness ~ gndr.c + age10.c + minority.c + environment.z.gmc + environment.z.gmc:minority.c + (environment.z.gmc:minority.c | cntry)
## mod4.environment: childlessness ~ gndr.c + age10.c + minority.c + environment.z.gmc + environment.z.gmc:minority.c + (environment.z.gmc + minority.c + environment.z.gmc:minority.c || cntry)
##                  npar   AIC   BIC  logLik deviance Chisq Df Pr(>Chisq)
## mod5.environment    9 15051 15123 -7516.5    15033                    
## mod4.environment   10 15056 15135 -7517.9    15036     0  1          1
```

```r
mod6.environment<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          environment.z.gmc+
          environment.z.gmc:minority.c+
          (environment.z.gmc:minority.c||cntry),
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
summary(mod6.environment)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + environment.z.gmc +  
##     environment.z.gmc:minority.c + (environment.z.gmc:minority.c ||      cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15051.7  15115.5  -7517.9  15035.7    21326 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7080 -0.4387 -0.2244  0.2384 12.8520 
## 
## Random effects:
##  Groups  Name                         Variance Std.Dev.
##  cntry   (Intercept)                  0.1164   0.3411  
##  cntry.1 environment.z.gmc:minority.c 0.0000   0.0000  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                  -1.33867    0.10237 -13.077  < 2e-16 ***
## gndr.c                       -0.30773    0.04052  -7.595 3.08e-14 ***
## age10.c                      -0.55300    0.01350 -40.949  < 2e-16 ***
## minority.c                   -0.38165    0.11227  -3.399 0.000676 ***
## environment.z.gmc            -0.19262    0.06513  -2.958 0.003101 ** 
## minority.c:environment.z.gmc -0.33523    0.13033  -2.572 0.010104 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. envr..
## gndr.c       0.011                            
## age10.c      0.104  0.006                     
## minority.c   0.521 -0.004  0.087              
## envrnmnt.z.  0.193 -0.007 -0.009  0.350       
## mnrty.c:n..  0.194 -0.021  0.023  0.355  0.926
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.environment,mod6.environment)
```

```
## Data: exdat
## Models:
## mod2.environment: childlessness ~ gndr.c + age10.c + minority.c + environment.z.gmc + environment.z.gmc:minority.c + (1 | cntry)
## mod6.environment: childlessness ~ gndr.c + age10.c + minority.c + environment.z.gmc + environment.z.gmc:minority.c + (environment.z.gmc:minority.c || cntry)
##                  npar   AIC   BIC  logLik deviance Chisq Df Pr(>Chisq)
## mod2.environment    7 15050 15106 -7517.9    15036                    
## mod6.environment    8 15052 15116 -7517.9    15036     0  1     0.9995
```

```r
anova(mod6.environment,mod5.environment)
```

```
## Data: exdat
## Models:
## mod6.environment: childlessness ~ gndr.c + age10.c + minority.c + environment.z.gmc + environment.z.gmc:minority.c + (environment.z.gmc:minority.c || cntry)
## mod5.environment: childlessness ~ gndr.c + age10.c + minority.c + environment.z.gmc + environment.z.gmc:minority.c + (environment.z.gmc:minority.c | cntry)
##                  npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)
## mod6.environment    8 15052 15116 -7517.9    15036                     
## mod5.environment    9 15051 15123 -7516.5    15033 2.6623  1     0.1028
```
### Marginal effects


```r
emtrends(mod2.environment,
         var="environment.z.gmc",
         specs="minority.c",
         at=list(minority.c=c(-0.5,0.5)),
         infer=c(T,T))
```

```
##  minority.c environment.z.gmc.trend     SE  df asymp.LCL asymp.UCL z.ratio
##        -0.5                  -0.025 0.0251 Inf   -0.0741    0.0241  -0.998
##         0.5                  -0.360 0.1278 Inf   -0.6108   -0.1096  -2.818
##  p.value
##   0.3182
##   0.0048
## 
## Results are averaged over the levels of: gndr.c 
## Confidence level used: 0.95
```

## regions

### Fixed


```r
mod2.regions<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          regions.z.gmc+
          regions.z.gmc:minority.c+
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
##     regions.z.gmc:minority.c + (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15043.6  15099.4  -7514.8  15029.6    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6901 -0.4392 -0.2245  0.2343 12.7529 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1187   0.3445  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                          Estimate Std. Error z value Pr(>|z|)    
## (Intercept)              -1.30448    0.10140 -12.864  < 2e-16 ***
## gndr.c                   -0.30590    0.04050  -7.553 4.26e-14 ***
## age10.c                  -0.55324    0.01347 -41.078  < 2e-16 ***
## minority.c               -0.31348    0.10645  -2.945 0.003231 ** 
## regions.z.gmc            -0.30404    0.08177  -3.718 0.000201 ***
## minority.c:regions.z.gmc -0.54867    0.16353  -3.355 0.000793 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. rgns..
## gndr.c       0.012                            
## age10.c      0.103  0.009                     
## minority.c   0.495 -0.004  0.087              
## regns.z.gmc  0.072 -0.015 -0.001  0.133       
## mnrty.c:r..  0.072 -0.017  0.010  0.135  0.956
```

### Random


```r
mod3.regions<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          regions.z.gmc+
          regions.z.gmc:minority.c+
          (regions.z.gmc+minority.c+regions.z.gmc:minority.c|cntry),
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
##     regions.z.gmc:minority.c + (regions.z.gmc + minority.c +  
##     regions.z.gmc:minority.c | cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15045.9  15173.4  -7507.0  15013.9    21318 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7118 -0.4388 -0.2255  0.2273 13.1134 
## 
## Random effects:
##  Groups Name                     Variance Std.Dev. Corr          
##  cntry  (Intercept)              0.132306 0.36374                
##         regions.z.gmc            0.060879 0.24674  1.00          
##         minority.c               0.003068 0.05539  1.00 1.00     
##         regions.z.gmc:minority.c 0.284666 0.53354  1.00 1.00 1.00
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                          Estimate Std. Error z value Pr(>|z|)    
## (Intercept)              -1.34009    0.10818 -12.388  < 2e-16 ***
## gndr.c                   -0.30728    0.04054  -7.580 3.45e-14 ***
## age10.c                  -0.55471    0.01349 -41.120  < 2e-16 ***
## minority.c               -0.39257    0.11836  -3.317  0.00091 ***
## regions.z.gmc            -0.31850    0.10605  -3.003  0.00267 ** 
## minority.c:regions.z.gmc -0.58053    0.21813  -2.661  0.00778 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. rgns..
## gndr.c       0.009                            
## age10.c      0.098  0.011                     
## minority.c   0.607 -0.008  0.081              
## regns.z.gmc  0.621 -0.024  0.007  0.327       
## mnrty.c:r..  0.642 -0.025  0.015  0.325  0.974
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.regions,mod3.regions)
```

```
## Data: exdat
## Models:
## mod2.regions: childlessness ~ gndr.c + age10.c + minority.c + regions.z.gmc + regions.z.gmc:minority.c + (1 | cntry)
## mod3.regions: childlessness ~ gndr.c + age10.c + minority.c + regions.z.gmc + regions.z.gmc:minority.c + (regions.z.gmc + minority.c + regions.z.gmc:minority.c | cntry)
##              npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)  
## mod2.regions    7 15044 15099 -7514.8    15030                       
## mod3.regions   16 15046 15173 -7507.0    15014 15.645  9    0.07467 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
mod4.regions<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          regions.z.gmc+
          regions.z.gmc:minority.c+
          (regions.z.gmc+minority.c+
             regions.z.gmc:minority.c||cntry),
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
summary(mod4.regions)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + regions.z.gmc +  
##     regions.z.gmc:minority.c + (regions.z.gmc + minority.c +  
##     regions.z.gmc:minority.c || cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15049.6  15129.3  -7514.8  15029.6    21324 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6901 -0.4392 -0.2245  0.2343 12.7530 
## 
## Random effects:
##  Groups  Name                     Variance  Std.Dev. 
##  cntry   (Intercept)              1.187e-01 3.446e-01
##  cntry.1 regions.z.gmc            0.000e+00 0.000e+00
##  cntry.2 minority.c               2.109e-14 1.452e-07
##  cntry.3 regions.z.gmc:minority.c 1.314e-10 1.146e-05
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                          Estimate Std. Error z value Pr(>|z|)    
## (Intercept)              -1.30447    0.10140 -12.864  < 2e-16 ***
## gndr.c                   -0.30590    0.04050  -7.553 4.26e-14 ***
## age10.c                  -0.55325    0.01347 -41.078  < 2e-16 ***
## minority.c               -0.31348    0.10645  -2.945 0.003230 ** 
## regions.z.gmc            -0.30404    0.08177  -3.718 0.000201 ***
## minority.c:regions.z.gmc -0.54867    0.16351  -3.356 0.000792 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. rgns..
## gndr.c       0.012                            
## age10.c      0.103  0.009                     
## minority.c   0.495 -0.004  0.087              
## regns.z.gmc  0.072 -0.015 -0.001  0.133       
## mnrty.c:r..  0.072 -0.017  0.010  0.135  0.956
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.regions,mod4.regions)
```

```
## Data: exdat
## Models:
## mod2.regions: childlessness ~ gndr.c + age10.c + minority.c + regions.z.gmc + regions.z.gmc:minority.c + (1 | cntry)
## mod4.regions: childlessness ~ gndr.c + age10.c + minority.c + regions.z.gmc + regions.z.gmc:minority.c + (regions.z.gmc + minority.c + regions.z.gmc:minority.c || cntry)
##              npar   AIC   BIC  logLik deviance Chisq Df Pr(>Chisq)
## mod2.regions    7 15044 15099 -7514.8    15030                    
## mod4.regions   10 15050 15129 -7514.8    15030     0  3          1
```

```r
anova(mod4.regions,mod3.regions)
```

```
## Data: exdat
## Models:
## mod4.regions: childlessness ~ gndr.c + age10.c + minority.c + regions.z.gmc + regions.z.gmc:minority.c + (regions.z.gmc + minority.c + regions.z.gmc:minority.c || cntry)
## mod3.regions: childlessness ~ gndr.c + age10.c + minority.c + regions.z.gmc + regions.z.gmc:minority.c + (regions.z.gmc + minority.c + regions.z.gmc:minority.c | cntry)
##              npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)  
## mod4.regions   10 15050 15129 -7514.8    15030                       
## mod3.regions   16 15046 15173 -7507.0    15014 15.645  6    0.01579 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
mod5.regions<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          regions.z.gmc+
          regions.z.gmc:minority.c+
          (regions.z.gmc:minority.c|cntry),
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
summary(mod5.regions)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + regions.z.gmc +  
##     regions.z.gmc:minority.c + (regions.z.gmc:minority.c | cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15045.2  15116.9  -7513.6  15027.2    21325 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6931 -0.4382 -0.2247  0.2331 13.1257 
## 
## Random effects:
##  Groups Name                     Variance Std.Dev. Corr
##  cntry  (Intercept)              0.116997 0.34205      
##         regions.z.gmc:minority.c 0.004155 0.06446  1.00
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                          Estimate Std. Error z value Pr(>|z|)    
## (Intercept)              -1.30500    0.10092 -12.931  < 2e-16 ***
## gndr.c                   -0.30509    0.04051  -7.531 5.03e-14 ***
## age10.c                  -0.55299    0.01347 -41.065  < 2e-16 ***
## minority.c               -0.31562    0.10643  -2.966 0.003021 ** 
## regions.z.gmc            -0.30208    0.08167  -3.699 0.000217 ***
## minority.c:regions.z.gmc -0.54759    0.16412  -3.337 0.000848 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. rgns..
## gndr.c       0.012                            
## age10.c      0.103  0.009                     
## minority.c   0.497 -0.004  0.087              
## regns.z.gmc  0.076 -0.016  0.000  0.141       
## mnrty.c:r..  0.158 -0.019  0.011  0.142  0.951
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.regions,mod5.regions)
```

```
## Data: exdat
## Models:
## mod2.regions: childlessness ~ gndr.c + age10.c + minority.c + regions.z.gmc + regions.z.gmc:minority.c + (1 | cntry)
## mod5.regions: childlessness ~ gndr.c + age10.c + minority.c + regions.z.gmc + regions.z.gmc:minority.c + (regions.z.gmc:minority.c | cntry)
##              npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)
## mod2.regions    7 15044 15099 -7514.8    15030                     
## mod5.regions    9 15045 15117 -7513.6    15027 2.4044  2     0.3005
```

```r
anova(mod5.regions,mod4.regions)
```

```
## Data: exdat
## Models:
## mod5.regions: childlessness ~ gndr.c + age10.c + minority.c + regions.z.gmc + regions.z.gmc:minority.c + (regions.z.gmc:minority.c | cntry)
## mod4.regions: childlessness ~ gndr.c + age10.c + minority.c + regions.z.gmc + regions.z.gmc:minority.c + (regions.z.gmc + minority.c + regions.z.gmc:minority.c || cntry)
##              npar   AIC   BIC  logLik deviance Chisq Df Pr(>Chisq)
## mod5.regions    9 15045 15117 -7513.6    15027                    
## mod4.regions   10 15050 15129 -7514.8    15030     0  1          1
```

```r
mod6.regions<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          regions.z.gmc+
          regions.z.gmc:minority.c+
          (regions.z.gmc:minority.c||cntry),
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
summary(mod6.regions)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + regions.z.gmc +  
##     regions.z.gmc:minority.c + (regions.z.gmc:minority.c || cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15045.6  15109.3  -7514.8  15029.6    21326 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6901 -0.4392 -0.2245  0.2343 12.7530 
## 
## Random effects:
##  Groups  Name                     Variance  Std.Dev. 
##  cntry   (Intercept)              1.187e-01 3.446e-01
##  cntry.1 regions.z.gmc:minority.c 2.711e-13 5.207e-07
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                          Estimate Std. Error z value Pr(>|z|)    
## (Intercept)              -1.30447    0.10139 -12.865  < 2e-16 ***
## gndr.c                   -0.30590    0.04050  -7.553 4.25e-14 ***
## age10.c                  -0.55325    0.01347 -41.078  < 2e-16 ***
## minority.c               -0.31348    0.10644  -2.945  0.00323 ** 
## regions.z.gmc            -0.30404    0.08175  -3.719  0.00020 ***
## minority.c:regions.z.gmc -0.54866    0.16348  -3.356  0.00079 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. rgns..
## gndr.c       0.012                            
## age10.c      0.103  0.009                     
## minority.c   0.495 -0.004  0.087              
## regns.z.gmc  0.072 -0.015 -0.001  0.133       
## mnrty.c:r..  0.072 -0.017  0.010  0.135  0.956
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.regions,mod6.regions)
```

```
## Data: exdat
## Models:
## mod2.regions: childlessness ~ gndr.c + age10.c + minority.c + regions.z.gmc + regions.z.gmc:minority.c + (1 | cntry)
## mod6.regions: childlessness ~ gndr.c + age10.c + minority.c + regions.z.gmc + regions.z.gmc:minority.c + (regions.z.gmc:minority.c || cntry)
##              npar   AIC   BIC  logLik deviance Chisq Df Pr(>Chisq)
## mod2.regions    7 15044 15099 -7514.8    15030                    
## mod6.regions    8 15046 15109 -7514.8    15030     0  1     0.9999
```

```r
anova(mod6.regions,mod5.regions)
```

```
## Data: exdat
## Models:
## mod6.regions: childlessness ~ gndr.c + age10.c + minority.c + regions.z.gmc + regions.z.gmc:minority.c + (regions.z.gmc:minority.c || cntry)
## mod5.regions: childlessness ~ gndr.c + age10.c + minority.c + regions.z.gmc + regions.z.gmc:minority.c + (regions.z.gmc:minority.c | cntry)
##              npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)
## mod6.regions    8 15046 15109 -7514.8    15030                     
## mod5.regions    9 15045 15117 -7513.6    15027 2.4044  1      0.121
```
### Marginal effects


```r
emtrends(mod2.regions,
         var="regions.z.gmc",
         specs="minority.c",
         at=list(minority.c=c(-0.5,0.5)),
         infer=c(T,T))
```

```
##  minority.c regions.z.gmc.trend     SE  df asymp.LCL asymp.UCL z.ratio p.value
##        -0.5             -0.0297 0.0242 Inf   -0.0772    0.0178  -1.226  0.2202
##         0.5             -0.5784 0.1617 Inf   -0.8954   -0.2614  -3.576  0.0003
## 
## Results are averaged over the levels of: gndr.c 
## Confidence level used: 0.95
```

## international_security

### Fixed


```r
mod2.international_security<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          international_security.z.gmc+
          international_security.z.gmc:minority.c+
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
##     international_security.z.gmc:minority.c + (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15052.8  15108.6  -7519.4  15038.8    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7092 -0.4386 -0.2243  0.2332 12.9710 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1182   0.3437  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                         Estimate Std. Error z value Pr(>|z|)
## (Intercept)                             -1.30798    0.10126 -12.917  < 2e-16
## gndr.c                                  -0.30570    0.04050  -7.549  4.4e-14
## age10.c                                 -0.55306    0.01349 -40.985  < 2e-16
## minority.c                              -0.31807    0.10628  -2.993  0.00277
## international_security.z.gmc             0.19982    0.08239   2.425  0.01530
## minority.c:international_security.z.gmc  0.34368    0.16480   2.085  0.03703
##                                            
## (Intercept)                             ***
## gndr.c                                  ***
## age10.c                                 ***
## minority.c                              ** 
## international_security.z.gmc            *  
## minority.c:international_security.z.gmc *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. int_..
## gndr.c       0.013                            
## age10.c      0.104  0.011                     
## minority.c   0.496  0.000  0.090              
## intrntnl_.. -0.099  0.016  0.011 -0.177       
## mnrty.c:_.. -0.099  0.006 -0.015 -0.180  0.936
```

### Random


```r
mod3.international_security<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          international_security.z.gmc+
          international_security.z.gmc:minority.c+
          (international_security.z.gmc+minority.c+international_security.z.gmc:minority.c|cntry),
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
##     international_security.z.gmc:minority.c + (international_security.z.gmc +  
##     minority.c + international_security.z.gmc:minority.c | cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15058.9  15186.4  -7513.4  15026.9    21318 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7758 -0.4394 -0.2239  0.2346 12.7723 
## 
## Random effects:
##  Groups Name                                    Variance Std.Dev. Corr       
##  cntry  (Intercept)                             0.10548  0.3248              
##         international_security.z.gmc            0.01131  0.1064   -1.00      
##         minority.c                              0.04466  0.2113   -0.02 -0.04
##         international_security.z.gmc:minority.c 0.12875  0.3588   -0.70  0.74
##       
##       
##       
##       
##  -0.70
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                         Estimate Std. Error z value Pr(>|z|)
## (Intercept)                             -1.25036    0.10699 -11.686  < 2e-16
## gndr.c                                  -0.30124    0.04061  -7.417 1.19e-13
## age10.c                                 -0.55308    0.01351 -40.928  < 2e-16
## minority.c                              -0.19293    0.15080  -1.279   0.2008
## international_security.z.gmc             0.20255    0.10783   1.878   0.0603
## minority.c:international_security.z.gmc  0.30653    0.23234   1.319   0.1871
##                                            
## (Intercept)                             ***
## gndr.c                                  ***
## age10.c                                 ***
## minority.c                                 
## international_security.z.gmc            .  
## minority.c:international_security.z.gmc    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. int_..
## gndr.c       0.008                            
## age10.c      0.086  0.012                     
## minority.c   0.580 -0.008  0.044              
## intrntnl_.. -0.335  0.018  0.019 -0.237       
## mnrty.c:_.. -0.357  0.012  0.001 -0.333  0.908
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.international_security,mod3.international_security)
```

```
## Data: exdat
## Models:
## mod2.international_security: childlessness ~ gndr.c + age10.c + minority.c + international_security.z.gmc + international_security.z.gmc:minority.c + (1 | cntry)
## mod3.international_security: childlessness ~ gndr.c + age10.c + minority.c + international_security.z.gmc + international_security.z.gmc:minority.c + (international_security.z.gmc + minority.c + international_security.z.gmc:minority.c | cntry)
##                             npar   AIC   BIC  logLik deviance  Chisq Df
## mod2.international_security    7 15053 15109 -7519.4    15039          
## mod3.international_security   16 15059 15186 -7513.4    15027 11.903  9
##                             Pr(>Chisq)
## mod2.international_security           
## mod3.international_security     0.2189
```

```r
mod4.international_security<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          international_security.z.gmc+
          international_security.z.gmc:minority.c+
          (international_security.z.gmc+minority.c+
             international_security.z.gmc:minority.c||cntry),
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
summary(mod4.international_security)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## childlessness ~ gndr.c + age10.c + minority.c + international_security.z.gmc +  
##     international_security.z.gmc:minority.c + (international_security.z.gmc +  
##     minority.c + international_security.z.gmc:minority.c || cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15051.9  15131.6  -7516.0  15031.9    21324 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7373 -0.4392 -0.2238  0.2364 12.7442 
## 
## Random effects:
##  Groups  Name                                    Variance Std.Dev.
##  cntry   (Intercept)                             0.11767  0.3430  
##  cntry.1 international_security.z.gmc            0.00000  0.0000  
##  cntry.2 minority.c                              0.00000  0.0000  
##  cntry.3 international_security.z.gmc:minority.c 0.05136  0.2266  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                         Estimate Std. Error z value Pr(>|z|)
## (Intercept)                             -1.31142    0.10114 -12.967  < 2e-16
## gndr.c                                  -0.30200    0.04060  -7.439 1.01e-13
## age10.c                                 -0.55255    0.01350 -40.924  < 2e-16
## minority.c                              -0.31783    0.10622  -2.992  0.00277
## international_security.z.gmc             0.17537    0.08315   2.109  0.03493
## minority.c:international_security.z.gmc  0.25125    0.18178   1.382  0.16693
##                                            
## (Intercept)                             ***
## gndr.c                                  ***
## age10.c                                 ***
## minority.c                              ** 
## international_security.z.gmc            *  
## minority.c:international_security.z.gmc    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. int_..
## gndr.c       0.012                            
## age10.c      0.104  0.011                     
## minority.c   0.496 -0.001  0.090              
## intrntnl_.. -0.096  0.010  0.010 -0.174       
## mnrty.c:_.. -0.086  0.003 -0.013 -0.163  0.857
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.international_security,mod4.international_security)
```

```
## Data: exdat
## Models:
## mod2.international_security: childlessness ~ gndr.c + age10.c + minority.c + international_security.z.gmc + international_security.z.gmc:minority.c + (1 | cntry)
## mod4.international_security: childlessness ~ gndr.c + age10.c + minority.c + international_security.z.gmc + international_security.z.gmc:minority.c + (international_security.z.gmc + minority.c + international_security.z.gmc:minority.c || cntry)
##                             npar   AIC   BIC  logLik deviance  Chisq Df
## mod2.international_security    7 15053 15109 -7519.4    15039          
## mod4.international_security   10 15052 15132 -7516.0    15032 6.8532  3
##                             Pr(>Chisq)  
## mod2.international_security             
## mod4.international_security    0.07673 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
anova(mod4.international_security,mod3.international_security)
```

```
## Data: exdat
## Models:
## mod4.international_security: childlessness ~ gndr.c + age10.c + minority.c + international_security.z.gmc + international_security.z.gmc:minority.c + (international_security.z.gmc + minority.c + international_security.z.gmc:minority.c || cntry)
## mod3.international_security: childlessness ~ gndr.c + age10.c + minority.c + international_security.z.gmc + international_security.z.gmc:minority.c + (international_security.z.gmc + minority.c + international_security.z.gmc:minority.c | cntry)
##                             npar   AIC   BIC  logLik deviance  Chisq Df
## mod4.international_security   10 15052 15132 -7516.0    15032          
## mod3.international_security   16 15059 15186 -7513.4    15027 5.0495  6
##                             Pr(>Chisq)
## mod4.international_security           
## mod3.international_security     0.5375
```

```r
mod5.international_security<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          international_security.z.gmc+
          international_security.z.gmc:minority.c+
          (international_security.z.gmc:minority.c|cntry),
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
summary(mod5.international_security)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## childlessness ~ gndr.c + age10.c + minority.c + international_security.z.gmc +  
##     international_security.z.gmc:minority.c + (international_security.z.gmc:minority.c |  
##     cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15049.9  15121.6  -7516.0  15031.9    21325 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7377 -0.4391 -0.2239  0.2367 12.7469 
## 
## Random effects:
##  Groups Name                                    Variance Std.Dev. Corr
##  cntry  (Intercept)                             0.11788  0.3433       
##         international_security.z.gmc:minority.c 0.05142  0.2268   0.04
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                         Estimate Std. Error z value Pr(>|z|)
## (Intercept)                             -1.31170    0.10123 -12.958  < 2e-16
## gndr.c                                  -0.30204    0.04060  -7.440 1.01e-13
## age10.c                                 -0.55257    0.01350 -40.921  < 2e-16
## minority.c                              -0.31779    0.10622  -2.992  0.00277
## international_security.z.gmc             0.17530    0.08317   2.108  0.03505
## minority.c:international_security.z.gmc  0.25051    0.18190   1.377  0.16844
##                                            
## (Intercept)                             ***
## gndr.c                                  ***
## age10.c                                 ***
## minority.c                              ** 
## international_security.z.gmc            *  
## minority.c:international_security.z.gmc    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. int_..
## gndr.c       0.013                            
## age10.c      0.104  0.012                     
## minority.c   0.495 -0.001  0.090              
## intrntnl_.. -0.095  0.010  0.010 -0.173       
## mnrty.c:_.. -0.074  0.003 -0.013 -0.163  0.857
```

```r
anova(mod2.international_security,mod5.international_security)
```

```
## Data: exdat
## Models:
## mod2.international_security: childlessness ~ gndr.c + age10.c + minority.c + international_security.z.gmc + international_security.z.gmc:minority.c + (1 | cntry)
## mod5.international_security: childlessness ~ gndr.c + age10.c + minority.c + international_security.z.gmc + international_security.z.gmc:minority.c + (international_security.z.gmc:minority.c | cntry)
##                             npar   AIC   BIC  logLik deviance  Chisq Df
## mod2.international_security    7 15053 15109 -7519.4    15039          
## mod5.international_security    9 15050 15122 -7516.0    15032 6.8646  2
##                             Pr(>Chisq)  
## mod2.international_security             
## mod5.international_security    0.03231 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
anova(mod5.international_security,mod4.international_security)
```

```
## Data: exdat
## Models:
## mod5.international_security: childlessness ~ gndr.c + age10.c + minority.c + international_security.z.gmc + international_security.z.gmc:minority.c + (international_security.z.gmc:minority.c | cntry)
## mod4.international_security: childlessness ~ gndr.c + age10.c + minority.c + international_security.z.gmc + international_security.z.gmc:minority.c + (international_security.z.gmc + minority.c + international_security.z.gmc:minority.c || cntry)
##                             npar   AIC   BIC logLik deviance Chisq Df
## mod5.international_security    9 15050 15122  -7516    15032         
## mod4.international_security   10 15052 15132  -7516    15032     0  1
##                             Pr(>Chisq)
## mod5.international_security           
## mod4.international_security          1
```

```r
mod6.international_security<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          international_security.z.gmc+
          international_security.z.gmc:minority.c+
          (international_security.z.gmc:minority.c||cntry),
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
summary(mod6.international_security)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## childlessness ~ gndr.c + age10.c + minority.c + international_security.z.gmc +  
##     international_security.z.gmc:minority.c + (international_security.z.gmc:minority.c ||  
##     cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15047.9  15111.7  -7516.0  15031.9    21326 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7373 -0.4392 -0.2238  0.2364 12.7442 
## 
## Random effects:
##  Groups  Name                                    Variance Std.Dev.
##  cntry   (Intercept)                             0.11767  0.3430  
##  cntry.1 international_security.z.gmc:minority.c 0.05136  0.2266  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                         Estimate Std. Error z value Pr(>|z|)
## (Intercept)                             -1.31142    0.10114 -12.967  < 2e-16
## gndr.c                                  -0.30200    0.04060  -7.439 1.01e-13
## age10.c                                 -0.55255    0.01350 -40.924  < 2e-16
## minority.c                              -0.31782    0.10622  -2.992  0.00277
## international_security.z.gmc             0.17537    0.08314   2.109  0.03493
## minority.c:international_security.z.gmc  0.25123    0.18177   1.382  0.16692
##                                            
## (Intercept)                             ***
## gndr.c                                  ***
## age10.c                                 ***
## minority.c                              ** 
## international_security.z.gmc            *  
## minority.c:international_security.z.gmc    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. int_..
## gndr.c       0.012                            
## age10.c      0.104  0.011                     
## minority.c   0.496 -0.001  0.090              
## intrntnl_.. -0.096  0.010  0.010 -0.174       
## mnrty.c:_.. -0.086  0.003 -0.013 -0.163  0.857
```

```r
anova(mod2.international_security,mod6.international_security)
```

```
## Data: exdat
## Models:
## mod2.international_security: childlessness ~ gndr.c + age10.c + minority.c + international_security.z.gmc + international_security.z.gmc:minority.c + (1 | cntry)
## mod6.international_security: childlessness ~ gndr.c + age10.c + minority.c + international_security.z.gmc + international_security.z.gmc:minority.c + (international_security.z.gmc:minority.c || cntry)
##                             npar   AIC   BIC  logLik deviance  Chisq Df
## mod2.international_security    7 15053 15109 -7519.4    15039          
## mod6.international_security    8 15048 15112 -7516.0    15032 6.8532  1
##                             Pr(>Chisq)   
## mod2.international_security              
## mod6.international_security   0.008848 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
anova(mod6.international_security,mod5.international_security)
```

```
## Data: exdat
## Models:
## mod6.international_security: childlessness ~ gndr.c + age10.c + minority.c + international_security.z.gmc + international_security.z.gmc:minority.c + (international_security.z.gmc:minority.c || cntry)
## mod5.international_security: childlessness ~ gndr.c + age10.c + minority.c + international_security.z.gmc + international_security.z.gmc:minority.c + (international_security.z.gmc:minority.c | cntry)
##                             npar   AIC   BIC logLik deviance  Chisq Df
## mod6.international_security    8 15048 15112  -7516    15032          
## mod5.international_security    9 15050 15122  -7516    15032 0.0114  1
##                             Pr(>Chisq)
## mod6.international_security           
## mod5.international_security     0.9148
```



## ethnic_minorities

### Fixed


```r
mod2.ethnic_minorities<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          ethnic_minorities.z.gmc+
          ethnic_minorities.z.gmc:minority.c+
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
##     ethnic_minorities.z.gmc:minority.c + (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15050.9  15106.6  -7518.4  15036.9    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7052 -0.4380 -0.2244  0.2349 12.8056 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1164   0.3411  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                    Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                        -1.34722    0.10360 -13.004  < 2e-16 ***
## gndr.c                             -0.30819    0.04049  -7.611 2.72e-14 ***
## age10.c                            -0.55390    0.01347 -41.113  < 2e-16 ***
## minority.c                         -0.39693    0.11634  -3.412 0.000645 ***
## ethnic_minorities.z.gmc            -0.22843    0.08416  -2.714 0.006644 ** 
## minority.c:ethnic_minorities.z.gmc -0.41251    0.16834  -2.450 0.014271 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. eth_..
## gndr.c       0.011                            
## age10.c      0.104  0.008                     
## minority.c   0.537 -0.003  0.085              
## ethnc_mnr..  0.247 -0.003  0.004  0.432       
## mnrty.c:_..  0.248 -0.010  0.018  0.434  0.949
```

### Random


```r
mod3.ethnic_minorities<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          ethnic_minorities.z.gmc+
          ethnic_minorities.z.gmc:minority.c+
          (ethnic_minorities.z.gmc+minority.c+ethnic_minorities.z.gmc:minority.c|cntry),
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
##     ethnic_minorities.z.gmc:minority.c + (ethnic_minorities.z.gmc +  
##     minority.c + ethnic_minorities.z.gmc:minority.c | cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15049.3  15176.8  -7508.7  15017.3    21318 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7046 -0.4396 -0.2259  0.2237 13.2817 
## 
## Random effects:
##  Groups Name                               Variance Std.Dev. Corr          
##  cntry  (Intercept)                        0.15742  0.3968                 
##         ethnic_minorities.z.gmc            0.12597  0.3549   0.87          
##         minority.c                         0.01696  0.1302   0.91 1.00     
##         ethnic_minorities.z.gmc:minority.c 0.65822  0.8113   0.85 1.00 0.99
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                    Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                        -1.35525    0.11833 -11.454  < 2e-16 ***
## gndr.c                             -0.30936    0.04054  -7.630 2.35e-14 ***
## age10.c                            -0.55459    0.01350 -41.069  < 2e-16 ***
## minority.c                         -0.43782    0.13571  -3.226  0.00125 ** 
## ethnic_minorities.z.gmc            -0.22632    0.14214  -1.592  0.11134    
## minority.c:ethnic_minorities.z.gmc -0.40252    0.30849  -1.305  0.19196    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. eth_..
## gndr.c       0.005                            
## age10.c      0.090  0.010                     
## minority.c   0.690 -0.012  0.074              
## ethnc_mnr..  0.649 -0.012  0.005  0.521       
## mnrty.c:_..  0.645 -0.014  0.012  0.506  0.980
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.ethnic_minorities,mod3.ethnic_minorities)
```

```
## Data: exdat
## Models:
## mod2.ethnic_minorities: childlessness ~ gndr.c + age10.c + minority.c + ethnic_minorities.z.gmc + ethnic_minorities.z.gmc:minority.c + (1 | cntry)
## mod3.ethnic_minorities: childlessness ~ gndr.c + age10.c + minority.c + ethnic_minorities.z.gmc + ethnic_minorities.z.gmc:minority.c + (ethnic_minorities.z.gmc + minority.c + ethnic_minorities.z.gmc:minority.c | cntry)
##                        npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)  
## mod2.ethnic_minorities    7 15051 15107 -7518.4    15037                       
## mod3.ethnic_minorities   16 15049 15177 -7508.7    15017 19.538  9    0.02099 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
mod4.ethnic_minorities<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          ethnic_minorities.z.gmc+
          ethnic_minorities.z.gmc:minority.c+
          (ethnic_minorities.z.gmc+minority.c+
             ethnic_minorities.z.gmc:minority.c||cntry),
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
summary(mod4.ethnic_minorities)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## childlessness ~ gndr.c + age10.c + minority.c + ethnic_minorities.z.gmc +  
##     ethnic_minorities.z.gmc:minority.c + (ethnic_minorities.z.gmc +  
##     minority.c + ethnic_minorities.z.gmc:minority.c || cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15055.6  15135.2  -7517.8  15035.6    21324 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6645 -0.4383 -0.2247  0.2358 13.0335 
## 
## Random effects:
##  Groups  Name                               Variance Std.Dev.
##  cntry   (Intercept)                        0.11616  0.3408  
##  cntry.1 ethnic_minorities.z.gmc            0.00000  0.0000  
##  cntry.2 minority.c                         0.00000  0.0000  
##  cntry.3 ethnic_minorities.z.gmc:minority.c 0.01203  0.1097  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                    Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                        -1.34551    0.10355 -12.994  < 2e-16 ***
## gndr.c                             -0.30745    0.04050  -7.591 3.19e-14 ***
## age10.c                            -0.55302    0.01349 -40.989  < 2e-16 ***
## minority.c                         -0.39211    0.11642  -3.368 0.000757 ***
## ethnic_minorities.z.gmc            -0.22918    0.08420  -2.722 0.006491 ** 
## minority.c:ethnic_minorities.z.gmc -0.41295    0.17327  -2.383 0.017159 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. eth_..
## gndr.c       0.011                            
## age10.c      0.104  0.009                     
## minority.c   0.537 -0.003  0.087              
## ethnc_mnr..  0.247 -0.003  0.004  0.431       
## mnrty.c:_..  0.241 -0.010  0.017  0.421  0.922
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.ethnic_minorities,mod4.ethnic_minorities)
```

```
## Data: exdat
## Models:
## mod2.ethnic_minorities: childlessness ~ gndr.c + age10.c + minority.c + ethnic_minorities.z.gmc + ethnic_minorities.z.gmc:minority.c + (1 | cntry)
## mod4.ethnic_minorities: childlessness ~ gndr.c + age10.c + minority.c + ethnic_minorities.z.gmc + ethnic_minorities.z.gmc:minority.c + (ethnic_minorities.z.gmc + minority.c + ethnic_minorities.z.gmc:minority.c || cntry)
##                        npar   AIC   BIC  logLik deviance Chisq Df Pr(>Chisq)
## mod2.ethnic_minorities    7 15051 15107 -7518.4    15037                    
## mod4.ethnic_minorities   10 15056 15135 -7517.8    15036 1.289  3     0.7318
```

```r
anova(mod4.ethnic_minorities,mod3.ethnic_minorities)
```

```
## Data: exdat
## Models:
## mod4.ethnic_minorities: childlessness ~ gndr.c + age10.c + minority.c + ethnic_minorities.z.gmc + ethnic_minorities.z.gmc:minority.c + (ethnic_minorities.z.gmc + minority.c + ethnic_minorities.z.gmc:minority.c || cntry)
## mod3.ethnic_minorities: childlessness ~ gndr.c + age10.c + minority.c + ethnic_minorities.z.gmc + ethnic_minorities.z.gmc:minority.c + (ethnic_minorities.z.gmc + minority.c + ethnic_minorities.z.gmc:minority.c | cntry)
##                        npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)
## mod4.ethnic_minorities   10 15056 15135 -7517.8    15036                     
## mod3.ethnic_minorities   16 15049 15177 -7508.7    15017 18.249  6    0.00564
##                          
## mod4.ethnic_minorities   
## mod3.ethnic_minorities **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
mod5.ethnic_minorities<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          ethnic_minorities.z.gmc+
          ethnic_minorities.z.gmc:minority.c+
          (ethnic_minorities.z.gmc:minority.c|cntry),
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
summary(mod5.ethnic_minorities)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## childlessness ~ gndr.c + age10.c + minority.c + ethnic_minorities.z.gmc +  
##     ethnic_minorities.z.gmc:minority.c + (ethnic_minorities.z.gmc:minority.c |  
##     cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15051.3  15123.0  -7516.6  15033.3    21325 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7242 -0.4388 -0.2250  0.2313 13.2690 
## 
## Random effects:
##  Groups Name                               Variance Std.Dev. Corr
##  cntry  (Intercept)                        0.113383 0.33672      
##         ethnic_minorities.z.gmc:minority.c 0.009856 0.09928  0.78
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                    Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                        -1.34258    0.10284 -13.055  < 2e-16 ***
## gndr.c                             -0.30755    0.04050  -7.593 3.12e-14 ***
## age10.c                            -0.55303    0.01349 -41.011  < 2e-16 ***
## minority.c                         -0.39279    0.11656  -3.370 0.000752 ***
## ethnic_minorities.z.gmc            -0.22683    0.08441  -2.687 0.007207 ** 
## minority.c:ethnic_minorities.z.gmc -0.41397    0.17164  -2.412 0.015874 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. eth_..
## gndr.c       0.011                            
## age10.c      0.105  0.009                     
## minority.c   0.541 -0.004  0.087              
## ethnc_mnr..  0.252 -0.004  0.004  0.436       
## mnrty.c:_..  0.340 -0.011  0.019  0.432  0.934
```

```r
anova(mod2.ethnic_minorities,mod5.ethnic_minorities)
```

```
## Data: exdat
## Models:
## mod2.ethnic_minorities: childlessness ~ gndr.c + age10.c + minority.c + ethnic_minorities.z.gmc + ethnic_minorities.z.gmc:minority.c + (1 | cntry)
## mod5.ethnic_minorities: childlessness ~ gndr.c + age10.c + minority.c + ethnic_minorities.z.gmc + ethnic_minorities.z.gmc:minority.c + (ethnic_minorities.z.gmc:minority.c | cntry)
##                        npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)
## mod2.ethnic_minorities    7 15051 15107 -7518.4    15037                     
## mod5.ethnic_minorities    9 15051 15123 -7516.6    15033 3.5784  2     0.1671
```

```r
anova(mod5.ethnic_minorities,mod4.ethnic_minorities)
```

```
## Data: exdat
## Models:
## mod5.ethnic_minorities: childlessness ~ gndr.c + age10.c + minority.c + ethnic_minorities.z.gmc + ethnic_minorities.z.gmc:minority.c + (ethnic_minorities.z.gmc:minority.c | cntry)
## mod4.ethnic_minorities: childlessness ~ gndr.c + age10.c + minority.c + ethnic_minorities.z.gmc + ethnic_minorities.z.gmc:minority.c + (ethnic_minorities.z.gmc + minority.c + ethnic_minorities.z.gmc:minority.c || cntry)
##                        npar   AIC   BIC  logLik deviance Chisq Df Pr(>Chisq)
## mod5.ethnic_minorities    9 15051 15123 -7516.6    15033                    
## mod4.ethnic_minorities   10 15056 15135 -7517.8    15036     0  1          1
```

```r
mod6.ethnic_minorities<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          ethnic_minorities.z.gmc+
          ethnic_minorities.z.gmc:minority.c+
          (ethnic_minorities.z.gmc:minority.c||cntry),
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
summary(mod6.ethnic_minorities)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## childlessness ~ gndr.c + age10.c + minority.c + ethnic_minorities.z.gmc +  
##     ethnic_minorities.z.gmc:minority.c + (ethnic_minorities.z.gmc:minority.c ||  
##     cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15051.6  15115.3  -7517.8  15035.6    21326 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6645 -0.4383 -0.2247  0.2358 13.0335 
## 
## Random effects:
##  Groups  Name                               Variance Std.Dev.
##  cntry   (Intercept)                        0.11616  0.3408  
##  cntry.1 ethnic_minorities.z.gmc:minority.c 0.01203  0.1097  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                    Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                        -1.34551    0.10354 -12.995  < 2e-16 ***
## gndr.c                             -0.30745    0.04050  -7.590 3.19e-14 ***
## age10.c                            -0.55302    0.01349 -40.989  < 2e-16 ***
## minority.c                         -0.39211    0.11641  -3.368 0.000756 ***
## ethnic_minorities.z.gmc            -0.22918    0.08419  -2.722 0.006489 ** 
## minority.c:ethnic_minorities.z.gmc -0.41295    0.17326  -2.383 0.017155 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. eth_..
## gndr.c       0.011                            
## age10.c      0.104  0.009                     
## minority.c   0.537 -0.003  0.087              
## ethnc_mnr..  0.247 -0.003  0.004  0.431       
## mnrty.c:_..  0.241 -0.010  0.017  0.421  0.922
```

```r
anova(mod2.ethnic_minorities,mod6.ethnic_minorities)
```

```
## Data: exdat
## Models:
## mod2.ethnic_minorities: childlessness ~ gndr.c + age10.c + minority.c + ethnic_minorities.z.gmc + ethnic_minorities.z.gmc:minority.c + (1 | cntry)
## mod6.ethnic_minorities: childlessness ~ gndr.c + age10.c + minority.c + ethnic_minorities.z.gmc + ethnic_minorities.z.gmc:minority.c + (ethnic_minorities.z.gmc:minority.c || cntry)
##                        npar   AIC   BIC  logLik deviance Chisq Df Pr(>Chisq)
## mod2.ethnic_minorities    7 15051 15107 -7518.4    15037                    
## mod6.ethnic_minorities    8 15052 15115 -7517.8    15036 1.289  1     0.2562
```

```r
anova(mod6.ethnic_minorities,mod5.ethnic_minorities)
```

```
## Data: exdat
## Models:
## mod6.ethnic_minorities: childlessness ~ gndr.c + age10.c + minority.c + ethnic_minorities.z.gmc + ethnic_minorities.z.gmc:minority.c + (ethnic_minorities.z.gmc:minority.c || cntry)
## mod5.ethnic_minorities: childlessness ~ gndr.c + age10.c + minority.c + ethnic_minorities.z.gmc + ethnic_minorities.z.gmc:minority.c + (ethnic_minorities.z.gmc:minority.c | cntry)
##                        npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)
## mod6.ethnic_minorities    8 15052 15115 -7517.8    15036                     
## mod5.ethnic_minorities    9 15051 15123 -7516.6    15033 2.2894  1     0.1303
```
### Marginal effects


```r
emtrends(mod5.ethnic_minorities,
         var="ethnic_minorities.z.gmc",
         specs="minority.c",
         at=list(minority.c=c(-0.5,0.5)),
         infer=c(T,T))
```

```
##  minority.c ethnic_minorities.z.gmc.trend     SE  df asymp.LCL asymp.UCL
##        -0.5                       -0.0198 0.0311 Inf   -0.0807     0.041
##         0.5                       -0.4338 0.1674 Inf   -0.7619    -0.106
##  z.ratio p.value
##   -0.639  0.5229
##   -2.592  0.0095
## 
## Results are averaged over the levels of: gndr.c 
## Confidence level used: 0.95
```

## nationalism

### Fixed


```r
mod2.nationalism<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          nationalism.z.gmc+
          nationalism.z.gmc:minority.c+
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
##     nationalism.z.gmc:minority.c + (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15054.9  15110.7  -7520.4  15040.9    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7314 -0.4389 -0.2244  0.2379 12.9514 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.117    0.342   
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                  -1.32962    0.10303 -12.905  < 2e-16 ***
## gndr.c                       -0.30758    0.04049  -7.596 3.05e-14 ***
## age10.c                      -0.55441    0.01347 -41.147  < 2e-16 ***
## minority.c                   -0.36453    0.11418  -3.193  0.00141 ** 
## nationalism.z.gmc            -0.13755    0.07187  -1.914  0.05565 .  
## minority.c:nationalism.z.gmc -0.28145    0.14374  -1.958  0.05022 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. ntnl..
## gndr.c       0.013                            
## age10.c      0.104  0.008                     
## minority.c   0.527 -0.001  0.087              
## ntnlsm.z.gm  0.221  0.001 -0.001  0.400       
## mnrty.c:n..  0.222 -0.009  0.018  0.403  0.935
```

### Random


```r
mod3.nationalism<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          nationalism.z.gmc+
          nationalism.z.gmc:minority.c+
          (nationalism.z.gmc+minority.c+nationalism.z.gmc:minority.c|cntry),
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
##     nationalism.z.gmc:minority.c + (nationalism.z.gmc + minority.c +  
##     nationalism.z.gmc:minority.c | cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15052.4  15179.9  -7510.2  15020.4    21318 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7427 -0.4395 -0.2251  0.2343 13.3119 
## 
## Random effects:
##  Groups Name                         Variance Std.Dev. Corr          
##  cntry  (Intercept)                  0.1714   0.4141                 
##         nationalism.z.gmc            0.1674   0.4092   0.89          
##         minority.c                   0.0265   0.1628   0.91 1.00     
##         nationalism.z.gmc:minority.c 0.7854   0.8862   0.88 1.00 1.00
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                  -1.35178    0.12164 -11.113  < 2e-16 ***
## gndr.c                       -0.31003    0.04054  -7.647 2.06e-14 ***
## age10.c                      -0.55590    0.01350 -41.172  < 2e-16 ***
## minority.c                   -0.41930    0.13743  -3.051  0.00228 ** 
## nationalism.z.gmc            -0.16387    0.14851  -1.103  0.26985    
## minority.c:nationalism.z.gmc -0.32445    0.31469  -1.031  0.30252    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. ntnl..
## gndr.c       0.008                            
## age10.c      0.089  0.009                     
## minority.c   0.711 -0.007  0.074              
## ntnlsm.z.gm  0.690 -0.004  0.005  0.543       
## mnrty.c:n..  0.687 -0.008  0.013  0.533  0.983
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.nationalism,mod3.nationalism)
```

```
## Data: exdat
## Models:
## mod2.nationalism: childlessness ~ gndr.c + age10.c + minority.c + nationalism.z.gmc + nationalism.z.gmc:minority.c + (1 | cntry)
## mod3.nationalism: childlessness ~ gndr.c + age10.c + minority.c + nationalism.z.gmc + nationalism.z.gmc:minority.c + (nationalism.z.gmc + minority.c + nationalism.z.gmc:minority.c | cntry)
##                  npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)  
## mod2.nationalism    7 15055 15111 -7520.4    15041                       
## mod3.nationalism   16 15052 15180 -7510.2    15020 20.473  9    0.01521 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


## antielite_salience

### Fixed


```r
mod2.antielite_salience<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          antielite_salience.z.gmc+
          antielite_salience.z.gmc:minority.c+
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
##     antielite_salience.z.gmc:minority.c + (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15057.3  15113.0  -7521.6  15043.3    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7234 -0.4390 -0.2242  0.2380 12.9684 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1169   0.3418  
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                     Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                         -1.28685    0.10026 -12.836  < 2e-16 ***
## gndr.c                              -0.30933    0.04051  -7.636 2.23e-14 ***
## age10.c                             -0.55420    0.01349 -41.068  < 2e-16 ***
## minority.c                          -0.27765    0.10385  -2.674  0.00751 ** 
## antielite_salience.z.gmc            -0.08655    0.07356  -1.177  0.23936    
## minority.c:antielite_salience.z.gmc -0.18541    0.14702  -1.261  0.20728    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. ant_..
## gndr.c      0.014                             
## age10.c     0.105  0.012                      
## minority.c  0.487  0.001  0.093               
## antlt_sln.. 0.006  0.036  0.029  0.010        
## mnrty.c:_.. 0.007  0.026  0.004  0.007  0.940
```

### Random


```r
mod3.antielite_salience<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          antielite_salience.z.gmc+
          antielite_salience.z.gmc:minority.c+
          (antielite_salience.z.gmc+minority.c+antielite_salience.z.gmc:minority.c|cntry),
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
##     antielite_salience.z.gmc:minority.c + (antielite_salience.z.gmc +  
##     minority.c + antielite_salience.z.gmc:minority.c | cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15062.9  15190.4  -7515.5  15030.9    21318 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.8549 -0.4391 -0.2242  0.2396 12.6946 
## 
## Random effects:
##  Groups Name                                Variance Std.Dev. Corr             
##  cntry  (Intercept)                         0.08714  0.2952                    
##         antielite_salience.z.gmc            0.01440  0.1200    1.00            
##         minority.c                          0.02060  0.1435   -0.63 -0.63      
##         antielite_salience.z.gmc:minority.c 0.06800  0.2608    0.78  0.78 -0.98
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                     Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                         -1.25277    0.10199 -12.284  < 2e-16 ***
## gndr.c                              -0.30809    0.04056  -7.595 3.08e-14 ***
## age10.c                             -0.55424    0.01352 -40.981  < 2e-16 ***
## minority.c                          -0.20336    0.14801  -1.374    0.169    
## antielite_salience.z.gmc            -0.08018    0.09764  -0.821    0.412    
## minority.c:antielite_salience.z.gmc -0.19390    0.19976  -0.971    0.332    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. ant_..
## gndr.c       0.014                            
## age10.c      0.096  0.013                     
## minority.c   0.535  0.000  0.053              
## antlt_sln..  0.400  0.024  0.013  0.217       
## mnrty.c:_..  0.344  0.016 -0.008  0.154  0.927
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.antielite_salience,mod3.antielite_salience)
```

```
## Data: exdat
## Models:
## mod2.antielite_salience: childlessness ~ gndr.c + age10.c + minority.c + antielite_salience.z.gmc + antielite_salience.z.gmc:minority.c + (1 | cntry)
## mod3.antielite_salience: childlessness ~ gndr.c + age10.c + minority.c + antielite_salience.z.gmc + antielite_salience.z.gmc:minority.c + (antielite_salience.z.gmc + minority.c + antielite_salience.z.gmc:minority.c | cntry)
##                         npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)
## mod2.antielite_salience    7 15057 15113 -7521.6    15043                     
## mod3.antielite_salience   16 15063 15190 -7515.5    15031 12.356  9      0.194
```

## corrupt_salience

### Fixed


```r
mod2.corrupt_salience<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          corrupt_salience.z.gmc+
          corrupt_salience.z.gmc:minority.c+
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
##     corrupt_salience.z.gmc:minority.c + (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15055.3  15111.1  -7520.6  15041.3    21327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7153 -0.4386 -0.2241  0.2346 12.9859 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1163   0.341   
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                   Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                       -1.29312    0.10026 -12.898  < 2e-16 ***
## gndr.c                            -0.31045    0.04051  -7.664  1.8e-14 ***
## age10.c                           -0.55353    0.01351 -40.971  < 2e-16 ***
## minority.c                        -0.28675    0.10441  -2.747  0.00602 ** 
## corrupt_salience.z.gmc            -0.18798    0.11646  -1.614  0.10651    
## minority.c:corrupt_salience.z.gmc -0.42346    0.23290  -1.818  0.06903 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. crr_..
## gndr.c       0.015                            
## age10.c      0.105  0.011                     
## minority.c   0.490  0.002  0.093              
## crrpt_sln..  0.050  0.039  0.022  0.081       
## mnrty.c:_..  0.050  0.035 -0.005  0.077  0.953
```

### Random


```r
mod3.corrupt_salience<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          corrupt_salience.z.gmc+
          corrupt_salience.z.gmc:minority.c+
          (corrupt_salience.z.gmc+minority.c+corrupt_salience.z.gmc:minority.c|cntry),
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
##     corrupt_salience.z.gmc:minority.c + (corrupt_salience.z.gmc +  
##     minority.c + corrupt_salience.z.gmc:minority.c | cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15064.8  15192.3  -7516.4  15032.8    21318 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.8321 -0.4386 -0.2248  0.2319 12.6655 
## 
## Random effects:
##  Groups Name                              Variance Std.Dev. Corr             
##  cntry  (Intercept)                       0.083431 0.28884                   
##         corrupt_salience.z.gmc            0.006189 0.07867   1.00            
##         minority.c                        0.024161 0.15544  -0.65 -0.65      
##         corrupt_salience.z.gmc:minority.c 0.065575 0.25608   0.42  0.42 -0.96
## Number of obs: 21334, groups:  cntry, 20
## 
## Fixed effects:
##                                   Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                       -1.24744    0.09878 -12.628  < 2e-16 ***
## gndr.c                            -0.31003    0.04056  -7.644  2.1e-14 ***
## age10.c                           -0.55404    0.01355 -40.873  < 2e-16 ***
## minority.c                        -0.19616    0.14125  -1.389   0.1649    
## corrupt_salience.z.gmc            -0.19882    0.11946  -1.664   0.0961 .  
## minority.c:corrupt_salience.z.gmc -0.40458    0.24762  -1.634   0.1023    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. crr_..
## gndr.c       0.013                            
## age10.c      0.102  0.009                     
## minority.c   0.497 -0.001  0.061              
## crrpt_sln..  0.228  0.033  0.023  0.118       
## mnrty.c:_..  0.164  0.031 -0.007  0.039  0.898
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.corrupt_salience,mod3.corrupt_salience)
```

```
## Data: exdat
## Models:
## mod2.corrupt_salience: childlessness ~ gndr.c + age10.c + minority.c + corrupt_salience.z.gmc + corrupt_salience.z.gmc:minority.c + (1 | cntry)
## mod3.corrupt_salience: childlessness ~ gndr.c + age10.c + minority.c + corrupt_salience.z.gmc + corrupt_salience.z.gmc:minority.c + (corrupt_salience.z.gmc + minority.c + corrupt_salience.z.gmc:minority.c | cntry)
##                       npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)
## mod2.corrupt_salience    7 15055 15111 -7520.6    15041                     
## mod3.corrupt_salience   16 15065 15192 -7516.4    15033 8.5264  9     0.4821
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


