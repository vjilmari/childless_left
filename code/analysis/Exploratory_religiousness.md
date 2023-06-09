---
title: "Exploratory analysis: Religiousness"
output: 
  html_document: 
    toc: yes
    keep_md: yes

---




# Preparations

## Packages


```r
library(lme4)
library(emmeans)
library(rio)
library(dplyr)
library(vjihelpers)
library(ggplot2)
library(MetBrewer)
library(tibble)
library(Hmisc)
```

## Custom functions


```r
source("../custom_functions.R")
```

## Data


```r
fdat<-import("../../data/processed/fdat.xlsx")
```

## Data exclusions


```r
exdat<-fdat %>%
  dplyr::select(childlessness,
                gndr.f,agea,minority,
                gndr.c,age10.c,minority.c,
                lrgen,
                lrecon,galtan,
                lrecon_salience,galtan_salience,
                rlgdgr,rlgdgr.z,
                cntry,
                anweight) %>%
  mutate(rlgdgr.z=as.numeric(rlgdgr.z)) %>%
  na.omit()
```

## Variable centering


```r
exdat<-
  group_mean_center(
    data=exdat,group.var="cntry",
    vars=c("rlgdgr","rlgdgr.z",
           "lrgen","lrecon","galtan",
           "lrecon_salience","galtan_salience"),
    grand.init = F)
```


## Scaling CHES variables


```r
CHES<-
  import("../../data/raw/2014_CHES_dataset_means.csv")

sd.lrgen<-sd(CHES$lrgen,na.rm=T)
sd.lrecon<-sd(CHES$lrecon,na.rm=T)
sd.galtan<-sd(CHES$galtan,na.rm=T)
sd.galtan_salience<-sd(CHES$galtan_salience,na.rm=T)
sd.lrecon_salience<-sd(CHES$lrecon_salience,na.rm=T)

exdat$lrgen.z.gmc<-exdat$lrgen.gmc/sd.lrgen
exdat$lrecon.z.gmc<-exdat$lrecon.gmc/sd.lrecon
exdat$galtan.z.gmc<-exdat$galtan.gmc/sd.galtan
exdat$galtan_salience.z.gmc<-
  exdat$galtan_salience.gmc/sd.galtan_salience
exdat$lrecon_salience.z.gmc<-
  exdat$lrecon_salience.gmc/sd.lrecon_salience
```


# Analysis 

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
##    17182    17198    -8589    17178    21300 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -1.9047 -0.4931 -0.3114  0.3658  7.8135 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1032   0.3213  
## Number of obs: 21302, groups:  cntry, 20
## 
## Fixed effects:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.06987    0.08187  -13.07   <2e-16 ***
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
##  15046.3  15086.1  -7518.1  15036.3    21297 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7210 -0.4395 -0.2249  0.2431 12.9306 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1148   0.3388  
## Number of obs: 21302, groups:  cntry, 20
## 
## Fixed effects:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.28286    0.09971 -12.865  < 2e-16 ***
## gndr.c      -0.30450    0.04048  -7.523 5.37e-14 ***
## age10.c     -0.55387    0.01346 -41.156  < 2e-16 ***
## minority.c  -0.27629    0.10390  -2.659  0.00783 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##            (Intr) gndr.c ag10.c
## gndr.c     0.015               
## age10.c    0.107  0.009        
## minority.c 0.490  0.002  0.093
```

## Fixed main effect


```r
mod2.rlgdgr<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          rlgdgr.z.gmc+
          (1|cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight)
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```r
summary(mod2.rlgdgr)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + rlgdgr.z.gmc +  
##     (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15038.6  15086.4  -7513.3  15026.6    21296 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7482 -0.4394 -0.2243  0.2451 13.5309 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1154   0.3397  
## Number of obs: 21302, groups:  cntry, 20
## 
## Fixed effects:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  -1.26722    0.09998 -12.675  < 2e-16 ***
## gndr.c       -0.28752    0.04085  -7.039 1.94e-12 ***
## age10.c      -0.54761    0.01360 -40.271  < 2e-16 ***
## minority.c   -0.23373    0.10477  -2.231  0.02569 *  
## rlgdgr.z.gmc -0.06753    0.02165  -3.119  0.00182 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.021                     
## age10.c      0.112  0.028              
## minority.c   0.491  0.018  0.109       
## rlgdgr.z.gm -0.047 -0.132 -0.139 -0.129
```

```r
export(rownames_to_column(
  getFE_glmer(mod2.rlgdgr,round = 10,p.round=10)),
       "../../results/estimates/FE_mod2.rlgdgr.xlsx",
       overwrite=T)

export(getVC(mod2.rlgdgr,round = 10),
       "../../results/estimates/VC_mod2.rlgdgr.xlsx",
       overwrite=T)
```

## Random


```r
mod3.rlgdgr<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          rlgdgr.z.gmc+
          (rlgdgr.z.gmc|cntry),
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
summary(mod3.rlgdgr)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + rlgdgr.z.gmc +  
##     (rlgdgr.z.gmc | cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15033.2  15096.9  -7508.6  15017.2    21294 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7677 -0.4391 -0.2243  0.2436 12.8970 
## 
## Random effects:
##  Groups Name         Variance Std.Dev. Corr 
##  cntry  (Intercept)  0.112753 0.33579       
##         rlgdgr.z.gmc 0.005507 0.07421  -0.67
## Number of obs: 21302, groups:  cntry, 20
## 
## Fixed effects:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  -1.26273    0.09913 -12.738  < 2e-16 ***
## gndr.c       -0.28688    0.04089  -7.016 2.28e-12 ***
## age10.c      -0.54585    0.01362 -40.067  < 2e-16 ***
## minority.c   -0.22904    0.10523  -2.177   0.0295 *  
## rlgdgr.z.gmc -0.07593    0.03335  -2.277   0.0228 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.020                     
## age10.c      0.112  0.030              
## minority.c   0.496  0.018  0.109       
## rlgdgr.z.gm -0.324 -0.092 -0.109 -0.095
```

```r
anova(mod2.rlgdgr,mod3.rlgdgr)
```

```
## Data: exdat
## Models:
## mod2.rlgdgr: childlessness ~ gndr.c + age10.c + minority.c + rlgdgr.z.gmc + (1 | cntry)
## mod3.rlgdgr: childlessness ~ gndr.c + age10.c + minority.c + rlgdgr.z.gmc + (rlgdgr.z.gmc | cntry)
##             npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)   
## mod2.rlgdgr    6 15039 15086 -7513.3    15027                        
## mod3.rlgdgr    8 15033 15097 -7508.6    15017 9.3673  2   0.009245 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
export(rownames_to_column(
  getFE_glmer(mod3.rlgdgr,round = 10,p.round=10)),
       "../../results/estimates/FE_mod3.rlgdgr.xlsx",
       overwrite=T)

export(getVC(mod3.rlgdgr,round = 10),
       "../../results/estimates/VC_mod3.rlgdgr.xlsx",
       overwrite=T)
```


## Random without random effect correlation


```r
mod4.rlgdgr<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          rlgdgr.z.gmc+
          (rlgdgr.z.gmc||cntry),
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
summary(mod4.rlgdgr)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + rlgdgr.z.gmc +  
##     (rlgdgr.z.gmc || cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15034.5  15090.3  -7510.3  15020.5    21295 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7195 -0.4382 -0.2237  0.2451 12.9304 
## 
## Random effects:
##  Groups  Name         Variance Std.Dev.
##  cntry   (Intercept)  0.113427 0.33679 
##  cntry.1 rlgdgr.z.gmc 0.004977 0.07055 
## Number of obs: 21302, groups:  cntry, 20
## 
## Fixed effects:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  -1.26634    0.09965 -12.708  < 2e-16 ***
## gndr.c       -0.28621    0.04088  -7.001 2.54e-12 ***
## age10.c      -0.54596    0.01362 -40.081  < 2e-16 ***
## minority.c   -0.22818    0.10554  -2.162   0.0306 *  
## rlgdgr.z.gmc -0.08157    0.03400  -2.399   0.0164 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.020                     
## age10.c      0.111  0.030              
## minority.c   0.496  0.017  0.107       
## rlgdgr.z.gm -0.016 -0.089 -0.103 -0.085
```

```r
anova(mod2.rlgdgr,mod4.rlgdgr)
```

```
## Data: exdat
## Models:
## mod2.rlgdgr: childlessness ~ gndr.c + age10.c + minority.c + rlgdgr.z.gmc + (1 | cntry)
## mod4.rlgdgr: childlessness ~ gndr.c + age10.c + minority.c + rlgdgr.z.gmc + (rlgdgr.z.gmc || cntry)
##             npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)  
## mod2.rlgdgr    6 15039 15086 -7513.3    15027                       
## mod4.rlgdgr    7 15034 15090 -7510.3    15020 6.0064  1    0.01425 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
anova(mod4.rlgdgr,mod3.rlgdgr)
```

```
## Data: exdat
## Models:
## mod4.rlgdgr: childlessness ~ gndr.c + age10.c + minority.c + rlgdgr.z.gmc + (rlgdgr.z.gmc || cntry)
## mod3.rlgdgr: childlessness ~ gndr.c + age10.c + minority.c + rlgdgr.z.gmc + (rlgdgr.z.gmc | cntry)
##             npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)  
## mod4.rlgdgr    7 15034 15090 -7510.3    15020                       
## mod3.rlgdgr    8 15033 15097 -7508.6    15017 3.3609  1    0.06676 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
export(rownames_to_column(
  getFE_glmer(mod4.rlgdgr,round = 10,p.round=10)),
       "../../results/estimates/FE_mod4.rlgdgr.xlsx",
       overwrite=T)

export(getVC(mod4.rlgdgr,round = 10),
       "../../results/estimates/VC_mod4.rlgdgr.xlsx",
       overwrite=T)
```


## Random interaction with galtan_salience while galtan PO is in the model with the same interaction


```r
mod13.rlgdgr<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          rlgdgr.z.gmc+
          galtan_salience.z.gmc+
          galtan_salience.z.gmc:rlgdgr.z.gmc+
          galtan.z.gmc+
          galtan_salience.z.gmc:galtan.z.gmc+
          (rlgdgr.z.gmc+galtan_salience.z.gmc+
          galtan_salience.z.gmc:rlgdgr.z.gmc||cntry),
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
summary(mod13.rlgdgr)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + rlgdgr.z.gmc +  
##     galtan_salience.z.gmc + galtan_salience.z.gmc:rlgdgr.z.gmc +  
##     galtan.z.gmc + galtan_salience.z.gmc:galtan.z.gmc + (rlgdgr.z.gmc +  
##     galtan_salience.z.gmc + galtan_salience.z.gmc:rlgdgr.z.gmc ||      cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15029.8  15133.4  -7501.9  15003.8    21289 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.3218 -0.4378 -0.2234  0.2424 12.9249 
## 
## Random effects:
##  Groups  Name                               Variance Std.Dev.
##  cntry   (Intercept)                        0.105741 0.32518 
##  cntry.1 rlgdgr.z.gmc                       0.005109 0.07148 
##  cntry.2 galtan_salience.z.gmc              0.002164 0.04652 
##  cntry.3 rlgdgr.z.gmc:galtan_salience.z.gmc 0.007397 0.08600 
## Number of obs: 21302, groups:  cntry, 20
## 
## Fixed effects:
##                                     Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                        -1.249451   0.097865 -12.767  < 2e-16 ***
## gndr.c                             -0.283368   0.041065  -6.901 5.18e-12 ***
## age10.c                            -0.542526   0.013690 -39.631  < 2e-16 ***
## minority.c                         -0.208179   0.106562  -1.954  0.05075 .  
## rlgdgr.z.gmc                       -0.081626   0.034521  -2.365  0.01805 *  
## galtan_salience.z.gmc               0.040226   0.039234   1.025  0.30523    
## galtan.z.gmc                        0.031616   0.030416   1.039  0.29859    
## rlgdgr.z.gmc:galtan_salience.z.gmc -0.008149   0.047415  -0.172  0.86354    
## galtan_salience.z.gmc:galtan.z.gmc -0.121587   0.039489  -3.079  0.00208 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty. rlgd.. glt_.. gltn.. r..:_.
## gndr.c       0.022                                                 
## age10.c      0.114  0.025                                          
## minority.c   0.509  0.021  0.108                                   
## rlgdgr.z.gm -0.022 -0.096 -0.100 -0.094                            
## gltn_slnc..  0.007  0.008  0.054  0.034 -0.047                     
## galtn.z.gmc  0.049  0.035 -0.007  0.066 -0.122 -0.106              
## rlgdg..:_..  0.004  0.001  0.001  0.014 -0.040  0.112  0.000       
## gltn_s..:.. -0.033 -0.021 -0.021 -0.019  0.014 -0.013 -0.476 -0.109
```

```r
export(rownames_to_column(
  getFE_glmer(mod13.rlgdgr,round = 10,p.round=10)),
       "../../results/estimates/FE_mod13.rlgdgr.xlsx",
       overwrite=T)

export(getVC(mod13.rlgdgr,round = 10),
       "../../results/estimates/VC_mod13.rlgdgr.xlsx",
       overwrite=T)
```

### Marginal effects


```r
mod13.trends<-
  emtrends(mod13.rlgdgr,var="galtan.z.gmc",specs="galtan_salience.z.gmc",
         at=list(galtan_salience.z.gmc=c(-1,0,1)),infer=c(T,T),adjust="none")
mod13.trends
```

```
##  galtan_salience.z.gmc galtan.z.gmc.trend     SE  df asymp.LCL asymp.UCL
##                     -1             0.1532 0.0602 Inf    0.0351    0.2713
##                      0             0.0316 0.0304 Inf   -0.0280    0.0912
##                      1            -0.0900 0.0366 Inf   -0.1617   -0.0182
##  z.ratio p.value
##    2.543  0.0110
##    1.039  0.2986
##   -2.457  0.0140
## 
## Results are averaged over the levels of: gndr.c, minority.c 
## Confidence level used: 0.95
```

```r
round(exp(c(-0.0900,-0.1617,-0.018)),2)
```

```
## [1] 0.91 0.85 0.98
```

# Session Information


```r
sinf<-sessionInfo()
print(sinf,locale=F)
```

```
## R version 4.3.0 (2023-04-21 ucrt)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 10 x64 (build 19045)
## 
## Matrix products: default
## 
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] Hmisc_5.1-0           tibble_3.2.1          MetBrewer_0.2.0      
##  [4] ggplot2_3.4.2         vjihelpers_0.0.0.9000 dplyr_1.1.2          
##  [7] rio_0.5.29            emmeans_1.8.6         lme4_1.1-33          
## [10] Matrix_1.5-4          knitr_1.42            rmarkdown_2.21       
## 
## loaded via a namespace (and not attached):
##  [1] gtable_0.3.3       xfun_0.39          bslib_0.4.2        htmlwidgets_1.6.2 
##  [5] lattice_0.21-8     vctrs_0.6.2        tools_4.3.0        generics_0.1.3    
##  [9] curl_5.0.0         fansi_1.0.4        cluster_2.1.4      pkgconfig_2.0.3   
## [13] data.table_1.14.8  checkmate_2.2.0    readxl_1.4.2       lifecycle_1.0.3   
## [17] stringr_1.5.0      compiler_4.3.0     munsell_0.5.0      htmltools_0.5.5   
## [21] sass_0.4.6         yaml_2.3.7         htmlTable_2.4.1    Formula_1.2-5     
## [25] pillar_1.9.0       nloptr_2.0.3       jquerylib_0.1.4    MASS_7.3-58.4     
## [29] cachem_1.0.8       rpart_4.1.19       boot_1.3-28.1      nlme_3.1-162      
## [33] tidyselect_1.2.0   zip_2.3.0          digest_0.6.31      mvtnorm_1.1-3     
## [37] stringi_1.7.12     forcats_1.0.0      splines_4.3.0      fastmap_1.1.1     
## [41] grid_4.3.0         colorspace_2.1-0   cli_3.6.1          magrittr_2.0.3    
## [45] base64enc_0.1-3    utf8_1.2.3         foreign_0.8-84     withr_2.5.0       
## [49] backports_1.4.1    scales_1.2.1       estimability_1.4.1 nnet_7.3-18       
## [53] gridExtra_2.3      cellranger_1.1.0   hms_1.1.3          openxlsx_4.2.5.2  
## [57] evaluate_0.21      haven_2.5.2        rlang_1.1.1        Rcpp_1.0.10       
## [61] xtable_1.8-4       glue_1.6.2         rstudioapi_0.14    minqa_1.2.5       
## [65] jsonlite_1.8.4     R6_2.5.1
```
