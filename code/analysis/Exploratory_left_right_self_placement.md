---
title: "Exploratory analysis: Left-right self-placement"
output: 
  html_document: 
    toc: yes
    keep_md: yes

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
## The following rio suggested packages are not installed: 'arrow', 'feather', 'fst', 'hexView', 'pzfx', 'readODS', 'rmatio'
## Use 'install_formats()' to install them
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
library(ggplot2)
library(MetBrewer)
library(tibble)
library(Hmisc)
```

```
## 
## Attaching package: 'Hmisc'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     src, summarize
```

```
## The following objects are masked from 'package:base':
## 
##     format.pval, units
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
                lrscale,lrscale.z,
                cntry,
                anweight) %>%
  mutate(lrscale.z=as.numeric(lrscale.z)) %>%
  na.omit()
```

## Variable centering


```r
exdat<-
  group_mean_center(
    data=exdat,group.var="cntry",
    vars=c("lrscale","lrscale.z",
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
##  16649.8  16665.7  -8322.9  16645.8    20607 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -1.9031 -0.4921 -0.3140  0.3816  7.7858 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1002   0.3165  
## Number of obs: 20609, groups:  cntry, 20
## 
## Fixed effects:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.06580    0.08118  -13.13   <2e-16 ***
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
##  14532.6  14572.2  -7261.3  14522.6    20604 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7447 -0.4394 -0.2252  0.2581 13.0833 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1104   0.3322  
## Number of obs: 20609, groups:  cntry, 20
## 
## Fixed effects:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.24687    0.09949 -12.532   <2e-16 ***
## gndr.c      -0.28759    0.04121  -6.978    3e-12 ***
## age10.c     -0.56310    0.01375 -40.959   <2e-16 ***
## minority.c  -0.19816    0.10683  -1.855   0.0636 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##            (Intr) gndr.c ag10.c
## gndr.c     0.017               
## age10.c    0.110  0.012        
## minority.c 0.505  0.003  0.093
```

## Fixed main effect


```r
mod2.lrscale<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          lrscale.z.gmc+
          (1|cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight)
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```r
summary(mod2.lrscale)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + lrscale.z.gmc +  
##     (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  14534.6  14582.2  -7261.3  14522.6    20603 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7445 -0.4393 -0.2253  0.2581 13.0715 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1103   0.3322  
## Number of obs: 20609, groups:  cntry, 20
## 
## Fixed effects:
##                Estimate Std. Error z value Pr(>|z|)    
## (Intercept)   -1.247057   0.099519 -12.531  < 2e-16 ***
## gndr.c        -0.287783   0.041290  -6.970 3.17e-12 ***
## age10.c       -0.563035   0.013777 -40.866  < 2e-16 ***
## minority.c    -0.198589   0.106981  -1.856   0.0634 .  
## lrscale.z.gmc -0.001664   0.021807  -0.076   0.9392    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.018                     
## age10.c      0.109  0.008              
## minority.c   0.505  0.006  0.090       
## lrscl.z.gmc  0.024  0.061 -0.065  0.052
```

```r
export(rownames_to_column(
  getFE_glmer(mod2.lrscale,round = 10,p.round=10)),
       "../../results/estimates/FE_mod2.lrscale.xlsx",
       overwrite=T)

export(getVC(mod2.lrscale,round = 10),
       "../../results/estimates/VC_mod2.lrscale.xlsx",
       overwrite=T)
```

## Random


```r
mod3.lrscale<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          lrscale.z.gmc+
          (lrscale.z.gmc|cntry),
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
summary(mod3.lrscale)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + lrscale.z.gmc +  
##     (lrscale.z.gmc | cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  14524.2  14587.7  -7254.1  14508.2    20601 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7434 -0.4387 -0.2246  0.2568 14.4468 
## 
## Random effects:
##  Groups Name          Variance Std.Dev. Corr 
##  cntry  (Intercept)   0.107567 0.32797       
##         lrscale.z.gmc 0.006797 0.08245  -0.53
## Number of obs: 20609, groups:  cntry, 20
## 
## Fixed effects:
##                Estimate Std. Error z value Pr(>|z|)    
## (Intercept)   -1.256674   0.098861 -12.712  < 2e-16 ***
## gndr.c        -0.287379   0.041350  -6.950 3.66e-12 ***
## age10.c       -0.561640   0.013795 -40.713  < 2e-16 ***
## minority.c    -0.217162   0.107493  -2.020   0.0434 *  
## lrscale.z.gmc -0.004619   0.033971  -0.136   0.8918    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.020                     
## age10.c      0.108  0.008              
## minority.c   0.512  0.009  0.089       
## lrscl.z.gmc -0.236  0.047 -0.027  0.037
```

```r
anova(mod2.lrscale,mod3.lrscale)
```

```
## Data: exdat
## Models:
## mod2.lrscale: childlessness ~ gndr.c + age10.c + minority.c + lrscale.z.gmc + (1 | cntry)
## mod3.lrscale: childlessness ~ gndr.c + age10.c + minority.c + lrscale.z.gmc + (lrscale.z.gmc | cntry)
##              npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)    
## mod2.lrscale    6 14535 14582 -7261.3    14523                         
## mod3.lrscale    8 14524 14588 -7254.1    14508 14.353  2  0.0007644 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
export(rownames_to_column(
  getFE_glmer(mod3.lrscale,round = 10,p.round=10)),
       "../../results/estimates/FE_mod3.lrscale.xlsx",
       overwrite=T)

export(getVC(mod3.lrscale,round = 10),
       "../../results/estimates/VC_mod3.lrscale.xlsx",
       overwrite=T)
```


## Random without random effect correlation


```r
mod4.lrscale<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          lrscale.z.gmc+
          (lrscale.z.gmc||cntry),
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
summary(mod4.lrscale)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + lrscale.z.gmc +  
##     (lrscale.z.gmc || cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  14524.1  14579.6  -7255.0  14510.1    20602 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7386 -0.4389 -0.2243  0.2585 14.2754 
## 
## Random effects:
##  Groups  Name          Variance Std.Dev.
##  cntry   (Intercept)   0.110597 0.3326  
##  cntry.1 lrscale.z.gmc 0.007328 0.0856  
## Number of obs: 20609, groups:  cntry, 20
## 
## Fixed effects:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)   -1.25906    0.09975 -12.622  < 2e-16 ***
## gndr.c        -0.28756    0.04135  -6.954 3.56e-12 ***
## age10.c       -0.56217    0.01380 -40.733  < 2e-16 ***
## minority.c    -0.21840    0.10747  -2.032   0.0421 *  
## lrscale.z.gmc -0.00438    0.03550  -0.123   0.9018    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.020                     
## age10.c      0.108  0.008              
## minority.c   0.507  0.009  0.089       
## lrscl.z.gmc  0.018  0.046 -0.022  0.036
```

```r
anova(mod2.lrscale,mod4.lrscale)
```

```
## Data: exdat
## Models:
## mod2.lrscale: childlessness ~ gndr.c + age10.c + minority.c + lrscale.z.gmc + (1 | cntry)
## mod4.lrscale: childlessness ~ gndr.c + age10.c + minority.c + lrscale.z.gmc + (lrscale.z.gmc || cntry)
##              npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)    
## mod2.lrscale    6 14535 14582 -7261.3    14523                         
## mod4.lrscale    7 14524 14580 -7255.0    14510 12.502  1  0.0004066 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
anova(mod4.lrscale,mod3.lrscale)
```

```
## Data: exdat
## Models:
## mod4.lrscale: childlessness ~ gndr.c + age10.c + minority.c + lrscale.z.gmc + (lrscale.z.gmc || cntry)
## mod3.lrscale: childlessness ~ gndr.c + age10.c + minority.c + lrscale.z.gmc + (lrscale.z.gmc | cntry)
##              npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)
## mod4.lrscale    7 14524 14580 -7255.0    14510                     
## mod3.lrscale    8 14524 14588 -7254.1    14508 1.8513  1     0.1736
```

```r
export(rownames_to_column(
  getFE_glmer(mod4.lrscale,round = 10,p.round=10)),
       "../../results/estimates/FE_mod4.lrscale.xlsx",
       overwrite=T)

export(getVC(mod4.lrscale,round = 10),
       "../../results/estimates/VC_mod4.lrscale.xlsx",
       overwrite=T)
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
