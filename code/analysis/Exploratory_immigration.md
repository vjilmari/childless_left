---
title: "Exploratory analysis: Immigration"
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
                imwbcnt,imwbcnt.z,
                cntry,
                anweight) %>%
  mutate(imwbcnt.z=as.numeric(imwbcnt.z)) %>%
  na.omit()
```

## Variable centering


```r
exdat<-
  group_mean_center(
    data=exdat,group.var="cntry",
    vars=c("imwbcnt","imwbcnt.z",
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
##  16940.3  16956.2  -8468.1  16936.3    20887 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -1.9130 -0.4942 -0.3119  0.3690  7.7840 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1026   0.3203  
## Number of obs: 20889, groups:  cntry, 20
## 
## Fixed effects:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.06167    0.08182  -12.98   <2e-16 ***
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
##  14800.6  14840.3  -7395.3  14790.6    20884 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7726 -0.4397 -0.2244  0.2639 13.1505 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1138   0.3374  
## Number of obs: 20889, groups:  cntry, 20
## 
## Fixed effects:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.27384    0.09977 -12.768  < 2e-16 ***
## gndr.c      -0.32568    0.04083  -7.977  1.5e-15 ***
## age10.c     -0.56055    0.01365 -41.057  < 2e-16 ***
## minority.c  -0.25169    0.10446  -2.409    0.016 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##            (Intr) gndr.c ag10.c
## gndr.c     0.017               
## age10.c    0.109  0.013        
## minority.c 0.493  0.002  0.091
```

## Fixed main effect


```r
mod2.imwbcnt<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          imwbcnt.z.gmc+
          (1|cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight)
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```r
summary(mod2.imwbcnt)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + imwbcnt.z.gmc +  
##     (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  14790.0  14837.7  -7389.0  14778.0    20883 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7472 -0.4382 -0.2242  0.2618 13.0197 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1134   0.3368  
## Number of obs: 20889, groups:  cntry, 20
## 
## Fixed effects:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)   -1.28396    0.09975 -12.872  < 2e-16 ***
## gndr.c        -0.31763    0.04090  -7.765 8.13e-15 ***
## age10.c       -0.55627    0.01370 -40.592  < 2e-16 ***
## minority.c    -0.27037    0.10470  -2.582  0.00981 ** 
## imwbcnt.z.gmc  0.07441    0.02102   3.540  0.00040 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.015                     
## age10.c      0.106  0.017              
## minority.c   0.494  0.000  0.087       
## imwbcnt.z.g -0.032  0.053  0.077 -0.051
```

```r
export(rownames_to_column(
  getFE_glmer(mod2.imwbcnt,round = 10,p.round=10)),
       "../../results/estimates/FE_mod2.imwbcnt.xlsx",
       overwrite=T)

export(getVC(mod2.imwbcnt,round = 10),
       "../../results/estimates/VC_mod2.imwbcnt.xlsx",
       overwrite=T)
```

## Random


```r
mod3.imwbcnt<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          imwbcnt.z.gmc+
          (imwbcnt.z.gmc|cntry),
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
summary(mod3.imwbcnt)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + imwbcnt.z.gmc +  
##     (imwbcnt.z.gmc | cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  14779.6  14843.2  -7381.8  14763.6    20881 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6949 -0.4379 -0.2245  0.2670 12.9929 
## 
## Random effects:
##  Groups Name          Variance Std.Dev. Corr
##  cntry  (Intercept)   0.108296 0.32908      
##         imwbcnt.z.gmc 0.008057 0.08976  0.39
## Number of obs: 20889, groups:  cntry, 20
## 
## Fixed effects:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)   -1.29297    0.09841 -13.138  < 2e-16 ***
## gndr.c        -0.31170    0.04100  -7.603  2.9e-14 ***
## age10.c       -0.55530    0.01371 -40.513  < 2e-16 ***
## minority.c    -0.28635    0.10530  -2.719  0.00654 ** 
## imwbcnt.z.gmc  0.05797    0.03718   1.559  0.11893    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.015                     
## age10.c      0.106  0.016              
## minority.c   0.504  0.000  0.085       
## imwbcnt.z.g  0.168  0.011  0.039 -0.020
```

```r
anova(mod2.imwbcnt,mod3.imwbcnt)
```

```
## Data: exdat
## Models:
## mod2.imwbcnt: childlessness ~ gndr.c + age10.c + minority.c + imwbcnt.z.gmc + (1 | cntry)
## mod3.imwbcnt: childlessness ~ gndr.c + age10.c + minority.c + imwbcnt.z.gmc + (imwbcnt.z.gmc | cntry)
##              npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)    
## mod2.imwbcnt    6 14790 14838 -7389.0    14778                         
## mod3.imwbcnt    8 14780 14843 -7381.8    14764 14.419  2  0.0007396 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
export(rownames_to_column(
  getFE_glmer(mod3.imwbcnt,round = 10,p.round=10)),
       "../../results/estimates/FE_mod3.imwbcnt.xlsx",
       overwrite=T)

export(getVC(mod3.imwbcnt,round = 10),
       "../../results/estimates/VC_mod3.imwbcnt.xlsx",
       overwrite=T)
```


## Random without random effect correlation


```r
mod4.imwbcnt<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          imwbcnt.z.gmc+
          (imwbcnt.z.gmc||cntry),
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
summary(mod4.imwbcnt)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + imwbcnt.z.gmc +  
##     (imwbcnt.z.gmc || cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  14778.5  14834.2  -7382.3  14764.5    20882 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6858 -0.4378 -0.2246  0.2657 12.9596 
## 
## Random effects:
##  Groups  Name          Variance Std.Dev.
##  cntry   (Intercept)   0.112040 0.33472 
##  cntry.1 imwbcnt.z.gmc 0.009129 0.09554 
## Number of obs: 20889, groups:  cntry, 20
## 
## Fixed effects:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)   -1.29279    0.09952 -12.991  < 2e-16 ***
## gndr.c        -0.31118    0.04100  -7.589 3.21e-14 ***
## age10.c       -0.55523    0.01371 -40.501  < 2e-16 ***
## minority.c    -0.28724    0.10528  -2.728  0.00637 ** 
## imwbcnt.z.gmc  0.05716    0.03896   1.467  0.14233    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.015                     
## age10.c      0.105  0.016              
## minority.c   0.499  0.000  0.085       
## imwbcnt.z.g -0.018  0.009  0.036 -0.020
```

```r
anova(mod2.imwbcnt,mod4.imwbcnt)
```

```
## Data: exdat
## Models:
## mod2.imwbcnt: childlessness ~ gndr.c + age10.c + minority.c + imwbcnt.z.gmc + (1 | cntry)
## mod4.imwbcnt: childlessness ~ gndr.c + age10.c + minority.c + imwbcnt.z.gmc + (imwbcnt.z.gmc || cntry)
##              npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)    
## mod2.imwbcnt    6 14790 14838 -7389.0    14778                         
## mod4.imwbcnt    7 14778 14834 -7382.3    14764 13.483  1  0.0002407 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
anova(mod4.imwbcnt,mod3.imwbcnt)
```

```
## Data: exdat
## Models:
## mod4.imwbcnt: childlessness ~ gndr.c + age10.c + minority.c + imwbcnt.z.gmc + (imwbcnt.z.gmc || cntry)
## mod3.imwbcnt: childlessness ~ gndr.c + age10.c + minority.c + imwbcnt.z.gmc + (imwbcnt.z.gmc | cntry)
##              npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)
## mod4.imwbcnt    7 14778 14834 -7382.3    14764                     
## mod3.imwbcnt    8 14780 14843 -7381.8    14764 0.9354  1     0.3335
```

```r
export(rownames_to_column(
  getFE_glmer(mod4.imwbcnt,round = 10,p.round=10)),
       "../../results/estimates/FE_mod4.imwbcnt.xlsx",
       overwrite=T)

export(getVC(mod4.imwbcnt,round = 10),
       "../../results/estimates/VC_mod4.imwbcnt.xlsx",
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
