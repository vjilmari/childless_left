---
title: "Exploratory analysis: Gay rights"
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
                freehms.R,freehms.R.z,
                cntry,
                anweight) %>%
  mutate(freehms.R.z=as.numeric(freehms.R.z)) %>%
  na.omit()
```

## Variable centering


```r
exdat<-
  group_mean_center(
    data=exdat,group.var="cntry",
    vars=c("freehms.R","freehms.R.z",
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
##  17070.6  17086.5  -8533.3  17066.6    21023 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -1.9119 -0.4946 -0.3118  0.3692  7.8257 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1074   0.3278  
## Number of obs: 21025, groups:  cntry, 20
## 
## Fixed effects:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.06348    0.08332  -12.76   <2e-16 ***
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
##  14944.2  14984.0  -7467.1  14934.2    21020 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7453 -0.4407 -0.2252  0.2598 13.0231 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1161   0.3407  
## Number of obs: 21025, groups:  cntry, 20
## 
## Fixed effects:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.27830    0.10039 -12.734  < 2e-16 ***
## gndr.c      -0.29938    0.04061  -7.372 1.68e-13 ***
## age10.c     -0.55720    0.01357 -41.073  < 2e-16 ***
## minority.c  -0.25924    0.10475  -2.475   0.0133 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##            (Intr) gndr.c ag10.c
## gndr.c     0.016               
## age10.c    0.109  0.010        
## minority.c 0.492  0.004  0.093
```

## Fixed main effect


```r
mod2.freehms.R<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          freehms.R.z.gmc+
          (1|cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight)
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```r
summary(mod2.freehms.R)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + freehms.R.z.gmc +  
##     (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  14946.2  14993.9  -7467.1  14934.2    21019 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7383 -0.4409 -0.2252  0.2588 12.9888 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.116    0.3406  
## Number of obs: 21025, groups:  cntry, 20
## 
## Fixed effects:
##                  Estimate Std. Error z value Pr(>|z|)    
## (Intercept)     -1.277193   0.100435 -12.717  < 2e-16 ***
## gndr.c          -0.300405   0.040786  -7.365 1.77e-13 ***
## age10.c         -0.556610   0.013739 -40.512  < 2e-16 ***
## minority.c      -0.256155   0.105364  -2.431   0.0151 *  
## freehms.R.z.gmc  0.007516   0.027901   0.269   0.7876    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.012                     
## age10.c      0.114 -0.004              
## minority.c   0.493 -0.006  0.108       
## frhms.R.z.g  0.040 -0.093  0.158  0.109
```

```r
export(rownames_to_column(
  getFE_glmer(mod2.freehms.R,round = 10,p.round=10)),
       "../../results/estimates/FE_mod2.freehms.R.xlsx",
       overwrite=T)

export(getVC(mod2.freehms.R,round = 10),
       "../../results/estimates/VC_mod2.freehms.R.xlsx",
       overwrite=T)
```

## Random


```r
mod3.freehms.R<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          freehms.R.z.gmc+
          (freehms.R.z.gmc|cntry),
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
summary(mod3.freehms.R)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + freehms.R.z.gmc +  
##     (freehms.R.z.gmc | cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  14946.1  15009.8  -7465.1  14930.1    21017 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7169 -0.4414 -0.2248  0.2580 13.4255 
## 
## Random effects:
##  Groups Name            Variance Std.Dev. Corr
##  cntry  (Intercept)     0.113467 0.33685      
##         freehms.R.z.gmc 0.005673 0.07532  0.57
## Number of obs: 21025, groups:  cntry, 20
## 
## Fixed effects:
##                 Estimate Std. Error z value Pr(>|z|)    
## (Intercept)     -1.27520    0.09969 -12.792  < 2e-16 ***
## gndr.c          -0.30132    0.04084  -7.377 1.62e-13 ***
## age10.c         -0.55549    0.01380 -40.257  < 2e-16 ***
## minority.c      -0.25432    0.10541  -2.413   0.0158 *  
## freehms.R.z.gmc  0.03920    0.04337   0.904   0.3660    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.010                     
## age10.c      0.118 -0.008              
## minority.c   0.496 -0.007  0.110       
## frhms.R.z.g  0.205 -0.054  0.086  0.074
```

```r
anova(mod2.freehms.R,mod3.freehms.R)
```

```
## Data: exdat
## Models:
## mod2.freehms.R: childlessness ~ gndr.c + age10.c + minority.c + freehms.R.z.gmc + (1 | cntry)
## mod3.freehms.R: childlessness ~ gndr.c + age10.c + minority.c + freehms.R.z.gmc + (freehms.R.z.gmc | cntry)
##                npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)
## mod2.freehms.R    6 14946 14994 -7467.1    14934                     
## mod3.freehms.R    8 14946 15010 -7465.1    14930 4.0144  2     0.1344
```

```r
export(rownames_to_column(
  getFE_glmer(mod3.freehms.R,round = 10,p.round=10)),
       "../../results/estimates/FE_mod3.freehms.R.xlsx",
       overwrite=T)

export(getVC(mod3.freehms.R,round = 10),
       "../../results/estimates/VC_mod3.freehms.R.xlsx",
       overwrite=T)
```


## Random without random effect correlation


```r
mod4.freehms.R<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          freehms.R.z.gmc+
          (freehms.R.z.gmc||cntry),
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
summary(mod4.freehms.R)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + freehms.R.z.gmc +  
##     (freehms.R.z.gmc || cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  14946.3  15001.9  -7466.1  14932.3    21018 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7230 -0.4411 -0.2247  0.2565 13.2610 
## 
## Random effects:
##  Groups  Name            Variance Std.Dev.
##  cntry   (Intercept)     0.115558 0.3399  
##  cntry.1 freehms.R.z.gmc 0.004678 0.0684  
## Number of obs: 21025, groups:  cntry, 20
## 
## Fixed effects:
##                 Estimate Std. Error z value Pr(>|z|)    
## (Intercept)     -1.28042    0.10045 -12.747  < 2e-16 ***
## gndr.c          -0.29992    0.04082  -7.347 2.03e-13 ***
## age10.c         -0.55668    0.01378 -40.386  < 2e-16 ***
## minority.c      -0.25526    0.10556  -2.418   0.0156 *  
## freehms.R.z.gmc  0.04195    0.04469   0.939   0.3479    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.011                     
## age10.c      0.115 -0.007              
## minority.c   0.494 -0.007  0.111       
## frhms.R.z.g  0.003 -0.058  0.095  0.071
```

```r
anova(mod2.freehms.R,mod4.freehms.R)
```

```
## Data: exdat
## Models:
## mod2.freehms.R: childlessness ~ gndr.c + age10.c + minority.c + freehms.R.z.gmc + (1 | cntry)
## mod4.freehms.R: childlessness ~ gndr.c + age10.c + minority.c + freehms.R.z.gmc + (freehms.R.z.gmc || cntry)
##                npar   AIC   BIC  logLik deviance Chisq Df Pr(>Chisq)
## mod2.freehms.R    6 14946 14994 -7467.1    14934                    
## mod4.freehms.R    7 14946 15002 -7466.1    14932 1.897  1     0.1684
```

```r
anova(mod4.freehms.R,mod3.freehms.R)
```

```
## Data: exdat
## Models:
## mod4.freehms.R: childlessness ~ gndr.c + age10.c + minority.c + freehms.R.z.gmc + (freehms.R.z.gmc || cntry)
## mod3.freehms.R: childlessness ~ gndr.c + age10.c + minority.c + freehms.R.z.gmc + (freehms.R.z.gmc | cntry)
##                npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)
## mod4.freehms.R    7 14946 15002 -7466.1    14932                     
## mod3.freehms.R    8 14946 15010 -7465.1    14930 2.1173  1     0.1456
```

```r
export(rownames_to_column(
  getFE_glmer(mod4.freehms.R,round = 10,p.round=10)),
       "../../results/estimates/FE_mod4.freehms.R.xlsx",
       overwrite=T)

export(getVC(mod4.freehms.R,round = 10),
       "../../results/estimates/VC_mod4.freehms.R.xlsx",
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
