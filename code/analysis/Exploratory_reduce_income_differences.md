---
title: "Exploratory analysis: Reduce income differences"
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
                gincdif.R,gincdif.R.z,
                cntry,
                anweight) %>%
  mutate(gincdif.R.z=as.numeric(gincdif.R.z)) %>%
  na.omit()
```

## Variable centering


```r
exdat<-
  group_mean_center(
    data=exdat,group.var="cntry",
    vars=c("gincdif.R","gincdif.R.z",
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
##  17149.1  17165.0  -8572.5  17145.1    21192 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -1.9089 -0.4931 -0.3101  0.3694  7.8127 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1047   0.3235  
## Number of obs: 21194, groups:  cntry, 20
## 
## Fixed effects:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.06651    0.08236  -12.95   <2e-16 ***
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
##  15019.5  15059.3  -7504.8  15009.5    21189 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7331 -0.4402 -0.2247  0.2497 12.9471 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1158   0.3403  
## Number of obs: 21194, groups:  cntry, 20
## 
## Fixed effects:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.27375    0.10014 -12.719  < 2e-16 ***
## gndr.c      -0.30675    0.04051  -7.572 3.67e-14 ***
## age10.c     -0.55422    0.01349 -41.090  < 2e-16 ***
## minority.c  -0.25712    0.10441  -2.462   0.0138 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##            (Intr) gndr.c ag10.c
## gndr.c     0.016               
## age10.c    0.107  0.010        
## minority.c 0.491  0.003  0.094
```

## Fixed main effect


```r
mod2.gincdif.R<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          gincdif.R.z.gmc+
          (1|cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight)
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```r
summary(mod2.gincdif.R)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + gincdif.R.z.gmc +  
##     (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15021.4  15069.2  -7504.7  15009.4    21188 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7406 -0.4401 -0.2248  0.2495 12.9875 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1157   0.3401  
## Number of obs: 21194, groups:  cntry, 20
## 
## Fixed effects:
##                  Estimate Std. Error z value Pr(>|z|)    
## (Intercept)     -1.274078   0.100112 -12.727  < 2e-16 ***
## gndr.c          -0.307585   0.040574  -7.581 3.44e-14 ***
## age10.c         -0.554523   0.013514 -41.034  < 2e-16 ***
## minority.c      -0.258150   0.104442  -2.472   0.0134 *  
## gincdif.R.z.gmc  0.007539   0.020453   0.369   0.7124    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.016                     
## age10.c      0.108  0.014              
## minority.c   0.491  0.004  0.095       
## gncdf.R.z.g -0.009 -0.056 -0.061 -0.027
```

```r
export(rownames_to_column(
  getFE_glmer(mod2.gincdif.R,round = 10,p.round=10)),
       "../../results/estimates/FE_mod2.gincdif.R.xlsx",
       overwrite=T)

export(getVC(mod2.gincdif.R,round = 10),
       "../../results/estimates/VC_mod2.gincdif.R.xlsx",
       overwrite=T)
```

## Random


```r
mod3.gincdif.R<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          gincdif.R.z.gmc+
          (gincdif.R.z.gmc|cntry),
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
summary(mod3.gincdif.R)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + gincdif.R.z.gmc +  
##     (gincdif.R.z.gmc | cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15023.3  15087.0  -7503.6  15007.3    21186 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7382 -0.4413 -0.2253  0.2496 12.7499 
## 
## Random effects:
##  Groups Name            Variance  Std.Dev. Corr
##  cntry  (Intercept)     0.1175087 0.34280      
##         gincdif.R.z.gmc 0.0005986 0.02447  1.00
## Number of obs: 21194, groups:  cntry, 20
## 
## Fixed effects:
##                 Estimate Std. Error z value Pr(>|z|)    
## (Intercept)     -1.27333    0.10059 -12.658  < 2e-16 ***
## gndr.c          -0.30676    0.04059  -7.558 4.08e-14 ***
## age10.c         -0.55404    0.01352 -40.991  < 2e-16 ***
## minority.c      -0.25899    0.10446  -2.479   0.0132 *  
## gincdif.R.z.gmc  0.00623    0.02131   0.292   0.7700    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.016                     
## age10.c      0.107  0.014              
## minority.c   0.489  0.004  0.094       
## gncdf.R.z.g  0.236 -0.055 -0.056 -0.022
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.gincdif.R,mod3.gincdif.R)
```

```
## Data: exdat
## Models:
## mod2.gincdif.R: childlessness ~ gndr.c + age10.c + minority.c + gincdif.R.z.gmc + (1 | cntry)
## mod3.gincdif.R: childlessness ~ gndr.c + age10.c + minority.c + gincdif.R.z.gmc + (gincdif.R.z.gmc | cntry)
##                npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)
## mod2.gincdif.R    6 15021 15069 -7504.7    15009                     
## mod3.gincdif.R    8 15023 15087 -7503.6    15007 2.1285  2      0.345
```

```r
export(rownames_to_column(
  getFE_glmer(mod3.gincdif.R,round = 10,p.round=10)),
       "../../results/estimates/FE_mod3.gincdif.R.xlsx",
       overwrite=T)

export(getVC(mod3.gincdif.R,round = 10),
       "../../results/estimates/VC_mod3.gincdif.R.xlsx",
       overwrite=T)
```


## Random without random effect correlation


```r
mod4.gincdif.R<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          gincdif.R.z.gmc+
          (gincdif.R.z.gmc||cntry),
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
summary(mod4.gincdif.R)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + gincdif.R.z.gmc +  
##     (gincdif.R.z.gmc || cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15023.4  15079.1  -7504.7  15009.4    21187 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7406 -0.4401 -0.2248  0.2495 12.9875 
## 
## Random effects:
##  Groups  Name            Variance Std.Dev.
##  cntry   (Intercept)     0.1157   0.3401  
##  cntry.1 gincdif.R.z.gmc 0.0000   0.0000  
## Number of obs: 21194, groups:  cntry, 20
## 
## Fixed effects:
##                  Estimate Std. Error z value Pr(>|z|)    
## (Intercept)     -1.274079   0.100109 -12.727  < 2e-16 ***
## gndr.c          -0.307585   0.040574  -7.581 3.44e-14 ***
## age10.c         -0.554522   0.013514 -41.034  < 2e-16 ***
## minority.c      -0.258148   0.104445  -2.472   0.0134 *  
## gincdif.R.z.gmc  0.007539   0.020453   0.369   0.7124    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.016                     
## age10.c      0.108  0.014              
## minority.c   0.491  0.004  0.095       
## gncdf.R.z.g -0.009 -0.056 -0.061 -0.027
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.gincdif.R,mod4.gincdif.R)
```

```
## Data: exdat
## Models:
## mod2.gincdif.R: childlessness ~ gndr.c + age10.c + minority.c + gincdif.R.z.gmc + (1 | cntry)
## mod4.gincdif.R: childlessness ~ gndr.c + age10.c + minority.c + gincdif.R.z.gmc + (gincdif.R.z.gmc || cntry)
##                npar   AIC   BIC  logLik deviance Chisq Df Pr(>Chisq)
## mod2.gincdif.R    6 15021 15069 -7504.7    15009                    
## mod4.gincdif.R    7 15023 15079 -7504.7    15009     0  1     0.9999
```

```r
anova(mod4.gincdif.R,mod3.gincdif.R)
```

```
## Data: exdat
## Models:
## mod4.gincdif.R: childlessness ~ gndr.c + age10.c + minority.c + gincdif.R.z.gmc + (gincdif.R.z.gmc || cntry)
## mod3.gincdif.R: childlessness ~ gndr.c + age10.c + minority.c + gincdif.R.z.gmc + (gincdif.R.z.gmc | cntry)
##                npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)
## mod4.gincdif.R    7 15023 15079 -7504.7    15009                     
## mod3.gincdif.R    8 15023 15087 -7503.6    15007 2.1285  1     0.1446
```

```r
export(rownames_to_column(
  getFE_glmer(mod4.gincdif.R,round = 10,p.round=10)),
       "../../results/estimates/FE_mod4.gincdif.R.xlsx",
       overwrite=T)

export(getVC(mod4.gincdif.R,round = 10),
       "../../results/estimates/VC_mod4.gincdif.R.xlsx",
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
