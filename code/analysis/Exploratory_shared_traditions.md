---
title: "Exploratory analysis: Importance of Shared Traditions"
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
                pplstrd.R,pplstrd.R.z,
                cntry,
                anweight) %>%
  mutate(pplstrd.R.z=as.numeric(pplstrd.R.z)) %>%
  na.omit()
```

## Variable centering


```r
exdat<-
  group_mean_center(
    data=exdat,group.var="cntry",
    vars=c("pplstrd.R","pplstrd.R.z",
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
##  17157.5  17173.5  -8576.8  17153.5    21215 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -1.9044 -0.4932 -0.3115  0.3707  7.8164 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1044   0.3231  
## Number of obs: 21217, groups:  cntry, 20
## 
## Fixed effects:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.06877    0.08227  -12.99   <2e-16 ***
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
##  15005.8  15045.6  -7497.9  14995.8    21212 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7527 -0.4388 -0.2243  0.2459 13.0448 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1157   0.3401  
## Number of obs: 21217, groups:  cntry, 20
## 
## Fixed effects:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.28768    0.09999 -12.879  < 2e-16 ***
## gndr.c      -0.31481    0.04054  -7.766 8.07e-15 ***
## age10.c     -0.55687    0.01350 -41.248  < 2e-16 ***
## minority.c  -0.28149    0.10391  -2.709  0.00675 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##            (Intr) gndr.c ag10.c
## gndr.c     0.015               
## age10.c    0.107  0.012        
## minority.c 0.489  0.002  0.093
```

## Fixed main effect


```r
mod2.pplstrd.R<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          pplstrd.R.z.gmc+
          (1|cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight)
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```r
summary(mod2.pplstrd.R)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + pplstrd.R.z.gmc +  
##     (1 | cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  15007.5  15055.2  -7497.7  14995.5    21211 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7254 -0.4392 -0.2246  0.2455 13.1655 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1156   0.34    
## Number of obs: 21217, groups:  cntry, 20
## 
## Fixed effects:
##                 Estimate Std. Error z value Pr(>|z|)    
## (Intercept)     -1.28794    0.09996 -12.884  < 2e-16 ***
## gndr.c          -0.31579    0.04057  -7.784 7.03e-15 ***
## age10.c         -0.55564    0.01366 -40.674  < 2e-16 ***
## minority.c      -0.28151    0.10390  -2.709  0.00674 ** 
## pplstrd.R.z.gmc -0.01256    0.02138  -0.588  0.55672    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.015                     
## age10.c      0.105  0.005              
## minority.c   0.489  0.002  0.091       
## pplstrd.R..  0.005  0.042 -0.152  0.000
```

```r
export(rownames_to_column(
  getFE_glmer(mod2.pplstrd.R,round = 10,p.round=10)),
       "../../results/estimates/FE_mod2.pplstrd.R.xlsx",
       overwrite=T)

export(getVC(mod2.pplstrd.R,round = 10),
       "../../results/estimates/VC_mod2.pplstrd.R.xlsx",
       overwrite=T)
```

## Random


```r
mod3.pplstrd.R<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          pplstrd.R.z.gmc+
          (pplstrd.R.z.gmc|cntry),
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
summary(mod3.pplstrd.R)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + pplstrd.R.z.gmc +  
##     (pplstrd.R.z.gmc | cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15002.8  15066.5  -7493.4  14986.8    21209 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7945 -0.4386 -0.2245  0.2448 13.0449 
## 
## Random effects:
##  Groups Name            Variance Std.Dev. Corr
##  cntry  (Intercept)     0.11493  0.33902      
##         pplstrd.R.z.gmc 0.00551  0.07423  0.01
## Number of obs: 21217, groups:  cntry, 20
## 
## Fixed effects:
##                 Estimate Std. Error z value Pr(>|z|)    
## (Intercept)     -1.29008    0.09982 -12.925  < 2e-16 ***
## gndr.c          -0.31571    0.04060  -7.777 7.44e-15 ***
## age10.c         -0.55663    0.01369 -40.659  < 2e-16 ***
## minority.c      -0.28185    0.10389  -2.713  0.00667 ** 
## pplstrd.R.z.gmc -0.02012    0.03340  -0.602  0.54694    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.016                     
## age10.c      0.106  0.006              
## minority.c   0.490  0.002  0.092       
## pplstrd.R..  0.015  0.028 -0.088  0.005
```

```r
anova(mod2.pplstrd.R,mod3.pplstrd.R)
```

```
## Data: exdat
## Models:
## mod2.pplstrd.R: childlessness ~ gndr.c + age10.c + minority.c + pplstrd.R.z.gmc + (1 | cntry)
## mod3.pplstrd.R: childlessness ~ gndr.c + age10.c + minority.c + pplstrd.R.z.gmc + (pplstrd.R.z.gmc | cntry)
##                npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)  
## mod2.pplstrd.R    6 15008 15055 -7497.7    14996                       
## mod3.pplstrd.R    8 15003 15066 -7493.4    14987 8.6962  2    0.01293 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
export(rownames_to_column(
  getFE_glmer(mod3.pplstrd.R,round = 10,p.round=10)),
       "../../results/estimates/FE_mod3.pplstrd.R.xlsx",
       overwrite=T)

export(getVC(mod3.pplstrd.R,round = 10),
       "../../results/estimates/VC_mod3.pplstrd.R.xlsx",
       overwrite=T)
```


## Random without random effect correlation


```r
mod4.pplstrd.R<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          pplstrd.R.z.gmc+
          (pplstrd.R.z.gmc||cntry),
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
summary(mod4.pplstrd.R)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + pplstrd.R.z.gmc +  
##     (pplstrd.R.z.gmc || cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  15000.8  15056.5  -7493.4  14986.8    21210 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7943 -0.4387 -0.2244  0.2448 13.0394 
## 
## Random effects:
##  Groups  Name            Variance Std.Dev.
##  cntry   (Intercept)     0.114873 0.33893 
##  cntry.1 pplstrd.R.z.gmc 0.005498 0.07415 
## Number of obs: 21217, groups:  cntry, 20
## 
## Fixed effects:
##                 Estimate Std. Error z value Pr(>|z|)    
## (Intercept)     -1.29004    0.09979 -12.928  < 2e-16 ***
## gndr.c          -0.31572    0.04060  -7.777 7.43e-15 ***
## age10.c         -0.55662    0.01369 -40.660  < 2e-16 ***
## minority.c      -0.28180    0.10388  -2.713  0.00668 ** 
## pplstrd.R.z.gmc -0.02011    0.03338  -0.602  0.54691    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.c ag10.c mnrty.
## gndr.c       0.016                     
## age10.c      0.106  0.006              
## minority.c   0.490  0.002  0.092       
## pplstrd.R..  0.011  0.028 -0.088  0.005
```

```r
anova(mod2.pplstrd.R,mod4.pplstrd.R)
```

```
## Data: exdat
## Models:
## mod2.pplstrd.R: childlessness ~ gndr.c + age10.c + minority.c + pplstrd.R.z.gmc + (1 | cntry)
## mod4.pplstrd.R: childlessness ~ gndr.c + age10.c + minority.c + pplstrd.R.z.gmc + (pplstrd.R.z.gmc || cntry)
##                npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)   
## mod2.pplstrd.R    6 15008 15055 -7497.7    14996                        
## mod4.pplstrd.R    7 15001 15056 -7493.4    14987 8.6958  1   0.003189 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
anova(mod4.pplstrd.R,mod3.pplstrd.R)
```

```
## Data: exdat
## Models:
## mod4.pplstrd.R: childlessness ~ gndr.c + age10.c + minority.c + pplstrd.R.z.gmc + (pplstrd.R.z.gmc || cntry)
## mod3.pplstrd.R: childlessness ~ gndr.c + age10.c + minority.c + pplstrd.R.z.gmc + (pplstrd.R.z.gmc | cntry)
##                npar   AIC   BIC  logLik deviance Chisq Df Pr(>Chisq)
## mod4.pplstrd.R    7 15001 15056 -7493.4    14987                    
## mod3.pplstrd.R    8 15003 15066 -7493.4    14987 4e-04  1     0.9833
```

```r
export(rownames_to_column(
  getFE_glmer(mod4.pplstrd.R,round = 10,p.round=10)),
       "../../results/estimates/FE_mod4.pplstrd.R.xlsx",
       overwrite=T)

export(getVC(mod4.pplstrd.R,round = 10),
       "../../results/estimates/VC_mod4.pplstrd.R.xlsx",
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
