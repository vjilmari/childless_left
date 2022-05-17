---
title: "ERQ6. Voting"
output: 
  html_document: 
    toc: yes
    keep_md: yes
date: '2022-05-02'
---




ERQ6. Is childlessness associated with not having voted in the previous national elections, despite being eligible to vote?

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
library(ggplot2)
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

## Code West and post-communist countries


```r
table(fdat$cntry)
```

```
## 
##   AT   BE   CH   CZ   DE   DK   EE   ES   FI   FR   GB   HU   IE   IL   LT   NL 
## 1795 1769 1532 2148 3045 1502 2051 1925 2087 1917 2264 1698 2390 2562 2250 1919 
##   NO   PL   PT   SE   SI 
## 1436 1615 1265 1791 1224
```

```r
fdat$West_vs_post_comm<-
  case_when(fdat$cntry == "AT" |
              fdat$cntry == "BE" |
              fdat$cntry == "CH" |
              fdat$cntry == "DE" |
              fdat$cntry == "DK" |
              fdat$cntry == "ES" |
              fdat$cntry == "FI" |
              fdat$cntry == "FR" |
              fdat$cntry == "GB" |
              fdat$cntry == "IE" |
              fdat$cntry == "IL" |
              fdat$cntry == "NL" |
              fdat$cntry == "NO" |
              fdat$cntry == "PT" |
              fdat$cntry == "SE" ~ -0.5,
            TRUE~0.5)

table(fdat$West_vs_post_comm,useNA="always")
```

```
## 
##  -0.5   0.5  <NA> 
## 29199 10986     0
```

## Voting and eligibility


```r
table(fdat$vote)
```

```
## 
##     1     2     3 
## 27867  8492  3498
```

```r
table(fdat$vote.c)
```

```
## 
##  -0.5   0.5 
##  8492 27867
```

## Data exclusions


```r
exdat<-fdat %>%
  dplyr::select(childlessness,
                gndr.c,age10.c,
                minority.c,
                vote.c,
                cntry,anweight) %>%
  na.omit()
```

## Variable centering


```r
#exdat<-
#  group_mean_center(
#    data=exdat,group.var="cntry",
#    vars=c("international_security.z",
#           "antielite_salience.z"),
#    grand.init = F)
```


# Analysis 

## Fixed main effect


```r
mod2.vote<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          vote.c+
          (1|cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight)
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a binomial
## glm!
```

```r
summary(mod2.vote)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + vote.c + (1 |  
##     cntry)
##    Data: exdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  24652.9  24703.8 -12320.4  24640.9    35636 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.0587 -0.4262 -0.2090  0.3460 15.7532 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.1173   0.3425  
## Number of obs: 35642, groups:  cntry, 21
## 
## Fixed effects:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.31953    0.08909 -14.810  < 2e-16 ***
## gndr.c      -0.45277    0.03153 -14.362  < 2e-16 ***
## age10.c     -0.58779    0.01082 -54.329  < 2e-16 ***
## minority.c  -0.37662    0.07558  -4.983 6.26e-07 ***
## vote.c       0.02235    0.03641   0.614    0.539    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##            (Intr) gndr.c ag10.c mnrty.
## gndr.c      0.010                     
## age10.c     0.131  0.039              
## minority.c  0.382 -0.003  0.074       
## vote.c     -0.089  0.026 -0.191  0.058
```

## Random main effect


```r
mod3.vote<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          vote.c+
          (vote.c|cntry),
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
summary(mod3.vote)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + vote.c + (vote.c |  
##     cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa")
## 
##      AIC      BIC   logLik deviance df.resid 
##  24656.8  24724.7 -12320.4  24640.8    35634 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.0583 -0.4263 -0.2091  0.3458 15.8114 
## 
## Random effects:
##  Groups Name        Variance  Std.Dev. Corr 
##  cntry  (Intercept) 1.180e-01 0.343530      
##         vote.c      3.075e-05 0.005545 -1.00
## Number of obs: 35642, groups:  cntry, 21
## 
## Fixed effects:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.31875    0.08940 -14.751  < 2e-16 ***
## gndr.c      -0.45284    0.03153 -14.363  < 2e-16 ***
## age10.c     -0.58784    0.01082 -54.320  < 2e-16 ***
## minority.c  -0.37704    0.07561  -4.986 6.15e-07 ***
## vote.c       0.02124    0.03689   0.576    0.565    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##            (Intr) gndr.c ag10.c mnrty.
## gndr.c      0.010                     
## age10.c     0.129  0.039              
## minority.c  0.379 -0.002  0.075       
## vote.c     -0.126  0.028 -0.185  0.062
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.vote,mod3.vote)
```

```
## Data: exdat
## Models:
## mod2.vote: childlessness ~ gndr.c + age10.c + minority.c + vote.c + (1 | cntry)
## mod3.vote: childlessness ~ gndr.c + age10.c + minority.c + vote.c + (vote.c | cntry)
##           npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
## mod2.vote    6 24653 24704 -12320    24641                     
## mod3.vote    8 24657 24725 -12320    24641 0.0377  2     0.9813
```

```r
mod3.vote.no.recov<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          vote.c+
          (vote.c||cntry),
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
summary(mod3.vote.no.recov)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: childlessness ~ gndr.c + age10.c + minority.c + vote.c + (vote.c ||  
##     cntry)
##    Data: exdat
## Weights: anweight
## Control: glmerControl(optimizer = "bobyqa")
## 
##      AIC      BIC   logLik deviance df.resid 
##  24654.9  24714.3 -12320.4  24640.9    35635 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.0587 -0.4262 -0.2090  0.3460 15.7532 
## 
## Random effects:
##  Groups  Name        Variance Std.Dev.
##  cntry   (Intercept) 0.1173   0.3425  
##  cntry.1 vote.c      0.0000   0.0000  
## Number of obs: 35642, groups:  cntry, 21
## 
## Fixed effects:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.31952    0.08909 -14.811  < 2e-16 ***
## gndr.c      -0.45277    0.03153 -14.362  < 2e-16 ***
## age10.c     -0.58779    0.01082 -54.330  < 2e-16 ***
## minority.c  -0.37662    0.07558  -4.983 6.25e-07 ***
## vote.c       0.02235    0.03641   0.614    0.539    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##            (Intr) gndr.c ag10.c mnrty.
## gndr.c      0.010                     
## age10.c     0.131  0.039              
## minority.c  0.382 -0.003  0.074       
## vote.c     -0.089  0.026 -0.191  0.058
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.vote,mod3.vote.no.recov)
```

```
## Data: exdat
## Models:
## mod2.vote: childlessness ~ gndr.c + age10.c + minority.c + vote.c + (1 | cntry)
## mod3.vote.no.recov: childlessness ~ gndr.c + age10.c + minority.c + vote.c + (vote.c || cntry)
##                    npar   AIC   BIC logLik deviance Chisq Df Pr(>Chisq)
## mod2.vote             6 24653 24704 -12320    24641                    
## mod3.vote.no.recov    7 24655 24714 -12320    24641     0  1     0.9999
```

```r
anova(mod3.vote.no.recov,mod3.vote)
```

```
## Data: exdat
## Models:
## mod3.vote.no.recov: childlessness ~ gndr.c + age10.c + minority.c + vote.c + (vote.c || cntry)
## mod3.vote: childlessness ~ gndr.c + age10.c + minority.c + vote.c + (vote.c | cntry)
##                    npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
## mod3.vote.no.recov    7 24655 24714 -12320    24641                     
## mod3.vote             8 24657 24725 -12320    24641 0.0377  1      0.846
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
## [1] ggplot2_3.3.5         vjihelpers_0.0.0.9000 dplyr_1.0.9          
## [4] rio_0.5.29            emmeans_1.7.3         lme4_1.1-29          
## [7] Matrix_1.4-1         
## 
## loaded via a namespace (and not attached):
##  [1] tidyselect_1.1.2  xfun_0.30         bslib_0.3.1       purrr_0.3.4      
##  [5] splines_4.2.0     haven_2.5.0       lattice_0.20-45   colorspace_2.0-3 
##  [9] generics_0.1.2    vctrs_0.4.1       htmltools_0.5.2   yaml_2.3.5       
## [13] utf8_1.2.2        rlang_1.0.2       jquerylib_0.1.4   nloptr_2.0.0     
## [17] pillar_1.7.0      withr_2.5.0       foreign_0.8-82    glue_1.6.2       
## [21] readxl_1.4.0      lifecycle_1.0.1   stringr_1.4.0     munsell_0.5.0    
## [25] gtable_0.3.0      cellranger_1.1.0  zip_2.2.0         mvtnorm_1.1-3    
## [29] evaluate_0.15     knitr_1.39        forcats_0.5.1     fastmap_1.1.0    
## [33] curl_4.3.2        fansi_1.0.3       Rcpp_1.0.8.3      xtable_1.8-4     
## [37] scales_1.2.0      jsonlite_1.8.0    hms_1.1.1         digest_0.6.29    
## [41] stringi_1.7.6     openxlsx_4.2.5    grid_4.2.0        cli_3.3.0        
## [45] tools_4.2.0       magrittr_2.0.3    sass_0.4.1        tibble_3.1.6     
## [49] crayon_1.5.1      pkgconfig_2.0.3   MASS_7.3-56       ellipsis_0.3.2   
## [53] data.table_1.14.2 estimability_1.3  minqa_1.2.4       rmarkdown_2.14   
## [57] rstudioapi_0.13   R6_2.5.1          boot_1.3-28       nlme_3.1-157     
## [61] compiler_4.2.0
```
