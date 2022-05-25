---
title: "Descriptive statistics"
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
                cntry,
                anweight) %>%
  na.omit()
```

## Variable centering


```r
exdat<-
  group_mean_center(
    data=exdat,group.var="cntry",
    vars=c("lrgen","lrecon","galtan",
           "lrecon_salience","galtan_salience"),
    grand.init = F)
```


## Variable scaling


```r
CHES<-
  import("../../data/raw/2014_CHES_dataset_means.csv")

(sd.lrgen<-sd(CHES$lrgen,na.rm=T))
```

```
## [1] 2.302111
```

```r
(sd.lrecon<-sd(CHES$lrecon,na.rm=T))
```

```
## [1] 2.210724
```

```r
(sd.galtan<-sd(CHES$galtan,na.rm=T))
```

```
## [1] 2.627548
```

```r
(sd.galtan_salience<-sd(CHES$galtan_salience,na.rm=T))
```

```
## [1] 1.551972
```

```r
(sd.lrecon_salience<-sd(CHES$lrecon_salience,na.rm=T))
```

```
## [1] 1.468366
```

```r
exdat$lrgen.z.gmc<-exdat$lrgen.gmc/sd.lrgen
exdat$lrecon.z.gmc<-exdat$lrecon.gmc/sd.lrecon
exdat$galtan.z.gmc<-exdat$galtan.gmc/sd.galtan
exdat$galtan_salience.z.gmc<-
  exdat$galtan_salience.gmc/sd.galtan_salience
exdat$lrecon_salience.z.gmc<-
  exdat$lrecon_salience.gmc/sd.lrecon_salience
```



# Descriptive statistics

## Sample 


```r
# n countries
length(unique(exdat$cntry))
```

```
## [1] 20
```

```r
# n initial observations
nrow(fdat[fdat$cntry!="IL",])
```

```
## [1] 37623
```

```r
# n who reported voting
table(fdat[fdat$cntry!="IL","vote"],useNA="always")
```

```
## 
##     1     2     3  <NA> 
## 25738  8187  3394   304
```

```r
# n for whom CHES was available

table(rowSums(is.na(fdat[fdat$cntry!="IL",
                   c("galtan","lrgen","lrecon")])),
              useNA="always")
```

```
## 
##     0     3  <NA> 
## 21684 15939     0
```

```r
21684/37623
```

```
## [1] 0.5763496
```

```r
21684-21374
```

```
## [1] 310
```

```r
#range between countries
range(table(exdat$cntry))
```

```
## [1]  541 2043
```

## Childlessness

### Unweighted


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

### Weighted


```r
exdat %>%
  group_by(cntry) %>%
  count(childlessness,wt=anweight) %>%
  ungroup() %>%
  group_by(cntry) %>%
  mutate(total=sum(n),percentage=round(100*n/total,1)) %>%
  filter(childlessness==1) %>%
  dplyr::select(cntry,percentage)
```

```
## # A tibble: 20 × 2
## # Groups:   cntry [20]
##    cntry percentage
##    <chr>      <dbl>
##  1 AT          38.8
##  2 BE          28  
##  3 CH          33.7
##  4 CZ          21.3
##  5 DE          32  
##  6 DK          22.1
##  7 EE          19.2
##  8 ES          29.4
##  9 FI          28.5
## 10 FR          13.2
## 11 GB          23.6
## 12 HU          30  
## 13 IE          28  
## 14 LT          11.8
## 15 NL          25.4
## 16 NO          24.1
## 17 PL          20.4
## 18 PT          21.4
## 19 SE          28.1
## 20 SI          22.2
```

```r
exdat %>%
  count(childlessness,wt=anweight) %>%
  mutate(total=sum(n),percentage=round(100*n/total,1))
```

```
##   childlessness         n    total percentage
## 1             0 13960.839 18758.81       74.4
## 2             1  4797.975 18758.81       25.6
```

```r
round(weighted.mean(exdat$childlessness,w=exdat$anweight),2)
```

```
## [1] 0.26
```

```r
round(sqrt(wtd.var(exdat$childlessness,w=exdat$anweight)),2)
```

```
## [1] 0.44
```

## Covariates

### Age


```r
round(weighted.mean(exdat$agea,w=exdat$anweight),2)
```

```
## [1] 52.52
```

```r
round(sqrt(wtd.var(exdat$agea,w=exdat$anweight)),2)
```

```
## [1] 17.13
```

### Gender


```r
exdat %>%
  count(gndr.f,wt=anweight) %>%
  mutate(total=sum(n),percentage=round(100*n/total,1))
```

```
##   gndr.f        n    total percentage
## 1 Female 9436.640 18758.81       50.3
## 2   Male 9322.174 18758.81       49.7
```

```r
round(weighted.mean(exdat$gndr.c,w=exdat$anweight),2)
```

```
## [1] 0
```

```r
round(sqrt(wtd.var(exdat$gndr.c,w=exdat$anweight)),2)
```

```
## [1] 0.5
```

### Minority ethnic group status


```r
exdat %>%
  count(minority,wt=anweight) %>%
  mutate(total=sum(n),percentage=round(100*n/total,1))
```

```
##   minority          n    total percentage
## 1        0 18096.2317 18758.81       96.5
## 2        1   662.5816 18758.81        3.5
```

```r
round(weighted.mean(exdat$minority.c,w=exdat$anweight),2)
```

```
## [1] -0.46
```

```r
round(sqrt(wtd.var(exdat$minority.c,w=exdat$anweight)),2)
```

```
## [1] 0.18
```

## Independent variables

### lrgen


```r
round(weighted.mean(exdat$lrgen,w=exdat$anweight),2)
```

```
## [1] 5.47
```

```r
round(sqrt(wtd.var(exdat$lrgen,w=exdat$anweight)),2)
```

```
## [1] 2.05
```

### lrecon


```r
round(weighted.mean(exdat$lrecon,w=exdat$anweight),2)
```

```
## [1] 5.19
```

```r
round(sqrt(wtd.var(exdat$lrecon,w=exdat$anweight)),2)
```

```
## [1] 1.99
```

### galtan


```r
round(weighted.mean(exdat$galtan,w=exdat$anweight),2)
```

```
## [1] 5.1
```

```r
round(sqrt(wtd.var(exdat$galtan,w=exdat$anweight)),2)
```

```
## [1] 2.24
```

### galtan salience


```r
round(weighted.mean(exdat$galtan_salience,w=exdat$anweight),2)
```

```
## [1] 5.77
```

```r
round(sqrt(wtd.var(exdat$galtan_salience,w=exdat$anweight)),2)
```

```
## [1] 1.36
```

## Correlation table


```r
cor.vars<-c("gndr.c","age10.c","minority.c",
            "lrgen.z.gmc","lrecon.z.gmc",
            "galtan.z.gmc","galtan_salience.z.gmc",
            "childlessness")

weighted_corr <- 
  cov.wt(exdat[,cor.vars],
         wt = exdat[,"anweight"], cor = TRUE)

corr_matrix <- weighted_corr$cor
round(corr_matrix,2)
```

```
##                       gndr.c age10.c minority.c lrgen.z.gmc lrecon.z.gmc
## gndr.c                  1.00    0.04      -0.01       -0.03        -0.02
## age10.c                 0.04    1.00      -0.08        0.03         0.04
## minority.c             -0.01   -0.08       1.00       -0.07        -0.07
## lrgen.z.gmc            -0.03    0.03      -0.07        1.00         0.84
## lrecon.z.gmc           -0.02    0.04      -0.07        0.84         1.00
## galtan.z.gmc           -0.03    0.09      -0.05        0.82         0.57
## galtan_salience.z.gmc   0.00   -0.06      -0.04        0.23        -0.01
## childlessness          -0.07   -0.36       0.01       -0.01        -0.02
##                       galtan.z.gmc galtan_salience.z.gmc childlessness
## gndr.c                       -0.03                  0.00         -0.07
## age10.c                       0.09                 -0.06         -0.36
## minority.c                   -0.05                 -0.04          0.01
## lrgen.z.gmc                   0.82                  0.23         -0.01
## lrecon.z.gmc                  0.57                 -0.01         -0.02
## galtan.z.gmc                  1.00                  0.18         -0.04
## galtan_salience.z.gmc         0.18                  1.00          0.04
## childlessness                -0.04                  0.04          1.00
```

```r
export(corr_matrix,
       "../../results/cors.weighted.pearson.r.xlsx",
       overwrite=T)

corr_matrix.t<-
  (corr_matrix*sqrt(weighted_corr$n.obs-2))/sqrt(1-corr_matrix^2)
```

```
## Warning in sqrt(1 - corr_matrix^2): NaNs produced
```

```r
## Warning in sqrt(1 - corr_matrix^2): NaNs produced

corr_matrix.p<-
  2*(1-pt(abs(corr_matrix.t),df=weighted_corr$n.obs-2))
round(corr_matrix.p,3)
```

```
##                       gndr.c age10.c minority.c lrgen.z.gmc lrecon.z.gmc
## gndr.c                 0.000       0      0.294        0.00        0.000
## age10.c                0.000     NaN      0.000        0.00        0.000
## minority.c             0.294       0      0.000        0.00        0.000
## lrgen.z.gmc            0.000       0      0.000         NaN        0.000
## lrecon.z.gmc           0.000       0      0.000        0.00        0.000
## galtan.z.gmc           0.000       0      0.000        0.00        0.000
## galtan_salience.z.gmc  0.667       0      0.000        0.00        0.032
## childlessness          0.000       0      0.293        0.04        0.009
##                       galtan.z.gmc galtan_salience.z.gmc childlessness
## gndr.c                           0                 0.667         0.000
## age10.c                          0                 0.000         0.000
## minority.c                       0                 0.000         0.293
## lrgen.z.gmc                      0                 0.000         0.040
## lrecon.z.gmc                     0                 0.032         0.009
## galtan.z.gmc                     0                 0.000         0.000
## galtan_salience.z.gmc            0                 0.000         0.000
## childlessness                    0                 0.000         0.000
```

```r
export(corr_matrix.p,
       "../../results/cors.weighted.pearson.r.p.xlsx",
       overwrite=T)
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
##  [1] Hmisc_4.7-0           Formula_1.2-4         survival_3.3-1       
##  [4] lattice_0.20-45       tibble_3.1.6          MetBrewer_0.2.0      
##  [7] ggplot2_3.3.5         vjihelpers_0.0.0.9000 dplyr_1.0.9          
## [10] rio_0.5.29            emmeans_1.7.3         lme4_1.1-29          
## [13] Matrix_1.4-1          knitr_1.39            rmarkdown_2.14       
## 
## loaded via a namespace (and not attached):
##  [1] sass_0.4.1          jsonlite_1.8.0      splines_4.2.0      
##  [4] bslib_0.3.1         highr_0.9           latticeExtra_0.6-29
##  [7] cellranger_1.1.0    yaml_2.3.5          pillar_1.7.0       
## [10] backports_1.4.1     glue_1.6.2          digest_0.6.29      
## [13] RColorBrewer_1.1-3  checkmate_2.1.0     minqa_1.2.4        
## [16] colorspace_2.0-3    htmltools_0.5.2     pkgconfig_2.0.3    
## [19] haven_2.5.0         purrr_0.3.4         xtable_1.8-4       
## [22] mvtnorm_1.1-3       scales_1.2.0        jpeg_0.1-9         
## [25] openxlsx_4.2.5      htmlTable_2.4.0     farver_2.1.0       
## [28] generics_0.1.2      ellipsis_0.3.2      withr_2.5.0        
## [31] nnet_7.3-17         cli_3.3.0           magrittr_2.0.3     
## [34] crayon_1.5.1        readxl_1.4.0        estimability_1.3   
## [37] evaluate_0.15       fansi_1.0.3         nlme_3.1-157       
## [40] MASS_7.3-56         forcats_0.5.1       foreign_0.8-82     
## [43] tools_4.2.0         data.table_1.14.2   hms_1.1.1          
## [46] lifecycle_1.0.1     stringr_1.4.0       munsell_0.5.0      
## [49] cluster_2.1.3       zip_2.2.0           compiler_4.2.0     
## [52] jquerylib_0.1.4     rlang_1.0.2         grid_4.2.0         
## [55] nloptr_2.0.0        rstudioapi_0.13     htmlwidgets_1.5.4  
## [58] labeling_0.4.2      base64enc_0.1-3     boot_1.3-28        
## [61] gtable_0.3.0        curl_4.3.2          R6_2.5.1           
## [64] gridExtra_2.3       fastmap_1.1.0       utf8_1.2.2         
## [67] stringi_1.7.6       Rcpp_1.0.8.3        vctrs_0.4.1        
## [70] rpart_4.1.16        png_0.1-7           tidyselect_1.1.2   
## [73] xfun_0.30           coda_0.19-4
```
