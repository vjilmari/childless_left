---
title: "Variable transformation before running the analysis"
output: 
  html_document: 
    toc: yes
    number_sections: yes
    keep_md: yes
---



# Preparations

## Load packages


```r
library(rio)
```

```
## The following rio suggested packages are not installed: 'arrow', 'feather', 'fst', 'hexView', 'pzfx', 'readODS', 'rmatio'
## Use 'install_formats()' to install them
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

## Load data


```r
# Long format data with ESS and CHES merged
dat<-import("../../data/processed/dat.xlsx")

# ESS raw data from which variable labels can be obtained
ESS.dat<-import("../../data/raw/ESS7e02_2.sav")
```

# Variable transformations

## Gender


```r
attr(ESS.dat$gndr,"labels")
```

```
##      Male    Female No answer 
##         1         2         9
```

```r
# Factorial gndr

dat$gndr.f<-case_when(dat$gndr==1~"Male",
                      dat$gndr==2~"Female",
                      TRUE~NA_character_)

table(dat$gndr.f,useNA="always")
```

```
## 
## Female   Male   <NA> 
##  21292  18871     22
```

```r
# Numerical gndr

dat$gndr.c<-case_when(dat$gndr==1~-0.5,
                      dat$gndr==2~0.5,
                      TRUE~NA_real_)

table(dat$gndr.c,useNA="always")
```

```
## 
##  -0.5   0.5  <NA> 
## 18871 21292    22
```

## Age


```r
attr(ESS.dat$agea,"labels")
```

```
## Not available 
##           999
```

```r
table(dat$agea==999)
```

```
## 
## FALSE 
## 40086
```

```r
# centered age divided by 10
dat$age10.c<-(dat$agea-mean(dat$agea,na.rm=T))/10
```

## Income


```r
attr(ESS.dat$hinctnta,"labels")
```

```
##  J - 1st decile  R - 2nd decile  C - 3rd decile  M - 4th decile 
##               1               2               3               4 
##  F - 5th decile  S - 6th decile  K - 7th decile  P - 8th decile 
##               5               6               7               8 
##  D - 9th decile H - 10th decile         Refusal      Don't know 
##               9              10              77              88 
##       No answer 
##              99
```

```r
# recode deciles to quintiles

dat$income<-case_when(
  dat$hinctnta==1 | dat$hinctnta==2 ~ "quint.1",
  dat$hinctnta==3 | dat$hinctnta==4 ~ "quint.2",
  dat$hinctnta==5 | dat$hinctnta==6 ~ "quint.3",
  dat$hinctnta==7 | dat$hinctnta==8 ~ "quint.4",
  dat$hinctnta==9 | dat$hinctnta==10 ~ "quint.5"
)

table(dat$income,useNA="always")
```

```
## 
## quint.1 quint.2 quint.3 quint.4 quint.5    <NA> 
##    6427    6999    6793    6408    5262    8296
```

```r
# add missing as additional factor level (to a new variable income.f)

dat$income.f<-case_when(
  is.na(dat$income) ~ "missing",
  TRUE ~ dat$income
)

#define reference level (top quintile)
table(dat$income.f,useNA="always")
```

```
## 
## missing quint.1 quint.2 quint.3 quint.4 quint.5    <NA> 
##    8296    6427    6999    6793    6408    5262       0
```

```r
dat$income.fr = relevel(as.factor(dat$income.f),
                        ref="quint.5")
table(dat$income.fr,useNA="always")
```

```
## 
## quint.5 missing quint.1 quint.2 quint.3 quint.4    <NA> 
##    5262    8296    6427    6999    6793    6408       0
```

## Education


```r
attr(ESS.dat$eisced,"labels")
```

```
##             Not possible to harmonise into ES-ISCED 
##                                                   0 
##              ES-ISCED I , less than lower secondary 
##                                                   1 
##                        ES-ISCED II, lower secondary 
##                                                   2 
##           ES-ISCED IIIb, lower tier upper secondary 
##                                                   3 
##           ES-ISCED IIIa, upper tier upper secondary 
##                                                   4 
##        ES-ISCED IV, advanced vocational, sub-degree 
##                                                   5 
##     ES-ISCED V1, lower tertiary education, BA level 
##                                                   6 
## ES-ISCED V2, higher tertiary education, >= MA level 
##                                                   7 
##                                               Other 
##                                                  55 
##                                             Refusal 
##                                                  77 
##                                          Don't know 
##                                                  88 
##                                           No answer 
##                                                  99
```

```r
# recode education variable

dat$edu<-case_when(dat$eisced==0~NA_character_,
                   dat$eisced==1~"1. <LS",
                   dat$eisced==2~"2. LS",
                   dat$eisced==3~"3. LUS",
                   dat$eisced==4~"4. UUS",
                   dat$eisced==5~"5. AV",
                   dat$eisced==6~"6. BA",
                   dat$eisced==7~"7. MA",
                   TRUE~NA_character_)

table(dat$edu,useNA="always")
```

```
## 
## 1. <LS  2. LS 3. LUS 4. UUS  5. AV  6. BA  7. MA   <NA> 
##   4085   6760   7213   7094   5671   4366   4730    266
```

```r
# recode reference education (highest) to a new variable (edu.f)

dat$edu.f<-relevel(as.factor(dat$edu),ref="7. MA")
table(dat$edu.f)
```

```
## 
##  7. MA 1. <LS  2. LS 3. LUS 4. UUS  5. AV  6. BA 
##   4730   4085   6760   7213   7094   5671   4366
```

```r
# code binary (college=0.5, no college=-0.5)

dat$edu.c<-case_when(dat$eisced==0~NA_real_,
                   dat$eisced>1 & dat$eisced<6~(-0.5),
                   dat$eisced>5 & dat$eisced<8~0.5,
                   TRUE~NA_real_)

table(dat$edu.c,useNA="always")
```

```
## 
##  -0.5   0.5  <NA> 
## 26738  9096  4351
```

## Political orientation


```r
#add scaling SDs to the data.frame from CHES dataset
CHES_2014<-
  import("../../data/raw/2014_CHES_dataset_means.csv")

#scale each variable with mean and sd from CHES

PI.vars<-c("lrgen","lrecon","galtan")
PP.vars<-c("spendvtax","deregulation","redistribution",
           "econ_interven","civlib_laworder","sociallifestyle",
           "religious_principle","immigrate_policy","multiculturalism",
           "urban_rural","environment","regions","international_security",
           "ethnic_minorities","nationalism")
SA.vars<-c("lrecon_salience","galtan_salience",
           "antielite_salience","corrupt_salience")
CHES.vars<-c(PI.vars,PP.vars,SA.vars)


dat$lrgen.z<-(dat[,CHES.vars[1]]-mean(CHES_2014[,CHES.vars[1]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[1]],na.rm=T)

dat$lrecon.z<-(dat[,CHES.vars[2]]-mean(CHES_2014[,CHES.vars[2]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[2]],na.rm=T)

dat$galtan.z<-(dat[,CHES.vars[3]]-mean(CHES_2014[,CHES.vars[3]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[3]],na.rm=T)

dat$spendvtax.z<-(dat[,CHES.vars[4]]-mean(CHES_2014[,CHES.vars[4]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[4]],na.rm=T)

dat$deregulation.z<-(dat[,CHES.vars[5]]-
                       mean(CHES_2014[,CHES.vars[5]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[5]],na.rm=T)

dat$redistribution.z<-(dat[,CHES.vars[6]]-
                         mean(CHES_2014[,CHES.vars[6]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[6]],na.rm=T)

dat$econ_interven.z<-(dat[,CHES.vars[7]]-
                        mean(CHES_2014[,CHES.vars[7]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[7]],na.rm=T)

dat$civlib_laworder.z<-(dat[,CHES.vars[8]]-
                          mean(CHES_2014[,CHES.vars[8]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[8]],na.rm=T)

dat$sociallifestyle.z<-(dat[,CHES.vars[9]]-
                          mean(CHES_2014[,CHES.vars[9]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[9]],na.rm=T)

dat$religious_principle.z<-(dat[,CHES.vars[10]]-
                              mean(CHES_2014[,CHES.vars[10]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[10]],na.rm=T)

dat$immigrate_policy.z<-(dat[,CHES.vars[11]]-
                           mean(CHES_2014[,CHES.vars[11]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[11]],na.rm=T)

dat$multiculturalism.z<-(dat[,CHES.vars[12]]-
                           mean(CHES_2014[,CHES.vars[12]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[12]],na.rm=T)

dat$urban_rural.z<-(dat[,CHES.vars[13]]-
                      mean(CHES_2014[,CHES.vars[13]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[13]],na.rm=T)

dat$environment.z<-(dat[,CHES.vars[14]]-
                      mean(CHES_2014[,CHES.vars[14]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[14]],na.rm=T)

dat$regions.z<-(dat[,CHES.vars[15]]-mean(CHES_2014[,CHES.vars[15]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[15]],na.rm=T)

dat$international_security.z<-(dat[,CHES.vars[16]]-
                                 mean(CHES_2014[,CHES.vars[16]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[16]],na.rm=T)

dat$ethnic_minorities.z<-(dat[,CHES.vars[17]]-
                            mean(CHES_2014[,CHES.vars[17]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[17]],na.rm=T)

dat$nationalism.z<-(dat[,CHES.vars[18]]-
                      mean(CHES_2014[,CHES.vars[18]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[18]],na.rm=T)

dat$lrecon_salience.z<-(dat[,CHES.vars[19]]-
                          mean(CHES_2014[,CHES.vars[19]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[19]],na.rm=T)

dat$galtan_salience.z<-(dat[,CHES.vars[20]]-
                          mean(CHES_2014[,CHES.vars[20]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[20]],na.rm=T)

dat$antielite_salience.z<-(dat[,CHES.vars[21]]-
                             mean(CHES_2014[,CHES.vars[21]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[21]],na.rm=T)

dat$corrupt_salience.z<-(dat[,CHES.vars[22]]-
                             mean(CHES_2014[,CHES.vars[22]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[22]],na.rm=T)
```

## Vote


```r
table(dat$vote)
```

```
## 
##     1     2     3 
## 27867  8492  3498
```

```r
attributes(ESS.dat$vote)
```

```
## $label
## [1] "Voted last national election"
## 
## $format.spss
## [1] "F1.0"
## 
## $display_width
## [1] 6
## 
## $labels
##                  Yes                   No Not eligible to vote 
##                    1                    2                    3 
##              Refusal           Don't know            No answer 
##                    7                    8                    9
```

```r
dat$vote.c<-case_when(dat$vote==1~0.5,
                      dat$vote==2~(-0.5),
                      TRUE~NA_real_)
table(dat$vote.c)
```

```
## 
##  -0.5   0.5 
##  8492 27867
```

## Childlessness

It was necessary to revise this from the preregistered, because there were over 14000 missing values in chldhhe based on the answers on the previous question (chldhm = Children living at home or not)


```r
attributes(ESS.dat$chldhhe)
```

```
## $label
## [1] "Ever had children living in household"
## 
## $format.spss
## [1] "F1.0"
## 
## $display_width
## [1] 9
## 
## $labels
##            Yes             No Not applicable        Refusal     Don't know 
##              1              2              6              7              8 
##      No answer 
##              9
```

```r
table(ESS.dat$chldhhe,useNA="always")
```

```
## 
##     1     2  <NA> 
## 13347 12299 14539
```

```r
attributes(ESS.dat$chldhm)
```

```
## $label
## [1] "Children living at home or not"
## 
## $format.spss
## [1] "F1.0"
## 
## $labels
## Respondent lives with children at household grid 
##                                                1 
##                                         Does not 
##                                                2 
##                                    Not available 
##                                                9
```

```r
table(ESS.dat$chldhm,useNA="always")
```

```
## 
##     1     2  <NA> 
## 14310 25870     5
```

```r
table(ESS.dat$chldhm,ESS.dat$chldhhe,useNA="always")
```

```
##       
##            1     2  <NA>
##   1        6     4 14300
##   2    13341 12295   234
##   <NA>     0     0     5
```

```r
dat$childlessness<-case_when(
  dat$chldhhe==2  & dat$chldhm==2~1,
  dat$chldhhe==1 | dat$chldhm==1 ~0,
  TRUE~NA_real_
)

table(dat$childlessness,useNA="always")
```

```
## 
##     0     1  <NA> 
## 27651 12295   239
```

```r
table(dat$cntry,dat$childlessness,useNA="always")
```

```
##       
##           0    1 <NA>
##   AT   1109  686    0
##   BE   1190  565   14
##   CH    903  617   12
##   CZ   1509  597   42
##   DE   2005 1032    8
##   DK   1062  435    5
##   EE   1575  474    2
##   ES   1254  658   13
##   FI   1366  720    1
##   FR   1416  493    8
##   GB   1643  619    2
##   HU   1199  498    1
##   IE   1507  858   25
##   IL   1776  727   59
##   LT   1684  559    7
##   NL   1304  614    1
##   NO    970  465    1
##   PL   1144  465    6
##   PT    917  348    0
##   SE   1248  542    1
##   SI    870  323   31
##   <NA>    0    0    0
```

## Belong to minor ethnic group


```r
attributes(ESS.dat$blgetmg)
```

```
## $label
## [1] "Belong to minority ethnic group in country"
## 
## $format.spss
## [1] "F1.0"
## 
## $display_width
## [1] 9
## 
## $labels
##        Yes         No    Refusal Don't know  No answer 
##          1          2          7          8          9
```

```r
table(ESS.dat$blgetmg,useNA="always")
```

```
## 
##     1     2  <NA> 
##  2568 37085   532
```

```r
dat$minority<-case_when(
  dat$blgetmg==1~1,
  dat$blgetmg==2~0,
  TRUE~NA_real_
)

dat$minority.c<-dat$minority-0.5
table(dat$minority.c)
```

```
## 
##  -0.5   0.5 
## 37085  2568
```

## Left-Right self-placement


```r
attributes(ESS.dat$lrscale)
```

```
## $label
## [1] "Placement on left right scale"
## 
## $format.spss
## [1] "F2.0"
## 
## $display_width
## [1] 9
## 
## $labels
##       Left          1          2          3          4          5          6 
##          0          1          2          3          4          5          6 
##          7          8          9      Right    Refusal Don't know  No answer 
##          7          8          9         10         77         88         99
```

```r
table(dat$lrscale,useNA="always")
```

```
## 
##     0     1     2     3     4     5     6     7     8     9    10  <NA> 
##  1357   907  2062  3788  3575 11557  3573  3790  2984   997  1265  4330
```

```r
dat$lrscale.z<-scale(dat$lrscale,center=T,scale=T)
```

## Reducing income differences


```r
attributes(ESS.dat$gincdif)
```

```
## $label
## [1] "Government should reduce differences in income levels"
## 
## $format.spss
## [1] "F1.0"
## 
## $display_width
## [1] 9
## 
## $labels
##             Agree strongly                      Agree 
##                          1                          2 
## Neither agree nor disagree                   Disagree 
##                          3                          4 
##          Disagree strongly                    Refusal 
##                          5                          7 
##                 Don't know                  No answer 
##                          8                          9
```

```r
table(dat$gincdif,useNA="always")
```

```
## 
##     1     2     3     4     5  <NA> 
## 12778 15801  5748  4101  1051   706
```

```r
# reverse code so that high number indicates endorsement
dat$gincdif.R<-(6-dat$gincdif)
table(dat$gincdif.R,useNA="always")
```

```
## 
##     1     2     3     4     5  <NA> 
##  1051  4101  5748 15801 12778   706
```

```r
dat$gincdif.R.z<-scale(dat$gincdif.R,center=T,scale=T)
```

## Gay rights


```r
attributes(ESS.dat$freehms)
```

```
## $label
## [1] "Gays and lesbians free to live life as they wish"
## 
## $format.spss
## [1] "F1.0"
## 
## $display_width
## [1] 9
## 
## $labels
##             Agree strongly                      Agree 
##                          1                          2 
## Neither agree nor disagree                   Disagree 
##                          3                          4 
##          Disagree strongly                    Refusal 
##                          5                          7 
##                 Don't know                  No answer 
##                          8                          9
```

```r
table(dat$freehms,useNA="always")
```

```
## 
##     1     2     3     4     5  <NA> 
## 15805 12716  5056  2913  2521  1174
```

```r
# reverse code so that high number indicates endorsement
dat$freehms.R<-(6-dat$freehms)
table(dat$freehms.R,useNA="always")
```

```
## 
##     1     2     3     4     5  <NA> 
##  2521  2913  5056 12716 15805  1174
```

```r
dat$freehms.R.z<-scale(dat$freehms.R,center=T,scale=T)
```

## Immigration


```r
attributes(ESS.dat$imwbcnt)
```

```
## $label
## [1] "Immigrants make country worse or better place to live"
## 
## $format.spss
## [1] "F2.0"
## 
## $display_width
## [1] 9
## 
## $labels
##  Worse place to live                    1                    2 
##                    0                    1                    2 
##                    3                    4                    5 
##                    3                    4                    5 
##                    6                    7                    8 
##                    6                    7                    8 
##                    9 Better place to live              Refusal 
##                    9                   10                   77 
##           Don't know            No answer 
##                   88                   99
```

```r
table(dat$imwbcnt,useNA="always")
```

```
## 
##     0     1     2     3     4     5     6     7     8     9    10  <NA> 
##  1705  1101  2403  3637  4015 11809  4087  4305  3340  1087  1175  1521
```

```r
dat$imwbcnt.z<-scale(dat$imwbcnt,center=T,scale=T)
```

## Shared traditions


```r
attributes(ESS.dat$pplstrd)
```

```
## $label
## [1] "Better for a country if almost everyone shares customs and traditions"
## 
## $format.spss
## [1] "F1.0"
## 
## $display_width
## [1] 9
## 
## $labels
##             Agree strongly                      Agree 
##                          1                          2 
## Neither agree nor disagree                   Disagree 
##                          3                          4 
##          Disagree strongly                    Refusal 
##                          5                          7 
##                 Don't know                  No answer 
##                          8                          9
```

```r
table(dat$pplstrd,useNA="always")
```

```
## 
##     1     2     3     4     5  <NA> 
##  4937 12467 11509  8475  2145   652
```

```r
# reverse code so that high number indicates endorsement
dat$pplstrd.R<-(6-dat$pplstrd)
table(dat$pplstrd.R,useNA="always")
```

```
## 
##     1     2     3     4     5  <NA> 
##  2145  8475 11509 12467  4937   652
```

```r
dat$pplstrd.R.z<-scale(dat$pplstrd.R,center=T,scale=T)
```

## Religiousness


```r
attributes(ESS.dat$rlgdgr)
```

```
## $label
## [1] "How religious are you"
## 
## $format.spss
## [1] "F2.0"
## 
## $labels
## Not at all religious                    1                    2 
##                    0                    1                    2 
##                    3                    4                    5 
##                    3                    4                    5 
##                    6                    7                    8 
##                    6                    7                    8 
##                    9       Very religious              Refusal 
##                    9                   10                   77 
##           Don't know            No answer 
##                   88                   99
```

```r
table(dat$rlgdgr,useNA="always")
```

```
## 
##    0    1    2    3    4    5    6    7    8    9   10 <NA> 
## 7454 2465 2998 3198 2669 6220 3647 4142 3547 1483 2072  290
```

```r
dat$rlgdgr.z<-scale(dat$rlgdgr,center=T,scale=T)
```

# Final set of variables needed for the analysis


```r
analysis.vars<-
  c("idno","cntry",
    "dweight","pspwght","pweight",
    "pt.nmbr","pt.name",
    "gndr.f","gndr.c","agea","age10.c",
    "income","income.f","income.fr",
    "edu","edu.f","edu.c",
    "vote","vote.c",
    all_of(CHES.vars),
    all_of(paste0(CHES.vars,".z")),
    "childlessness",
    "minority",
    "minority.c",
    "lrscale","lrscale.z",
    "gincdif","gincdif.R","gincdif.R.z",
    "freehms","freehms.R","freehms.R.z",
    "imwbcnt","imwbcnt.z",
    "pplstrd","pplstrd.R","pplstrd.R.z",
    "rlgdgr","rlgdgr.z")
```

```
## Warning: Using `all_of()` outside of a selecting function was deprecated in tidyselect
## 1.2.0.
## ℹ See details at
##   <https://tidyselect.r-lib.org/reference/faq-selection-context.html>
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

```r
# test if they are all in the data file
analysis.vars %in% names(dat)
```

```
##  [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
## [16] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
## [31] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
## [46] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
## [61] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
## [76] TRUE TRUE TRUE TRUE TRUE TRUE
```

```r
# exclude variable not needed
fdat<-dat[,analysis.vars]
str(fdat)
```

```
## 'data.frame':	40185 obs. of  81 variables:
##  $ idno                    : num  1 2 3 4 5 6 7 13 14 21 ...
##  $ cntry                   : chr  "AT" "AT" "AT" "AT" ...
##  $ dweight                 : num  0.938 0.938 0.938 0.938 0.938 ...
##  $ pspwght                 : num  0.871 0.864 1.419 1.026 0.739 ...
##  $ pweight                 : num  0.406 0.406 0.406 0.406 0.406 ...
##  $ pt.nmbr                 : num  NA 6 2 3 NA 1 1 1 7 2 ...
##  $ pt.name                 : chr  NA NA "ÖVP" "FPÖ" ...
##  $ gndr.f                  : chr  "Male" "Male" "Female" "Male" ...
##  $ gndr.c                  : num  -0.5 -0.5 0.5 -0.5 0.5 0.5 -0.5 0.5 0.5 0.5 ...
##  $ agea                    : num  51 67 89 32 56 67 66 67 34 66 ...
##  $ age10.c                 : num  0.172 1.772 3.972 -1.728 0.672 ...
##  $ income                  : chr  "quint.2" "quint.2" "quint.1" "quint.2" ...
##  $ income.f                : chr  "quint.2" "quint.2" "quint.1" "quint.2" ...
##  $ income.fr               : Factor w/ 6 levels "quint.5","missing",..: 4 4 3 4 6 5 4 4 4 3 ...
##  $ edu                     : chr  "3. LUS" "1. <LS" "1. <LS" "3. LUS" ...
##  $ edu.f                   : Factor w/ 7 levels "7. MA","1. <LS",..: 4 2 2 4 4 4 4 6 1 4 ...
##  $ edu.c                   : num  -0.5 NA NA -0.5 -0.5 -0.5 -0.5 -0.5 0.5 -0.5 ...
##  $ vote                    : num  1 1 1 1 1 1 1 1 1 1 ...
##  $ vote.c                  : num  0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 ...
##  $ lrgen                   : num  NA NA 6.1 8.7 NA ...
##  $ lrecon                  : num  NA NA 6.4 5.5 NA ...
##  $ galtan                  : num  NA NA 7.2 8.8 NA ...
##  $ spendvtax               : num  NA NA 6.2 5.6 NA ...
##  $ deregulation            : num  NA NA 5.6 5.2 NA ...
##  $ redistribution          : num  NA NA 6.3 4.7 NA ...
##  $ econ_interven           : num  NA NA 5.67 4.89 NA ...
##  $ civlib_laworder         : num  NA NA 6 8.5 NA ...
##  $ sociallifestyle         : num  NA NA 6.78 8.89 NA ...
##  $ religious_principle     : num  NA NA 7.6 6.3 NA ...
##  $ immigrate_policy        : num  NA NA 6.11 9.89 NA ...
##  $ multiculturalism        : num  NA NA 7 9.9 NA ...
##  $ urban_rural             : num  NA NA 8 4.9 NA ...
##  $ environment             : num  NA NA 5.8 6 NA ...
##  $ regions                 : num  NA NA 3.5 5.44 NA ...
##  $ international_security  : num  NA NA 5 7.5 NA ...
##  $ ethnic_minorities       : num  NA NA 5.9 8.8 NA ...
##  $ nationalism             : num  NA NA 6.2 9.4 NA ...
##  $ lrecon_salience         : num  NA NA 7.6 5 NA ...
##  $ galtan_salience         : num  NA NA 5.3 7 NA ...
##  $ antielite_salience      : num  NA NA 1.6 8 NA ...
##  $ corrupt_salience        : num  NA NA 2.4 5.1 NA ...
##  $ lrgen.z                 : num  NA NA 0.34 1.47 NA ...
##  $ lrecon.z                : num  NA NA 0.672 0.265 NA ...
##  $ galtan.z                : num  NA NA 0.795 1.404 NA ...
##  $ spendvtax.z             : num  NA NA 0.665 0.371 NA ...
##  $ deregulation.z          : num  NA NA 0.334 0.155 NA ...
##  $ redistribution.z        : num  NA NA 0.825 0.0702 NA ...
##  $ econ_interven.z         : num  NA NA 0.4459 0.0883 NA ...
##  $ civlib_laworder.z       : num  NA NA 0.366 1.429 NA ...
##  $ sociallifestyle.z       : num  NA NA 0.753 1.51 NA ...
##  $ religious_principle.z   : num  NA NA 1.248 0.749 NA ...
##  $ immigrate_policy.z      : num  NA NA 0.359 1.942 NA ...
##  $ multiculturalism.z      : num  NA NA 0.681 1.866 NA ...
##  $ urban_rural.z           : num  NA NA 1.7827 0.0672 NA ...
##  $ environment.z           : num  NA NA 0.282 0.379 NA ...
##  $ regions.z               : num  NA NA -0.573 0.531 NA ...
##  $ international_security.z: num  NA NA -0.000678 1.323276 NA ...
##  $ ethnic_minorities.z     : num  NA NA 0.411 1.549 NA ...
##  $ nationalism.z           : num  NA NA 0.319 1.597 NA ...
##  $ lrecon_salience.z       : num  NA NA 0.446 -1.324 NA ...
##  $ galtan_salience.z       : num  NA NA -0.385 0.71 NA ...
##  $ antielite_salience.z    : num  NA NA -1.26 1.21 NA ...
##  $ corrupt_salience.z      : num  NA NA -1.1205 0.0423 NA ...
##  $ childlessness           : num  1 0 0 1 0 1 0 0 1 1 ...
##  $ minority                : num  0 0 0 0 0 0 0 1 0 0 ...
##  $ minority.c              : num  -0.5 -0.5 -0.5 -0.5 -0.5 -0.5 -0.5 0.5 -0.5 -0.5 ...
##  $ lrscale                 : num  4 0 NA 7 NA 10 5 3 5 4 ...
##  $ lrscale.z               : num [1:40185, 1] -0.481 -2.273 NA 0.863 NA ...
##   ..- attr(*, "scaled:center")= num 5.07
##   ..- attr(*, "scaled:scale")= num 2.23
##  $ gincdif                 : num  1 1 5 2 1 3 1 2 2 2 ...
##  $ gincdif.R               : num  5 5 1 4 5 3 5 4 4 4 ...
##  $ gincdif.R.z             : num [1:40185, 1] 1.052 1.052 -2.741 0.104 1.052 ...
##   ..- attr(*, "scaled:center")= num 3.89
##   ..- attr(*, "scaled:scale")= num 1.05
##  $ freehms                 : num  2 1 3 2 1 2 2 3 1 3 ...
##  $ freehms.R               : num  4 5 3 4 5 4 4 3 5 3 ...
##  $ freehms.R.z             : num [1:40185, 1] 0.057 0.899 -0.785 0.057 0.899 ...
##   ..- attr(*, "scaled:center")= num 3.93
##   ..- attr(*, "scaled:scale")= num 1.19
##  $ imwbcnt                 : num  4 5 1 2 1 5 6 5 3 5 ...
##  $ imwbcnt.z               : num [1:40185, 1] -0.4586 -0.0173 -1.7826 -1.3413 -1.7826 ...
##   ..- attr(*, "scaled:center")= num 5.04
##   ..- attr(*, "scaled:scale")= num 2.27
##  $ pplstrd                 : num  2 1 4 1 NA 3 1 3 2 3 ...
##  $ pplstrd.R               : num  4 5 2 5 NA 3 5 3 4 3 ...
##  $ pplstrd.R.z             : num [1:40185, 1] 0.695 1.613 -1.14 1.613 NA ...
##   ..- attr(*, "scaled:center")= num 3.24
##   ..- attr(*, "scaled:scale")= num 1.09
##  $ rlgdgr                  : num  4 3 4 5 8 2 6 5 4 5 ...
##  $ rlgdgr.z                : num [1:40185, 1] -0.11 -0.435 -0.11 0.214 1.188 ...
##   ..- attr(*, "scaled:center")= num 4.34
##   ..- attr(*, "scaled:scale")= num 3.08
```

```r
# construct analysis weights

fdat$anweight=fdat$pspwght*fdat$pweight

# save the final data file
export(fdat,
       "../../data/processed/fdat.xlsx",overwrite=T)
```
# Session information


```r
s<-sessionInfo()
print(s,locale=F)
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
## [1] dplyr_1.1.2    rio_0.5.29     knitr_1.42     rmarkdown_2.21
## 
## loaded via a namespace (and not attached):
##  [1] jsonlite_1.8.4    crayon_1.5.2      compiler_4.3.0    tidyselect_1.2.0 
##  [5] Rcpp_1.0.10       zip_2.3.0         jquerylib_0.1.4   yaml_2.3.7       
##  [9] fastmap_1.1.1     readxl_1.4.2      readr_2.1.4       R6_2.5.1         
## [13] generics_0.1.3    curl_5.0.0        openxlsx_4.2.5.2  forcats_1.0.0    
## [17] tibble_3.2.1      tzdb_0.4.0        bslib_0.4.2       pillar_1.9.0     
## [21] rlang_1.1.1       utf8_1.2.3        cachem_1.0.8      stringi_1.7.12   
## [25] xfun_0.39         sass_0.4.6        cli_3.6.1         magrittr_2.0.3   
## [29] digest_0.6.31     rstudioapi_0.14   haven_2.5.2       hms_1.1.3        
## [33] lifecycle_1.0.3   vctrs_0.6.2       evaluate_0.21     glue_1.6.2       
## [37] data.table_1.14.8 cellranger_1.1.0  fansi_1.0.4       foreign_0.8-84   
## [41] tools_4.3.0       pkgconfig_2.0.3   htmltools_0.5.5
```
