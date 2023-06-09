---
title: "Merging ESS and CHES by vote"
output: 
  html_document: 
    toc: yes
    number_sections: yes
    keep_md: yes
---



# Preparations

## Packages


```r
library(dplyr)
library(rio)
library(sjlabelled)
```

## Import ESS and CHES data with ESS party keys


```r
ESS7<-
  import("../../data/raw/ESS7e02_2.sav")

CHES_2014.vote.keys<-
  import("../../data/processed/CHES_2014.vote.keys.xlsx")
```

# Construct single vote party number variable in ESS


```r
# use vote.dat for that

vote.dat<-
  import("../../data/processed/vote.dat.xlsx")


# obtain cote variable names in ESS
vote.vars<-unique(vote.dat$vote.var)

# exclude unused vote variable

# Germany second variable
vote.vars<-vote.vars[-which(vote.vars=="prtvede2")]

# Lithuania second and third variables

vote.vars<-vote.vars[-which(vote.vars=="prtvalt2")]
vote.vars<-vote.vars[-which(vote.vars=="prtvalt3")]

# Israel entirely
vote.vars<-vote.vars[-which(vote.vars=="prtvtcil")]

vote.vars
```

```
##  [1] "prtvtbat" "prtvtcbe" "prtvtech" "prtvtdcz" "prtvede1" "prtvtcdk"
##  [7] "prtvteee" "prtvtces" "prtvtcfi" "prtvtcfr" "prtvtbgb" "prtvtehu"
## [13] "prtvtaie" "prtvalt1" "prtvtfnl" "prtvtbno" "prtvtcpl" "prtvtbpt"
## [19] "prtvtbse" "prtvtesi"
```

```r
# check if the remaining variable names are found in ESS

vote.vars %in% names(ESS7)
```

```
##  [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
## [16] TRUE TRUE TRUE TRUE TRUE
```

```r
# sum across the voting variables

ESS7$pt.nmbr<-rowSums(ESS7[,vote.vars],na.rm=T)
table(ESS7$pt.nmbr,useNA="always")
```

```
## 
##     0     1     2     3     4     5     6     7     8     9    10    11    12 
## 17379  4819  4501  2443  2076  1715  1405  1410  1107   690   831   400   426 
##    13    14    15    16    17    18    19    44    55  <NA> 
##   500   195    87    69    46    29    18    14    25     0
```

```r
# code zeros as NA

ESS7$pt.nmbr<-ifelse(ESS7$pt.nmbr==0,NA,ESS7$pt.nmbr)
table(ESS7$pt.nmbr,useNA="always")
```

```
## 
##     1     2     3     4     5     6     7     8     9    10    11    12    13 
##  4819  4501  2443  2076  1715  1405  1410  1107   690   831   400   426   500 
##    14    15    16    17    18    19    44    55  <NA> 
##   195    87    69    46    29    18    14    25 17379
```

# Combine the election coalitions in CHES ratings


```r
# first select only the variables of interest

PI.vars<-c("lrgen","lrecon","galtan")
PP.vars<-c("spendvtax","deregulation","redistribution",
           "econ_interven","civlib_laworder","sociallifestyle",
           "religious_principle","immigrate_policy","multiculturalism",
           "urban_rural","environment","regions","international_security",
           "ethnic_minorities","nationalism")
SA.vars<-c("lrecon_salience","galtan_salience",
           "antielite_salience","corrupt_salience")

CHES_2014.vote.keys.combined<-CHES_2014.vote.keys %>%
  filter(vote.var!="prtvede2" & 
           vote.var!="prtvalt2" & 
           vote.var!="prtvalt3" & 
           cntry!="IL") %>%
  dplyr::select(cntry,pt.nmbr,vote.var,pt.name,
                cname,party_name,party_id,
                all_of(PI.vars),all_of(PP.vars),all_of(SA.vars))

# group duplicates (coalitions) based on ESS names and calculate averages
CHES_2014.vote.keys.combined<-CHES_2014.vote.keys.combined %>%
  group_by(cntry,pt.name) %>%
  summarise(pt.nmbr=mean(pt.nmbr),
            lrgen=mean(lrgen),
            lrecon=mean(lrecon),
            galtan=mean(galtan),
            spendvtax=mean(spendvtax),
            deregulation=mean(deregulation),
            redistribution=mean(redistribution),
            econ_interven=mean(econ_interven),
            civlib_laworder=mean(civlib_laworder),
            sociallifestyle=mean(sociallifestyle),
            religious_principle=mean(religious_principle),
            immigrate_policy=mean(immigrate_policy),
            multiculturalism=mean(multiculturalism),
            urban_rural=mean(urban_rural),
            environment=mean(environment),
            regions=mean(regions),
            international_security=mean(international_security),
            ethnic_minorities=mean(ethnic_minorities),
            nationalism=mean(nationalism),
            lrecon_salience=mean(lrecon_salience),
            galtan_salience=mean(galtan_salience),
            antielite_salience=mean(antielite_salience),
            corrupt_salience=mean(corrupt_salience))
```

```
## `summarise()` has grouped output by 'cntry'. You can override using the
## `.groups` argument.
```

# Merge the files by country and party number


```r
dat<-left_join(
  x=ESS7,
  y=CHES_2014.vote.keys.combined,
  by=c("cntry","pt.nmbr")
)
```



## Test if the merge was successful


```r
for (i in 1:length(vote.vars)){

  vote.var<-vote.vars[i]
  country<-
    vote.dat[vote.dat$vote.var==vote.var,"cntry"][1]
  
  print(country)
  
  tmp.dat<-dat %>%
    filter(cntry==country)
  
  name1<-as.character(as_label(tmp.dat[,vote.var]))
  name2<-as.character(tmp.dat[,"pt.name"])

  
  for (j in 1:length(unique(name1))){
    print(c(unique(name1)[j],
            name2[which(name1==unique(name1)[j])[1]]))
  }
  
}
```

```
## [1] "AT"
## [1] "<dbl>" NA     
## [1] "BE"
## [1] "<dbl>"    "Open VLD"
## [1] "CH"
## [1] "<dbl>" NA     
## [1] "CZ"
## [1] "<dbl>" NA     
## [1] "DE"
## [1] "<dbl>" "SPD"  
## [1] "DK"
## [1] "<dbl>" NA     
## [1] "EE"
## [1] "<dbl>"             "Eesti Keskerakond"
## [1] "ES"
## [1] "<dbl>"  "AMAIUR"
## [1] "FI"
## [1] "<dbl>"            "The Centre Party"
## [1] "FR"
## [1] "<dbl>" NA     
## [1] "GB"
## [1] "<dbl>"            "Liberal Democrat"
## [1] "HU"
## [1] "<dbl>" NA     
## [1] "IE"
## [1] "<dbl>"  "Labour"
## [1] "LT"
## [1] "<dbl>"                                    
## [2] "Lithuanian Social Democratic Party (LSDP)"
## [1] "NL"
## [1] "<dbl>" NA     
## [1] "NO"
## [1] "<dbl>"                "Progress Party (FRP)"
## [1] "PL"
## [1] "<dbl>" NA     
## [1] "PT"
## [1] "<dbl>" NA     
## [1] "SE"
## [1] "<dbl>"                    "Moderata samlingspartiet"
## [1] "SI"
## [1] "<dbl>" NA
```

Looks good!

# Export the combined party data and the entire long format dataset


```r
export(CHES_2014.vote.keys.combined,
       "../../data/processed/CHES_2014.vote.keys.combined.xlsx",
       overwrite=T)

export(dat,"../../data/processed/dat.xlsx",
       overwrite=T)
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
## [1] dplyr_1.1.2      sjlabelled_1.2.0 rio_0.5.29       knitr_1.42      
## [5] rmarkdown_2.21  
## 
## loaded via a namespace (and not attached):
##  [1] jsonlite_1.8.4    compiler_4.3.0    tidyselect_1.2.0  Rcpp_1.0.10      
##  [5] zip_2.3.0         jquerylib_0.1.4   yaml_2.3.7        fastmap_1.1.1    
##  [9] readxl_1.4.2      readr_2.1.4       R6_2.5.1          generics_0.1.3   
## [13] curl_5.0.0        openxlsx_4.2.5.2  forcats_1.0.0     tibble_3.2.1     
## [17] insight_0.19.2    tzdb_0.4.0        bslib_0.4.2       pillar_1.9.0     
## [21] rlang_1.1.1       utf8_1.2.3        cachem_1.0.8      stringi_1.7.12   
## [25] xfun_0.39         sass_0.4.6        cli_3.6.1         withr_2.5.0      
## [29] magrittr_2.0.3    digest_0.6.31     rstudioapi_0.14   haven_2.5.2      
## [33] hms_1.1.3         lifecycle_1.0.3   vctrs_0.6.2       evaluate_0.21    
## [37] glue_1.6.2        data.table_1.14.8 cellranger_1.1.0  fansi_1.0.4      
## [41] foreign_0.8-84    tools_4.3.0       pkgconfig_2.0.3   htmltools_0.5.5
```
