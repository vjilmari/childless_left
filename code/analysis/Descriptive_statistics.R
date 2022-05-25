#' ---
#' title: "Descriptive statistics"
#' output: 
#'   html_document: 
#'     toc: yes
#'     keep_md: yes
#' date: '2022-05-02'
#' ---
#' 
#' 
## ---- include=FALSE-------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
#' # Preparations
#' 
#' ## Packages
#' 
## -------------------------------------------------------------------------------

library(lme4)
library(emmeans)
library(rio)
library(dplyr)
library(vjihelpers)
library(ggplot2)
library(MetBrewer)
library(tibble)
library(Hmisc)


#' 
#' ## Custom functions
#' 
## -------------------------------------------------------------------------------
source("../custom_functions.R")


#' 
#' ## Data
#' 
## -------------------------------------------------------------------------------
fdat<-import("../../data/processed/fdat.xlsx")

#' 
#' ## Data exclusions
#' 
## -------------------------------------------------------------------------------

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

#' 
#' ## Variable centering
#' 
## -------------------------------------------------------------------------------
exdat<-
  group_mean_center(
    data=exdat,group.var="cntry",
    vars=c("lrgen","lrecon","galtan",
           "lrecon_salience","galtan_salience"),
    grand.init = F)


#' 
#' 
#' ## Variable scaling
#' 
## -------------------------------------------------------------------------------
CHES<-
  import("../../data/raw/2014_CHES_dataset_means.csv")

(sd.lrgen<-sd(CHES$lrgen,na.rm=T))
(sd.lrecon<-sd(CHES$lrecon,na.rm=T))
(sd.galtan<-sd(CHES$galtan,na.rm=T))
(sd.galtan_salience<-sd(CHES$galtan_salience,na.rm=T))
(sd.lrecon_salience<-sd(CHES$lrecon_salience,na.rm=T))

exdat$lrgen.z.gmc<-exdat$lrgen.gmc/sd.lrgen
exdat$lrecon.z.gmc<-exdat$lrecon.gmc/sd.lrecon
exdat$galtan.z.gmc<-exdat$galtan.gmc/sd.galtan
exdat$galtan_salience.z.gmc<-
  exdat$galtan_salience.gmc/sd.galtan_salience
exdat$lrecon_salience.z.gmc<-
  exdat$lrecon_salience.gmc/sd.lrecon_salience

#' 
#' 
#' 
#' # Descriptive statistics
#' 
#' ## Sample 
#' 
## -------------------------------------------------------------------------------
# n countries
length(unique(exdat$cntry))


# n initial observations
nrow(fdat[fdat$cntry!="IL",])

# n who reported voting
table(fdat[fdat$cntry!="IL","vote"],useNA="always")

# n for whom CHES was available

table(rowSums(is.na(fdat[fdat$cntry!="IL",
                   c("galtan","lrgen","lrecon")])),
              useNA="always")

21684/37623
21684-21374

#range between countries
range(table(exdat$cntry))


#' 
#' ## Childlessness
#' 
#' ### Unweighted
#' 
## -------------------------------------------------------------------------------
table(exdat$cntry,exdat$childlessness)
round(100*prop.table(table(exdat$cntry,
                           exdat$childlessness),1),1)

prop.table(table(exdat$childlessness))

#' 
#' ### Weighted
#' 
## -------------------------------------------------------------------------------
exdat %>%
  group_by(cntry) %>%
  count(childlessness,wt=anweight) %>%
  ungroup() %>%
  group_by(cntry) %>%
  mutate(total=sum(n),percentage=round(100*n/total,1)) %>%
  filter(childlessness==1) %>%
  dplyr::select(cntry,percentage)

exdat %>%
  count(childlessness,wt=anweight) %>%
  mutate(total=sum(n),percentage=round(100*n/total,1))

round(weighted.mean(exdat$childlessness,w=exdat$anweight),2)
round(sqrt(wtd.var(exdat$childlessness,w=exdat$anweight)),2)

#' 
#' ## Covariates
#' 
#' ### Age
#' 
## -------------------------------------------------------------------------------
round(weighted.mean(exdat$agea,w=exdat$anweight),2)
round(sqrt(wtd.var(exdat$agea,w=exdat$anweight)),2)

#' 
#' ### Gender
#' 
## -------------------------------------------------------------------------------
exdat %>%
  count(gndr.f,wt=anweight) %>%
  mutate(total=sum(n),percentage=round(100*n/total,1))

round(weighted.mean(exdat$gndr.c,w=exdat$anweight),2)
round(sqrt(wtd.var(exdat$gndr.c,w=exdat$anweight)),2)

#' 
#' ### Minority ethnic group status
#' 
## -------------------------------------------------------------------------------
exdat %>%
  count(minority,wt=anweight) %>%
  mutate(total=sum(n),percentage=round(100*n/total,1))

round(weighted.mean(exdat$minority.c,w=exdat$anweight),2)
round(sqrt(wtd.var(exdat$minority.c,w=exdat$anweight)),2)

#' 
#' ## Independent variables
#' 
#' ### lrgen
#' 
## -------------------------------------------------------------------------------

round(weighted.mean(exdat$lrgen,w=exdat$anweight),2)
round(sqrt(wtd.var(exdat$lrgen,w=exdat$anweight)),2)

#' 
#' ### lrecon
#' 
## -------------------------------------------------------------------------------

round(weighted.mean(exdat$lrecon,w=exdat$anweight),2)
round(sqrt(wtd.var(exdat$lrecon,w=exdat$anweight)),2)

#' 
#' ### galtan
#' 
## -------------------------------------------------------------------------------

round(weighted.mean(exdat$galtan,w=exdat$anweight),2)
round(sqrt(wtd.var(exdat$galtan,w=exdat$anweight)),2)

#' 
#' ### galtan salience
#' 
## -------------------------------------------------------------------------------

round(weighted.mean(exdat$galtan_salience,w=exdat$anweight),2)
round(sqrt(wtd.var(exdat$galtan_salience,w=exdat$anweight)),2)

#' 
#' ## Correlation table
#' 
## -------------------------------------------------------------------------------

cor.vars<-c("gndr.c","age10.c","minority.c",
            "lrgen.z.gmc","lrecon.z.gmc",
            "galtan.z.gmc","galtan_salience.z.gmc",
            "childlessness")

weighted_corr <- 
  cov.wt(exdat[,cor.vars],
         wt = exdat[,"anweight"], cor = TRUE)

corr_matrix <- weighted_corr$cor
round(corr_matrix,2)


export(corr_matrix,
       "../../results/cors.weighted.pearson.r.xlsx",
       overwrite=T)

corr_matrix.t<-
  (corr_matrix*sqrt(weighted_corr$n.obs-2))/sqrt(1-corr_matrix^2)

## Warning in sqrt(1 - corr_matrix^2): NaNs produced

corr_matrix.p<-
  2*(1-pt(abs(corr_matrix.t),df=weighted_corr$n.obs-2))
round(corr_matrix.p,3)

export(corr_matrix.p,
       "../../results/cors.weighted.pearson.r.p.xlsx",
       overwrite=T)

#' 
#' # Session Information
#' 
## -------------------------------------------------------------------------------
sinf<-sessionInfo()
print(sinf,locale=F)

