#' ---
#' title: "Exploratory analysis: Religiousness"
#' output: 
#'   html_document: 
#'     toc: yes
#'     keep_md: yes
#' 
#' ---
#' 
#' 
## ---- include=FALSE-----------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
#' # Preparations
#' 
#' ## Packages
#' 
## -----------------------------------------------------------------------------

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
## -----------------------------------------------------------------------------
source("../custom_functions.R")


#' 
#' ## Data
#' 
## -----------------------------------------------------------------------------
fdat<-import("../../data/processed/fdat.xlsx")

#' 
#' ## Data exclusions
#' 
## -----------------------------------------------------------------------------

exdat<-fdat %>%
  dplyr::select(childlessness,
                gndr.f,agea,minority,
                gndr.c,age10.c,minority.c,
                lrgen,
                lrecon,galtan,
                lrecon_salience,galtan_salience,
                rlgdgr,rlgdgr.z,
                cntry,
                anweight) %>%
  mutate(rlgdgr.z=as.numeric(rlgdgr.z)) %>%
  na.omit()


#' 
#' ## Variable centering
#' 
## -----------------------------------------------------------------------------

exdat<-
  group_mean_center(
    data=exdat,group.var="cntry",
    vars=c("rlgdgr","rlgdgr.z",
           "lrgen","lrecon","galtan",
           "lrecon_salience","galtan_salience"),
    grand.init = F)


#' 
#' 
#' ## Scaling CHES variables
#' 
## -----------------------------------------------------------------------------
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

#' 
#' 
#' # Analysis 
#' 
#' ## Baserate only
#' 
## -----------------------------------------------------------------------------
mod0<-
  glmer(childlessness~(1|cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight)

summary(mod0)


#' 
#' ## Covariates
#' 
## -----------------------------------------------------------------------------
mod1<-
  glmer(childlessness~gndr.c+age10.c+minority.c+(1|cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight)

summary(mod1)


#' 
#' ## Fixed main effect
#' 
## -----------------------------------------------------------------------------
mod2.rlgdgr<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          rlgdgr.z.gmc+
          (1|cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight)
summary(mod2.rlgdgr)

export(rownames_to_column(
  getFE_glmer(mod2.rlgdgr,round = 10,p.round=10)),
       "../../results/estimates/FE_mod2.rlgdgr.xlsx",
       overwrite=T)

export(getVC(mod2.rlgdgr,round = 10),
       "../../results/estimates/VC_mod2.rlgdgr.xlsx",
       overwrite=T)

#' 
#' ## Random
#' 
## -----------------------------------------------------------------------------
mod3.rlgdgr<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          rlgdgr.z.gmc+
          (rlgdgr.z.gmc|cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight,
        control = glmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e6)))
summary(mod3.rlgdgr)

anova(mod2.rlgdgr,mod3.rlgdgr)

export(rownames_to_column(
  getFE_glmer(mod3.rlgdgr,round = 10,p.round=10)),
       "../../results/estimates/FE_mod3.rlgdgr.xlsx",
       overwrite=T)

export(getVC(mod3.rlgdgr,round = 10),
       "../../results/estimates/VC_mod3.rlgdgr.xlsx",
       overwrite=T)

#' 
#' 
#' ## Random without random effect correlation
#' 
## -----------------------------------------------------------------------------
mod4.rlgdgr<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          rlgdgr.z.gmc+
          (rlgdgr.z.gmc||cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight,
        control = glmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e6)))
summary(mod4.rlgdgr)

anova(mod2.rlgdgr,mod4.rlgdgr)
anova(mod4.rlgdgr,mod3.rlgdgr)

export(rownames_to_column(
  getFE_glmer(mod4.rlgdgr,round = 10,p.round=10)),
       "../../results/estimates/FE_mod4.rlgdgr.xlsx",
       overwrite=T)

export(getVC(mod4.rlgdgr,round = 10),
       "../../results/estimates/VC_mod4.rlgdgr.xlsx",
       overwrite=T)

#' 
#' 
#' ## Random interaction with galtan_salience while galtan PO is in the model with the same interaction
#' 
## -----------------------------------------------------------------------------
mod13.rlgdgr<-
  glmer(childlessness~gndr.c+age10.c+minority.c+
          rlgdgr.z.gmc+
          galtan_salience.z.gmc+
          galtan_salience.z.gmc:rlgdgr.z.gmc+
          galtan.z.gmc+
          galtan_salience.z.gmc:galtan.z.gmc+
          (rlgdgr.z.gmc+galtan_salience.z.gmc+
          galtan_salience.z.gmc:rlgdgr.z.gmc||cntry),
        data=exdat,
        family=binomial(link="logit"),weights = anweight,
        control = glmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e6)))
summary(mod13.rlgdgr)


export(rownames_to_column(
  getFE_glmer(mod13.rlgdgr,round = 10,p.round=10)),
       "../../results/estimates/FE_mod13.rlgdgr.xlsx",
       overwrite=T)

export(getVC(mod13.rlgdgr,round = 10),
       "../../results/estimates/VC_mod13.rlgdgr.xlsx",
       overwrite=T)

#' 
#' ### Marginal effects
#' 
## -----------------------------------------------------------------------------
mod13.trends<-
  emtrends(mod13.rlgdgr,var="galtan.z.gmc",specs="galtan_salience.z.gmc",
         at=list(galtan_salience.z.gmc=c(-1,0,1)),infer=c(T,T),adjust="none")
mod13.trends

round(exp(c(-0.0900,-0.1617,-0.018)),2)

#' 
#' # Session Information
#' 
## -----------------------------------------------------------------------------
sinf<-sessionInfo()
print(sinf,locale=F)

