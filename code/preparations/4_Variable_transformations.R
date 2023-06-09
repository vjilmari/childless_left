#' ---
#' title: "Variable transformation before running the analysis"
#' output: 
#'   html_document: 
#'     toc: yes
#'     number_sections: yes
#'     keep_md: yes
#' ---
#' 
## ---- include=FALSE----------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
#' # Preparations
#' 
#' ## Load packages
#' 
## ----------------------------------------------------------------------------
library(rio)
library(dplyr)

#' 
#' ## Load data
#' 
## ----------------------------------------------------------------------------
# Long format data with ESS and CHES merged
dat<-import("../../data/processed/dat.xlsx")

# ESS raw data from which variable labels can be obtained
ESS.dat<-import("../../data/raw/ESS7e02_2.sav")

#' 
#' # Variable transformations
#' 
#' ## Gender
#' 
## ----------------------------------------------------------------------------
attr(ESS.dat$gndr,"labels")

# Factorial gndr

dat$gndr.f<-case_when(dat$gndr==1~"Male",
                      dat$gndr==2~"Female",
                      TRUE~NA_character_)

table(dat$gndr.f,useNA="always")

# Numerical gndr

dat$gndr.c<-case_when(dat$gndr==1~-0.5,
                      dat$gndr==2~0.5,
                      TRUE~NA_real_)

table(dat$gndr.c,useNA="always")


#' 
#' ## Age
#' 
## ----------------------------------------------------------------------------
attr(ESS.dat$agea,"labels")

table(dat$agea==999)

# centered age divided by 10
dat$age10.c<-(dat$agea-mean(dat$agea,na.rm=T))/10


#' 
#' ## Income
#' 
## ----------------------------------------------------------------------------
attr(ESS.dat$hinctnta,"labels")

# recode deciles to quintiles

dat$income<-case_when(
  dat$hinctnta==1 | dat$hinctnta==2 ~ "quint.1",
  dat$hinctnta==3 | dat$hinctnta==4 ~ "quint.2",
  dat$hinctnta==5 | dat$hinctnta==6 ~ "quint.3",
  dat$hinctnta==7 | dat$hinctnta==8 ~ "quint.4",
  dat$hinctnta==9 | dat$hinctnta==10 ~ "quint.5"
)

table(dat$income,useNA="always")

# add missing as additional factor level (to a new variable income.f)

dat$income.f<-case_when(
  is.na(dat$income) ~ "missing",
  TRUE ~ dat$income
)

#define reference level (top quintile)
table(dat$income.f,useNA="always")
dat$income.fr = relevel(as.factor(dat$income.f),
                        ref="quint.5")
table(dat$income.fr,useNA="always")


#' 
#' ## Education
#' 
## ----------------------------------------------------------------------------
attr(ESS.dat$eisced,"labels")

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

# recode reference education (highest) to a new variable (edu.f)

dat$edu.f<-relevel(as.factor(dat$edu),ref="7. MA")
table(dat$edu.f)

# code binary (college=0.5, no college=-0.5)

dat$edu.c<-case_when(dat$eisced==0~NA_real_,
                   dat$eisced>1 & dat$eisced<6~(-0.5),
                   dat$eisced>5 & dat$eisced<8~0.5,
                   TRUE~NA_real_)

table(dat$edu.c,useNA="always")

#' 
#' ## Political orientation
#' 
## ----------------------------------------------------------------------------
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
  

#' 
#' ## Vote
#' 
## ----------------------------------------------------------------------------
table(dat$vote)
attributes(ESS.dat$vote)

dat$vote.c<-case_when(dat$vote==1~0.5,
                      dat$vote==2~(-0.5),
                      TRUE~NA_real_)
table(dat$vote.c)

#' 
#' ## Childlessness
#' 
#' It was necessary to revise this from the preregistered, because there were over 14000 missing values in chldhhe based on the answers on the previous question (chldhm = Children living at home or not)
#' 
## ----------------------------------------------------------------------------
attributes(ESS.dat$chldhhe)
table(ESS.dat$chldhhe,useNA="always")

attributes(ESS.dat$chldhm)
table(ESS.dat$chldhm,useNA="always")

table(ESS.dat$chldhm,ESS.dat$chldhhe,useNA="always")

dat$childlessness<-case_when(
  dat$chldhhe==2  & dat$chldhm==2~1,
  dat$chldhhe==1 | dat$chldhm==1 ~0,
  TRUE~NA_real_
)

table(dat$childlessness,useNA="always")
table(dat$cntry,dat$childlessness,useNA="always")

#' 
#' ## Belong to minor ethnic group
#' 
## ----------------------------------------------------------------------------
attributes(ESS.dat$blgetmg)
table(ESS.dat$blgetmg,useNA="always")

dat$minority<-case_when(
  dat$blgetmg==1~1,
  dat$blgetmg==2~0,
  TRUE~NA_real_
)

dat$minority.c<-dat$minority-0.5
table(dat$minority.c)

#' 
#' ## Left-Right self-placement
#' 
## ----------------------------------------------------------------------------
attributes(ESS.dat$lrscale)
table(dat$lrscale,useNA="always")

dat$lrscale.z<-scale(dat$lrscale,center=T,scale=T)


#' 
#' ## Reducing income differences
#' 
## ----------------------------------------------------------------------------
attributes(ESS.dat$gincdif)
table(dat$gincdif,useNA="always")

# reverse code so that high number indicates endorsement
dat$gincdif.R<-(6-dat$gincdif)
table(dat$gincdif.R,useNA="always")

dat$gincdif.R.z<-scale(dat$gincdif.R,center=T,scale=T)


#' 
#' ## Gay rights
#' 
## ----------------------------------------------------------------------------
attributes(ESS.dat$freehms)
table(dat$freehms,useNA="always")

# reverse code so that high number indicates endorsement
dat$freehms.R<-(6-dat$freehms)
table(dat$freehms.R,useNA="always")

dat$freehms.R.z<-scale(dat$freehms.R,center=T,scale=T)


#' 
#' ## Immigration
#' 
## ----------------------------------------------------------------------------
attributes(ESS.dat$imwbcnt)
table(dat$imwbcnt,useNA="always")

dat$imwbcnt.z<-scale(dat$imwbcnt,center=T,scale=T)


#' 
#' ## Shared traditions
#' 
## ----------------------------------------------------------------------------
attributes(ESS.dat$pplstrd)
table(dat$pplstrd,useNA="always")

# reverse code so that high number indicates endorsement
dat$pplstrd.R<-(6-dat$pplstrd)
table(dat$pplstrd.R,useNA="always")

dat$pplstrd.R.z<-scale(dat$pplstrd.R,center=T,scale=T)


#' 
#' ## Religiousness
#' 
## ----------------------------------------------------------------------------
attributes(ESS.dat$rlgdgr)
table(dat$rlgdgr,useNA="always")

dat$rlgdgr.z<-scale(dat$rlgdgr,center=T,scale=T)

#' 
#' # Final set of variables needed for the analysis
#' 
## ----------------------------------------------------------------------------
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

# test if they are all in the data file
analysis.vars %in% names(dat)

# exclude variable not needed
fdat<-dat[,analysis.vars]
str(fdat)

# construct analysis weights

fdat$anweight=fdat$pspwght*fdat$pweight

# save the final data file
export(fdat,
       "../../data/processed/fdat.xlsx",overwrite=T)

#' # Session information
#' 
## ----------------------------------------------------------------------------
s<-sessionInfo()
print(s,locale=F)

