#' ---
#' title: "Merging ESS and CHES by vote"
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
#' ## Packages
#' 
## ----------------------------------------------------------------------------
library(dplyr)
library(rio)
library(sjlabelled)

#' 
#' ## Import ESS and CHES data with ESS party keys
#' 
## ----------------------------------------------------------------------------
ESS7<-
  import("../../data/raw/ESS7e02_2.sav")

CHES_2014.vote.keys<-
  import("../../data/processed/CHES_2014.vote.keys.xlsx")

#' 
#' # Construct single vote party number variable in ESS
#' 
## ----------------------------------------------------------------------------
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

# check if the remaining variable names are found in ESS

vote.vars %in% names(ESS7)

# sum across the voting variables

ESS7$pt.nmbr<-rowSums(ESS7[,vote.vars],na.rm=T)
table(ESS7$pt.nmbr,useNA="always")

# code zeros as NA

ESS7$pt.nmbr<-ifelse(ESS7$pt.nmbr==0,NA,ESS7$pt.nmbr)
table(ESS7$pt.nmbr,useNA="always")

#' 
#' # Combine the election coalitions in CHES ratings
#' 
## ----------------------------------------------------------------------------
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


#' 
#' # Merge the files by country and party number
#' 
## ----------------------------------------------------------------------------
dat<-left_join(
  x=ESS7,
  y=CHES_2014.vote.keys.combined,
  by=c("cntry","pt.nmbr")
)

#' 
#' 
#' 
#' ## Test if the merge was successful
#' 
## ----------------------------------------------------------------------------
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

#' 
#' Looks good!
#' 
#' # Export the combined party data and the entire long format dataset
#' 
## ----------------------------------------------------------------------------
export(CHES_2014.vote.keys.combined,
       "../../data/processed/CHES_2014.vote.keys.combined.xlsx",
       overwrite=T)

export(dat,"../../data/processed/dat.xlsx",
       overwrite=T)

#' 
#' # Session information
#' 
## ----------------------------------------------------------------------------
s<-sessionInfo()
print(s,locale=F)

