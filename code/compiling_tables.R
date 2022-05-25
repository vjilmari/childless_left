
# Compiling tables

library(rio)
library(finalfit)
library(dplyr)
library(rempsyc)
library(flextable)
library(officer)
library(multid)
library(tibble)

# Correlation table

rtab<-import("results/cors.weighted.pearson.r.xlsx")
names(rtab)
cor.names<-c(
  "1. Gender",
  "2. Age",
  "3. Ethnic minority",
  "4. Left-Right Gen.",
  "5. Left-Right Econ.",
  "6. GAL-TAN",
  "7. GAL-TAN salience",
  "8. Childlessness")

rownames(rtab)<-cor.names
colnames(rtab)<-paste0(1:nrow(rtab),".")
rtab

ptab<-import("results/cors.weighted.pearson.r.p.xlsx")

round(ptab,3)

nice_table(rownames_to_column(rtab),
           col.format.r = T)

save_as_docx(nice_table(rownames_to_column(rtab),
                        col.format.r = T),
             path = "results/tables/Table_cors.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

# Table for RQ1-2

## start from the most complex model

mod9.galtan.FE<-import("results/estimates/FE_mod9.galtan.xlsx")

mod9.galtan.FE %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="galtan.z.gmc"~"GAL-TAN",
    rowname=="galtan_salience.z.gmc"~"Salience",
    rowname=="galtan.z.gmc:galtan_salience.z.gmc"~"GAL-TAN x Salience",
                             TRUE~rowname),
         OR=round_tidy(exp(b),2),
         Est.=round_tidy(b,2),SE=round_tidy(SE,2),
         CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.mod9.galtan.FE

tidy.mod9.galtan.FE

mod9.galtan.VC<-import("results/estimates/VC_mod9.galtan.xlsx")
mod9.galtan.VC


mod9.galtan.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="galtan_salience.z.gmc" & is.na(var2) ~"Salience",
    var1=="galtan_salience.z.gmc:galtan.z.gmc" & is.na(var2) ~"GAL-TAN x Salience",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.mod9.galtan.VC
  
tidy.mod9.galtan.FE
tidy.mod9.galtan.VC
tidy.mod9.galtan<-
  rbind(tidy.mod9.galtan.FE,
      cbind(tidy.mod9.galtan.VC,p=NA,CI=NA,OR=NA))

nice_table(tidy.mod9.galtan)

# mod8

mod8.galtan.FE<-import("results/estimates/FE_mod8.galtan.xlsx")

mod8.galtan.FE %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="galtan.z.gmc"~"GAL-TAN",
    rowname=="galtan_salience.z.gmc"~"Salience",
    rowname=="galtan.z.gmc:galtan_salience.z.gmc"~"GAL-TAN x Salience",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.mod8.galtan.FE

tidy.mod8.galtan.FE

mod8.galtan.VC<-import("results/estimates/VC_mod8.galtan.xlsx")
mod8.galtan.VC


mod8.galtan.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="galtan_salience.z.gmc" & is.na(var2) ~"Salience",
    var1=="galtan_salience.z.gmc:galtan.z.gmc" & is.na(var2) ~"GAL-TAN x Salience",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.mod8.galtan.VC

tidy.mod8.galtan.FE
tidy.mod8.galtan.VC
tidy.mod8.galtan<-
  rbind(tidy.mod8.galtan.FE,
        cbind(tidy.mod8.galtan.VC,p=NA,CI=NA,OR=NA))

nice_table(tidy.mod8.galtan)

comb.FEs<-
  left_join(tidy.mod9.galtan.FE,
            tidy.mod8.galtan.FE,by="Parameter")
comb.FEs
nice_table(comb.FEs,col.format.p=c(3))
?nice_table


# mod7

mod7.galtan.FE<-import("results/estimates/FE_mod7.galtan.xlsx")

mod7.galtan.FE %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="galtan.z.gmc"~"GAL-TAN",
    rowname=="galtan_salience.z.gmc"~"Salience",
    rowname=="galtan.z.gmc:galtan_salience.z.gmc"~"GAL-TAN x Salience",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.mod7.galtan.FE

tidy.mod7.galtan.FE

mod7.galtan.VC<-import("results/estimates/VC_mod7.galtan.xlsx")
mod7.galtan.VC


mod7.galtan.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="galtan_salience.z.gmc" & is.na(var2) ~"Salience",
    var1=="galtan_salience.z.gmc:galtan.z.gmc" & is.na(var2) ~"GAL-TAN x Salience",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.mod7.galtan.VC

tidy.mod7.galtan.FE
tidy.mod7.galtan.VC
tidy.mod7.galtan<-
  rbind(tidy.mod7.galtan.FE,
        cbind(tidy.mod7.galtan.VC,p=NA,CI=NA,OR=NA))

nice_table(tidy.mod7.galtan)



# mod6

mod6.galtan.FE<-import("results/estimates/FE_mod6.galtan.xlsx")

mod6.galtan.FE %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="galtan.z.gmc"~"GAL-TAN",
    rowname=="galtan_salience.z.gmc"~"Salience",
    rowname=="galtan.z.gmc:galtan_salience.z.gmc"~"GAL-TAN x Salience",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.mod6.galtan.FE

tidy.mod6.galtan.FE

mod6.galtan.VC<-import("results/estimates/VC_mod6.galtan.xlsx")
mod6.galtan.VC


mod6.galtan.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="galtan_salience.z.gmc" & is.na(var2) ~"Salience",
    var1=="galtan_salience.z.gmc:galtan.z.gmc" & is.na(var2) ~"GAL-TAN x Salience",
    var1=="(Intercept)" & var2=="galtan_salience.z.gmc" ~"cor(Intercept, Salience)",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.mod6.galtan.VC

tidy.mod6.galtan.FE
tidy.mod6.galtan.VC
tidy.mod6.galtan<-
  rbind(tidy.mod6.galtan.FE,
        cbind(tidy.mod6.galtan.VC,p=NA,CI=NA,OR=NA))

nice_table(tidy.mod6.galtan)



# mod5

mod5.galtan.FE<-import("results/estimates/FE_mod5.galtan.xlsx")

mod5.galtan.FE %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="galtan.z.gmc"~"GAL-TAN",
    rowname=="galtan_salience.z.gmc"~"Salience",
    rowname=="galtan.z.gmc:galtan_salience.z.gmc"~"GAL-TAN x Salience",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.mod5.galtan.FE

tidy.mod5.galtan.FE

mod5.galtan.VC<-import("results/estimates/VC_mod5.galtan.xlsx")
mod5.galtan.VC


mod5.galtan.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="galtan_salience.z.gmc" & is.na(var2) ~"Salience",
    var1=="galtan_salience.z.gmc:galtan.z.gmc" & is.na(var2) ~"GAL-TAN x Salience",
    var1=="(Intercept)" & var2=="galtan_salience.z.gmc" ~"cor(Intercept, Salience)",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.mod5.galtan.VC

tidy.mod5.galtan.FE
tidy.mod5.galtan.VC
tidy.mod5.galtan<-
  rbind(tidy.mod5.galtan.FE,
        cbind(tidy.mod5.galtan.VC,p=NA,CI=NA,OR=NA))

nice_table(tidy.mod5.galtan)



# mod4.galtan

mod4.galtan.FE<-import("results/estimates/FE_mod4.galtan.xlsx")

mod4.galtan.FE %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="galtan.z.gmc"~"GAL-TAN",
    rowname=="galtan_salience.z.gmc"~"Salience",
    rowname=="galtan.z.gmc:galtan_salience.z.gmc"~"GAL-TAN x Salience",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.mod4.galtan.FE

tidy.mod4.galtan.FE

mod4.galtan.VC<-import("results/estimates/VC_mod4.galtan.xlsx")
mod4.galtan.VC


mod4.galtan.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="galtan.z.gmc" & is.na(var2) ~"GAL-TAN",
    var1=="galtan_salience.z.gmc" & is.na(var2) ~"Salience",
    var1=="galtan_salience.z.gmc:galtan.z.gmc" & is.na(var2) ~"GAL-TAN x Salience",
    var1=="(Intercept)" & var2=="galtan_salience.z.gmc" ~"cor(Intercept, Salience)",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.mod4.galtan.VC

tidy.mod4.galtan.FE
tidy.mod4.galtan.VC
tidy.mod4.galtan<-
  rbind(tidy.mod4.galtan.FE,
        cbind(tidy.mod4.galtan.VC,p=NA,CI=NA,OR=NA))

nice_table(tidy.mod4.galtan)


# mod3.galtan

mod3.galtan.FE<-import("results/estimates/FE_mod3.galtan.xlsx")

mod3.galtan.FE %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="galtan.z.gmc"~"GAL-TAN",
    rowname=="galtan_salience.z.gmc"~"Salience",
    rowname=="galtan.z.gmc:galtan_salience.z.gmc"~"GAL-TAN x Salience",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.mod3.galtan.FE

tidy.mod3.galtan.FE

mod3.galtan.VC<-import("results/estimates/VC_mod3.galtan.xlsx")
mod3.galtan.VC


mod3.galtan.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="galtan.z.gmc" & is.na(var2) ~"GAL-TAN",
    var1=="galtan_salience.z.gmc" & is.na(var2) ~"Salience",
    var1=="galtan_salience.z.gmc:galtan.z.gmc" & is.na(var2) ~"GAL-TAN x Salience",
    var1=="(Intercept)" & var2=="galtan_salience.z.gmc" ~"cor(Intercept, Salience)",
    var1=="(Intercept)" & var2=="galtan.z.gmc" ~"cor(Intercept, GAL-TAN)",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.mod3.galtan.VC

tidy.mod3.galtan.FE
tidy.mod3.galtan.VC
tidy.mod3.galtan<-
  rbind(tidy.mod3.galtan.FE,
        cbind(tidy.mod3.galtan.VC,p=NA,CI=NA,OR=NA))

nice_table(tidy.mod3.galtan)


# mod2.galtan

mod2.galtan.FE<-import("results/estimates/FE_mod2.galtan.xlsx")

mod2.galtan.FE %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="galtan.z.gmc"~"GAL-TAN",
    rowname=="galtan_salience.z.gmc"~"Salience",
    rowname=="galtan.z.gmc:galtan_salience.z.gmc"~"GAL-TAN x Salience",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.mod2.galtan.FE

tidy.mod2.galtan.FE

mod2.galtan.VC<-import("results/estimates/VC_mod2.galtan.xlsx")
mod2.galtan.VC


mod2.galtan.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="galtan.z.gmc" & is.na(var2) ~"GAL-TAN",
    var1=="galtan_salience.z.gmc" & is.na(var2) ~"Salience",
    var1=="galtan_salience.z.gmc:galtan.z.gmc" & is.na(var2) ~"GAL-TAN x Salience",
    var1=="(Intercept)" & var2=="galtan_salience.z.gmc" ~"cor(Intercept, Salience)",
    var1=="(Intercept)" & var2=="galtan.z.gmc" ~"cor(Intercept, GAL-TAN)",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.mod2.galtan.VC

tidy.mod2.galtan.FE
tidy.mod2.galtan.VC
tidy.mod2.galtan<-
  rbind(tidy.mod2.galtan.FE,
        cbind(tidy.mod2.galtan.VC,p=NA,CI=NA,OR=NA))

nice_table(tidy.mod2.galtan)




# mod4.lrgen

mod4.lrgen.FE<-import("results/estimates/FE_mod4.lrgen.xlsx")

mod4.lrgen.FE %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="lrgen.z.gmc"~"Left-Right Gen.",
    rowname=="lrgen_salience.z.gmc"~"Salience",
    rowname=="lrgen.z.gmc:lrgen_salience.z.gmc"~"Left-Right Gen. x Salience",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.mod4.lrgen.FE

tidy.mod4.lrgen.FE

mod4.lrgen.VC<-import("results/estimates/VC_mod4.lrgen.xlsx")
mod4.lrgen.VC


mod4.lrgen.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="lrgen.z.gmc" & is.na(var2) ~"Left-Right Gen.",
    var1=="lrgen_salience.z.gmc" & is.na(var2) ~"Salience",
    var1=="lrgen_salience.z.gmc:lrgen.z.gmc" & is.na(var2) ~"Left-Right Gen. x Salience",
    var1=="(Intercept)" & var2=="lrgen_salience.z.gmc" ~"cor(Intercept, Salience)",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.mod4.lrgen.VC

tidy.mod4.lrgen.FE
tidy.mod4.lrgen.VC
tidy.mod4.lrgen<-
  rbind(tidy.mod4.lrgen.FE,
        cbind(tidy.mod4.lrgen.VC,p=NA,CI=NA,OR=NA))

nice_table(tidy.mod4.lrgen)


# mod3.lrgen

mod3.lrgen.FE<-import("results/estimates/FE_mod3.lrgen.xlsx")

mod3.lrgen.FE %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="lrgen.z.gmc"~"Left-Right Gen.",
    rowname=="lrgen_salience.z.gmc"~"Salience",
    rowname=="lrgen.z.gmc:lrgen_salience.z.gmc"~"Left-Right Gen. x Salience",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.mod3.lrgen.FE

tidy.mod3.lrgen.FE

mod3.lrgen.VC<-import("results/estimates/VC_mod3.lrgen.xlsx")
mod3.lrgen.VC


mod3.lrgen.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="lrgen.z.gmc" & is.na(var2) ~"Left-Right Gen.",
    var1=="lrgen_salience.z.gmc" & is.na(var2) ~"Salience",
    var1=="lrgen_salience.z.gmc:lrgen.z.gmc" & is.na(var2) ~"Left-Right Gen. x Salience",
    var1=="(Intercept)" & var2=="lrgen_salience.z.gmc" ~"cor(Intercept, Salience)",
    var1=="(Intercept)" & var2=="lrgen.z.gmc" ~"cor(Intercept, Left-Right Gen.)",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.mod3.lrgen.VC

tidy.mod3.lrgen.FE
tidy.mod3.lrgen.VC
tidy.mod3.lrgen<-
  rbind(tidy.mod3.lrgen.FE,
        cbind(tidy.mod3.lrgen.VC,p=NA,CI=NA,OR=NA))

nice_table(tidy.mod3.lrgen)


# mod2.lrgen

mod2.lrgen.FE<-import("results/estimates/FE_mod2.lrgen.xlsx")

mod2.lrgen.FE %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="lrgen.z.gmc"~"Left-Right Gen.",
    rowname=="lrgen_salience.z.gmc"~"Salience",
    rowname=="lrgen.z.gmc:lrgen_salience.z.gmc"~"Left-Right Gen. x Salience",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.mod2.lrgen.FE

tidy.mod2.lrgen.FE

mod2.lrgen.VC<-import("results/estimates/VC_mod2.lrgen.xlsx")
mod2.lrgen.VC


mod2.lrgen.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="lrgen.z.gmc" & is.na(var2) ~"Left-Right Gen.",
    var1=="lrgen_salience.z.gmc" & is.na(var2) ~"Salience",
    var1=="lrgen_salience.z.gmc:lrgen.z.gmc" & is.na(var2) ~"Left-Right Gen. x Salience",
    var1=="(Intercept)" & var2=="lrgen_salience.z.gmc" ~"cor(Intercept, Salience)",
    var1=="(Intercept)" & var2=="lrgen.z.gmc" ~"cor(Intercept, Left-Right Gen.)",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.mod2.lrgen.VC

tidy.mod2.lrgen.FE
tidy.mod2.lrgen.VC
tidy.mod2.lrgen<-
  rbind(tidy.mod2.lrgen.FE,
        cbind(tidy.mod2.lrgen.VC,p=NA,CI=NA,OR=NA))

nice_table(tidy.mod2.lrgen)




# mod4.lrecon

mod4.lrecon.FE<-import("results/estimates/FE_mod4.lrecon.xlsx")

mod4.lrecon.FE %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="lrecon.z.gmc"~"Left-Right Econ.",
    rowname=="lrecon_salience.z.gmc"~"Salience",
    rowname=="lrecon.z.gmc:lrecon_salience.z.gmc"~"Left-Right Econ. x Salience",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.mod4.lrecon.FE

tidy.mod4.lrecon.FE

mod4.lrecon.VC<-import("results/estimates/VC_mod4.lrecon.xlsx")
mod4.lrecon.VC


mod4.lrecon.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="lrecon.z.gmc" & is.na(var2) ~"Left-Right Econ.",
    var1=="lrecon_salience.z.gmc" & is.na(var2) ~"Salience",
    var1=="lrecon_salience.z.gmc:lrecon.z.gmc" & is.na(var2) ~"Left-Right Econ. x Salience",
    var1=="(Intercept)" & var2=="lrecon_salience.z.gmc" ~"cor(Intercept, Salience)",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.mod4.lrecon.VC

tidy.mod4.lrecon.FE
tidy.mod4.lrecon.VC
tidy.mod4.lrecon<-
  rbind(tidy.mod4.lrecon.FE,
        cbind(tidy.mod4.lrecon.VC,p=NA,CI=NA,OR=NA))

nice_table(tidy.mod4.lrecon)


# mod3.lrecon

mod3.lrecon.FE<-import("results/estimates/FE_mod3.lrecon.xlsx")

mod3.lrecon.FE %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="lrecon.z.gmc"~"Left-Right Econ.",
    rowname=="lrecon_salience.z.gmc"~"Salience",
    rowname=="lrecon.z.gmc:lrecon_salience.z.gmc"~"Left-Right Econ. x Salience",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.mod3.lrecon.FE

tidy.mod3.lrecon.FE

mod3.lrecon.VC<-import("results/estimates/VC_mod3.lrecon.xlsx")
mod3.lrecon.VC


mod3.lrecon.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="lrecon.z.gmc" & is.na(var2) ~"Left-Right Econ.",
    var1=="lrecon_salience.z.gmc" & is.na(var2) ~"Salience",
    var1=="lrecon_salience.z.gmc:lrecon.z.gmc" & is.na(var2) ~"Left-Right Econ. x Salience",
    var1=="(Intercept)" & var2=="lrecon_salience.z.gmc" ~"cor(Intercept, Salience)",
    var1=="(Intercept)" & var2=="lrecon.z.gmc" ~"cor(Intercept, Left-Right Econ.)",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.mod3.lrecon.VC

tidy.mod3.lrecon.FE
tidy.mod3.lrecon.VC
tidy.mod3.lrecon<-
  rbind(tidy.mod3.lrecon.FE,
        cbind(tidy.mod3.lrecon.VC,p=NA,CI=NA,OR=NA))

nice_table(tidy.mod3.lrecon)


# mod2.lrecon

mod2.lrecon.FE<-import("results/estimates/FE_mod2.lrecon.xlsx")

mod2.lrecon.FE %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="lrecon.z.gmc"~"Left-Right Econ.",
    rowname=="lrecon_salience.z.gmc"~"Salience",
    rowname=="lrecon.z.gmc:lrecon_salience.z.gmc"~"Left-Right Econ. x Salience",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.mod2.lrecon.FE

tidy.mod2.lrecon.FE

mod2.lrecon.VC<-import("results/estimates/VC_mod2.lrecon.xlsx")
mod2.lrecon.VC


mod2.lrecon.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="lrecon.z.gmc" & is.na(var2) ~"Left-Right Econ.",
    var1=="lrecon_salience.z.gmc" & is.na(var2) ~"Salience",
    var1=="lrecon_salience.z.gmc:lrecon.z.gmc" & is.na(var2) ~"Left-Right Econ. x Salience",
    var1=="(Intercept)" & var2=="lrecon_salience.z.gmc" ~"cor(Intercept, Salience)",
    var1=="(Intercept)" & var2=="lrecon.z.gmc" ~"cor(Intercept, Left-Right Econ.)",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.mod2.lrecon.VC

tidy.mod2.lrecon.FE
tidy.mod2.lrecon.VC
tidy.mod2.lrecon<-
  rbind(tidy.mod2.lrecon.FE,
        cbind(tidy.mod2.lrecon.VC,p=NA,CI=NA,OR=NA))

nice_table(tidy.mod2.lrecon)


# mod1

mod1.FE<-import("results/estimates/FE_mod1.xlsx")

mod1.FE %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="lrecon.z.gmc"~"Left-Right Econ.",
    rowname=="lrecon_salience.z.gmc"~"Salience",
    rowname=="lrecon.z.gmc:lrecon_salience.z.gmc"~"Left-Right Econ. x Salience",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.mod1.FE

tidy.mod1.FE

mod1.VC<-import("results/estimates/VC_mod1.xlsx")
mod1.VC


mod1.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="lrecon.z.gmc" & is.na(var2) ~"Left-Right Econ.",
    var1=="lrecon_salience.z.gmc" & is.na(var2) ~"Salience",
    var1=="lrecon_salience.z.gmc:lrecon.z.gmc" & is.na(var2) ~"Left-Right Econ. x Salience",
    var1=="(Intercept)" & var2=="lrecon_salience.z.gmc" ~"cor(Intercept, Salience)",
    var1=="(Intercept)" & var2=="lrecon.z.gmc" ~"cor(Intercept, Left-Right Econ.)",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.mod1.VC

tidy.mod1.FE
tidy.mod1.VC
tidy.mod1<-
  rbind(tidy.mod1.FE,
        cbind(tidy.mod1.VC,p=NA,CI=NA,OR=NA))

nice_table(tidy.mod1)

# mod0

mod0.FE<-import("results/estimates/FE_mod0.xlsx")

mod0.FE %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="lrecon.z.gmc"~"Left-Right Econ.",
    rowname=="lrecon_salience.z.gmc"~"Salience",
    rowname=="lrecon.z.gmc:lrecon_salience.z.gmc"~"Left-Right Econ. x Salience",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.mod0.FE

tidy.mod0.FE

mod0.VC<-import("results/estimates/VC_mod0.xlsx")
mod0.VC


mod0.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="lrecon.z.gmc" & is.na(var2) ~"Left-Right Econ.",
    var1=="lrecon_salience.z.gmc" & is.na(var2) ~"Salience",
    var1=="lrecon_salience.z.gmc:lrecon.z.gmc" & is.na(var2) ~"Left-Right Econ. x Salience",
    var1=="(Intercept)" & var2=="lrecon_salience.z.gmc" ~"cor(Intercept, Salience)",
    var1=="(Intercept)" & var2=="lrecon.z.gmc" ~"cor(Intercept, Left-Right Econ.)",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.mod0.VC

tidy.mod0.FE
tidy.mod0.VC
tidy.mod0<-
  rbind(tidy.mod0.FE,
        cbind(tidy.mod0.VC,p=NA,CI=NA,OR=NA))

nice_table(tidy.mod0)

# selected models, mod0, mod1, mod2.lrgen, mod2.lrecon, mod2.galtan, mod8.galtan

# combine fixed effects


tab.FEs<-right_join(
  tidy.mod2.galtan.FE,
  tidy.mod8.galtan.FE,
  by="Parameter"
)

tab.FEs<-full_join(
  tidy.mod2.lrecon.FE,
  tab.FEs,
  by="Parameter"
)

tab.FEs<-full_join(
  tidy.mod2.lrgen.FE,
  tab.FEs,
  by="Parameter"
)

tab.FEs<-full_join(
  tidy.mod1.FE,
  tab.FEs,
  by="Parameter"
)

tab.FEs<-full_join(
  tidy.mod0.FE,
  tab.FEs,
  by="Parameter"
)

tab.FEs

tab.VCs<-full_join(
  tidy.mod2.galtan.VC,
  tidy.mod8.galtan.VC,
  by="Parameter"
)

tab.VCs<-full_join(
  tidy.mod2.lrecon.VC,
  tab.VCs,
  by="Parameter"
)

tab.VCs<-full_join(
  tidy.mod2.lrgen.VC,
  tab.VCs,
  by="Parameter"
)

tab.VCs<-full_join(
  tidy.mod1.VC,
  tab.VCs,
  by="Parameter"
)

tab.VCs<-full_join(
  tidy.mod0.VC,
  tab.VCs,
  by="Parameter"
)

tab.VCs

grepl("p",names(tab.FEs))

grepl("OR",names(tab.FEs))

tab.FEs.no.OR<-tab.FEs[,!grepl("OR",names(tab.FEs))]

tab.FEs.no.OR

nice_table(tab.FEs.no.OR,
           col.format.p = which(grepl("p",names(tab.FEs.no.OR))))



save_as_docx(nice_table(tab.FEs.no.OR,
                        col.format.p = which(grepl("p",names(tab.FEs.no.OR)))),
             path = "results/tables/Table1FE.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))




#### a table for each studied CHES-variables

# lrgen

# test which main effect model is reported

lrgen.sing<-
  import("code/analysis/lrgen/singularity.xlsx")
lrgen.sing

lrgen.main.mod<-
  case_when(!lrgen.sing[lrgen.sing$rowname=="mod3",
                       "singularity"]~"mod3",
            !lrgen.sing[lrgen.sing$rowname=="mod4",
                        "singularity"]~"mod4",
            TRUE~"mod2")
lrgen.main.mod

# obtain parameter estimates for selected main effect model

lrgen.main<-
  import(paste0("code/analysis/lrgen/",
                paste0(lrgen.main.mod,"_FE.xlsx")))
lrgen.main

lrgen.main %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="lrgen.z.gmc"~"Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.lrgen.main

tidy.lrgen.main



lrgen.main.VC<-import(paste0("code/analysis/lrgen/",
                             paste0(lrgen.main.mod,"_RE.xlsx")))
lrgen.main.VC

lrgen.main.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="galtan_salience.z.gmc" & is.na(var2) ~"Salience",
    var1=="galtan_salience.z.gmc:galtan.z.gmc" & is.na(var2) ~"GAL-TAN x Salience",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.lrgen.main.VC
tidy.lrgen.main.VC

# gndr model selection
lrgen.sing

lrgen.gndr.mod<-
  case_when(!lrgen.sing[lrgen.sing$rowname=="mod2.gndr",
                        "singularity"]~"mod2.gndr",
            !lrgen.sing[lrgen.sing$rowname=="mod3.gndr",
                        "singularity"]~"mod3.gndr",
            TRUE~"mod1.gndr")
lrgen.gndr.mod


lrgen.gndr<-
  import(paste0("code/analysis/lrgen/",
                paste0(lrgen.gndr.mod,"_FE.xlsx")))
lrgen.gndr

lrgen.gndr %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="lrgen.z.gmc"~"Left-Right Gen.",
    rowname=="gndr.c:lrgen.z.gmc"~"Gender x Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.lrgen.gndr

tidy.lrgen.gndr



lrgen.gndr.VC<-import(paste0("code/analysis/lrgen/",
                             paste0(lrgen.gndr.mod,"_RE.xlsx")))
lrgen.gndr.VC

lrgen.gndr.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="gndr.c" & is.na(var2) ~"Gender",
    var1=="gndr.c:lrgen.z.gmc" & is.na(var2) ~"Gender x Left-Right Gen.",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.lrgen.gndr.VC
tidy.lrgen.gndr.VC

# minority model selection
lrgen.sing

lrgen.minority.mod<-
  case_when(!lrgen.sing[lrgen.sing$rowname=="mod2.minority",
                        "singularity"]~"mod2.minority",
            !lrgen.sing[lrgen.sing$rowname=="mod3.minority",
                        "singularity"]~"mod3.minority",
            TRUE~"mod1.minority")
lrgen.minority.mod


lrgen.minority<-
  import(paste0("code/analysis/lrgen/",
                paste0(lrgen.minority.mod,"_FE.xlsx")))
lrgen.minority

lrgen.minority %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="lrgen.z.gmc"~"Left-Right Gen.",
    rowname=="gndr.c:lrgen.z.gmc"~"Gender x Left-Right Gen.",
    rowname=="minority.c:lrgen.z.gmc"~"Ethnic Minority x Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.lrgen.minority

tidy.lrgen.minority

lrgen.minority.VC<-import(paste0("code/analysis/lrgen/",
                             paste0(lrgen.minority.mod,"_RE.xlsx")))
lrgen.minority.VC

lrgen.minority.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="minority.c" & is.na(var2) ~"Ethnic minority",
    var1=="minority.c:lrgen.z.gmc" & is.na(var2) ~"Ethnic minority x Left-Right Gen.",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.lrgen.minority.VC
tidy.lrgen.minority.VC

# combine the models

lrgen.FEs<-full_join(
  full_join(
    tidy.lrgen.main,
    tidy.lrgen.gndr,
    by="Parameter"
  ),
  tidy.lrgen.minority,
  by="Parameter"
)

lrgen.FEs

lrgen.VCs<-full_join(
  full_join(
    tidy.lrgen.main.VC,
    tidy.lrgen.gndr.VC,
    by="Parameter"
  ),
  tidy.lrgen.minority.VC,
  by="Parameter"
)

lrgen.VCs

nice_table(lrgen.FEs,
           col.format.p = which(grepl("p",names(lrgen.FEs))))

save_as_docx(nice_table(lrgen.FEs,
                        col.format.p = which(grepl("p",names(lrgen.FEs)))),
             path = "results/tables/lrgen.FEs.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

save_as_docx(nice_table(lrgen.VCs),
             path = "results/tables/lrgen.VCs.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))


# lrecon

# test which main effect model is reported

lrecon.sing<-
  import("code/analysis/lrecon/singularity.xlsx")
lrecon.sing

lrecon.main.mod<-
  case_when(!lrecon.sing[lrecon.sing$rowname=="mod3",
                        "singularity"]~"mod3",
            !lrecon.sing[lrecon.sing$rowname=="mod4",
                        "singularity"]~"mod4",
            TRUE~"mod2")
lrecon.main.mod

# obtain parameter estimates for selected main effect model

lrecon.main<-
  import(paste0("code/analysis/lrecon/",
                paste0(lrecon.main.mod,"_FE.xlsx")))
lrecon.main

lrecon.main %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="lrecon.z.gmc"~"Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.lrecon.main

tidy.lrecon.main



lrecon.main.VC<-import(paste0("code/analysis/lrecon/",
                             paste0(lrecon.main.mod,"_RE.xlsx")))
lrecon.main.VC

lrecon.main.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="galtan_salience.z.gmc" & is.na(var2) ~"Salience",
    var1=="galtan_salience.z.gmc:galtan.z.gmc" & is.na(var2) ~"GAL-TAN x Salience",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.lrecon.main.VC
tidy.lrecon.main.VC

# gndr model selection
lrecon.sing

lrecon.gndr.mod<-
  case_when(!lrecon.sing[lrecon.sing$rowname=="mod2.gndr",
                        "singularity"]~"mod2.gndr",
            !lrecon.sing[lrecon.sing$rowname=="mod3.gndr",
                        "singularity"]~"mod3.gndr",
            TRUE~"mod1.gndr")
lrecon.gndr.mod


lrecon.gndr<-
  import(paste0("code/analysis/lrecon/",
                paste0(lrecon.gndr.mod,"_FE.xlsx")))
lrecon.gndr

lrecon.gndr %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="lrecon.z.gmc"~"Left-Right Gen.",
    rowname=="gndr.c:lrecon.z.gmc"~"Gender x Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.lrecon.gndr

tidy.lrecon.gndr



lrecon.gndr.VC<-import(paste0("code/analysis/lrecon/",
                             paste0(lrecon.gndr.mod,"_RE.xlsx")))
lrecon.gndr.VC

lrecon.gndr.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="gndr.c" & is.na(var2) ~"Gender",
    var1=="gndr.c:lrecon.z.gmc" & is.na(var2) ~"Gender x Left-Right Gen.",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.lrecon.gndr.VC
tidy.lrecon.gndr.VC

# minority model selection
lrecon.sing

lrecon.minority.mod<-
  case_when(!lrecon.sing[lrecon.sing$rowname=="mod2.minority",
                        "singularity"]~"mod2.minority",
            !lrecon.sing[lrecon.sing$rowname=="mod3.minority",
                        "singularity"]~"mod3.minority",
            TRUE~"mod1.minority")
lrecon.minority.mod


lrecon.minority<-
  import(paste0("code/analysis/lrecon/",
                paste0(lrecon.minority.mod,"_FE.xlsx")))
lrecon.minority

lrecon.minority %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="lrecon.z.gmc"~"Left-Right Gen.",
    rowname=="gndr.c:lrecon.z.gmc"~"Gender x Left-Right Gen.",
    rowname=="minority.c:lrecon.z.gmc"~"Ethnic Minority x Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.lrecon.minority

tidy.lrecon.minority

lrecon.minority.VC<-import(paste0("code/analysis/lrecon/",
                                 paste0(lrecon.minority.mod,"_RE.xlsx")))
lrecon.minority.VC

lrecon.minority.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="minority.c" & is.na(var2) ~"Ethnic minority",
    var1=="minority.c:lrecon.z.gmc" & is.na(var2) ~"Ethnic minority x Left-Right Gen.",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.lrecon.minority.VC
tidy.lrecon.minority.VC

# combine the models

lrecon.FEs<-full_join(
  full_join(
    tidy.lrecon.main,
    tidy.lrecon.gndr,
    by="Parameter"
  ),
  tidy.lrecon.minority,
  by="Parameter"
)

lrecon.FEs

lrecon.VCs<-full_join(
  full_join(
    tidy.lrecon.main.VC,
    tidy.lrecon.gndr.VC,
    by="Parameter"
  ),
  tidy.lrecon.minority.VC,
  by="Parameter"
)

lrecon.VCs

nice_table(lrecon.FEs,
           col.format.p = which(grepl("p",names(lrecon.FEs))))

save_as_docx(nice_table(lrecon.FEs,
                        col.format.p = which(grepl("p",names(lrecon.FEs)))),
             path = "results/tables/lrecon.FEs.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

save_as_docx(nice_table(lrecon.VCs),
             path = "results/tables/lrecon.VCs.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))





# galtan

# test which main effect model is reported

galtan.sing<-
  import("code/analysis/galtan/singularity.xlsx")
galtan.sing

galtan.main.mod<-
  case_when(!galtan.sing[galtan.sing$rowname=="mod3",
                         "singularity"]~"mod3",
            !galtan.sing[galtan.sing$rowname=="mod4",
                         "singularity"]~"mod4",
            TRUE~"mod2")
galtan.main.mod

# obtain parameter estimates for selected main effect model

galtan.main<-
  import(paste0("code/analysis/galtan/",
                paste0(galtan.main.mod,"_FE.xlsx")))
galtan.main

galtan.main %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="galtan.z.gmc"~"Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.galtan.main

tidy.galtan.main



galtan.main.VC<-import(paste0("code/analysis/galtan/",
                              paste0(galtan.main.mod,"_RE.xlsx")))
galtan.main.VC

galtan.main.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="galtan_salience.z.gmc" & is.na(var2) ~"Salience",
    var1=="galtan_salience.z.gmc:galtan.z.gmc" & is.na(var2) ~"GAL-TAN x Salience",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.galtan.main.VC
tidy.galtan.main.VC

# gndr model selection
galtan.sing

galtan.gndr.mod<-
  case_when(!galtan.sing[galtan.sing$rowname=="mod2.gndr",
                         "singularity"]~"mod2.gndr",
            !galtan.sing[galtan.sing$rowname=="mod3.gndr",
                         "singularity"]~"mod3.gndr",
            TRUE~"mod1.gndr")
galtan.gndr.mod


galtan.gndr<-
  import(paste0("code/analysis/galtan/",
                paste0(galtan.gndr.mod,"_FE.xlsx")))
galtan.gndr

galtan.gndr %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="galtan.z.gmc"~"Left-Right Gen.",
    rowname=="gndr.c:galtan.z.gmc"~"Gender x Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.galtan.gndr

tidy.galtan.gndr



galtan.gndr.VC<-import(paste0("code/analysis/galtan/",
                              paste0(galtan.gndr.mod,"_RE.xlsx")))
galtan.gndr.VC

galtan.gndr.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="gndr.c" & is.na(var2) ~"Gender",
    var1=="gndr.c:galtan.z.gmc" & is.na(var2) ~"Gender x Left-Right Gen.",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.galtan.gndr.VC
tidy.galtan.gndr.VC

# minority model selection
galtan.sing

galtan.minority.mod<-
  case_when(!galtan.sing[galtan.sing$rowname=="mod2.minority",
                         "singularity"]~"mod2.minority",
            !galtan.sing[galtan.sing$rowname=="mod3.minority",
                         "singularity"]~"mod3.minority",
            TRUE~"mod1.minority")
galtan.minority.mod


galtan.minority<-
  import(paste0("code/analysis/galtan/",
                paste0(galtan.minority.mod,"_FE.xlsx")))
galtan.minority

galtan.minority %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="galtan.z.gmc"~"Left-Right Gen.",
    rowname=="gndr.c:galtan.z.gmc"~"Gender x Left-Right Gen.",
    rowname=="minority.c:galtan.z.gmc"~"Ethnic Minority x Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.galtan.minority

tidy.galtan.minority

galtan.minority.VC<-import(paste0("code/analysis/galtan/",
                                  paste0(galtan.minority.mod,"_RE.xlsx")))
galtan.minority.VC

galtan.minority.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="minority.c" & is.na(var2) ~"Ethnic minority",
    var1=="minority.c:galtan.z.gmc" & is.na(var2) ~"Ethnic minority x Left-Right Gen.",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.galtan.minority.VC
tidy.galtan.minority.VC

# combine the models

galtan.FEs<-full_join(
  full_join(
    tidy.galtan.main,
    tidy.galtan.gndr,
    by="Parameter"
  ),
  tidy.galtan.minority,
  by="Parameter"
)

galtan.FEs

galtan.VCs<-full_join(
  full_join(
    tidy.galtan.main.VC,
    tidy.galtan.gndr.VC,
    by="Parameter"
  ),
  tidy.galtan.minority.VC,
  by="Parameter"
)

galtan.VCs

nice_table(galtan.FEs,
           col.format.p = which(grepl("p",names(galtan.FEs))))

save_as_docx(nice_table(galtan.FEs,
                        col.format.p = which(grepl("p",names(galtan.FEs)))),
             path = "results/tables/galtan.FEs.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

save_as_docx(nice_table(galtan.VCs),
             path = "results/tables/galtan.VCs.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))




# spendvtax

# test which main effect model is reported

spendvtax.sing<-
  import("code/analysis/spendvtax/singularity.xlsx")
spendvtax.sing

spendvtax.main.mod<-
  case_when(!spendvtax.sing[spendvtax.sing$rowname=="mod3",
                         "singularity"]~"mod3",
            !spendvtax.sing[spendvtax.sing$rowname=="mod4",
                         "singularity"]~"mod4",
            TRUE~"mod2")
spendvtax.main.mod

# obtain parameter estimates for selected main effect model

spendvtax.main<-
  import(paste0("code/analysis/spendvtax/",
                paste0(spendvtax.main.mod,"_FE.xlsx")))
spendvtax.main

spendvtax.main %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="spendvtax.z.gmc"~"Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.spendvtax.main

tidy.spendvtax.main



spendvtax.main.VC<-import(paste0("code/analysis/spendvtax/",
                              paste0(spendvtax.main.mod,"_RE.xlsx")))
spendvtax.main.VC

spendvtax.main.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="spendvtax_salience.z.gmc" & is.na(var2) ~"Salience",
    var1=="spendvtax_salience.z.gmc:spendvtax.z.gmc" & is.na(var2) ~"GAL-TAN x Salience",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.spendvtax.main.VC
tidy.spendvtax.main.VC

# gndr model selection
spendvtax.sing

spendvtax.gndr.mod<-
  case_when(!spendvtax.sing[spendvtax.sing$rowname=="mod2.gndr",
                         "singularity"]~"mod2.gndr",
            !spendvtax.sing[spendvtax.sing$rowname=="mod3.gndr",
                         "singularity"]~"mod3.gndr",
            TRUE~"mod1.gndr")
spendvtax.gndr.mod


spendvtax.gndr<-
  import(paste0("code/analysis/spendvtax/",
                paste0(spendvtax.gndr.mod,"_FE.xlsx")))
spendvtax.gndr

spendvtax.gndr %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="spendvtax.z.gmc"~"Left-Right Gen.",
    rowname=="gndr.c:spendvtax.z.gmc"~"Gender x Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.spendvtax.gndr

tidy.spendvtax.gndr



spendvtax.gndr.VC<-import(paste0("code/analysis/spendvtax/",
                              paste0(spendvtax.gndr.mod,"_RE.xlsx")))
spendvtax.gndr.VC

spendvtax.gndr.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="gndr.c" & is.na(var2) ~"Gender",
    var1=="gndr.c:spendvtax.z.gmc" & is.na(var2) ~"Gender x Left-Right Gen.",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.spendvtax.gndr.VC
tidy.spendvtax.gndr.VC

# minority model selection
spendvtax.sing

spendvtax.minority.mod<-
  case_when(!spendvtax.sing[spendvtax.sing$rowname=="mod2.minority",
                         "singularity"]~"mod2.minority",
            !spendvtax.sing[spendvtax.sing$rowname=="mod3.minority",
                         "singularity"]~"mod3.minority",
            TRUE~"mod1.minority")
spendvtax.minority.mod


spendvtax.minority<-
  import(paste0("code/analysis/spendvtax/",
                paste0(spendvtax.minority.mod,"_FE.xlsx")))
spendvtax.minority

spendvtax.minority %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="spendvtax.z.gmc"~"Left-Right Gen.",
    rowname=="gndr.c:spendvtax.z.gmc"~"Gender x Left-Right Gen.",
    rowname=="minority.c:spendvtax.z.gmc"~"Ethnic Minority x Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.spendvtax.minority

tidy.spendvtax.minority

spendvtax.minority.VC<-import(paste0("code/analysis/spendvtax/",
                                  paste0(spendvtax.minority.mod,"_RE.xlsx")))
spendvtax.minority.VC

spendvtax.minority.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="minority.c" & is.na(var2) ~"Ethnic minority",
    var1=="minority.c:spendvtax.z.gmc" & is.na(var2) ~"Ethnic minority x Left-Right Gen.",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.spendvtax.minority.VC
tidy.spendvtax.minority.VC

# combine the models

spendvtax.FEs<-full_join(
  full_join(
    tidy.spendvtax.main,
    tidy.spendvtax.gndr,
    by="Parameter"
  ),
  tidy.spendvtax.minority,
  by="Parameter"
)

spendvtax.FEs

spendvtax.VCs<-full_join(
  full_join(
    tidy.spendvtax.main.VC,
    tidy.spendvtax.gndr.VC,
    by="Parameter"
  ),
  tidy.spendvtax.minority.VC,
  by="Parameter"
)

spendvtax.VCs

nice_table(spendvtax.FEs,
           col.format.p = which(grepl("p",names(spendvtax.FEs))))

save_as_docx(nice_table(spendvtax.FEs,
                        col.format.p = which(grepl("p",names(spendvtax.FEs)))),
             path = "results/tables/spendvtax.FEs.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

save_as_docx(nice_table(spendvtax.VCs),
             path = "results/tables/spendvtax.VCs.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))


# deregulation

deregulation.sing<-
  import("code/analysis/deregulation/singularity.xlsx")
deregulation.sing

deregulation.main.mod<-
  case_when(!deregulation.sing[deregulation.sing$rowname=="mod3",
                            "singularity"]~"mod3",
            !deregulation.sing[deregulation.sing$rowname=="mod4",
                            "singularity"]~"mod4",
            TRUE~"mod2")
deregulation.main.mod

# obtain parameter estimates for selected main effect model

deregulation.main<-
  import(paste0("code/analysis/deregulation/",
                paste0(deregulation.main.mod,"_FE.xlsx")))
deregulation.main

deregulation.main %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="deregulation.z.gmc"~"Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.deregulation.main

tidy.deregulation.main



deregulation.main.VC<-import(paste0("code/analysis/deregulation/",
                                 paste0(deregulation.main.mod,"_RE.xlsx")))
deregulation.main.VC

deregulation.main.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="deregulation_salience.z.gmc" & is.na(var2) ~"Salience",
    var1=="deregulation_salience.z.gmc:deregulation.z.gmc" & is.na(var2) ~"GAL-TAN x Salience",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.deregulation.main.VC
tidy.deregulation.main.VC

# gndr model selection
deregulation.sing

deregulation.gndr.mod<-
  case_when(!deregulation.sing[deregulation.sing$rowname=="mod2.gndr",
                            "singularity"]~"mod2.gndr",
            !deregulation.sing[deregulation.sing$rowname=="mod3.gndr",
                            "singularity"]~"mod3.gndr",
            TRUE~"mod1.gndr")
deregulation.gndr.mod


deregulation.gndr<-
  import(paste0("code/analysis/deregulation/",
                paste0(deregulation.gndr.mod,"_FE.xlsx")))
deregulation.gndr

deregulation.gndr %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="deregulation.z.gmc"~"Left-Right Gen.",
    rowname=="gndr.c:deregulation.z.gmc"~"Gender x Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.deregulation.gndr

tidy.deregulation.gndr



deregulation.gndr.VC<-import(paste0("code/analysis/deregulation/",
                                 paste0(deregulation.gndr.mod,"_RE.xlsx")))
deregulation.gndr.VC

deregulation.gndr.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="gndr.c" & is.na(var2) ~"Gender",
    var1=="gndr.c:deregulation.z.gmc" & is.na(var2) ~"Gender x Left-Right Gen.",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.deregulation.gndr.VC
tidy.deregulation.gndr.VC

# minority model selection
deregulation.sing

deregulation.minority.mod<-
  case_when(!deregulation.sing[deregulation.sing$rowname=="mod2.minority",
                            "singularity"]~"mod2.minority",
            !deregulation.sing[deregulation.sing$rowname=="mod3.minority",
                            "singularity"]~"mod3.minority",
            TRUE~"mod1.minority")
deregulation.minority.mod


deregulation.minority<-
  import(paste0("code/analysis/deregulation/",
                paste0(deregulation.minority.mod,"_FE.xlsx")))
deregulation.minority

deregulation.minority %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="deregulation.z.gmc"~"Left-Right Gen.",
    rowname=="gndr.c:deregulation.z.gmc"~"Gender x Left-Right Gen.",
    rowname=="minority.c:deregulation.z.gmc"~"Ethnic Minority x Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.deregulation.minority

tidy.deregulation.minority

deregulation.minority.VC<-import(paste0("code/analysis/deregulation/",
                                     paste0(deregulation.minority.mod,"_RE.xlsx")))
deregulation.minority.VC

deregulation.minority.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="minority.c" & is.na(var2) ~"Ethnic minority",
    var1=="minority.c:deregulation.z.gmc" & is.na(var2) ~"Ethnic minority x Left-Right Gen.",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.deregulation.minority.VC
tidy.deregulation.minority.VC

# combine the models

deregulation.FEs<-full_join(
  full_join(
    tidy.deregulation.main,
    tidy.deregulation.gndr,
    by="Parameter"
  ),
  tidy.deregulation.minority,
  by="Parameter"
)

deregulation.FEs

deregulation.VCs<-full_join(
  full_join(
    tidy.deregulation.main.VC,
    tidy.deregulation.gndr.VC,
    by="Parameter"
  ),
  tidy.deregulation.minority.VC,
  by="Parameter"
)

deregulation.VCs

nice_table(deregulation.FEs,
           col.format.p = which(grepl("p",names(deregulation.FEs))))

save_as_docx(nice_table(deregulation.FEs,
                        col.format.p = which(grepl("p",names(deregulation.FEs)))),
             path = "results/tables/deregulation.FEs.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

save_as_docx(nice_table(deregulation.VCs),
             path = "results/tables/deregulation.VCs.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))






# redistribution

redistribution.sing<-
  import("code/analysis/redistribution/singularity.xlsx")
redistribution.sing

redistribution.main.mod<-
  case_when(!redistribution.sing[redistribution.sing$rowname=="mod3",
                               "singularity"]~"mod3",
            !redistribution.sing[redistribution.sing$rowname=="mod4",
                               "singularity"]~"mod4",
            TRUE~"mod2")
redistribution.main.mod

# obtain parameter estimates for selected main effect model

redistribution.main<-
  import(paste0("code/analysis/redistribution/",
                paste0(redistribution.main.mod,"_FE.xlsx")))
redistribution.main

redistribution.main %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="redistribution.z.gmc"~"Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.redistribution.main

tidy.redistribution.main



redistribution.main.VC<-import(paste0("code/analysis/redistribution/",
                                    paste0(redistribution.main.mod,"_RE.xlsx")))
redistribution.main.VC

redistribution.main.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="redistribution_salience.z.gmc" & is.na(var2) ~"Salience",
    var1=="redistribution_salience.z.gmc:redistribution.z.gmc" & is.na(var2) ~"GAL-TAN x Salience",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.redistribution.main.VC
tidy.redistribution.main.VC

# gndr model selection
redistribution.sing

redistribution.gndr.mod<-
  case_when(!redistribution.sing[redistribution.sing$rowname=="mod2.gndr",
                               "singularity"]~"mod2.gndr",
            !redistribution.sing[redistribution.sing$rowname=="mod3.gndr",
                               "singularity"]~"mod3.gndr",
            TRUE~"mod1.gndr")
redistribution.gndr.mod


redistribution.gndr<-
  import(paste0("code/analysis/redistribution/",
                paste0(redistribution.gndr.mod,"_FE.xlsx")))
redistribution.gndr

redistribution.gndr %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="redistribution.z.gmc"~"Left-Right Gen.",
    rowname=="gndr.c:redistribution.z.gmc"~"Gender x Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.redistribution.gndr

tidy.redistribution.gndr



redistribution.gndr.VC<-import(paste0("code/analysis/redistribution/",
                                    paste0(redistribution.gndr.mod,"_RE.xlsx")))
redistribution.gndr.VC

redistribution.gndr.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="gndr.c" & is.na(var2) ~"Gender",
    var1=="gndr.c:redistribution.z.gmc" & is.na(var2) ~"Gender x Left-Right Gen.",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.redistribution.gndr.VC
tidy.redistribution.gndr.VC

# minority model selection
redistribution.sing

redistribution.minority.mod<-
  case_when(!redistribution.sing[redistribution.sing$rowname=="mod2.minority",
                               "singularity"]~"mod2.minority",
            !redistribution.sing[redistribution.sing$rowname=="mod3.minority",
                               "singularity"]~"mod3.minority",
            TRUE~"mod1.minority")
redistribution.minority.mod


redistribution.minority<-
  import(paste0("code/analysis/redistribution/",
                paste0(redistribution.minority.mod,"_FE.xlsx")))
redistribution.minority

redistribution.minority %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="redistribution.z.gmc"~"Left-Right Gen.",
    rowname=="gndr.c:redistribution.z.gmc"~"Gender x Left-Right Gen.",
    rowname=="minority.c:redistribution.z.gmc"~"Ethnic Minority x Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.redistribution.minority

tidy.redistribution.minority

redistribution.minority.VC<-import(paste0("code/analysis/redistribution/",
                                        paste0(redistribution.minority.mod,"_RE.xlsx")))
redistribution.minority.VC

redistribution.minority.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="minority.c" & is.na(var2) ~"Ethnic minority",
    var1=="minority.c:redistribution.z.gmc" & is.na(var2) ~"Ethnic minority x Left-Right Gen.",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.redistribution.minority.VC
tidy.redistribution.minority.VC

# combine the models

redistribution.FEs<-full_join(
  full_join(
    tidy.redistribution.main,
    tidy.redistribution.gndr,
    by="Parameter"
  ),
  tidy.redistribution.minority,
  by="Parameter"
)

redistribution.FEs

redistribution.VCs<-full_join(
  full_join(
    tidy.redistribution.main.VC,
    tidy.redistribution.gndr.VC,
    by="Parameter"
  ),
  tidy.redistribution.minority.VC,
  by="Parameter"
)

redistribution.VCs

nice_table(redistribution.FEs,
           col.format.p = which(grepl("p",names(redistribution.FEs))))

save_as_docx(nice_table(redistribution.FEs,
                        col.format.p = which(grepl("p",names(redistribution.FEs)))),
             path = "results/tables/redistribution.FEs.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

save_as_docx(nice_table(redistribution.VCs),
             path = "results/tables/redistribution.VCs.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))








# econ_interven

econ_interven.sing<-
  import("code/analysis/econ_interven/singularity.xlsx")
econ_interven.sing

econ_interven.main.mod<-
  case_when(!econ_interven.sing[econ_interven.sing$rowname=="mod3",
                                 "singularity"]~"mod3",
            !econ_interven.sing[econ_interven.sing$rowname=="mod4",
                                 "singularity"]~"mod4",
            TRUE~"mod2")
econ_interven.main.mod

# obtain parameter estimates for selected main effect model

econ_interven.main<-
  import(paste0("code/analysis/econ_interven/",
                paste0(econ_interven.main.mod,"_FE.xlsx")))
econ_interven.main

econ_interven.main %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="econ_interven.z.gmc"~"Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.econ_interven.main

tidy.econ_interven.main



econ_interven.main.VC<-import(paste0("code/analysis/econ_interven/",
                                      paste0(econ_interven.main.mod,"_RE.xlsx")))
econ_interven.main.VC

econ_interven.main.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="econ_interven_salience.z.gmc" & is.na(var2) ~"Salience",
    var1=="econ_interven_salience.z.gmc:econ_interven.z.gmc" & is.na(var2) ~"GAL-TAN x Salience",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.econ_interven.main.VC
tidy.econ_interven.main.VC

# gndr model selection
econ_interven.sing

econ_interven.gndr.mod<-
  case_when(!econ_interven.sing[econ_interven.sing$rowname=="mod2.gndr",
                                 "singularity"]~"mod2.gndr",
            !econ_interven.sing[econ_interven.sing$rowname=="mod3.gndr",
                                 "singularity"]~"mod3.gndr",
            TRUE~"mod1.gndr")
econ_interven.gndr.mod


econ_interven.gndr<-
  import(paste0("code/analysis/econ_interven/",
                paste0(econ_interven.gndr.mod,"_FE.xlsx")))
econ_interven.gndr

econ_interven.gndr %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="econ_interven.z.gmc"~"Left-Right Gen.",
    rowname=="gndr.c:econ_interven.z.gmc"~"Gender x Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.econ_interven.gndr

tidy.econ_interven.gndr



econ_interven.gndr.VC<-import(paste0("code/analysis/econ_interven/",
                                      paste0(econ_interven.gndr.mod,"_RE.xlsx")))
econ_interven.gndr.VC

econ_interven.gndr.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="gndr.c" & is.na(var2) ~"Gender",
    var1=="gndr.c:econ_interven.z.gmc" & is.na(var2) ~"Gender x Left-Right Gen.",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.econ_interven.gndr.VC
tidy.econ_interven.gndr.VC

# minority model selection
econ_interven.sing

econ_interven.minority.mod<-
  case_when(!econ_interven.sing[econ_interven.sing$rowname=="mod2.minority",
                                 "singularity"]~"mod2.minority",
            !econ_interven.sing[econ_interven.sing$rowname=="mod3.minority",
                                 "singularity"]~"mod3.minority",
            TRUE~"mod1.minority")
econ_interven.minority.mod


econ_interven.minority<-
  import(paste0("code/analysis/econ_interven/",
                paste0(econ_interven.minority.mod,"_FE.xlsx")))
econ_interven.minority

econ_interven.minority %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="econ_interven.z.gmc"~"Left-Right Gen.",
    rowname=="gndr.c:econ_interven.z.gmc"~"Gender x Left-Right Gen.",
    rowname=="minority.c:econ_interven.z.gmc"~"Ethnic Minority x Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.econ_interven.minority

tidy.econ_interven.minority

econ_interven.minority.VC<-import(paste0("code/analysis/econ_interven/",
                                          paste0(econ_interven.minority.mod,"_RE.xlsx")))
econ_interven.minority.VC

econ_interven.minority.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="minority.c" & is.na(var2) ~"Ethnic minority",
    var1=="minority.c:econ_interven.z.gmc" & is.na(var2) ~"Ethnic minority x Left-Right Gen.",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.econ_interven.minority.VC
tidy.econ_interven.minority.VC

# combine the models

econ_interven.FEs<-full_join(
  full_join(
    tidy.econ_interven.main,
    tidy.econ_interven.gndr,
    by="Parameter"
  ),
  tidy.econ_interven.minority,
  by="Parameter"
)

econ_interven.FEs

econ_interven.VCs<-full_join(
  full_join(
    tidy.econ_interven.main.VC,
    tidy.econ_interven.gndr.VC,
    by="Parameter"
  ),
  tidy.econ_interven.minority.VC,
  by="Parameter"
)

econ_interven.VCs

nice_table(econ_interven.FEs,
           col.format.p = which(grepl("p",names(econ_interven.FEs))))

save_as_docx(nice_table(econ_interven.FEs,
                        col.format.p = which(grepl("p",names(econ_interven.FEs)))),
             path = "results/tables/econ_interven.FEs.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

save_as_docx(nice_table(econ_interven.VCs),
             path = "results/tables/econ_interven.VCs.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))






# civlib_laworder

civlib_laworder.sing<-
  import("code/analysis/civlib_laworder/singularity.xlsx")
civlib_laworder.sing

civlib_laworder.main.mod<-
  case_when(!civlib_laworder.sing[civlib_laworder.sing$rowname=="mod3",
                                "singularity"]~"mod3",
            !civlib_laworder.sing[civlib_laworder.sing$rowname=="mod4",
                                "singularity"]~"mod4",
            TRUE~"mod2")
civlib_laworder.main.mod

# obtain parameter estimates for selected main effect model

civlib_laworder.main<-
  import(paste0("code/analysis/civlib_laworder/",
                paste0(civlib_laworder.main.mod,"_FE.xlsx")))
civlib_laworder.main

civlib_laworder.main %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="civlib_laworder.z.gmc"~"Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.civlib_laworder.main

tidy.civlib_laworder.main



civlib_laworder.main.VC<-import(paste0("code/analysis/civlib_laworder/",
                                     paste0(civlib_laworder.main.mod,"_RE.xlsx")))
civlib_laworder.main.VC

civlib_laworder.main.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="civlib_laworder_salience.z.gmc" & is.na(var2) ~"Salience",
    var1=="civlib_laworder_salience.z.gmc:civlib_laworder.z.gmc" & is.na(var2) ~"GAL-TAN x Salience",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.civlib_laworder.main.VC
tidy.civlib_laworder.main.VC

# gndr model selection
civlib_laworder.sing

civlib_laworder.gndr.mod<-
  case_when(!civlib_laworder.sing[civlib_laworder.sing$rowname=="mod2.gndr",
                                "singularity"]~"mod2.gndr",
            !civlib_laworder.sing[civlib_laworder.sing$rowname=="mod3.gndr",
                                "singularity"]~"mod3.gndr",
            TRUE~"mod1.gndr")
civlib_laworder.gndr.mod


civlib_laworder.gndr<-
  import(paste0("code/analysis/civlib_laworder/",
                paste0(civlib_laworder.gndr.mod,"_FE.xlsx")))
civlib_laworder.gndr

civlib_laworder.gndr %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="civlib_laworder.z.gmc"~"Left-Right Gen.",
    rowname=="gndr.c:civlib_laworder.z.gmc"~"Gender x Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.civlib_laworder.gndr

tidy.civlib_laworder.gndr



civlib_laworder.gndr.VC<-import(paste0("code/analysis/civlib_laworder/",
                                     paste0(civlib_laworder.gndr.mod,"_RE.xlsx")))
civlib_laworder.gndr.VC

civlib_laworder.gndr.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="gndr.c" & is.na(var2) ~"Gender",
    var1=="gndr.c:civlib_laworder.z.gmc" & is.na(var2) ~"Gender x Left-Right Gen.",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.civlib_laworder.gndr.VC
tidy.civlib_laworder.gndr.VC

# minority model selection
civlib_laworder.sing

civlib_laworder.minority.mod<-
  case_when(!civlib_laworder.sing[civlib_laworder.sing$rowname=="mod2.minority",
                                "singularity"]~"mod2.minority",
            !civlib_laworder.sing[civlib_laworder.sing$rowname=="mod3.minority",
                                "singularity"]~"mod3.minority",
            TRUE~"mod1.minority")
civlib_laworder.minority.mod


civlib_laworder.minority<-
  import(paste0("code/analysis/civlib_laworder/",
                paste0(civlib_laworder.minority.mod,"_FE.xlsx")))
civlib_laworder.minority

civlib_laworder.minority %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="civlib_laworder.z.gmc"~"Left-Right Gen.",
    rowname=="gndr.c:civlib_laworder.z.gmc"~"Gender x Left-Right Gen.",
    rowname=="minority.c:civlib_laworder.z.gmc"~"Ethnic Minority x Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.civlib_laworder.minority

tidy.civlib_laworder.minority

civlib_laworder.minority.VC<-import(paste0("code/analysis/civlib_laworder/",
                                         paste0(civlib_laworder.minority.mod,"_RE.xlsx")))
civlib_laworder.minority.VC

civlib_laworder.minority.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="minority.c" & is.na(var2) ~"Ethnic minority",
    var1=="minority.c:civlib_laworder.z.gmc" & is.na(var2) ~"Ethnic minority x Left-Right Gen.",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.civlib_laworder.minority.VC
tidy.civlib_laworder.minority.VC

# combine the models

civlib_laworder.FEs<-full_join(
  full_join(
    tidy.civlib_laworder.main,
    tidy.civlib_laworder.gndr,
    by="Parameter"
  ),
  tidy.civlib_laworder.minority,
  by="Parameter"
)

civlib_laworder.FEs

civlib_laworder.VCs<-full_join(
  full_join(
    tidy.civlib_laworder.main.VC,
    tidy.civlib_laworder.gndr.VC,
    by="Parameter"
  ),
  tidy.civlib_laworder.minority.VC,
  by="Parameter"
)

civlib_laworder.VCs

nice_table(civlib_laworder.FEs,
           col.format.p = which(grepl("p",names(civlib_laworder.FEs))))

save_as_docx(nice_table(civlib_laworder.FEs,
                        col.format.p = which(grepl("p",names(civlib_laworder.FEs)))),
             path = "results/tables/civlib_laworder.FEs.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

save_as_docx(nice_table(civlib_laworder.VCs),
             path = "results/tables/civlib_laworder.VCs.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))






# sociallifestyle

sociallifestyle.sing<-
  import("code/analysis/sociallifestyle/singularity.xlsx")
sociallifestyle.sing

sociallifestyle.main.mod<-
  case_when(!sociallifestyle.sing[sociallifestyle.sing$rowname=="mod3",
                                  "singularity"]~"mod3",
            !sociallifestyle.sing[sociallifestyle.sing$rowname=="mod4",
                                  "singularity"]~"mod4",
            TRUE~"mod2")
sociallifestyle.main.mod

# obtain parameter estimates for selected main effect model

sociallifestyle.main<-
  import(paste0("code/analysis/sociallifestyle/",
                paste0(sociallifestyle.main.mod,"_FE.xlsx")))
sociallifestyle.main

sociallifestyle.main %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="sociallifestyle.z.gmc"~"Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.sociallifestyle.main

tidy.sociallifestyle.main



sociallifestyle.main.VC<-import(paste0("code/analysis/sociallifestyle/",
                                       paste0(sociallifestyle.main.mod,"_RE.xlsx")))
sociallifestyle.main.VC

sociallifestyle.main.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="sociallifestyle_salience.z.gmc" & is.na(var2) ~"Salience",
    var1=="sociallifestyle_salience.z.gmc:sociallifestyle.z.gmc" & is.na(var2) ~"GAL-TAN x Salience",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.sociallifestyle.main.VC
tidy.sociallifestyle.main.VC

# gndr model selection
sociallifestyle.sing

sociallifestyle.gndr.mod<-
  case_when(!sociallifestyle.sing[sociallifestyle.sing$rowname=="mod2.gndr",
                                  "singularity"]~"mod2.gndr",
            !sociallifestyle.sing[sociallifestyle.sing$rowname=="mod3.gndr",
                                  "singularity"]~"mod3.gndr",
            TRUE~"mod1.gndr")
sociallifestyle.gndr.mod


sociallifestyle.gndr<-
  import(paste0("code/analysis/sociallifestyle/",
                paste0(sociallifestyle.gndr.mod,"_FE.xlsx")))
sociallifestyle.gndr

sociallifestyle.gndr %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="sociallifestyle.z.gmc"~"Left-Right Gen.",
    rowname=="gndr.c:sociallifestyle.z.gmc"~"Gender x Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.sociallifestyle.gndr

tidy.sociallifestyle.gndr



sociallifestyle.gndr.VC<-import(paste0("code/analysis/sociallifestyle/",
                                       paste0(sociallifestyle.gndr.mod,"_RE.xlsx")))
sociallifestyle.gndr.VC

sociallifestyle.gndr.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="gndr.c" & is.na(var2) ~"Gender",
    var1=="gndr.c:sociallifestyle.z.gmc" & is.na(var2) ~"Gender x Left-Right Gen.",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.sociallifestyle.gndr.VC
tidy.sociallifestyle.gndr.VC

# minority model selection
sociallifestyle.sing

sociallifestyle.minority.mod<-
  case_when(!sociallifestyle.sing[sociallifestyle.sing$rowname=="mod2.minority",
                                  "singularity"]~"mod2.minority",
            !sociallifestyle.sing[sociallifestyle.sing$rowname=="mod3.minority",
                                  "singularity"]~"mod3.minority",
            TRUE~"mod1.minority")
sociallifestyle.minority.mod


sociallifestyle.minority<-
  import(paste0("code/analysis/sociallifestyle/",
                paste0(sociallifestyle.minority.mod,"_FE.xlsx")))
sociallifestyle.minority

sociallifestyle.minority %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="sociallifestyle.z.gmc"~"Left-Right Gen.",
    rowname=="gndr.c:sociallifestyle.z.gmc"~"Gender x Left-Right Gen.",
    rowname=="minority.c:sociallifestyle.z.gmc"~"Ethnic Minority x Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.sociallifestyle.minority

tidy.sociallifestyle.minority

sociallifestyle.minority.VC<-import(paste0("code/analysis/sociallifestyle/",
                                           paste0(sociallifestyle.minority.mod,"_RE.xlsx")))
sociallifestyle.minority.VC

sociallifestyle.minority.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="minority.c" & is.na(var2) ~"Ethnic minority",
    var1=="minority.c:sociallifestyle.z.gmc" & is.na(var2) ~"Ethnic minority x Left-Right Gen.",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.sociallifestyle.minority.VC
tidy.sociallifestyle.minority.VC

# combine the models

sociallifestyle.FEs<-full_join(
  full_join(
    tidy.sociallifestyle.main,
    tidy.sociallifestyle.gndr,
    by="Parameter"
  ),
  tidy.sociallifestyle.minority,
  by="Parameter"
)

sociallifestyle.FEs

sociallifestyle.VCs<-full_join(
  full_join(
    tidy.sociallifestyle.main.VC,
    tidy.sociallifestyle.gndr.VC,
    by="Parameter"
  ),
  tidy.sociallifestyle.minority.VC,
  by="Parameter"
)

sociallifestyle.VCs

nice_table(sociallifestyle.FEs,
           col.format.p = which(grepl("p",names(sociallifestyle.FEs))))

save_as_docx(nice_table(sociallifestyle.FEs,
                        col.format.p = which(grepl("p",names(sociallifestyle.FEs)))),
             path = "results/tables/sociallifestyle.FEs.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

save_as_docx(nice_table(sociallifestyle.VCs),
             path = "results/tables/sociallifestyle.VCs.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))






# religious_principle

religious_principle.sing<-
  import("code/analysis/religious_principle/singularity.xlsx")
religious_principle.sing

religious_principle.main.mod<-
  case_when(!religious_principle.sing[religious_principle.sing$rowname=="mod3",
                                  "singularity"]~"mod3",
            !religious_principle.sing[religious_principle.sing$rowname=="mod4",
                                  "singularity"]~"mod4",
            TRUE~"mod2")
religious_principle.main.mod

# obtain parameter estimates for selected main effect model

religious_principle.main<-
  import(paste0("code/analysis/religious_principle/",
                paste0(religious_principle.main.mod,"_FE.xlsx")))
religious_principle.main

religious_principle.main %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="religious_principle.z.gmc"~"Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.religious_principle.main

tidy.religious_principle.main



religious_principle.main.VC<-import(paste0("code/analysis/religious_principle/",
                                       paste0(religious_principle.main.mod,"_RE.xlsx")))
religious_principle.main.VC

religious_principle.main.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="religious_principle_salience.z.gmc" & is.na(var2) ~"Salience",
    var1=="religious_principle_salience.z.gmc:religious_principle.z.gmc" & is.na(var2) ~"GAL-TAN x Salience",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.religious_principle.main.VC
tidy.religious_principle.main.VC

# gndr model selection
religious_principle.sing

religious_principle.gndr.mod<-
  case_when(!religious_principle.sing[religious_principle.sing$rowname=="mod2.gndr",
                                  "singularity"]~"mod2.gndr",
            !religious_principle.sing[religious_principle.sing$rowname=="mod3.gndr",
                                  "singularity"]~"mod3.gndr",
            TRUE~"mod1.gndr")
religious_principle.gndr.mod


religious_principle.gndr<-
  import(paste0("code/analysis/religious_principle/",
                paste0(religious_principle.gndr.mod,"_FE.xlsx")))
religious_principle.gndr

religious_principle.gndr %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="religious_principle.z.gmc"~"Left-Right Gen.",
    rowname=="gndr.c:religious_principle.z.gmc"~"Gender x Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.religious_principle.gndr

tidy.religious_principle.gndr



religious_principle.gndr.VC<-import(paste0("code/analysis/religious_principle/",
                                       paste0(religious_principle.gndr.mod,"_RE.xlsx")))
religious_principle.gndr.VC

religious_principle.gndr.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="gndr.c" & is.na(var2) ~"Gender",
    var1=="gndr.c:religious_principle.z.gmc" & is.na(var2) ~"Gender x Left-Right Gen.",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.religious_principle.gndr.VC
tidy.religious_principle.gndr.VC

# minority model selection
religious_principle.sing

religious_principle.minority.mod<-
  case_when(!religious_principle.sing[religious_principle.sing$rowname=="mod2.minority",
                                  "singularity"]~"mod2.minority",
            !religious_principle.sing[religious_principle.sing$rowname=="mod3.minority",
                                  "singularity"]~"mod3.minority",
            TRUE~"mod1.minority")
religious_principle.minority.mod


religious_principle.minority<-
  import(paste0("code/analysis/religious_principle/",
                paste0(religious_principle.minority.mod,"_FE.xlsx")))
religious_principle.minority

religious_principle.minority %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="religious_principle.z.gmc"~"Left-Right Gen.",
    rowname=="gndr.c:religious_principle.z.gmc"~"Gender x Left-Right Gen.",
    rowname=="minority.c:religious_principle.z.gmc"~"Ethnic Minority x Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.religious_principle.minority

tidy.religious_principle.minority

religious_principle.minority.VC<-import(paste0("code/analysis/religious_principle/",
                                           paste0(religious_principle.minority.mod,"_RE.xlsx")))
religious_principle.minority.VC

religious_principle.minority.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="minority.c" & is.na(var2) ~"Ethnic minority",
    var1=="minority.c:religious_principle.z.gmc" & is.na(var2) ~"Ethnic minority x Left-Right Gen.",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.religious_principle.minority.VC
tidy.religious_principle.minority.VC

# combine the models

religious_principle.FEs<-full_join(
  full_join(
    tidy.religious_principle.main,
    tidy.religious_principle.gndr,
    by="Parameter"
  ),
  tidy.religious_principle.minority,
  by="Parameter"
)

religious_principle.FEs

religious_principle.VCs<-full_join(
  full_join(
    tidy.religious_principle.main.VC,
    tidy.religious_principle.gndr.VC,
    by="Parameter"
  ),
  tidy.religious_principle.minority.VC,
  by="Parameter"
)

religious_principle.VCs

nice_table(religious_principle.FEs,
           col.format.p = which(grepl("p",names(religious_principle.FEs))))

save_as_docx(nice_table(religious_principle.FEs,
                        col.format.p = which(grepl("p",names(religious_principle.FEs)))),
             path = "results/tables/religious_principle.FEs.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

save_as_docx(nice_table(religious_principle.VCs),
             path = "results/tables/religious_principle.VCs.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))







# immigrate_policy

immigrate_policy.sing<-
  import("code/analysis/immigrate_policy/singularity.xlsx")
immigrate_policy.sing

immigrate_policy.main.mod<-
  case_when(!immigrate_policy.sing[immigrate_policy.sing$rowname=="mod3",
                                      "singularity"]~"mod3",
            !immigrate_policy.sing[immigrate_policy.sing$rowname=="mod4",
                                      "singularity"]~"mod4",
            TRUE~"mod2")
immigrate_policy.main.mod

# obtain parameter estimates for selected main effect model

immigrate_policy.main<-
  import(paste0("code/analysis/immigrate_policy/",
                paste0(immigrate_policy.main.mod,"_FE.xlsx")))
immigrate_policy.main

immigrate_policy.main %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="immigrate_policy.z.gmc"~"Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.immigrate_policy.main

tidy.immigrate_policy.main



immigrate_policy.main.VC<-import(paste0("code/analysis/immigrate_policy/",
                                           paste0(immigrate_policy.main.mod,"_RE.xlsx")))
immigrate_policy.main.VC

immigrate_policy.main.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="immigrate_policy_salience.z.gmc" & is.na(var2) ~"Salience",
    var1=="immigrate_policy_salience.z.gmc:immigrate_policy.z.gmc" & is.na(var2) ~"GAL-TAN x Salience",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.immigrate_policy.main.VC
tidy.immigrate_policy.main.VC

# gndr model selection
immigrate_policy.sing

immigrate_policy.gndr.mod<-
  case_when(!immigrate_policy.sing[immigrate_policy.sing$rowname=="mod2.gndr",
                                      "singularity"]~"mod2.gndr",
            !immigrate_policy.sing[immigrate_policy.sing$rowname=="mod3.gndr",
                                      "singularity"]~"mod3.gndr",
            TRUE~"mod1.gndr")
immigrate_policy.gndr.mod


immigrate_policy.gndr<-
  import(paste0("code/analysis/immigrate_policy/",
                paste0(immigrate_policy.gndr.mod,"_FE.xlsx")))
immigrate_policy.gndr

immigrate_policy.gndr %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="immigrate_policy.z.gmc"~"Left-Right Gen.",
    rowname=="gndr.c:immigrate_policy.z.gmc"~"Gender x Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.immigrate_policy.gndr

tidy.immigrate_policy.gndr



immigrate_policy.gndr.VC<-import(paste0("code/analysis/immigrate_policy/",
                                           paste0(immigrate_policy.gndr.mod,"_RE.xlsx")))
immigrate_policy.gndr.VC

immigrate_policy.gndr.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="gndr.c" & is.na(var2) ~"Gender",
    var1=="gndr.c:immigrate_policy.z.gmc" & is.na(var2) ~"Gender x Left-Right Gen.",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.immigrate_policy.gndr.VC
tidy.immigrate_policy.gndr.VC

# minority model selection
immigrate_policy.sing

immigrate_policy.minority.mod<-
  case_when(!immigrate_policy.sing[immigrate_policy.sing$rowname=="mod2.minority",
                                      "singularity"]~"mod2.minority",
            !immigrate_policy.sing[immigrate_policy.sing$rowname=="mod3.minority",
                                      "singularity"]~"mod3.minority",
            TRUE~"mod1.minority")
immigrate_policy.minority.mod


immigrate_policy.minority<-
  import(paste0("code/analysis/immigrate_policy/",
                paste0(immigrate_policy.minority.mod,"_FE.xlsx")))
immigrate_policy.minority

immigrate_policy.minority %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="immigrate_policy.z.gmc"~"Left-Right Gen.",
    rowname=="gndr.c:immigrate_policy.z.gmc"~"Gender x Left-Right Gen.",
    rowname=="minority.c:immigrate_policy.z.gmc"~"Ethnic Minority x Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.immigrate_policy.minority

tidy.immigrate_policy.minority

immigrate_policy.minority.VC<-import(paste0("code/analysis/immigrate_policy/",
                                               paste0(immigrate_policy.minority.mod,"_RE.xlsx")))
immigrate_policy.minority.VC

immigrate_policy.minority.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="minority.c" & is.na(var2) ~"Ethnic minority",
    var1=="minority.c:immigrate_policy.z.gmc" & is.na(var2) ~"Ethnic minority x Left-Right Gen.",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.immigrate_policy.minority.VC
tidy.immigrate_policy.minority.VC

# combine the models

immigrate_policy.FEs<-full_join(
  full_join(
    tidy.immigrate_policy.main,
    tidy.immigrate_policy.gndr,
    by="Parameter"
  ),
  tidy.immigrate_policy.minority,
  by="Parameter"
)

immigrate_policy.FEs

immigrate_policy.VCs<-full_join(
  full_join(
    tidy.immigrate_policy.main.VC,
    tidy.immigrate_policy.gndr.VC,
    by="Parameter"
  ),
  tidy.immigrate_policy.minority.VC,
  by="Parameter"
)

immigrate_policy.VCs

nice_table(immigrate_policy.FEs,
           col.format.p = which(grepl("p",names(immigrate_policy.FEs))))

save_as_docx(nice_table(immigrate_policy.FEs,
                        col.format.p = which(grepl("p",names(immigrate_policy.FEs)))),
             path = "results/tables/immigrate_policy.FEs.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

save_as_docx(nice_table(immigrate_policy.VCs),
             path = "results/tables/immigrate_policy.VCs.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))





# multiculturalism

multiculturalism.sing<-
  import("code/analysis/multiculturalism/singularity.xlsx")
multiculturalism.sing

multiculturalism.main.mod<-
  case_when(!multiculturalism.sing[multiculturalism.sing$rowname=="mod3",
                                   "singularity"]~"mod3",
            !multiculturalism.sing[multiculturalism.sing$rowname=="mod4",
                                   "singularity"]~"mod4",
            TRUE~"mod2")
multiculturalism.main.mod

# obtain parameter estimates for selected main effect model

multiculturalism.main<-
  import(paste0("code/analysis/multiculturalism/",
                paste0(multiculturalism.main.mod,"_FE.xlsx")))
multiculturalism.main

multiculturalism.main %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="multiculturalism.z.gmc"~"Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.multiculturalism.main

tidy.multiculturalism.main



multiculturalism.main.VC<-import(paste0("code/analysis/multiculturalism/",
                                        paste0(multiculturalism.main.mod,"_RE.xlsx")))
multiculturalism.main.VC

multiculturalism.main.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="multiculturalism_salience.z.gmc" & is.na(var2) ~"Salience",
    var1=="multiculturalism_salience.z.gmc:multiculturalism.z.gmc" & is.na(var2) ~"GAL-TAN x Salience",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.multiculturalism.main.VC
tidy.multiculturalism.main.VC

# gndr model selection
multiculturalism.sing

multiculturalism.gndr.mod<-
  case_when(!multiculturalism.sing[multiculturalism.sing$rowname=="mod2.gndr",
                                   "singularity"]~"mod2.gndr",
            !multiculturalism.sing[multiculturalism.sing$rowname=="mod3.gndr",
                                   "singularity"]~"mod3.gndr",
            TRUE~"mod1.gndr")
multiculturalism.gndr.mod


multiculturalism.gndr<-
  import(paste0("code/analysis/multiculturalism/",
                paste0(multiculturalism.gndr.mod,"_FE.xlsx")))
multiculturalism.gndr

multiculturalism.gndr %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="multiculturalism.z.gmc"~"Left-Right Gen.",
    rowname=="gndr.c:multiculturalism.z.gmc"~"Gender x Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.multiculturalism.gndr

tidy.multiculturalism.gndr



multiculturalism.gndr.VC<-import(paste0("code/analysis/multiculturalism/",
                                        paste0(multiculturalism.gndr.mod,"_RE.xlsx")))
multiculturalism.gndr.VC

multiculturalism.gndr.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="gndr.c" & is.na(var2) ~"Gender",
    var1=="gndr.c:multiculturalism.z.gmc" & is.na(var2) ~"Gender x Left-Right Gen.",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.multiculturalism.gndr.VC
tidy.multiculturalism.gndr.VC

# minority model selection
multiculturalism.sing

multiculturalism.minority.mod<-
  case_when(!multiculturalism.sing[multiculturalism.sing$rowname=="mod2.minority",
                                   "singularity"]~"mod2.minority",
            !multiculturalism.sing[multiculturalism.sing$rowname=="mod3.minority",
                                   "singularity"]~"mod3.minority",
            TRUE~"mod1.minority")
multiculturalism.minority.mod


multiculturalism.minority<-
  import(paste0("code/analysis/multiculturalism/",
                paste0(multiculturalism.minority.mod,"_FE.xlsx")))
multiculturalism.minority

multiculturalism.minority %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="multiculturalism.z.gmc"~"Left-Right Gen.",
    rowname=="gndr.c:multiculturalism.z.gmc"~"Gender x Left-Right Gen.",
    rowname=="minority.c:multiculturalism.z.gmc"~"Ethnic Minority x Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.multiculturalism.minority

tidy.multiculturalism.minority

multiculturalism.minority.VC<-import(paste0("code/analysis/multiculturalism/",
                                            paste0(multiculturalism.minority.mod,"_RE.xlsx")))
multiculturalism.minority.VC

multiculturalism.minority.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="minority.c" & is.na(var2) ~"Ethnic minority",
    var1=="minority.c:multiculturalism.z.gmc" & is.na(var2) ~"Ethnic minority x Left-Right Gen.",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.multiculturalism.minority.VC
tidy.multiculturalism.minority.VC

# combine the models

multiculturalism.FEs<-full_join(
  full_join(
    tidy.multiculturalism.main,
    tidy.multiculturalism.gndr,
    by="Parameter"
  ),
  tidy.multiculturalism.minority,
  by="Parameter"
)

multiculturalism.FEs

multiculturalism.VCs<-full_join(
  full_join(
    tidy.multiculturalism.main.VC,
    tidy.multiculturalism.gndr.VC,
    by="Parameter"
  ),
  tidy.multiculturalism.minority.VC,
  by="Parameter"
)

multiculturalism.VCs

nice_table(multiculturalism.FEs,
           col.format.p = which(grepl("p",names(multiculturalism.FEs))))

save_as_docx(nice_table(multiculturalism.FEs,
                        col.format.p = which(grepl("p",names(multiculturalism.FEs)))),
             path = "results/tables/multiculturalism.FEs.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

save_as_docx(nice_table(multiculturalism.VCs),
             path = "results/tables/multiculturalism.VCs.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))






# urban_rural

urban_rural.sing<-
  import("code/analysis/urban_rural/singularity.xlsx")
urban_rural.sing

urban_rural.main.mod<-
  case_when(!urban_rural.sing[urban_rural.sing$rowname=="mod3",
                                   "singularity"]~"mod3",
            !urban_rural.sing[urban_rural.sing$rowname=="mod4",
                                   "singularity"]~"mod4",
            TRUE~"mod2")
urban_rural.main.mod

# obtain parameter estimates for selected main effect model

urban_rural.main<-
  import(paste0("code/analysis/urban_rural/",
                paste0(urban_rural.main.mod,"_FE.xlsx")))
urban_rural.main

urban_rural.main %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="urban_rural.z.gmc"~"Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.urban_rural.main

tidy.urban_rural.main



urban_rural.main.VC<-import(paste0("code/analysis/urban_rural/",
                                        paste0(urban_rural.main.mod,"_RE.xlsx")))
urban_rural.main.VC

urban_rural.main.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="urban_rural_salience.z.gmc" & is.na(var2) ~"Salience",
    var1=="urban_rural_salience.z.gmc:urban_rural.z.gmc" & is.na(var2) ~"GAL-TAN x Salience",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.urban_rural.main.VC
tidy.urban_rural.main.VC

# gndr model selection
urban_rural.sing

urban_rural.gndr.mod<-
  case_when(!urban_rural.sing[urban_rural.sing$rowname=="mod2.gndr",
                                   "singularity"]~"mod2.gndr",
            !urban_rural.sing[urban_rural.sing$rowname=="mod3.gndr",
                                   "singularity"]~"mod3.gndr",
            TRUE~"mod1.gndr")
urban_rural.gndr.mod


urban_rural.gndr<-
  import(paste0("code/analysis/urban_rural/",
                paste0(urban_rural.gndr.mod,"_FE.xlsx")))
urban_rural.gndr

urban_rural.gndr %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="urban_rural.z.gmc"~"Left-Right Gen.",
    rowname=="gndr.c:urban_rural.z.gmc"~"Gender x Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.urban_rural.gndr

tidy.urban_rural.gndr



urban_rural.gndr.VC<-import(paste0("code/analysis/urban_rural/",
                                        paste0(urban_rural.gndr.mod,"_RE.xlsx")))
urban_rural.gndr.VC

urban_rural.gndr.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="gndr.c" & is.na(var2) ~"Gender",
    var1=="gndr.c:urban_rural.z.gmc" & is.na(var2) ~"Gender x Left-Right Gen.",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.urban_rural.gndr.VC
tidy.urban_rural.gndr.VC

# minority model selection
urban_rural.sing

urban_rural.minority.mod<-
  case_when(!urban_rural.sing[urban_rural.sing$rowname=="mod2.minority",
                                   "singularity"]~"mod2.minority",
            !urban_rural.sing[urban_rural.sing$rowname=="mod3.minority",
                                   "singularity"]~"mod3.minority",
            TRUE~"mod1.minority")
urban_rural.minority.mod


urban_rural.minority<-
  import(paste0("code/analysis/urban_rural/",
                paste0(urban_rural.minority.mod,"_FE.xlsx")))
urban_rural.minority

urban_rural.minority %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="urban_rural.z.gmc"~"Left-Right Gen.",
    rowname=="gndr.c:urban_rural.z.gmc"~"Gender x Left-Right Gen.",
    rowname=="minority.c:urban_rural.z.gmc"~"Ethnic Minority x Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.urban_rural.minority

tidy.urban_rural.minority

urban_rural.minority.VC<-import(paste0("code/analysis/urban_rural/",
                                            paste0(urban_rural.minority.mod,"_RE.xlsx")))
urban_rural.minority.VC

urban_rural.minority.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="minority.c" & is.na(var2) ~"Ethnic minority",
    var1=="minority.c:urban_rural.z.gmc" & is.na(var2) ~"Ethnic minority x Left-Right Gen.",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.urban_rural.minority.VC
tidy.urban_rural.minority.VC

# combine the models

urban_rural.FEs<-full_join(
  full_join(
    tidy.urban_rural.main,
    tidy.urban_rural.gndr,
    by="Parameter"
  ),
  tidy.urban_rural.minority,
  by="Parameter"
)

urban_rural.FEs

urban_rural.VCs<-full_join(
  full_join(
    tidy.urban_rural.main.VC,
    tidy.urban_rural.gndr.VC,
    by="Parameter"
  ),
  tidy.urban_rural.minority.VC,
  by="Parameter"
)

urban_rural.VCs

nice_table(urban_rural.FEs,
           col.format.p = which(grepl("p",names(urban_rural.FEs))))

save_as_docx(nice_table(urban_rural.FEs,
                        col.format.p = which(grepl("p",names(urban_rural.FEs)))),
             path = "results/tables/urban_rural.FEs.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

save_as_docx(nice_table(urban_rural.VCs),
             path = "results/tables/urban_rural.VCs.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))






# environment

environment.sing<-
  import("code/analysis/environment/singularity.xlsx")
environment.sing

environment.main.mod<-
  case_when(!environment.sing[environment.sing$rowname=="mod3",
                              "singularity"]~"mod3",
            !environment.sing[environment.sing$rowname=="mod4",
                              "singularity"]~"mod4",
            TRUE~"mod2")
environment.main.mod

# obtain parameter estimates for selected main effect model

environment.main<-
  import(paste0("code/analysis/environment/",
                paste0(environment.main.mod,"_FE.xlsx")))
environment.main

environment.main %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="environment.z.gmc"~"Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.environment.main

tidy.environment.main



environment.main.VC<-import(paste0("code/analysis/environment/",
                                   paste0(environment.main.mod,"_RE.xlsx")))
environment.main.VC

environment.main.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="environment_salience.z.gmc" & is.na(var2) ~"Salience",
    var1=="environment_salience.z.gmc:environment.z.gmc" & is.na(var2) ~"GAL-TAN x Salience",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.environment.main.VC
tidy.environment.main.VC

# gndr model selection
environment.sing

environment.gndr.mod<-
  case_when(!environment.sing[environment.sing$rowname=="mod2.gndr",
                              "singularity"]~"mod2.gndr",
            !environment.sing[environment.sing$rowname=="mod3.gndr",
                              "singularity"]~"mod3.gndr",
            TRUE~"mod1.gndr")
environment.gndr.mod


environment.gndr<-
  import(paste0("code/analysis/environment/",
                paste0(environment.gndr.mod,"_FE.xlsx")))
environment.gndr

environment.gndr %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="environment.z.gmc"~"Left-Right Gen.",
    rowname=="gndr.c:environment.z.gmc"~"Gender x Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.environment.gndr

tidy.environment.gndr



environment.gndr.VC<-import(paste0("code/analysis/environment/",
                                   paste0(environment.gndr.mod,"_RE.xlsx")))
environment.gndr.VC

environment.gndr.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="gndr.c" & is.na(var2) ~"Gender",
    var1=="gndr.c:environment.z.gmc" & is.na(var2) ~"Gender x Left-Right Gen.",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.environment.gndr.VC
tidy.environment.gndr.VC

# minority model selection
environment.sing

environment.minority.mod<-
  case_when(!environment.sing[environment.sing$rowname=="mod2.minority",
                              "singularity"]~"mod2.minority",
            !environment.sing[environment.sing$rowname=="mod3.minority",
                              "singularity"]~"mod3.minority",
            TRUE~"mod1.minority")
environment.minority.mod


environment.minority<-
  import(paste0("code/analysis/environment/",
                paste0(environment.minority.mod,"_FE.xlsx")))
environment.minority

environment.minority %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="environment.z.gmc"~"Left-Right Gen.",
    rowname=="gndr.c:environment.z.gmc"~"Gender x Left-Right Gen.",
    rowname=="minority.c:environment.z.gmc"~"Ethnic Minority x Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.environment.minority

tidy.environment.minority

environment.minority.VC<-import(paste0("code/analysis/environment/",
                                       paste0(environment.minority.mod,"_RE.xlsx")))
environment.minority.VC

environment.minority.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="minority.c" & is.na(var2) ~"Ethnic minority",
    var1=="minority.c:environment.z.gmc" & is.na(var2) ~"Ethnic minority x Left-Right Gen.",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.environment.minority.VC
tidy.environment.minority.VC

# combine the models

environment.FEs<-full_join(
  full_join(
    tidy.environment.main,
    tidy.environment.gndr,
    by="Parameter"
  ),
  tidy.environment.minority,
  by="Parameter"
)

environment.FEs

environment.VCs<-full_join(
  full_join(
    tidy.environment.main.VC,
    tidy.environment.gndr.VC,
    by="Parameter"
  ),
  tidy.environment.minority.VC,
  by="Parameter"
)

environment.VCs

nice_table(environment.FEs,
           col.format.p = which(grepl("p",names(environment.FEs))))

save_as_docx(nice_table(environment.FEs,
                        col.format.p = which(grepl("p",names(environment.FEs)))),
             path = "results/tables/environment.FEs.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

save_as_docx(nice_table(environment.VCs),
             path = "results/tables/environment.VCs.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))






# regions

regions.sing<-
  import("code/analysis/regions/singularity.xlsx")
regions.sing

regions.main.mod<-
  case_when(!regions.sing[regions.sing$rowname=="mod3",
                              "singularity"]~"mod3",
            !regions.sing[regions.sing$rowname=="mod4",
                              "singularity"]~"mod4",
            TRUE~"mod2")
regions.main.mod

# obtain parameter estimates for selected main effect model

regions.main<-
  import(paste0("code/analysis/regions/",
                paste0(regions.main.mod,"_FE.xlsx")))
regions.main

regions.main %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="regions.z.gmc"~"Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.regions.main

tidy.regions.main



regions.main.VC<-import(paste0("code/analysis/regions/",
                                   paste0(regions.main.mod,"_RE.xlsx")))
regions.main.VC

regions.main.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="regions_salience.z.gmc" & is.na(var2) ~"Salience",
    var1=="regions_salience.z.gmc:regions.z.gmc" & is.na(var2) ~"GAL-TAN x Salience",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.regions.main.VC
tidy.regions.main.VC

# gndr model selection
regions.sing

regions.gndr.mod<-
  case_when(!regions.sing[regions.sing$rowname=="mod2.gndr",
                              "singularity"]~"mod2.gndr",
            !regions.sing[regions.sing$rowname=="mod3.gndr",
                              "singularity"]~"mod3.gndr",
            TRUE~"mod1.gndr")
regions.gndr.mod


regions.gndr<-
  import(paste0("code/analysis/regions/",
                paste0(regions.gndr.mod,"_FE.xlsx")))
regions.gndr

regions.gndr %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="regions.z.gmc"~"Left-Right Gen.",
    rowname=="gndr.c:regions.z.gmc"~"Gender x Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.regions.gndr

tidy.regions.gndr



regions.gndr.VC<-import(paste0("code/analysis/regions/",
                                   paste0(regions.gndr.mod,"_RE.xlsx")))
regions.gndr.VC

regions.gndr.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="gndr.c" & is.na(var2) ~"Gender",
    var1=="gndr.c:regions.z.gmc" & is.na(var2) ~"Gender x Left-Right Gen.",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.regions.gndr.VC
tidy.regions.gndr.VC

# minority model selection
regions.sing

regions.minority.mod<-
  case_when(!regions.sing[regions.sing$rowname=="mod2.minority",
                              "singularity"]~"mod2.minority",
            !regions.sing[regions.sing$rowname=="mod3.minority",
                              "singularity"]~"mod3.minority",
            TRUE~"mod1.minority")
regions.minority.mod


regions.minority<-
  import(paste0("code/analysis/regions/",
                paste0(regions.minority.mod,"_FE.xlsx")))
regions.minority

regions.minority %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="regions.z.gmc"~"Left-Right Gen.",
    rowname=="gndr.c:regions.z.gmc"~"Gender x Left-Right Gen.",
    rowname=="minority.c:regions.z.gmc"~"Ethnic Minority x Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.regions.minority

tidy.regions.minority

regions.minority.VC<-import(paste0("code/analysis/regions/",
                                       paste0(regions.minority.mod,"_RE.xlsx")))
regions.minority.VC

regions.minority.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="minority.c" & is.na(var2) ~"Ethnic minority",
    var1=="minority.c:regions.z.gmc" & is.na(var2) ~"Ethnic minority x Left-Right Gen.",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.regions.minority.VC
tidy.regions.minority.VC

# combine the models

regions.FEs<-full_join(
  full_join(
    tidy.regions.main,
    tidy.regions.gndr,
    by="Parameter"
  ),
  tidy.regions.minority,
  by="Parameter"
)

regions.FEs

regions.VCs<-full_join(
  full_join(
    tidy.regions.main.VC,
    tidy.regions.gndr.VC,
    by="Parameter"
  ),
  tidy.regions.minority.VC,
  by="Parameter"
)

regions.VCs

nice_table(regions.FEs,
           col.format.p = which(grepl("p",names(regions.FEs))))

save_as_docx(nice_table(regions.FEs,
                        col.format.p = which(grepl("p",names(regions.FEs)))),
             path = "results/tables/regions.FEs.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

save_as_docx(nice_table(regions.VCs),
             path = "results/tables/regions.VCs.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))






# international_security

international_security.sing<-
  import("code/analysis/international_security/singularity.xlsx")
international_security.sing

international_security.main.mod<-
  case_when(!international_security.sing[international_security.sing$rowname=="mod3",
                          "singularity"]~"mod3",
            !international_security.sing[international_security.sing$rowname=="mod4",
                          "singularity"]~"mod4",
            TRUE~"mod2")
international_security.main.mod

# obtain parameter estimates for selected main effect model

international_security.main<-
  import(paste0("code/analysis/international_security/",
                paste0(international_security.main.mod,"_FE.xlsx")))
international_security.main

international_security.main %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="international_security.z.gmc"~"Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.international_security.main

tidy.international_security.main



international_security.main.VC<-import(paste0("code/analysis/international_security/",
                               paste0(international_security.main.mod,"_RE.xlsx")))
international_security.main.VC

international_security.main.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="international_security_salience.z.gmc" & is.na(var2) ~"Salience",
    var1=="international_security_salience.z.gmc:international_security.z.gmc" & is.na(var2) ~"GAL-TAN x Salience",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.international_security.main.VC
tidy.international_security.main.VC

# gndr model selection
international_security.sing

international_security.gndr.mod<-
  case_when(!international_security.sing[international_security.sing$rowname=="mod2.gndr",
                          "singularity"]~"mod2.gndr",
            !international_security.sing[international_security.sing$rowname=="mod3.gndr",
                          "singularity"]~"mod3.gndr",
            TRUE~"mod1.gndr")
international_security.gndr.mod


international_security.gndr<-
  import(paste0("code/analysis/international_security/",
                paste0(international_security.gndr.mod,"_FE.xlsx")))
international_security.gndr

international_security.gndr %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="international_security.z.gmc"~"Left-Right Gen.",
    rowname=="gndr.c:international_security.z.gmc"~"Gender x Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.international_security.gndr

tidy.international_security.gndr



international_security.gndr.VC<-import(paste0("code/analysis/international_security/",
                               paste0(international_security.gndr.mod,"_RE.xlsx")))
international_security.gndr.VC

international_security.gndr.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="gndr.c" & is.na(var2) ~"Gender",
    var1=="gndr.c:international_security.z.gmc" & is.na(var2) ~"Gender x Left-Right Gen.",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.international_security.gndr.VC
tidy.international_security.gndr.VC

# minority model selection
international_security.sing

international_security.minority.mod<-
  case_when(!international_security.sing[international_security.sing$rowname=="mod2.minority",
                          "singularity"]~"mod2.minority",
            !international_security.sing[international_security.sing$rowname=="mod3.minority",
                          "singularity"]~"mod3.minority",
            TRUE~"mod1.minority")
international_security.minority.mod


international_security.minority<-
  import(paste0("code/analysis/international_security/",
                paste0(international_security.minority.mod,"_FE.xlsx")))
international_security.minority

international_security.minority %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="international_security.z.gmc"~"Left-Right Gen.",
    rowname=="gndr.c:international_security.z.gmc"~"Gender x Left-Right Gen.",
    rowname=="minority.c:international_security.z.gmc"~"Ethnic Minority x Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.international_security.minority

tidy.international_security.minority

international_security.minority.VC<-import(paste0("code/analysis/international_security/",
                                   paste0(international_security.minority.mod,"_RE.xlsx")))
international_security.minority.VC

international_security.minority.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="minority.c" & is.na(var2) ~"Ethnic minority",
    var1=="minority.c:international_security.z.gmc" & is.na(var2) ~"Ethnic minority x Left-Right Gen.",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.international_security.minority.VC
tidy.international_security.minority.VC

# combine the models

international_security.FEs<-full_join(
  full_join(
    tidy.international_security.main,
    tidy.international_security.gndr,
    by="Parameter"
  ),
  tidy.international_security.minority,
  by="Parameter"
)

international_security.FEs

international_security.VCs<-full_join(
  full_join(
    tidy.international_security.main.VC,
    tidy.international_security.gndr.VC,
    by="Parameter"
  ),
  tidy.international_security.minority.VC,
  by="Parameter"
)

international_security.VCs

nice_table(international_security.FEs,
           col.format.p = which(grepl("p",names(international_security.FEs))))

save_as_docx(nice_table(international_security.FEs,
                        col.format.p = which(grepl("p",names(international_security.FEs)))),
             path = "results/tables/international_security.FEs.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

save_as_docx(nice_table(international_security.VCs),
             path = "results/tables/international_security.VCs.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))






# ethnic_minorities

ethnic_minorities.sing<-
  import("code/analysis/ethnic_minorities/singularity.xlsx")
ethnic_minorities.sing

ethnic_minorities.main.mod<-
  case_when(!ethnic_minorities.sing[ethnic_minorities.sing$rowname=="mod3",
                                         "singularity"]~"mod3",
            !ethnic_minorities.sing[ethnic_minorities.sing$rowname=="mod4",
                                         "singularity"]~"mod4",
            TRUE~"mod2")
ethnic_minorities.main.mod

# obtain parameter estimates for selected main effect model

ethnic_minorities.main<-
  import(paste0("code/analysis/ethnic_minorities/",
                paste0(ethnic_minorities.main.mod,"_FE.xlsx")))
ethnic_minorities.main

ethnic_minorities.main %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="ethnic_minorities.z.gmc"~"Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.ethnic_minorities.main

tidy.ethnic_minorities.main



ethnic_minorities.main.VC<-import(paste0("code/analysis/ethnic_minorities/",
                                              paste0(ethnic_minorities.main.mod,"_RE.xlsx")))
ethnic_minorities.main.VC

ethnic_minorities.main.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="ethnic_minorities_salience.z.gmc" & is.na(var2) ~"Salience",
    var1=="ethnic_minorities_salience.z.gmc:ethnic_minorities.z.gmc" & is.na(var2) ~"GAL-TAN x Salience",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.ethnic_minorities.main.VC
tidy.ethnic_minorities.main.VC

# gndr model selection
ethnic_minorities.sing

ethnic_minorities.gndr.mod<-
  case_when(!ethnic_minorities.sing[ethnic_minorities.sing$rowname=="mod2.gndr",
                                         "singularity"]~"mod2.gndr",
            !ethnic_minorities.sing[ethnic_minorities.sing$rowname=="mod3.gndr",
                                         "singularity"]~"mod3.gndr",
            TRUE~"mod1.gndr")
ethnic_minorities.gndr.mod


ethnic_minorities.gndr<-
  import(paste0("code/analysis/ethnic_minorities/",
                paste0(ethnic_minorities.gndr.mod,"_FE.xlsx")))
ethnic_minorities.gndr

ethnic_minorities.gndr %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="ethnic_minorities.z.gmc"~"Left-Right Gen.",
    rowname=="gndr.c:ethnic_minorities.z.gmc"~"Gender x Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.ethnic_minorities.gndr

tidy.ethnic_minorities.gndr



ethnic_minorities.gndr.VC<-import(paste0("code/analysis/ethnic_minorities/",
                                              paste0(ethnic_minorities.gndr.mod,"_RE.xlsx")))
ethnic_minorities.gndr.VC

ethnic_minorities.gndr.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="gndr.c" & is.na(var2) ~"Gender",
    var1=="gndr.c:ethnic_minorities.z.gmc" & is.na(var2) ~"Gender x Left-Right Gen.",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.ethnic_minorities.gndr.VC
tidy.ethnic_minorities.gndr.VC

# minority model selection
ethnic_minorities.sing

ethnic_minorities.minority.mod<-
  case_when(!ethnic_minorities.sing[ethnic_minorities.sing$rowname=="mod2.minority",
                                         "singularity"]~"mod2.minority",
            !ethnic_minorities.sing[ethnic_minorities.sing$rowname=="mod3.minority",
                                         "singularity"]~"mod3.minority",
            TRUE~"mod1.minority")
ethnic_minorities.minority.mod


ethnic_minorities.minority<-
  import(paste0("code/analysis/ethnic_minorities/",
                paste0(ethnic_minorities.minority.mod,"_FE.xlsx")))
ethnic_minorities.minority

ethnic_minorities.minority %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="ethnic_minorities.z.gmc"~"Left-Right Gen.",
    rowname=="gndr.c:ethnic_minorities.z.gmc"~"Gender x Left-Right Gen.",
    rowname=="minority.c:ethnic_minorities.z.gmc"~"Ethnic Minority x Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.ethnic_minorities.minority

tidy.ethnic_minorities.minority

ethnic_minorities.minority.VC<-import(paste0("code/analysis/ethnic_minorities/",
                                                  paste0(ethnic_minorities.minority.mod,"_RE.xlsx")))
ethnic_minorities.minority.VC

ethnic_minorities.minority.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="minority.c" & is.na(var2) ~"Ethnic minority",
    var1=="minority.c:ethnic_minorities.z.gmc" & is.na(var2) ~"Ethnic minority x Left-Right Gen.",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.ethnic_minorities.minority.VC
tidy.ethnic_minorities.minority.VC

# combine the models

ethnic_minorities.FEs<-full_join(
  full_join(
    tidy.ethnic_minorities.main,
    tidy.ethnic_minorities.gndr,
    by="Parameter"
  ),
  tidy.ethnic_minorities.minority,
  by="Parameter"
)

ethnic_minorities.FEs

ethnic_minorities.VCs<-full_join(
  full_join(
    tidy.ethnic_minorities.main.VC,
    tidy.ethnic_minorities.gndr.VC,
    by="Parameter"
  ),
  tidy.ethnic_minorities.minority.VC,
  by="Parameter"
)

ethnic_minorities.VCs

nice_table(ethnic_minorities.FEs,
           col.format.p = which(grepl("p",names(ethnic_minorities.FEs))))

save_as_docx(nice_table(ethnic_minorities.FEs,
                        col.format.p = which(grepl("p",names(ethnic_minorities.FEs)))),
             path = "results/tables/ethnic_minorities.FEs.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

save_as_docx(nice_table(ethnic_minorities.VCs),
             path = "results/tables/ethnic_minorities.VCs.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))






# nationalism

nationalism.sing<-
  import("code/analysis/nationalism/singularity.xlsx")
nationalism.sing

nationalism.main.mod<-
  case_when(!nationalism.sing[nationalism.sing$rowname=="mod3",
                                    "singularity"]~"mod3",
            !nationalism.sing[nationalism.sing$rowname=="mod4",
                                    "singularity"]~"mod4",
            TRUE~"mod2")
nationalism.main.mod

# obtain parameter estimates for selected main effect model

nationalism.main<-
  import(paste0("code/analysis/nationalism/",
                paste0(nationalism.main.mod,"_FE.xlsx")))
nationalism.main

nationalism.main %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="nationalism.z.gmc"~"Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.nationalism.main

tidy.nationalism.main



nationalism.main.VC<-import(paste0("code/analysis/nationalism/",
                                         paste0(nationalism.main.mod,"_RE.xlsx")))
nationalism.main.VC

nationalism.main.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="nationalism_salience.z.gmc" & is.na(var2) ~"Salience",
    var1=="nationalism_salience.z.gmc:nationalism.z.gmc" & is.na(var2) ~"GAL-TAN x Salience",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.nationalism.main.VC
tidy.nationalism.main.VC

# gndr model selection
nationalism.sing

nationalism.gndr.mod<-
  case_when(!nationalism.sing[nationalism.sing$rowname=="mod2.gndr",
                                    "singularity"]~"mod2.gndr",
            !nationalism.sing[nationalism.sing$rowname=="mod3.gndr",
                                    "singularity"]~"mod3.gndr",
            TRUE~"mod1.gndr")
nationalism.gndr.mod


nationalism.gndr<-
  import(paste0("code/analysis/nationalism/",
                paste0(nationalism.gndr.mod,"_FE.xlsx")))
nationalism.gndr

nationalism.gndr %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="nationalism.z.gmc"~"Left-Right Gen.",
    rowname=="gndr.c:nationalism.z.gmc"~"Gender x Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.nationalism.gndr

tidy.nationalism.gndr



nationalism.gndr.VC<-import(paste0("code/analysis/nationalism/",
                                         paste0(nationalism.gndr.mod,"_RE.xlsx")))
nationalism.gndr.VC

nationalism.gndr.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="gndr.c" & is.na(var2) ~"Gender",
    var1=="gndr.c:nationalism.z.gmc" & is.na(var2) ~"Gender x Left-Right Gen.",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.nationalism.gndr.VC
tidy.nationalism.gndr.VC

# minority model selection
nationalism.sing

nationalism.minority.mod<-
  case_when(!nationalism.sing[nationalism.sing$rowname=="mod2.minority",
                                    "singularity"]~"mod2.minority",
            !nationalism.sing[nationalism.sing$rowname=="mod3.minority",
                                    "singularity"]~"mod3.minority",
            TRUE~"mod1.minority")
nationalism.minority.mod


nationalism.minority<-
  import(paste0("code/analysis/nationalism/",
                paste0(nationalism.minority.mod,"_FE.xlsx")))
nationalism.minority

nationalism.minority %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="nationalism.z.gmc"~"Left-Right Gen.",
    rowname=="gndr.c:nationalism.z.gmc"~"Gender x Left-Right Gen.",
    rowname=="minority.c:nationalism.z.gmc"~"Ethnic Minority x Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.nationalism.minority

tidy.nationalism.minority

nationalism.minority.VC<-import(paste0("code/analysis/nationalism/",
                                             paste0(nationalism.minority.mod,"_RE.xlsx")))
nationalism.minority.VC

nationalism.minority.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="minority.c" & is.na(var2) ~"Ethnic minority",
    var1=="minority.c:nationalism.z.gmc" & is.na(var2) ~"Ethnic minority x Left-Right Gen.",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.nationalism.minority.VC
tidy.nationalism.minority.VC

# combine the models

nationalism.FEs<-full_join(
  full_join(
    tidy.nationalism.main,
    tidy.nationalism.gndr,
    by="Parameter"
  ),
  tidy.nationalism.minority,
  by="Parameter"
)

nationalism.FEs

nationalism.VCs<-full_join(
  full_join(
    tidy.nationalism.main.VC,
    tidy.nationalism.gndr.VC,
    by="Parameter"
  ),
  tidy.nationalism.minority.VC,
  by="Parameter"
)

nationalism.VCs

nice_table(nationalism.FEs,
           col.format.p = which(grepl("p",names(nationalism.FEs))))

save_as_docx(nice_table(nationalism.FEs,
                        col.format.p = which(grepl("p",names(nationalism.FEs)))),
             path = "results/tables/nationalism.FEs.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

save_as_docx(nice_table(nationalism.VCs),
             path = "results/tables/nationalism.VCs.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))






# antielite_salience

antielite_salience.sing<-
  import("code/analysis/antielite_salience/singularity.xlsx")
antielite_salience.sing

antielite_salience.main.mod<-
  case_when(!antielite_salience.sing[antielite_salience.sing$rowname=="mod3",
                              "singularity"]~"mod3",
            !antielite_salience.sing[antielite_salience.sing$rowname=="mod4",
                              "singularity"]~"mod4",
            TRUE~"mod2")
antielite_salience.main.mod

# obtain parameter estimates for selected main effect model

antielite_salience.main<-
  import(paste0("code/analysis/antielite_salience/",
                paste0(antielite_salience.main.mod,"_FE.xlsx")))
antielite_salience.main

antielite_salience.main %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="antielite_salience.z.gmc"~"Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.antielite_salience.main

tidy.antielite_salience.main



antielite_salience.main.VC<-import(paste0("code/analysis/antielite_salience/",
                                   paste0(antielite_salience.main.mod,"_RE.xlsx")))
antielite_salience.main.VC

antielite_salience.main.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="antielite_salience_salience.z.gmc" & is.na(var2) ~"Salience",
    var1=="antielite_salience_salience.z.gmc:antielite_salience.z.gmc" & is.na(var2) ~"GAL-TAN x Salience",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.antielite_salience.main.VC
tidy.antielite_salience.main.VC

# gndr model selection
antielite_salience.sing

antielite_salience.gndr.mod<-
  case_when(!antielite_salience.sing[antielite_salience.sing$rowname=="mod2.gndr",
                              "singularity"]~"mod2.gndr",
            !antielite_salience.sing[antielite_salience.sing$rowname=="mod3.gndr",
                              "singularity"]~"mod3.gndr",
            TRUE~"mod1.gndr")
antielite_salience.gndr.mod


antielite_salience.gndr<-
  import(paste0("code/analysis/antielite_salience/",
                paste0(antielite_salience.gndr.mod,"_FE.xlsx")))
antielite_salience.gndr

antielite_salience.gndr %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="antielite_salience.z.gmc"~"Left-Right Gen.",
    rowname=="gndr.c:antielite_salience.z.gmc"~"Gender x Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.antielite_salience.gndr

tidy.antielite_salience.gndr



antielite_salience.gndr.VC<-import(paste0("code/analysis/antielite_salience/",
                                   paste0(antielite_salience.gndr.mod,"_RE.xlsx")))
antielite_salience.gndr.VC

antielite_salience.gndr.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="gndr.c" & is.na(var2) ~"Gender",
    var1=="gndr.c:antielite_salience.z.gmc" & is.na(var2) ~"Gender x Left-Right Gen.",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.antielite_salience.gndr.VC
tidy.antielite_salience.gndr.VC

# minority model selection
antielite_salience.sing

antielite_salience.minority.mod<-
  case_when(!antielite_salience.sing[antielite_salience.sing$rowname=="mod2.minority",
                              "singularity"]~"mod2.minority",
            !antielite_salience.sing[antielite_salience.sing$rowname=="mod3.minority",
                              "singularity"]~"mod3.minority",
            TRUE~"mod1.minority")
antielite_salience.minority.mod


antielite_salience.minority<-
  import(paste0("code/analysis/antielite_salience/",
                paste0(antielite_salience.minority.mod,"_FE.xlsx")))
antielite_salience.minority

antielite_salience.minority %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="antielite_salience.z.gmc"~"Left-Right Gen.",
    rowname=="gndr.c:antielite_salience.z.gmc"~"Gender x Left-Right Gen.",
    rowname=="minority.c:antielite_salience.z.gmc"~"Ethnic Minority x Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.antielite_salience.minority

tidy.antielite_salience.minority

antielite_salience.minority.VC<-import(paste0("code/analysis/antielite_salience/",
                                       paste0(antielite_salience.minority.mod,"_RE.xlsx")))
antielite_salience.minority.VC

antielite_salience.minority.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="minority.c" & is.na(var2) ~"Ethnic minority",
    var1=="minority.c:antielite_salience.z.gmc" & is.na(var2) ~"Ethnic minority x Left-Right Gen.",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.antielite_salience.minority.VC
tidy.antielite_salience.minority.VC

# combine the models

antielite_salience.FEs<-full_join(
  full_join(
    tidy.antielite_salience.main,
    tidy.antielite_salience.gndr,
    by="Parameter"
  ),
  tidy.antielite_salience.minority,
  by="Parameter"
)

antielite_salience.FEs

antielite_salience.VCs<-full_join(
  full_join(
    tidy.antielite_salience.main.VC,
    tidy.antielite_salience.gndr.VC,
    by="Parameter"
  ),
  tidy.antielite_salience.minority.VC,
  by="Parameter"
)

antielite_salience.VCs

nice_table(antielite_salience.FEs,
           col.format.p = which(grepl("p",names(antielite_salience.FEs))))

save_as_docx(nice_table(antielite_salience.FEs,
                        col.format.p = which(grepl("p",names(antielite_salience.FEs)))),
             path = "results/tables/antielite_salience.FEs.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

save_as_docx(nice_table(antielite_salience.VCs),
             path = "results/tables/antielite_salience.VCs.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))






# corrupt_salience

corrupt_salience.sing<-
  import("code/analysis/corrupt_salience/singularity.xlsx")
corrupt_salience.sing

corrupt_salience.main.mod<-
  case_when(!corrupt_salience.sing[corrupt_salience.sing$rowname=="mod3",
                                     "singularity"]~"mod3",
            !corrupt_salience.sing[corrupt_salience.sing$rowname=="mod4",
                                     "singularity"]~"mod4",
            TRUE~"mod2")
corrupt_salience.main.mod

# obtain parameter estimates for selected main effect model

corrupt_salience.main<-
  import(paste0("code/analysis/corrupt_salience/",
                paste0(corrupt_salience.main.mod,"_FE.xlsx")))
corrupt_salience.main

corrupt_salience.main %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="corrupt_salience.z.gmc"~"Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.corrupt_salience.main

tidy.corrupt_salience.main



corrupt_salience.main.VC<-import(paste0("code/analysis/corrupt_salience/",
                                          paste0(corrupt_salience.main.mod,"_RE.xlsx")))
corrupt_salience.main.VC

corrupt_salience.main.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="corrupt_salience_salience.z.gmc" & is.na(var2) ~"Salience",
    var1=="corrupt_salience_salience.z.gmc:corrupt_salience.z.gmc" & is.na(var2) ~"GAL-TAN x Salience",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.corrupt_salience.main.VC
tidy.corrupt_salience.main.VC

# gndr model selection
corrupt_salience.sing

corrupt_salience.gndr.mod<-
  case_when(!corrupt_salience.sing[corrupt_salience.sing$rowname=="mod2.gndr",
                                     "singularity"]~"mod2.gndr",
            !corrupt_salience.sing[corrupt_salience.sing$rowname=="mod3.gndr",
                                     "singularity"]~"mod3.gndr",
            TRUE~"mod1.gndr")
corrupt_salience.gndr.mod


corrupt_salience.gndr<-
  import(paste0("code/analysis/corrupt_salience/",
                paste0(corrupt_salience.gndr.mod,"_FE.xlsx")))
corrupt_salience.gndr

corrupt_salience.gndr %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="corrupt_salience.z.gmc"~"Left-Right Gen.",
    rowname=="gndr.c:corrupt_salience.z.gmc"~"Gender x Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.corrupt_salience.gndr

tidy.corrupt_salience.gndr



corrupt_salience.gndr.VC<-import(paste0("code/analysis/corrupt_salience/",
                                          paste0(corrupt_salience.gndr.mod,"_RE.xlsx")))
corrupt_salience.gndr.VC

corrupt_salience.gndr.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="gndr.c" & is.na(var2) ~"Gender",
    var1=="gndr.c:corrupt_salience.z.gmc" & is.na(var2) ~"Gender x Left-Right Gen.",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.corrupt_salience.gndr.VC
tidy.corrupt_salience.gndr.VC

# minority model selection
corrupt_salience.sing

corrupt_salience.minority.mod<-
  case_when(!corrupt_salience.sing[corrupt_salience.sing$rowname=="mod2.minority",
                                     "singularity"]~"mod2.minority",
            !corrupt_salience.sing[corrupt_salience.sing$rowname=="mod3.minority",
                                     "singularity"]~"mod3.minority",
            TRUE~"mod1.minority")
corrupt_salience.minority.mod


corrupt_salience.minority<-
  import(paste0("code/analysis/corrupt_salience/",
                paste0(corrupt_salience.minority.mod,"_FE.xlsx")))
corrupt_salience.minority

corrupt_salience.minority %>%
  dplyr::select(rowname,b=Est.,SE,p,LL,UL) %>%
  mutate(Parameter=case_when(
    rowname=="(Intercept)"~"Intercept",
    rowname=="gndr.c"~"Gender",
    rowname=="age10.c"~"Age",
    rowname=="minority.c"~"Ethnic minority",
    rowname=="corrupt_salience.z.gmc"~"Left-Right Gen.",
    rowname=="gndr.c:corrupt_salience.z.gmc"~"Gender x Left-Right Gen.",
    rowname=="minority.c:corrupt_salience.z.gmc"~"Ethnic Minority x Left-Right Gen.",
    TRUE~rowname),
    OR=round_tidy(exp(b),2),
    Est.=round_tidy(b,2),SE=round_tidy(SE,2),
    CI=paste0("[",round_tidy(LL,2),", ",round_tidy(UL,2),"]")) %>%
  dplyr::select(Parameter,'Est.',SE,p)  -> tidy.corrupt_salience.minority

tidy.corrupt_salience.minority

corrupt_salience.minority.VC<-import(paste0("code/analysis/corrupt_salience/",
                                              paste0(corrupt_salience.minority.mod,"_RE.xlsx")))
corrupt_salience.minority.VC

corrupt_salience.minority.VC %>%
  dplyr::select(var1,var2,sdcor) %>%
  mutate(Parameter=case_when(
    var1=="(Intercept)" & is.na(var2) ~"Intercept",
    var1=="minority.c" & is.na(var2) ~"Ethnic minority",
    var1=="minority.c:corrupt_salience.z.gmc" & is.na(var2) ~"Ethnic minority x Left-Right Gen.",
    TRUE~var1),
    'Est.'=round_tidy(sdcor,2)) %>%
  dplyr::select(Parameter,'Est.') -> tidy.corrupt_salience.minority.VC
tidy.corrupt_salience.minority.VC

# combine the models

corrupt_salience.FEs<-full_join(
  full_join(
    tidy.corrupt_salience.main,
    tidy.corrupt_salience.gndr,
    by="Parameter"
  ),
  tidy.corrupt_salience.minority,
  by="Parameter"
)

corrupt_salience.FEs

corrupt_salience.VCs<-full_join(
  full_join(
    tidy.corrupt_salience.main.VC,
    tidy.corrupt_salience.gndr.VC,
    by="Parameter"
  ),
  tidy.corrupt_salience.minority.VC,
  by="Parameter"
)

corrupt_salience.VCs

nice_table(corrupt_salience.FEs,
           col.format.p = which(grepl("p",names(corrupt_salience.FEs))))

save_as_docx(nice_table(corrupt_salience.FEs,
                        col.format.p = which(grepl("p",names(corrupt_salience.FEs)))),
             path = "results/tables/corrupt_salience.FEs.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

save_as_docx(nice_table(corrupt_salience.VCs),
             path = "results/tables/corrupt_salience.VCs.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))
