
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

