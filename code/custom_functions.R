get.ESS.label<-function(var){
  attr(var,which = "label")
}



vif.mer <- function (fit) {
  ## adapted from rms::vif
  
  v <- vcov(fit)
  nam <- names(fixef(fit))
  
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}


# pipe for level-1 moderations

analysis_pipe<-function(predictor,directory){
  
  #predictor="lrgen"
  IV=paste0(predictor,".z.gmc")
  #directory="Z:/postdoc/childless_left/code/analysis"
  dir.create(path = paste0(directory,"/",predictor))
  
  dir.temp<-paste0(directory,"/",predictor,"/")
  
  # select only the defined variables
  exdat<-fdat %>%
    dplyr::select(childlessness,
                  gndr.f,agea,minority,
                  gndr.c,age10.c,minority.c,
                  paste0(predictor,".z"),
                  cntry,
                  West_vs_post_comm,
                  anweight) %>%
    na.omit()
  
  exdat<-
    group_mean_center(
      data=exdat,group.var="cntry",
      vars=paste0(predictor,".z"),
      grand.init = F)
  
  
  # fit fixed effects model
  # define the formula
  mod2.f<-
    as.formula(paste0("childlessness","~",
                      "gndr.c+age10.c+minority.c+",
                      IV,"+(1|cntry)"))
  
  ## fit the model
  mod2<-glmer(mod2.f,
              data=exdat,
              family=binomial(link="logit"),weights = anweight,
              control = glmerControl(optimizer="bobyqa",
                                     optCtrl=list(maxfun=2e6)))
  ## collect important output
  getFE_glmer(mod2,round = 10,p.round=10)
  ## Fixed effects
  export(rownames_to_column(getFE_glmer(mod2,round = 10,p.round=10)),
         paste0(dir.temp,"mod2_FE.xlsx"),overwrite=T)
  
  ## Random effects
  export(getVC(mod2,round = 12),
         paste0(dir.temp,"mod2_RE.xlsx"),overwrite=T)
  
  # fit random effects model
  # define the formula
  mod3.f<-
    as.formula(paste0("childlessness","~",
                      "gndr.c+age10.c+minority.c+",
                      IV,"+(",IV,"|cntry)"))
  
  ## fit the model
  mod3<-glmer(mod3.f,
              data=exdat,
              family=binomial(link="logit"),weights = anweight,
              control = glmerControl(optimizer="bobyqa",
                                     optCtrl=list(maxfun=2e6)))
  ## collect important output
  
  ## Fixed effects
  export(rownames_to_column(getFE_glmer(mod3,round = 10,p.round=10)),
         paste0(dir.temp,"mod3_FE.xlsx"),overwrite=T)
  
  ## Random effects
  export(getVC(mod3,round = 12),
         paste0(dir.temp,"mod3_RE.xlsx"),overwrite=T)
  
  ## Model comparison to fixed only
  
  export(rownames_to_column(data.frame(anova(mod2,mod3))),
         paste0(dir.temp,"mod3_MC.xlsx"),overwrite=T)
  
  # fit random effects model without random effect correlation
  # define the formula
  mod4.f<-
    as.formula(paste0("childlessness","~",
                      "gndr.c+age10.c+minority.c+",
                      IV,"+(",IV,"||cntry)"))
  
  ## fit the model
  mod4<-glmer(mod4.f,
              data=exdat,
              family=binomial(link="logit"),weights = anweight,
              control = glmerControl(optimizer="bobyqa",
                                     optCtrl=list(maxfun=2e6)))
  ## collect important output
  
  ## Fixed effects
  export(rownames_to_column(getFE_glmer(mod4,round = 10,p.round=10)),
         paste0(dir.temp,"mod4_FE.xlsx"),overwrite=T)
  
  ## Random effects
  export(getVC(mod4,round = 12),
         paste0(dir.temp,"mod4_RE.xlsx"),overwrite=T)
  
  ## Model comparison to fixed only
  
  export(rownames_to_column(data.frame(anova(mod2,mod4))),
         paste0(dir.temp,"mod4_MC.xlsx"),overwrite=T)
  
  ## Model comparison to random with correlation
  
  export(rownames_to_column(data.frame(anova(mod4,mod3))),
         paste0(dir.temp,"mod4_MC2.xlsx"),overwrite=T)
  
  ## Moderation by gender
  
  ### select which model is extended
  
  if (!isSingular(mod3)) {
    temp.mod<-mod3
  } else if (!isSingular(mod4)){
    temp.mod<-mod4
  } else {
    temp.mod<-mod2
  }
  
  # formula for fixed only
  
  mod1.gndr.f<-
    as.formula(paste0("childlessness","~",
                                paste0(as.character(formula(temp.mod))[3],
                                       "+gndr.c:",IV)))
  
  ## fit the model
  mod1.gndr<-glmer(mod1.gndr.f,
              data=exdat,
              family=binomial(link="logit"),weights = anweight,
              control = glmerControl(optimizer="bobyqa",
                                     optCtrl=list(maxfun=2e6)))

  ## collect important output
  
  ## Fixed effects
  export(rownames_to_column(getFE_glmer(mod1.gndr,round = 10,p.round=10)),
         paste0(dir.temp,"mod1.gndr_FE.xlsx"),overwrite=T)
  
  ## Random effects
  export(getVC(mod1.gndr,round = 12),
         paste0(dir.temp,"mod1.gndr_RE.xlsx"),overwrite=T)
  
  # formula for random sex and interaction effect
  
  mod2.gndr.f<-
    as.formula(paste0("childlessness","~",
                      paste0(as.character(formula(temp.mod))[3],
                             "+gndr.c:",IV,
                             "+(0+gndr.c+gndr.c:",IV,"|cntry)")))
  
  ## fit the model
  mod2.gndr<-glmer(mod2.gndr.f,
                   data=exdat,
                   family=binomial(link="logit"),weights = anweight,
                   control = glmerControl(optimizer="bobyqa",
                                          optCtrl=list(maxfun=2e6)))
  
  
  ## collect important output
  
  ## Fixed effects
  export(rownames_to_column(getFE_glmer(mod2.gndr,round = 10,p.round=10)),
         paste0(dir.temp,"mod2.gndr_FE.xlsx"),overwrite=T)
  
  ## Random effects
  export(getVC(mod2.gndr,round = 12),
         paste0(dir.temp,"mod2.gndr_RE.xlsx"),overwrite=T)
  
  ## Model comparison to fixed gender model
  
  export(rownames_to_column(data.frame(anova(mod1.gndr,mod2.gndr))),
         paste0(dir.temp,"mod2.gndr_MC.xlsx"),overwrite=T)
  
  # formula for random sex and interaction effect without correlation
  
  mod3.gndr.f<-
    as.formula(paste0("childlessness","~",
                      paste0(as.character(formula(temp.mod))[3],
                             "+gndr.c:",IV,
                             "+(0+gndr.c+gndr.c:",IV,"||cntry)")))
  
  ## fit the model
  mod3.gndr<-glmer(mod3.gndr.f,
                   data=exdat,
                   family=binomial(link="logit"),weights = anweight,
                   control = glmerControl(optimizer="bobyqa",
                                          optCtrl=list(maxfun=2e6)))
  
  
  ## collect important output
  
  ## Fixed effects
  export(rownames_to_column(getFE_glmer(mod3.gndr,round = 10,p.round=10)),
         paste0(dir.temp,"mod3.gndr_FE.xlsx"),overwrite=T)
  
  ## Random effects
  export(getVC(mod3.gndr,round = 12),
         paste0(dir.temp,"mod3.gndr_RE.xlsx"),overwrite=T)
  
  ## Model comparison to fixed gender model
  
  export(rownames_to_column(data.frame(anova(mod1.gndr,mod3.gndr))),
         paste0(dir.temp,"mod3.gndr_MC.xlsx"),overwrite=T)
  
  ## Model comparison to random gender model with correlations
  
  export(rownames_to_column(data.frame(anova(mod3.gndr,mod2.gndr))),
         paste0(dir.temp,"mod3.gndr_MC2.xlsx"),overwrite=T)
  
  ## Marginal effects
  
  ### select which model is used
  
  if (!isSingular(mod2.gndr)) {
    temp.gndr.mod<-mod2.gndr
  } else if (!isSingular(mod3.gndr)){
    temp.gndr.mod<-mod3.gndr
  } else {
    temp.gndr.mod<-mod1.gndr
  }
  
  temp.gndr.marg<-
    emtrends(temp.gndr.mod,
             var=IV,
             specs="gndr.c",
             at=list(gndr.c=c(-0.5,0.5)),
             infer=c(T,T))
  
  
  export(data.frame(temp.gndr.marg),
         paste0(dir.temp,"gndr_marg.xlsx"),
         overwrite=T)
  
  ## Moderation by minority ethnic status
  
  # formula for fixed only
  
  mod1.minority.f<-
    as.formula(paste0("childlessness","~",
                      paste0(as.character(formula(temp.mod))[3],
                             "+minority.c:",IV)))
  
  ## fit the model
  mod1.minority<-glmer(mod1.minority.f,
                   data=exdat,
                   family=binomial(link="logit"),weights = anweight,
                   control = glmerControl(optimizer="bobyqa",
                                          optCtrl=list(maxfun=2e6)))
  
  ## collect important output
  
  ## Fixed effects
  export(rownames_to_column(getFE_glmer(mod1.minority,round = 10,p.round=10)),
         paste0(dir.temp,"mod1.minority_FE.xlsx"),overwrite=T)
  
  ## Random effects
  export(getVC(mod1.minority,round = 12),
         paste0(dir.temp,"mod1.minority_RE.xlsx"),overwrite=T)
  
  # formula for random minority and interaction effect
  
  mod2.minority.f<-
    as.formula(paste0("childlessness","~",
                      paste0(as.character(formula(temp.mod))[3],
                             "+minority.c:",IV,
                             "+(0+minority.c+minority.c:",IV,"|cntry)")))
  
  ## fit the model
  mod2.minority<-glmer(mod2.minority.f,
                   data=exdat,
                   family=binomial(link="logit"),weights = anweight,
                   control = glmerControl(optimizer="bobyqa",
                                          optCtrl=list(maxfun=2e6)))
  
  
  ## collect important output
  
  ## Fixed effects
  export(rownames_to_column(getFE_glmer(mod2.minority,round = 10,p.round=10)),
         paste0(dir.temp,"mod2.minority_FE.xlsx"),overwrite=T)
  
  ## Random effects
  export(getVC(mod2.minority,round = 12),
         paste0(dir.temp,"mod2.minority_RE.xlsx"),overwrite=T)
  
  ## Model comparison to fixed minority model
  
  export(rownames_to_column(data.frame(anova(mod1.minority,mod2.minority))),
         paste0(dir.temp,"mod2.minority_MC.xlsx"),overwrite=T)
  
  # formula for random minority and interaction effect without correlation
  
  mod3.minority.f<-
    as.formula(paste0("childlessness","~",
                      paste0(as.character(formula(temp.mod))[3],
                             "+minority.c:",IV,
                             "+(0+minority.c+minority.c:",IV,"||cntry)")))
  
  ## fit the model
  mod3.minority<-glmer(mod3.minority.f,
                   data=exdat,
                   family=binomial(link="logit"),weights = anweight,
                   control = glmerControl(optimizer="bobyqa",
                                          optCtrl=list(maxfun=2e6)))
  
  
  ## collect important output
  
  ## Fixed effects
  export(rownames_to_column(getFE_glmer(mod3.minority,round = 10,p.round=10)),
         paste0(dir.temp,"mod3.minority_FE.xlsx"),overwrite=T)
  
  ## Random effects
  export(getVC(mod3.minority,round = 12),
         paste0(dir.temp,"mod3.minority_RE.xlsx"),overwrite=T)
  
  ## Model comparison to fixed minority model
  
  export(rownames_to_column(data.frame(anova(mod1.minority,mod3.minority))),
         paste0(dir.temp,"mod3.minority_MC.xlsx"),overwrite=T)
  
  ## Model comparison to random minority model with correlations
  
  export(rownames_to_column(data.frame(anova(mod3.minority,mod2.minority))),
         paste0(dir.temp,"mod3.minority_MC2.xlsx"),overwrite=T)
  
  ## Marginal effects
  
  ### select which model is used
  
  if (!isSingular(mod2.minority)) {
    temp.minority.mod<-mod2.minority
  } else if (!isSingular(mod3.minority)){
    temp.minority.mod<-mod3.minority
  } else {
    temp.minority.mod<-mod1.minority
  }
  
  temp.minority.marg<-
    emtrends(temp.minority.mod,
             var=IV,
             specs="minority.c",
             at=list(minority.c=c(-0.5,0.5)),
             infer=c(T,T))
  
  export(data.frame(temp.minority.marg),
         paste0(dir.temp,"minority_marg.xlsx"),
         overwrite=T)
  
  
  ## Moderation by West_vs_post_comm 
  
  # Is only tested if there was random variation across countries in the main
  
  if (anova(mod2,mod3)$`Pr(>Chisq)`[2] < .05 |
      anova(mod2,mod4)$`Pr(>Chisq)`[2] < .05)
  {
    # formula for fixed only
    
    mod1.West_vs_post_comm.f<-
      as.formula(paste0("childlessness","~",
                        paste0(as.character(formula(temp.mod))[3],
                               "+West_vs_post_comm+West_vs_post_comm:",IV)))
    
    ## fit the model
    mod1.West_vs_post_comm<-
      glmer(mod1.West_vs_post_comm.f,
            data=exdat,
            family=binomial(link="logit"),
            weights = anweight,
            control = glmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=2e6)))
    
    ## collect important output
    
    ## Fixed effects
    export(rownames_to_column(getFE_glmer(mod1.West_vs_post_comm,round = 10,p.round=10)),
           paste0(dir.temp,"mod1.West_vs_post_comm_FE.xlsx"),overwrite=T)
    
    ## Random effects
    export(getVC(mod1.West_vs_post_comm,round = 12),
           paste0(dir.temp,"mod1.West_vs_post_comm_RE.xlsx"),overwrite=T)
    
    ## Marginal effects
    
    temp.West_vs_post_comm.marg<-
      emtrends(mod1.West_vs_post_comm,
               var=IV,
               specs="West_vs_post_comm",
               at=list(West_vs_post_comm=c(-0.5,0.5)),
               infer=c(T,T))
    
    export(data.frame(temp.West_vs_post_comm.marg),
           paste0(dir.temp,"West_vs_post_comm_marg.xlsx"),
           overwrite=T)
    
    
  }
  
  # compile singularity information
  
  singularity<-
    c(mod2=isSingular(mod2),
      mod3=isSingular(mod3),
      mod4=isSingular(mod4),
      mod1.gndr=isSingular(mod1.gndr),
      mod2.gndr=isSingular(mod2.gndr),
      mod3.gndr=isSingular(mod3.gndr),
      mod1.minority=isSingular(mod1.minority),
      mod2.minority=isSingular(mod2.minority),
      mod3.minority=isSingular(mod3.minority))
    
  export(rownames_to_column(data.frame(singularity)),
         paste0(dir.temp,"singularity.xlsx"),
         overwrite=T)  
    
}

