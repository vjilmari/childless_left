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



# make a separate function to obtain nice data frame from r2mlm

## decomposition

DE.frame<-function(model){
  
  s<-r2mlm(model,bargraph = F)[[1]]
  srn<-rownames(s)
  scn<-colnames(s)
  
  attr(s, "dimnames") <- NULL
  
  d<-data.frame(matrix(as.vector(s),
                       nrow=dim(s)[1],
                       ncol=dim(s)[2],
                       byrow=F))
  colnames(d)<-scn
  rownames(d)<-srn
  return(d)
  
}

## effect size

R2.frame<-function(model){
  
  s<-r2mlm(model,bargraph = F)[[2]]
  srn<-rownames(s)
  scn<-colnames(s)
  
  attr(s, "dimnames") <- NULL
  
  d<-data.frame(matrix(as.vector(s),
                       nrow=dim(s)[1],
                       ncol=dim(s)[2],
                       byrow=F))
  colnames(d)<-scn
  rownames(d)<-srn
  return(d)
  
}


## effect size change

R2c.frame<-function(modelA,modelB){
  
  s<-r2mlm_comp(modelA,modelB,bargraph = F)[[3]]
  srn<-rownames(s)
  scn<-colnames(s)
  
  attr(s, "dimnames") <- NULL
  
  d<-data.frame(matrix(as.vector(s),
                       nrow=dim(s)[1],
                       ncol=dim(s)[2],
                       byrow=F))
  colnames(d)<-scn
  rownames(d)<-srn
  return(d)
  
}


DV_by_values_pipe<-function(DV,IV,IV.c,directory){
  
  #DV=DV.vars[1]
  #IV=value.vars
  #IV.c=value.vars.c
  #directory="Z:/postdoc/Values and Voting/results/main"
  
  dir.create(path = paste0(directory,"/",DV))
  
  dir.temp<-paste0(directory,"/",DV,"/")
  
  # select only the defined variables
  temp.fdat<- fdat %>% 
    dplyr::select(all_of(IV),
                  all_of(IV.c),
                  all_of(DV),
                  "gndr.c",
                  "age10.c",
                  "cntry",
                  "anweight") %>%
    na.omit()
  
  # center all IVs within country
  
  temp.fdat<-
    group_mean_center(data=temp.fdat,group.var = "cntry",
                      vars = c(IV,
                               IV.c,
                               "gndr.c",
                               "age10.c"))
  
  # fit DV-only model
  # define the formula
  mod0.f<-
    as.formula(paste0(DV,"~","(1|cntry)"))
  
  ## fit the model
  mod0<-lmer(mod0.f,
             weights = anweight,data=temp.fdat)
  
  ## collect important output
  
  ## Fixed effects
  export(rownames_to_column(data.frame(summary(mod0)$coefficients)),
         paste0(dir.temp,"mod0_FE.xlsx"),overwrite=T)
  
  ## Random effects
  export(getVC(mod0,round = 12),
         paste0(dir.temp,"mod0_RE.xlsx"),overwrite=T)
  
  ## Decompositions of variance
  
  export(rownames_to_column(DE.frame(mod0)),
         paste0(dir.temp,"mod0_DC.xlsx"),overwrite=T)
  
  ## Variance explained
  
  export(rownames_to_column(R2.frame(mod0)),
         paste0(dir.temp,"mod0_R2.xlsx"),overwrite=T)
  
  # fit covariates-only model
  # define the formula
  mod1.f<-
    as.formula(paste0(DV,"~","gndr.c.gmc+age10.c.gmc+","(1|cntry)"))
  
  ## fit the model
  mod1<-lmer(mod1.f,
             weights = anweight,data=temp.fdat)
  
  ## collect important output
  
  ## Fixed effects
  export(rownames_to_column(getFE(mod1,round=12,p.round=12)),
         paste0(dir.temp,"mod1_FE.xlsx"),overwrite=T)
  
  ## Random effects
  export(getVC(mod1,round = 12),
         paste0(dir.temp,"mod1_RE.xlsx"),overwrite=T)
  
  ## Decompositions of variance
  
  export(rownames_to_column(DE.frame(mod1)),
         paste0(dir.temp,"mod1_DC.xlsx"),overwrite=T)
  
  ## Variance explained
  
  export(rownames_to_column(R2.frame(mod1)),
         paste0(dir.temp,"mod1_R2.xlsx"),overwrite=T)
  
  # Loop through each IV (and) IV.c
  
  for (i in 1:length(IV)){
    
    IV.temp<-paste0(IV[i],".gmc")
    IV.c.temp<-paste0(IV.c[i],".gmc")
    
    # formula for FE sole predictor
    
    mod2.f<-
      as.formula(paste0(DV,"~",
                        "gndr.c.gmc+age10.c.gmc+",
                        IV.c.temp,"+",
                        "(1|cntry)"))
    
    ## fit the model
    mod2<-lmer(mod2.f,
               weights = anweight,data=temp.fdat)
    
    ## collect important output
    
    ## Fixed effects
    export(rownames_to_column(getFE(mod2,round=12,p.round=12)),
           paste0(dir.temp,paste0(IV[i],"_mod2_FE.xlsx")),overwrite=T)
    
    ## Random effects
    export(getVC(mod2,round = 12),
           paste0(dir.temp,paste0(IV[i],"_mod2_RE.xlsx")),overwrite=T)
    
    ## Decompositions of variance
    
    export(rownames_to_column(DE.frame(mod2)),
           paste0(dir.temp,paste0(IV[i],"_mod2_DC.xlsx")),overwrite=T)
    
    ## Variance explained
    
    export(rownames_to_column(R2.frame(mod2)),
           paste0(dir.temp,paste0(IV[i],"_mod2_R2.xlsx")),overwrite=T)
    
    ## Variance explained change
    
    export(rownames_to_column(R2c.frame(mod1,mod2)),
           paste0(dir.temp,paste0(IV[i],"_mod2_R2c.xlsx")),overwrite=T)
    
    # formula for FE sole predictor
    
    mod3.f<-
      as.formula(paste0(DV,"~",
                        "gndr.c.gmc+age10.c.gmc+",
                        IV.c.temp,"+(",IV.c.temp,"|cntry)"))
    
    ## fit the model
    mod3<-lmer(mod3.f,
               weights = anweight,data=temp.fdat)
    
    ## collect important output
    
    ## Fixed effects
    export(rownames_to_column(getFE(mod3,round=12,p.round=12)),
           paste0(dir.temp,paste0(IV[i],"_mod3_FE.xlsx")),overwrite=T)
    
    ## Random effects
    export(getVC(mod3,round = 12),
           paste0(dir.temp,paste0(IV[i],"_mod3_RE.xlsx")),overwrite=T)
    
    ## Decompositions of variance
    
    export(rownames_to_column(DE.frame(mod3)),
           paste0(dir.temp,paste0(IV[i],"_mod3_DC.xlsx")),overwrite=T)
    
    ## Variance explained
    
    export(rownames_to_column(R2.frame(mod3)),
           paste0(dir.temp,paste0(IV[i],"_mod3_R2.xlsx")),overwrite=T)
    
    ## Variance explained change
    
    export(rownames_to_column(R2c.frame(mod2,mod3)),
           paste0(dir.temp,paste0(IV[i],"_mod3_R2c.xlsx")),overwrite=T)
    
    ## Test for model improvement
    
    export(rownames_to_column(data.frame(anova(mod2,mod3))),
           paste0(dir.temp,paste0(IV[i],"_mod3_MC.xlsx")),overwrite=T)
    
    # refit without random effect correlation
    
    mod3.f.no.recov<-
      as.formula(paste0(DV,"~",
                        "gndr.c.gmc+age10.c.gmc+",
                        IV.c.temp,"+(",IV.c.temp,"||cntry)"))
    
    export(rownames_to_column(data.frame(anova(lmer(mod3.f.no.recov,
                                                    weights = anweight,
                                                    data=temp.fdat),
                                               mod3))),
           paste0(dir.temp,paste0(IV[i],"_mod3_RECOV.xlsx")),overwrite=T)
    
    
    # formula for FE as one of many predictors
    
    mod4.f<-
      as.formula(paste0(DV,"~",
                        "gndr.c.gmc+age10.c.gmc+",
                        paste0(paste0(IV,".gmc"),collapse=" + "),
                        "+",
                        "(1|cntry)"))
    
    ## fit the model
    mod4<-lmer(mod4.f,
               weights = anweight,data=temp.fdat)
    
    ## collect important output
    
    ## Fixed effects
    export(rownames_to_column(getFE(mod4,round=12,p.round=12)),
           paste0(dir.temp,paste0(IV[i],"_mod4_FE.xlsx")),overwrite=T)
    
    ## Random effects
    export(getVC(mod4,round = 12),
           paste0(dir.temp,paste0(IV[i],"_mod4_RE.xlsx")),overwrite=T)
    
    ## Decompositions of variance
    
    export(rownames_to_column(DE.frame(mod4)),
           paste0(dir.temp,paste0(IV[i],"_mod4_DC.xlsx")),overwrite=T)
    
    ## Variance explained
    
    export(rownames_to_column(R2.frame(mod4)),
           paste0(dir.temp,paste0(IV[i],"_mod4_R2.xlsx")),overwrite=T)
    
    ## Variance explained change (total)
    
    export(rownames_to_column(R2c.frame(mod1,mod4)),
           paste0(dir.temp,paste0(IV[i],"_mod4_R2c_total.xlsx")),overwrite=T)
    
    # compare to model without the focal predictor
    
    mod4.red.f<-
      as.formula(paste0(DV,"~",
                        "gndr.c.gmc+age10.c.gmc+",
                        paste0(paste0(IV[-i],".gmc"),collapse=" + "),
                        "+",
                        "(1|cntry)"))
    
    mod4.red<-lmer(mod4.red.f,
                   weights = anweight,data=temp.fdat)
    
    ## Variance explained unique
    
    export(rownames_to_column(R2c.frame(mod4.red,mod4)),
           paste0(dir.temp,
                  paste0(IV[i],"_mod4_R2c_unique.xlsx")),overwrite=T)
    
    # formula for RE as one of many predictors
    
    mod5.f<-
      as.formula(paste0(DV,"~",
                        "gndr.c.gmc+age10.c.gmc+",
                        paste0(paste0(IV,".gmc"),collapse=" + "),
                        "+",
                        "(",IV.temp,"|cntry)"))
    
    ## fit the model
    mod5<-lmer(mod5.f,
               weights = anweight,data=temp.fdat)
    
    ## collect important output
    
    ## Fixed effects
    export(rownames_to_column(getFE(mod5,round=12,p.round=12)),
           paste0(dir.temp,paste0(IV[i],"_mod5_FE.xlsx")),overwrite=T)
    
    ## Random effects
    export(getVC(mod5,round = 12),
           paste0(dir.temp,paste0(IV[i],"_mod5_RE.xlsx")),overwrite=T)
    
    ## Decompositions of variance
    
    export(rownames_to_column(DE.frame(mod5)),
           paste0(dir.temp,paste0(IV[i],"_mod5_DC.xlsx")),overwrite=T)
    
    ## Variance explained
    
    export(rownames_to_column(R2.frame(mod5)),
           paste0(dir.temp,paste0(IV[i],"_mod5_R2.xlsx")),overwrite=T)
    
    ## Variance explained change (total)
    
    export(rownames_to_column(R2c.frame(mod1,mod5)),
           paste0(dir.temp,paste0(IV[i],"_mod5_R2c_total.xlsx")),
           overwrite=T)
    
    ## Variance explained unique
    
    export(rownames_to_column(R2c.frame(mod4,mod5)),
           paste0(dir.temp,
                  paste0(IV[i],"_mod5_R2c_unique.xlsx")),overwrite=T)
    
    
  }
  
  
}

