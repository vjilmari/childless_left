# Understanding not using product terms for interactions

# on understanding cov.reduce (for continuous covariates): https://stats.stackexchange.com/questions/410968/understanding-cov-reduce-argument-in-emmeans-function
# a vignette: https://www.r-bloggers.com/2020/04/estimating-and-testing-glms-with-emmeans/
# basic vignette on emmeans: https://cran.r-project.org/web/packages/emmeans/vignettes/basics.html
# modglm function: https://github.com/connorjmccabe/modglm

# test with ESS8
library(rio)
#library(dplyr)
library(emmeans)
library(magrittr)
#devtools::install_github("connorjmccabe/modglm")
library(modglm)
library(lme4)

dat<-import("z:/postdoc/ESS/ESS8e02_1.sav")

table(dat$gndr)
attributes(dat$gndr)

# predict being a female
dat$sex<-dat$gndr-1
table(dat$sex,useNA="always")

# with age and two values, all centered

dat$agea.c<-dat$agea-mean(dat$agea,na.rm=T)
names(dat)
attributes(dat$iphlppl)
attributes(dat$ipsuces)

table(dat$iphlppl)
dat$iphlppl.c<-7-dat$iphlppl
dat$iphlppl.c<-dat$iphlppl.c-mean(dat$iphlppl.c,na.rm=T)

dat$ipsuces.c<-7-dat$ipsuces
dat$ipsuces.c<-dat$ipsuces.c-mean(dat$ipsuces.c,na.rm=T)

# an empty model

mod0<-glm(sex~1,data=dat,family=binomial(link="logit"))
summary(mod0)

emmeans(mod0,specs= ~1,infer=c(T,T))
emmeans(mod0,specs= ~1,infer=c(T,T),type="latent")
emmeans(mod0,specs= ~1,infer=c(T,T),trans="response")
emmeans(mod0,specs= ~1,infer=c(T,T),type="response")


# make a model with all main effect

mod1<-glm(sex~agea.c+iphlppl.c+ipsuces.c,
          data=dat,family=binomial(link="logit"))
summary(mod1)

emmeans(mod1,specs= ~1,infer=c(T,T),type="latent")
emmeans(mod1,specs= ~1,infer=c(T,T),type="response")

# obtain point probabilities for iphlppl.c
iphlppl.c.points<-
  c(-1*sd(dat$iphlppl.c,na.rm=T),
   0,
   1*sd(dat$iphlppl.c,na.rm=T))

ipsuces.c.points<-
  c(-1*sd(dat$ipsuces.c,na.rm=T),
    0,
    1*sd(dat$ipsuces.c,na.rm=T))

agea.c.points<-
  c(-1*sd(dat$agea.c,na.rm=T),
    0,
    1*sd(dat$agea.c,na.rm=T))

emmeans(mod1,specs= "iphlppl.c",infer=c(T,T),type="latent",
        at=list(iphlppl.c=iphlppl.c.points))

emmeans(mod1,specs= "iphlppl.c",infer=c(T,T),type="response",
        at=list(iphlppl.c=iphlppl.c.points))

# add some values for the other predictors to see if the p-probs hold

ref_grid(mod1)
# this will keep all the values of the covariate in the grid
ref_grid(mod1,cov.keep = "ipsuces.c")
# this will add range of values, but for all values
ref_grid(mod1,cov.reduce = range)
# This adds all values (by this the average interaction could perhaps be tested)
ref_grid(mod1,cov.reduce = F)
# you can also condition the values
ref_grid(mod1,cov.reduce = iphlppl.c~agea.c+ipsuces.c)

emmeans(mod1,specs= ~ agea.c+iphlppl.c+ipsuces.c,
        infer=c(T,T),type="latent",
        at=list(iphlppl.c=iphlppl.c.points,
                agea.c = agea.c.points))

emmeans(mod1,specs= ~ agea.c+iphlppl.c+ipsuces.c,
        infer=c(T,T),type="response",
        at=list(iphlppl.c=iphlppl.c.points,
                agea.c = agea.c.points))

# the above is trivial, but what about the trends

emtrends(mod1,var="iphlppl.c",
         #specs= ~ agea.c+ipsuces.c, # this gives the same output
         specs=c("agea.c","ipsuces.c"),
        infer=c(T,T),type="latent",
        at=list(agea.c = agea.c.points))

emtrends(mod1,var="iphlppl.c",
         #specs= ~ agea.c+ipsuces.c, # this gives the same output
         specs=c("agea.c","ipsuces.c"),
         infer=c(T,T),regrid="response",
         at=list(agea.c = agea.c.points))

emtrends(mod1,var="iphlppl.c",
         #specs= ~ agea.c+ipsuces.c, # this gives the same output
         specs=c("agea.c","ipsuces.c"),
         infer=c(T,T),#type="latent",
         at=list(ipsuces.c = ipsuces.c.points))

emtrends(mod1,var="iphlppl.c",
         #specs= ~ agea.c+ipsuces.c, # this gives the same output
         specs=c("agea.c","ipsuces.c"),
         infer=c(T,T),regrid="response",
         at=list(ipsuces.c = ipsuces.c.points))
range(dat$ipsuces.c,na.rm=T)

# this gives trends (probability increase by 1 unit) for all covariate values

emtrends(mod1,var="iphlppl.c",
         #specs= ~ agea.c+ipsuces.c, # this gives the same output
         specs=c("agea.c","ipsuces.c"),
         infer=c(T,T),regrid="response",
         cov.keep = "ipsuces.c")

# this includes combinations of both covariates

emtrends(mod1,var="iphlppl.c",
         #specs= ~ agea.c+ipsuces.c, # this gives the same output
         specs=c("agea.c","ipsuces.c"),
         infer=c(T,T),regrid="response",
         cov.keep = c("agea.c","ipsuces.c"))

# this should give all the combinations

emtrends(mod1,var="iphlppl.c",
         #specs= ~ agea.c+ipsuces.c, # this gives the same output
         specs=c("agea.c","ipsuces.c"),
         infer=c(T,T),regrid="response",
         cov.reduce = F)

# Add an interaction term
dat$int<-dat$agea.c*dat$iphlppl.c

mod2<-glm(sex~agea.c+iphlppl.c+int+ipsuces.c,
          data=dat,family=binomial(link="logit"))
summary(mod2)

# how to test if this is significant at different values of 


emtrends(mod2,var="int",
         #specs= ~ agea.c+ipsuces.c, # this gives the same output
         specs=c("agea.c","iphlppl.c","ipsuces.c"),
         infer=c(T,T),regrid="response",
         cov.keep = "ipsuces.c")

emtrends(mod2,var="int",
         #specs= ~ agea.c+ipsuces.c, # this gives the same output
         specs=c("ipsuces.c","iphlppl.c","agea.c"),
         infer=c(T,T),regrid="response",
         cov.keep = c("ipsuces.c","iphlppl.c","agea.c"))

# save so that all combinations in the data are saved

int.for.all<-
  emtrends(mod2,var="int",
           #specs= ~ agea.c+ipsuces.c, # this gives the same output
           specs=c("ipsuces.c","iphlppl.c","agea.c"),
           infer=c(T,T),regrid="response",
           cov.reduce = F)

str(int.for.all)
summary(int.for.all)
table(data.frame(int.for.all)$p.value<.001)

# try the modglm functions
set.seed(23356)
dat2<-sample_n(dat,1000)
dat3<-dat[,c("sex","agea.c","iphlppl.c","ipsuces.c")]
mod3<-glm(sex~agea.c+iphlppl.c+agea.c:iphlppl.c+ipsuces.c,
          data=dat3,family=binomial(link="logit"))
summary(mod3)

# to estimate the interactions
# (this should be possible even without the product term)

# type is important:
# For continuous variable interactions, specify type="cpd" for computing the second-order cross-partial derivative. For continuous-by-discrete variable interactions, specify type="fd" for computing the finite difference in the partial derivative. For discrete variable interactions, specify type="dd" for computing the double finite difference.

memory.size() ### Checking your memory size
memory.size(max = FALSE)
memory.size(max = T)

memory.limit() ## Checking the set limit 
memory.limit(size=56000)

mod3.ints<-
  modglm(model=mod3,
         vars=c("agea.c","iphlppl.c"),
         data=dat3, type="cpd", hyps="means")

names(mod3.ints)
?modglm

#obints provides the interaction effect conditioned on each observation in the data. E.g:
head(mod3.ints$obints,n=100)

#`inthyp` provides the results of the hypothetical condition
mod3.ints$inthyp

#`aie` refers to the average interaction effect
# This is computed as the mean of all interaction
# effects in the observed data:
mod3.ints$aie

names(mod3.ints)
nrow(mod3.ints$jac)

mod3.ints$intsplot
mod3.ints$model.summary
mod3.ints$prop.sig

# try to make this happen for multilevel glmer

mod4<-glmer(sex~agea.c+iphlppl.c+agea.c:iphlppl.c+ipsuces.c+(1|cntry),
          data=dat,family=binomial(link="logit"))
summary(mod4)


mod4.ints<-
  modglm(model=mod4,
         vars=c("agea.c","iphlppl.c"),
         data=dat, type="cpd", hyps="means")

mod4.ints<-
  modglmer_logit(model=mod4,
         vars=c("agea.c","iphlppl.c"),
         data=dat, type="cpd", hyps="means")


#obints provides the interaction effect conditioned on each observation in the data. E.g:
head(mod4.ints$obints,n=100)

#`inthyp` provides the results of the hypothetical condition
mod4.ints$inthyp

#`aie` refers to the average interaction effect
# This is computed as the mean of all interaction
# effects in the observed data:
mod4.ints$aie

names(mod4.ints)
nrow(mod4.ints$jac)

mod4.ints$intsplot
mod4.ints$model.summary
mod4.ints$prop.sig

# set the slopes as random

mod5<-glmer(sex~agea.c+iphlppl.c+agea.c:iphlppl.c+ipsuces.c+(ipsuces.c|cntry),
            data=dat,family=binomial(link="logit"))
summary(mod5)

mod5.ints<-
  modglmer_logit(model=mod5,
                 vars=c("agea.c","iphlppl.c"),
                 data=dat, type="cpd", hyps="means",group.var="cntry")

#obints provides the interaction effect conditioned on each observation in the data. E.g:
head(mod5.ints$obints,n=100)

#`inthyp` provides the results of the hypothetical condition
mod5.ints$inthyp


#`aie` refers to the average interaction effect
# This is computed as the mean of all interaction
# effects in the observed data:
mod5.ints$aie

names(mod5.ints)
nrow(mod5.ints$jac)

mod5.ints$intsplot
mod5.ints$model.summary
mod5.ints$prop.sig


# Try to work this function for glmer-model

model=mod4
vars=c("agea.c","iphlppl.c")
data=dat2
type="cpd"
hyps="means"
group.var="cntry"


modglmer_logit<-function(model, vars, data, hyps="means", plotby=NULL,type="cpd",group.var)
{
  if(length(vars)>2){stop("You have selected more than two interaction predictors. Please select only two predictors.")}
  if(length(vars)<2){stop("You have not selected enough predictors. Please select two predictors.")}
  
  if(2 %in% lapply(data[,vars],function(x){length(unique(x))}) & type=="cpd"){warning("Note: one or more of your interaction variables may be dichotomous, but continuous variable interaction ('cpd') has been requested. Please check the variables and consider finite differences ('fd') or discrete finite differences ('dd') as the interaction 'type' if these variables are dichotomous.")}
  
  if(FALSE %in% (lapply(data[,vars],function(x){length(unique(x))})>2) & type=="dd"){warning("Note: one or more of your interaction variables may be continuous, but discrete variable interaction ('dd') has been specified. Please check the variables and consider finite differences ('fd') or cross partial derivatives ('cpd') as the interaction 'type' if one or more of these variables is continuous.")}
  
  if((length(unique(data[,vars[1]]))>2)!=(length(unique(data[,vars[2]]))>2) & type!="fd"){warning("Note: one of your variables appears continous and the other discrete. Finite differences ('fd') may be most appropriate for the interaction 'type' given these variables.")}
  ints<<-list()
  #This defines a string for the interaction term
  (int.varpossible <- c(paste(vars, collapse = ":"),paste(rev(vars), collapse = ":")))
  #if(any(int.varpossible %in% names(model$coefficients)))(int.var<-int.varpossible[(int.varpossible %in% names(model$coefficients))]) else(int.var<-"No product term specified")
  if(any(int.varpossible %in% names(coefficients(model)[[group.var]])))(int.var<-int.varpossible[(int.varpossible %in% names(coefficients(model)[[group.var]]))]) else(int.var<-"No product term specified")
  jacs<<-list()
  
  #(b <- model$coef)
  (b <- fixef(model))
  
  if(model@call[1]=="gee()"){
    dftemp<-na.omit(data[,which(names(data) %in% names(coefficients(model)[[group.var]]))])
    X<-as.data.frame(cbind(rep(1,nrow(dftemp)),dftemp))
  } else(X<-as.data.frame(cbind(rep(1,nrow(model@frame)),model@frame[,-1])))
  
  X<-as.data.frame(cbind(rep(1,nrow(model@frame)),model@frame[,-1]))
  
  # exclude group.var
  X<-X[,-which(names(X)==group.var)]
  
  # this calculates mean values for all columns
  hyps[1]
  if(hyps[1]=="means"){cfs <- matrix(colMeans(X),nrow=1)} else(cfs<-hyps)
  cfs
  
  #this repeats is as many times as needed
  Xcfs<-as.data.frame(matrix(rep(cfs,each=nrow(X)),nrow=nrow(X)))
  colnames(X)[1]<-"(Intercept)"
  colnames(Xcfs)<-colnames(X)
  Xcfs
  
  xmats<<-list()
  xmats$obs<-X
  xmats$means<-Xcfs
  
  ints<<-list()
  
  for(i in 1:length(xmats)){
    
    X<-xmats[[i]]
    
    if (int.var %in% names(coefficients(model)[[group.var]])){
      X[,int.var]<-X[,vars[1]]*X[,vars[2]]
      
    }
    
    X<-as.matrix(X)
    
    (xb <- (X %*% b)[,,drop=F])
    
    if(type=="cpd"){
      if (model@resp$family$link == "logit")
      {
        hat<-1/(1+ exp(-xb))
        #computes var(Yi)
        deriv1 <- exp(-xb)*(1+exp(-xb))^(-2)
        deriv2 <- ((exp(-xb)-1)*exp(-xb))/((exp(-xb)+1)^3)
        deriv3 <- (exp(-xb)*(exp(2*-xb)-4*exp(-xb)+1))/((exp(-xb)+1)^4)
      }
      
      else if (model@resp$family$link == "log")
      {
        hat<-exp(xb)
        deriv1 <- exp(xb)
        deriv2 <- exp(xb)
        deriv3 <- exp(xb)
      }
      else if (model@resp$family$link == "identity")
      {
        hat<-xb
        deriv1 <- 1
        deriv2 <- 0
        deriv3 <- 0
        
      }
    }
    else if(type=="fd"){
      if(model$call[1]=="gee()"){dum <- vars[which(sapply(apply(dftemp[, vars], 2, table), length) ==2)]}
      else(dum <- vars[which(sapply(apply(model$model[, vars], 2, table), length) ==2)])
      cont <- vars[which(vars != dum)]
      X1 <- X2 <- as.data.frame(X)
      X1[, dum] <- 1
      X2[, dum] <- 0
      
      if (int.var %in% names(coefficients(model)[[group.var]])==T){
        
        X1[, int.var] <- X1[, cont] * X1[, dum]
        X2[, int.var] <- X2[, cont] * X2[, dum]
        bint<-b[int.var]
      }
      else{
        bint<-0
      }
      X1<-as.matrix(X1)
      X2<-as.matrix(X2)
      
      x1b<-X1 %*% b
      x2b<-X2 %*% b
      
      if(model@resp$family$link == "logit"){
        hat <- 1/(1+ exp(-xb))
        
        hat1 <- 1/(1+ exp(-x1b))
        d1f1 <- exp(-x1b)*(1+exp(-x1b))^(-2)#phi1
        d2f1 <- ((exp(-x1b)-1)*exp(-x1b))/((exp(-x1b)+1)^3) #d2f1
        
        hat2 <- 1/(1+ exp(-x2b))
        d1f2 <- exp(-x2b)*(1+exp(-x2b))^(-2) #phi2
        d2f2 <- ((exp(-x2b)-1)*exp(-x2b))/((exp(-x2b)+1)^3) #d2f2
        
      }
      
      else if (model@resp$family$link == "log"){
        hat<-exp(X %*% b)
        
        hat1 <- exp(X1 %*% b)
        d1f1 <- exp(X1 %*% b)
        d2f1 <- exp(X1 %*% b)
        hat2 <- exp(X2 %*% b)
        d1f2 <- exp(X2 %*% b)
        d2f2 <- exp(X2 %*% b)
      }
      else if (model@resp$family$link == "identity"){
        hat<-X %*% b
        
        hat1 <- X1 %*% b
        d1f1 <- 1
        d2f1 <- 0
        hat2 <- X2 %*% b
        d1f2 <- 1
        d2f2 <- 0
      }
      
      ie1 <- (b[cont] + bint) * d1f1
      ie2 <- b[cont] * d1f2
      int.est <- ie1 - ie2
      
      deriv1 <- d1f1 - d1f2 + b[cont] * X[, cont] * (d2f1 - d2f2) +
        bint * X[, cont] * d2f1
      deriv2 <- (b[cont] + bint) * d2f1
      deriv3 <- d1f1 + (b[cont] + bint) * d2f1 * X[, cont]
      deriv0 <- (b[cont] + bint) * d2f1 - b[cont] * d2f2
    }
    
    else if(type=="dd"){
      
      X00 <- X01 <- X10 <- X11<-as.data.frame(X)
      
      X00[, vars[1]] <- 0
      X00[, vars[2]] <- 0
      X01[, vars[1]] <- 0
      X01[, vars[2]] <- 1
      X10[, vars[1]] <- 1
      X10[, vars[2]] <- 0
      X11[, vars[1]] <- 1
      X11[, vars[2]] <- 1
      
      if (int.var %in% names(coefficients(model)[[group.var]])==T){
        
        X00[, int.var] <- X00[, vars[1]] *X00[, vars[2]]
        X01[, int.var] <- X01[, vars[1]] *X01[, vars[2]]
        X10[, int.var] <- X10[, vars[1]] *X10[, vars[2]]
        X11[, int.var] <- X11[, vars[1]] *X11[, vars[2]]
        
        bint<-b[int.var]
      }
      else{
        bint<-0
      }
      X00<-as.matrix(X00)
      X01<-as.matrix(X01)
      X10<-as.matrix(X10)
      X11<-as.matrix(X11)
      
      x00b<-X00 %*% b
      x01b<-X01 %*% b
      x10b<-X10 %*% b
      x11b<-X11 %*% b
      
      if(model@resp$family$link == "logit"){
        hat <- 1/(1+ exp(-xb))
        
        hat00 <- 1/(1+ exp(-x00b))
        d1f00 <- exp(-x00b)*(1+exp(-x00b))^(-2)
        
        hat01 <- 1/(1+ exp(-x01b))
        d1f01 <- exp(-x01b)*(1+exp(-x01b))^(-2)
        
        hat10 <- 1/(1+ exp(-x10b))
        d1f10 <- exp(-x10b)*(1+exp(-x10b))^(-2)
        
        hat11 <- 1/(1+ exp(-x11b))
        d1f11 <- exp(-x11b)*(1+exp(-x11b))^(-2)
        
      }
      
      else if (model@resp$family$link == "log"){
        hat<-exp(X %*% b)
        
        hat00 <- exp(x00b)
        d1f00 <- exp(x00b)
        
        hat01 <- exp(x01b)
        d1f01 <- exp(x01b)
        
        hat10 <- exp(x10b)
        d1f10 <- exp(x10b)
        
        hat11 <- exp(x11b)
        d1f11 <- exp(x11b)
      }
      else if (model@resp$family$link == "identity"){
        hat<-X %*% b
        
        hat00 <- x00b
        d1f00 <- x00b
        
        hat01 <- x01b
        d1f01 <- x01b
        
        hat10 <- x10b
        d1f10 <- x10b
        
        hat11 <- x11b
        d1f11 <- x11b
      }
      
      int.est <- (hat11-hat10)-(hat01-hat00)
      
      deriv1 <- d1f11-d1f10
      deriv2 <- d1f11-d1f01
      deriv3 <- d1f11
      deriv0 <- (d1f11-d1f01)-(d1f10-d1f00)
    }
    
    if(type=="cpd"){
      
      
      if (int.var %in% names(coefficients(model)[[group.var]])==F){bint<-0}
      else(bint<-b[int.var])
      
      int.est <- bint * deriv1 +
        (b[vars[1]] + bint * X[, vars[2]]) *
        (b[vars[2]] + bint * X[, vars[1]]) *
        deriv2
      
      b1b4x2 <- b[vars[1]] + bint * X[, vars[2]]
      b2b4x1 <- b[vars[2]] + bint * X[, vars[1]]
      
      #Taking derivative of the interaction term with respect to X1
      deriv11 <- bint * deriv2 * X[, vars[1]] +
        b2b4x1 * deriv2 +
        b1b4x2 * b2b4x1 * deriv3 * X[, vars[1]]
      
      #Same with respect to X2
      deriv22 <- bint * deriv2 * X[, vars[2]] +
        b1b4x2 * deriv2 +
        b1b4x2 * b2b4x1 * X[, vars[2]] * deriv3
      
      #With respect to X1X2
      deriv44 <- deriv1 +
        bint * deriv2 * X[, vars[1]] * X[, vars[2]] +
        X[, vars[2]] * b2b4x1 * deriv2 + X[, vars[1]] * b1b4x2 *
        deriv2 + b1b4x2 * b2b4x1 * X[, vars[1]] * X[, vars[2]] *
        deriv3
      
      #with respect to the intercept?
      derivcc <- bint * deriv2 + b1b4x2 * b2b4x1 * deriv3
    }
    
    ##NOTE: This will add covariate values to the matxi
    if (int.var %in% names(coefficients(model)[[group.var]])==T){covars <- X[, -c(1, match(c(vars, int.var), names(b)))]} else {covars <- X[, -c(1, match(vars, names(b)))]}
    
    if (!("matrix" %in% class(covars))) {
      covars <- matrix(covars, nrow = nrow(X))
    }
    
    if ((int.var %in% names(coefficients(model)[[group.var]]))==T)
    {colnames(covars) <- colnames(X)[-c(1, match(c(vars, int.var),
                                                 names(b)))]} else{colnames(covars) <- colnames(X)[-c(1, match(vars,names(b)))]}
    
    if(type=="cpd"){jcovar <- apply(covars, 2, function(x) bint * deriv2 * x +b1b4x2 * b2b4x1 * x * deriv3)}
    else if(type=="fd"){jcovar <- apply(covars, 2, function(x) ((b[cont] + bint) *d2f1 - b[cont] * d2f2) * x)}
    else if(type=="dd"){jcovar <- apply(covars, 2, function(x) ((d1f11 - d1f01) - (d1f10 - d1f00)) * x)}
    
    jcovar <- array(jcovar, dim=dim(covars))
    dimnames(jcovar) <- dimnames(covars)
    
    if(type=="cpd")(jac <- cbind(deriv11, deriv22, deriv44, jcovar, derivcc)[,,drop=F])
    else if(type=="fd"| type=="dd"){jac<-cbind(deriv1, deriv2, deriv3, jcovar, deriv0)[,,drop=F]}
    colnames(jac) <- c(vars, int.var, colnames(jcovar), "(Intercept)")
    
    jac <- jac[, match(colnames(X), colnames(jac)), drop=F]
    
    
    #if(model$call[1]=="gee()"){se <- sqrt(diag(jac %*% gee_Rap_full$robust.variance %*% t(jac)))} else{se <- sqrt(diag(jac %*% as.matrix(vcov(model)) %*% t(jac)))}
    se <- sqrt(diag(jac %*% as.matrix(vcov(model)) %*% t(jac)))
    
    t.val <- int.est/se
    
    
    ints[[i]] <- data.frame(int.est = int.est,
                            hat = hat,
                            se.int.est = se,
                            t.val = t.val)
    jacs[[i]] <- jac
  }
  
  head(ints)
  ints$aie<-data.frame(aie.est=NA,aie.se.delta=NA)
  names(ints)<-c("obints","inthyp","aie")
  
  ints$jac<-jacs[[1]]
  ints$vcov<-as.matrix(vcov(model))
  
  ints$aie$aie.est<-mean(ints$obints$int.est)
  #if(model$call[1]=="gee()"){ints$aie$aie.se <- sqrt(as.vector(colMeans(jacs[[1]]))%*%gee_Rap_full$robust.variance %*%as.vector(t(colMeans(jacs[[1]]))))} else{ints$aie$aie.se.delta <- sqrt(as.vector(colMeans(jacs[[1]]))%*%as.matrix(vcov(model))%*%as.vector(t(colMeans(jacs[[1]]))))}
  ints$aie$aie.se.delta <- sqrt(as.vector(colMeans(jacs[[1]]))%*%as.matrix(vcov(model))%*%as.vector(t(colMeans(jacs[[1]]))))
  
  ints$aie$aie.ll<-ints$aie$aie.est-1.96*ints$aie$aie.se.delta
  ints$aie$aie.ul<-ints$aie$aie.est+1.96*ints$aie$aie.se.delta
  ints$prop.sig<-length(which(abs(ints$obints$t.val)>1.96))/length(ints$obints$t.val)
  
  ints$obints$sig[abs(ints$obints$t.val)>=1.96]<-"Sig."
  ints$obints$sig[abs(ints$obints$t.val)<1.96]<-"N.S."
  
  ints$inthyp<-as.vector(ints$inthyp[1,])
  
  ints$model.summary<-summary(model)
  
  requireNamespace("ggplot2")
  
  plotdf<-ints$obints
  
  if(is.null(plotby)){
    ints$intsplot<-ggplot2::ggplot(data=plotdf,ggplot2::aes(x=hat,y=int.est, color=sig)) +
      ggplot2::geom_point(size=.75) +
      ggplot2::labs(x="Predicted Value",y="Interaction Effect") +
      ggplot2::theme_bw()
  }
  else{
    
    plotdf[,plotby]<-as.factor(data[,plotby])
    
    ints$intsplot<-ggplot2::ggplot(data=plotdf,ggplot2::aes(x=hat,y=betas, color=sig, fill=sig,shape=plotdf[,plotby])) +
      ggplot2::geom_point(size=1.5) +
      ggplot2::labs(x="Predicted Value",y="Interaction Effect") +
      scale_shape_manual(values=c(1,3)) +
      theme_bw()
    
  }
  
  invisible(ints)
}
