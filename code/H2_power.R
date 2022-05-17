# statistical power

# simulate 1000 logistic regressions
set.seed(25042022)
n=22000 #sample size
b0=-1.386299 #give roughly 0.20 base-rate for childlessness
b1=0.10 #GAL-TAN effect on childlessness
b2=0.00 #GAL-TAN salience effect on childlessness
b3=0.05 #GAL-TAN x health problem load interaction on CAM-use


p.list<-list()

for (i in 1:1000){
  
  x1 = rnorm(n) #GAL-TAN orientation
  x2 = rnorm(n) #GAL-TAN salience
  
  z = b0+b1*x1+b2*x2+b3*x1*x2  
  pr = 1/(1+exp(-z))         # pass through an inv-logit function
  y = rbinom(n,1,pr)
  
  p.list[[i]]<-coefficients(summary(glm(y~x1+x2+x1:x2,family=binomial(link="logit"))))[4,]
}

p.df<-do.call(rbind,p.list)
head(p.df)

#mean effect across repeats
mean(exp(p.df[,1]))

#power
prop.table(table(p.df[,"Pr(>|z|)"]<.05 & p.df[,"Estimate"]>0))
