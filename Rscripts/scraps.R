
  LLsurface.secr (secr0 , c("g0", "sigma"),
   xval = seq(0.02,0.10,0.005), yval = seq(160,360,20))
  
  
  secrb <- secr.fit(bearCH, model = g0~b, buffer = 100, trace = FALSE) # trap response model
  AIC (secr0, secrb)
  
  
  
  
  
  
#' Other stuff from scrbook  
  
  #' Aggregate data over periods & then fit Bayes model
  bearmat<-apply(y3d, 1:2, sum)
  
#' augment
  Xaug<-matrix(0, nrow=M, ncol=dim(traps)[1])
  Xaug[1:dim(bearmat)[1],]<-bearmat

#' set state space coordinates
  xl<-min(trapmat[,1])-100
  xu<-max(trapmat[,1])+100
  yl<-min(trapmat[,2])-100
  yu<-max(trapmat[,2])+100

  set.seed(2013)

#' run MCMC algorithm
  mod0<-SCR0binom.cl(y=Xaug,X=traps[,-1],M=M, xl=xl,xu=xu,yl=yl,yu=yu,K=dim(y3d)[3],
  delta=c(0.1, 0.05, 2), niter=1000)
  summary(mod0)
  
#' use coda to look at output
  library(coda)
  summary(window(mcmc(mod0), start=1001))  # did not work well....

#' What if boild down to 2 dimensional data set and apply in SECR & JAGS
   y2d <- apply(y3d,c(1,2),sum) # 2 dimensional data set
   sex<-tapply(enc[,5], enc[,2], unique) 

   caps<-array2secr(y, detector="proximity") #no sex specification, all individuals get session=1
   caps.sex<-array2secr(y3d, session=sex, detector="proximity") #each individual has sex assigned as a unique session

#' Convert to secr format
   out1<-SCR0bayes(data,M=200,engine="jags",ni=2000,nb=1000)  
  
  
   
#' ##  Analysis using the counts of each individual at each trap x Period
#' 
#'   
#' Now for multi detector using the count of observations made at each detector 
  bearCHP<-read.capthist("../data/BearCHP.csv",  "../data/UnScaled.csv",  detector= 'multi', covnames="Sex")  
  summary(bearCHP)
  
  
