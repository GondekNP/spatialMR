#' # Initial SECR models 
#'
 
#+ echo=FALSE 
 # wd <- ifelse(basename(getwd())=="Rscripts", 
#               gsub("/Rscripts", "", getwd()),
 #              getwd())
#  opts_knit$set(root.dir=wd,comment=NA, fig.height=8, fig.width=12)
  opts_knit$set(comment=NA, fig.height=8, fig.width=12)#, cache=TRUE)
  rm(list = ls()) # clear out memory
  
#' Load libraries
#+ warning=FALSE, message=FALSE 
   library(dplyr) # for data manipulation
   library(secr) 
   library(secrdesign)
   library(scrbook)
   library(ggplot2)
   options(width=160)

#' ## Initial data explorations

#' Lets look at the trapping grid.   
#'   
  trap.gr<-read.csv("../data/detectorfileScaled.csv", header=FALSE)
  names(trap.gr)<-c("Trap", "X", "Y")
  plot(trap.gr$X, trap.gr$Y, main="Trap Grid", xlab="X", ylab="Y") 
   
   
#' Read in data created using "Data Exploration.R" file, names(bearCH)<-c("Session", "ID", "Occasion", "Detector", "Count", "Sex")
  bearCH<-read.capthist("../data/BearCH.csv", "../data/detectorfileScaled.csv",  detector= 'proximity', covnames="Sex")
  
#' Try plotting method from scrbook package 
  enc<-read.csv("../data/BearCH.csv", header=F) # Encounter data
  enc[,1]<-as.numeric(1) # Treat individuals and periods as factor variables
  enc[,2]<-as.numeric(enc[,2])
  traps<-read.csv("../data/detectorfileScaled.csv", header=FALSE) # Trap data
  traps.3d<-cbind(traps, matrix(1, nrow(traps),6))  # add columns to say when traps were active
  y3d<-SCR23darray(enc[,1:4], traps.3d) # Create 3d array from capture history
  spiderplot(y3d, traps[,2:3]) # plot centroids along with recpatures for each individual
  

#' ### Summary of the data provided by SECR
#' 
#' - n = number of individuals detected on each occasion
#' - u = number of individuals detected for the first time on each occasion
#' - f =  number of individuals detected exactly f times
#' - M(t+1) =  cumulative number of individuals detected
#' - losses	= number of individuals reported as not released on each occasion
#' - detections	= number of detections, including within-occasion `recaptures'
#' - detectors visited = number of detectors at which at least one detection was recorded
#' - detectors used =  number of detectors, excluding any `not set' in usage attribute of traps attribute
  summary(bearCH)
  str(bearCH)
  
#' ## Models 
#'   
#'  First, explore the effect of scaling the trapping grid coordinates
#'  
#'  - Model detection as a function of period, movement as function of sex
#'  - Compare models fit using scaled and unscaled grid
#'     
#' Model detection as a function of capture Period and movement as a function of sex of the animal.
#'  Buffer if scaled = 10, if unscaled = 10*10000 = 10,000
  secr0 <- secr.fit(bearCH, model =list(g0~t+Sex, sigma~Sex), buffer = 10, trace = FALSE, CL=TRUE) # null model
  
#' For some reason, I am getting different answers depending on whether I "knit" the file or run from the console.  Try to calculate
#' the logL's for each set of parameters.  Secr0.A uses the parameters I obtained when "knitting" the file.  Secr0.B when running 
#' in the console.  Interesting, the logL's don't match those from secr0 above.  
  secr0.A<- secr.fit(bearCH, model =list(g0~t+Sex, sigma~Sex), buffer = 10, trace = FALSE, CL=TRUE,
                     details=list(LLonly=TRUE), start=c( -2.1684004,  0.9448443,  0.7901649,  1.5231371,  1.2350439,  0.4311504, -0.1298321,  0.6217320,  0.6001064))
  secr0.B<- secr.fit(bearCH, model =list(g0~t+Sex, sigma~Sex), buffer = 10, trace = FALSE, CL=TRUE,
                     details=list(LLonly=TRUE), start=c( 2.0106951,  1.3681691,  1.2842788,  1.7731416,  1.5592968 , 1.1002393, -0.6942304,  0.8666626,  0.6107980))
  secr0.A
  secr0.B
  secr0
  plot(secr0$mask)
  plot(bearCH, tracks=T, add=T)

#' Calculate density estimate (a derived parameter)
  ds<-derived(secr0)
  
#' Compare density estimates (just different scales)  
   ds  # scaled
  
#' ## Additional Models
#' 
#' In all of these models, I will assume sigma~sex (males move more than females)
#' 
#' - g0 ~ T (time trend in detection probabilities)
#' - g0 ~ b (behavioral response following initial detection)
#' - g0 ~ b + t (time specific detections & behavioral response)
#' - g0 ~ b + T (time trend and behavioral response)
#'
#'


#' #### Time trend in detection  
  secr1 <- secr.fit(bearCH, model =list(g0~T+Sex, sigma~Sex), buffer = 10, trace = FALSE, CL=TRUE) # null model
  (d1<-derived(secr1))
  
#' #### Behavioral response  
  secr2 <- secr.fit(bearCH, model =list(g0~b+Sex, sigma~Sex), buffer = 10, trace = FALSE, CL=TRUE) # null model
  (d2<-derived(secr2))
  
#' #### Time specific detection and behavioral response  
  secr3 <- secr.fit(bearCH, model =list(g0~b+t+Sex, sigma~Sex), buffer = 10, trace = FALSE, CL=TRUE) # null model
  (d3<-derived(secr3))
  
#' #### Time trend and behavioral response
  secr4 <- secr.fit(bearCH, model =list(g0~b+T+Sex, sigma~Sex), buffer = 10, trace = FALSE, CL=TRUE) # null model
  (d4<-derived(secr4))

#' Compare models using AIC  
  AIC(secr0, secr1, secr2, secr3, secr4)

#' ## Parameter estimates  
#' 
#' #### Time specific detection 
  secr0
  
#' #### Time trend in detection  
  secr1
  
#' #### Behavioral response    
  secr2
  
#' #### Time specific detection and behavioral response  
  secr3
  

#' #### Time trend and behavioral response
   secr4
  
 
#' ## Density estimates
#'   
#' Convert density esitmate to N/sq mile.  As reported on the help page for secr.fit:
#' 
#' - Density for the unscaled analysis is reported in terms of animals/ha (with distances measured in m). So...
#' - For the scaled analysis, density will be reported in terms of animals/(1000x1000)ha (with distances measured 
#' in terms of km).  
#' 
#' There are 3861.022 square miles in 1000x1000 ha.  To convert, we use:
#' 
#' D (per 100 $mi^2$) = (N/ha)*(1000ha/3.861022 mi^2)*100  
#' 
 desnconv<-function(x){
   100*x/3861.022
   }
  desnconv(ds[2,c(1,3,4)]) # point estimate anbd 95% CU **time varying detection model**
  desnconv(d1[2,c(1,3,4)]) # point estimate anbd 95% CU **trend in detection model**
  desnconv(d2[2,c(1,3,4)]) # point estimate anbd 95% CU **Behav response**
  desnconv(d3[2,c(1,3,4)]) # point estimate anbd 95% CU **Time specific detection and behavioral response**
  desnconv(d4[2,c(1,3,4)]) # point estimate anbd 95% CU **Time trend and behavioral response**
  
  
#' #### Calculate detection functions for the go~t+Sex, sigma~sex model
#' 
  x<-seq(0,5, length=100)
  lx<-length(x)
  bet<-secr0$fit$par
  g0s.F<-plogis(rep(bet[1],6)+c(0,bet[2:6]))
  g0s.M<-plogis(rep(bet[1],6)+c(0,bet[2:6])+rep(bet[7],6))
  sigs<-c(exp(bet[8]), exp(bet[8]+bet[9]))
  phat<-NULL
  for(i in 1:6){
    pfemale<-g0s.F[i]*exp(-x^2/(2*sigs[1]))
    pmale<-g0s.M[i]*exp(-x^2/(2*sigs[2]))
    phat<-rbind(phat, data.frame(x=rep(x,2),sex=rep(c("F","M"),each=lx), p=c(pfemale, pmale), period=as.factor(rep(i,each=2*lx))))  
  }
  
#' Plot fitted detection functions.  Looks like the "time effect" is much bigger than then sex effect. 
  ggplot(phat, aes(x, p, shape=sex, colour=period)) +geom_line()+facet_wrap(~sex)+xlab("Distance from HR center (km)")+ ylab("Detection Probability")
    
  
#' Some functions to look at
#' - reduce
#' - RMarkInput (to allow fitting of traditional M-R models in Program Mark, but from R)
#' - write.DA 
#' - write.SPACECAP (to allow Bayesian fitting)
#'  
#' Could also try:
#' 
#'  - Other detection functions (exponential, etc)
#'  - Resource selection map
#'  - SPACECAP/JAGS models   
#'  - Fit of models to data pooled across sessions (counts of individuals across occassions)
#'  - Fit of models using count data at each session  
  
#'
#'  For how to deal with "non-habitat" in JAGS, see: http://www.mikemeredith.net/blog/1309_SECR_in_JAGS_patchy_habitat.htm#fold  
  