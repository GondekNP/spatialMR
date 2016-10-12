#' This document will explore possible capture probabilities using different behavioral and IH parameters.

#' Sourcing logit stuff from Spatial.R with some modification for simplicity
behav <- -.7
int.g0<- 1
IH <- 0
sig <- sqrt(3/pi)/1000
captured<-FALSE
g0frame<-data.frame()
xdists<-seq(0,3000, 10)
#'control
logit.g0<- int.g0 + behav*captured + IH
g0<-plogis(logit.g0)
g0dists <- g0 * dhalfnorm(scale = sig, x = xdists) * 1000
new<-data.frame(xdists, g0dists, type="control")
g0frame<-rbind(g0frame, new)

#'captured
captured<-TRUE

logit.g0<- int.g0 + behav*captured + IH
g0<-plogis(logit.g0)
g0dists <- g0 * dhalfnorm(scale = sig, x = xdists) * 1000
new<-data.frame(xdists, g0dists, type="captured")
g0frame<-rbind(g0frame, new)


#'Extreme IH values
captured<-FALSE
IH<-2.25

logit.g0<- int.g0 + behav*captured + IH
g0<-plogis(logit.g0)
g0dists <- g0 * dhalfnorm(scale = sig, x = xdists) * 1000
new<-data.frame(xdists, g0dists, type="IH.hi")
g0frame<-rbind(g0frame, new)


captured<-FALSE
IH<- -2.25

logit.g0 <- int.g0 + behav*captured + IH
g0<-plogis(logit.g0)
g0dists <- g0 * dhalfnorm(scale = sig, x = xdists) * 1000
new<-data.frame(xdists, g0dists, type="IH.lo")
g0frame<-rbind(g0frame, new)

#'Extreme low IH plus behav effect
captured<-TRUE
IH<- -2.25

logit.g0 <- int.g0 + behav*captured + IH
g0<-plogis(logit.g0)
g0dists <- g0 * dhalfnorm(scale = sig, x = xdists) * 1000
new<-data.frame(xdists, g0dists, type="IH.lo and captured")
g0frame<-rbind(g0frame, new)

ggplot(data=g0frame,
       aes(x=xdists, y=g0dists, colour=type)) +
       geom_line()

#'Updated Sim.bear func
source('~/Google Drive/spatialMR/Rscripts/Simulation/Spatial.R')
t1<-sim.bear(known, sig, traplocs, int.g0=1, behav=0, IH=0, sessions = 4, redun=0, inhib=.2)
t2<-sim.bear(known, sig, traplocs, int.g0=1, behav=-.7, IH=0, sessions = 4, redun=0, inhib=.2)
t3<-sim.bear(known, sig, traplocs, int.g0=1, behav=0, IH=1.25, sessions = 4, redun=0, inhib=.2)
t4<-sim.bear(known, sig, traplocs, int.g0=1, behav=0, IH=0, sessions = 4, redun=1, inhib=.2)
t5<-sim.bear(known, sig, traplocs, int.g0=1, behav=0, IH=1.25, sessions = 4, redun=1, inhib=.2)
t6<-sim.bear(known, sig, traplocs, int.g0=1, behav=-.7, IH=1.25, sessions = 4, redun=1, inhib=.2)
t7<-sim.bear(known, sig, traplocs, int.g0=1, behav=0, IH=0, sessions = 4, redun=0, inhib=.2, stratDensity = .75)
