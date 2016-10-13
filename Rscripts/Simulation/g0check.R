
for (j in 1:1000){
samps<-sim.bear(known = known, sig = sig, int.g0 =.5 , traplocs=traplocs, behav=0, IH=0, sessions=10, redun=0, stratDensity=0)
patht0<-tempfile(fileext = ".csv")
write.table(samps, file=patht0, sep = ",")
t0caphist<-read.capthist(captfile = patht0, trapfile = trapPath, detector = 'proximity')
fitted<-secr.fit(t0caphist, model = list(g0~1), buffer = 1000, trace = FALSE, CL=TRUE, detectfn = 0, start = list(.4, 0, 650))
g0<-fitted$fit$par[1]
sigma<-fitted$fit$par[2]
newLine<-data.frame(g0, sigma, trial="t1")
g0check<-rbind(g0check, newLine)

samps<-sim.bear(known = known, sig = sig, int.g0 =.5 , traplocs=traplocs, behav=-1, IH=0, sessions=10, redun=0, stratDensity=0)
patht0<-tempfile(fileext = ".csv")
write.table(samps, file=patht0, sep = ",")
t0caphist<-read.capthist(captfile = patht0, trapfile = trapPath, detector = 'proximity')
fitted<-secr.fit(t0caphist, model = list(g0~1), buffer = 1000, trace = FALSE, CL=TRUE, detectfn = 0, start = list(.4, 0, 650))
g0<-fitted$fit$par[1]
sigma<-fitted$fit$par[2]
newLine<-data.frame(g0, sigma, trial="t2")
g0check<-rbind(g0check, newLine)

  samps<-sim.bear(known = known, sig = sig, int.g0 =.5 , traplocs=traplocs, behav=0, IH=0, sessions=10, redun=0, stratDensity=0)
  patht0<-tempfile(fileext = ".csv")
  write.table(samps, file=patht0, sep = ",")
  t0caphist<-read.capthist(captfile = patht0, trapfile = trapPath, detector = 'proximity')
  fitted<-secr.fit(t0caphist, model = list(g0~b), buffer = 1000, trace = FALSE, CL=TRUE, detectfn = 0, start = list(.4, 0, 650))
  g0<-fitted$fit$par[1]
  g0.bTRUE<-fitted$fit$par[2]
  sigma<-fitted$fit$par[3]
  newLine2<-data.frame(g0, g0.bTRUE, sigma, trial="t1")
  g0check2<-rbind(g0check2, newLine2)
  
  samps<-sim.bear(known = known, sig = sig, int.g0 =.5 , traplocs=traplocs, behav=-1, IH=0, sessions=10, redun=0, stratDensity=0)
  patht0<-tempfile(fileext = ".csv")
  write.table(samps, file=patht0, sep = ",")
  t0caphist<-read.capthist(captfile = patht0, trapfile = trapPath, detector = 'proximity')
  fitted<-secr.fit(t0caphist, model = list(g0~b), buffer = 1000, trace = FALSE, CL=TRUE, detectfn = 0, start = list(.4, 0, 650))
  g0<-fitted$fit$par[1]
  g0.bTRUE<-fitted$fit$par[2]
  sigma<-fitted$fit$par[3]
  newLine2<-data.frame(g0, g0.bTRUE, sigma, trial="t2")
  g0check2<-rbind(g0check2, newLine2)
  }
