sim.bear <- function (known, sig, traplocs, int.g0=1, behav= -.7, IH=0, sessions=2, redun = 0, inhib=.2, stratDensity=0){
  library(sp)
  library(mosaic)
  library(LaplacesDemon)
  
  #'Defining 'observation window' in spatstat for rSSI to work properly
  library(spatstat)
  traprange<-owin(xrange=c(min(as.numeric(traplocs$X))-1, max(as.numeric(traplocs$X))+1),
                  yrange=c(min(as.numeric(traplocs$Y))-1, max(as.numeric(traplocs$Y))+1))
  traprange$units$singular<-"meter"
  traprange$units$plural<-"meters"
  
  #Simulating AC's for 15 bears, with inhibition range 'r' defined by homerange radius.
  #If strat density is nonzero, create disproportionate grid 
  if(stratDensity!=0){
    #Get some AC's for the whole grid first
    AC <- rSSI(r = inhib, n=round(length(known)*(1-stratDensity)), win = traprange, giveup = 10000)
    #Half the size of the grid
    traprange$xrange<-c(0, max(traprange$xrange)/2)
    #Generate the rest of the points on the half grid
    AC2 <- rSSI(r = inhib, n=round(length(known)*stratDensity), win = traprange, giveup = 10000)
    AC <- rbind(data.frame(AC), data.frame(AC2))
  }
  
  else {AC <- rSSI(r = inhib, n=length(known), win = traprange, giveup = 10000)}
  
  ACs<-data.frame(AC, ID=known, captured=rep(FALSE,length(known)), IHconstant = rnorm(n = length(known), mean=0, sd = IH))
  
  BearSamps<-data.frame()
  
  
  for (s in 1:sessions){ #Captures for each session
    
    for (b in known){ #Captures for each bear in each session
      bAC<-as.numeric(filter(ACs, ID==b)[,c("x","y")])
      #Euclidean distance between AC for this bear and each trap location, and subsequent half-normal capture prob
      trapMatrix<-data.matrix(traplocs[,c("X", "Y")])
      dists<-data.frame(dist=spDistsN1(pts=trapMatrix, pt = bAC), trapID=traplocs$Detector)
      dists$dist<-dists$dist*1000 #In km for some reason...
      
      for (h in 1:nrow(dists)){ #For each individual trap
        # intercept capture prob + behavior effect + effect from individual heterogeneity
        logit.g0<- int.g0 + behav*filter(ACs, ID==b)[,"captured"] + filter(ACs, ID==b)[,"IHconstant"]
        g0<-plogis(logit.g0)
        dists<-mutate(dists, g = g0 * dhalfnorm(scale = sig, x = dist) * 1000) #TODO: Ask john why this works 
        
        capProb <- dists$g[h] #Default capture probability
        IHc<-filter(ACs, ID==b)[,"IHconstant"]
        
        if ( rbinom(n=1, size=1, prob=capProb) == 1 ){ ##Coin flip - if captured (evals to 1), add a row to the samps
          newSamp<-data.frame(type="BearMR", ID = b, Period=s, site=dists$trapID[h]) #first (non-redundant) sample
          BearSamps<-rbind(BearSamps, newSamp)
          if (redun!=0){
            for (v in (1:(rpois(1, (redun + exp(IHc)))))) {BearSamps<-rbind(BearSamps, newSamp)} #if redun is 0, evals to 1, only one samp
          }
          ACs$captured[which(ACs$ID==b)] <- TRUE ##Bear is captured, next time the cap prob will change depending on 'behav'
          
        }
        
      }
      
    }
    
  }
  BearSamps$Period<-as.numeric(BearSamps$Period)
  return(BearSamps)
}

secr.from.samples<-function (full, samps, trapcsv, subtype, modEval, trial, number, size=200, fullN)  { 
  
  if(trial!="t1" && trial!="t2" && trial!="t3" && trial!="t4" && trial!="t5" && trial!="t6" && trial!="t7" && trial!="t8"){return("invalid trial name: must be t1 to t8 for proper storage of fitted model")}
  
  samps<-samps[!duplicated(samps$INDuniqID),]
  
  library(secr)
  fitted<-NULL
  
  strt<-Sys.time()
  patht0<-tempfile(fileext = ".csv")
  write.table(samps, file=patht0, sep = ",") 
  
  t0caphist<-read.capthist(captfile = patht0, trapfile = trapcsv, detector = 'proximity')
  try({fitted<-secr.fit(t0caphist, model = modEval, buffer = 1000, trace = FALSE, CL=TRUE, detectfn = 0)})
  if(!is.null(fitted)){outcome<-TRUE} else{outcome<-FALSE; print("Model fit failed.")}
  fitted$timeElapsed<-Sys.time() - strt
  fitted$subsamps<-samps
  fitted$outcome<-outcome
  fitted$fullsamps<-full
  fitted$fullN<-fullN
  
  #now save the object in some logical way as RDS
  modEval<-Reduce(paste, deparse(modEval))
  modelPathName<-gsub(pattern = "~", replacement = "tilde" , x = modEval)
  pathRDS<-paste("~/Google Drive/spatialMR/data/SimulationData/", trial, "/" , modelPathName , "/", subtype, number, ".rds", collapse="", sep="")
  saveRDS(fitted, file = pathRDS)
}

get.rds.starts <- function(){
  starts<-data.frame()
  for(j in c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8")){
    for (k in c("g0 tilde b", "g0 tilde b + t", "g0 tilde t")){
      
      path<-paste("~/Google Drive/spatialMR/data/SimulationData/", j, "/", k, sep="", collapse="")
      files<-list.files(path)
      
      if (length(which(files == "SimpleRandom10001.rds"))==0){
        newLine<-data.frame(trial=j,model=k, startno=10001)
      }
      else{
        maxFile<-max(files)
        maxFile<-strsplit(maxFile, "")[[1]]
        last<-maxFile[(length(maxFile)-8):(length(maxFile)-4)] ##This will only pick spread.one (or whatever the max char subsample is in the future, but that shouldn't matter because the trials are paired, and their max numbers are the same 
        last<-as.numeric(paste(last, sep="", collapse=""))
        newLine<-data.frame(trial=j,model=k, startno=last+1)
      }
      starts<-rbind(starts, newLine)
      
    }
  }
  colnames(starts)<-c("trial", "model", "startno")
  return(starts)
}

bear.init <- function(known, trial, model, sig, n=200, traplocs, trapPath, behav=0, IH=0, sessions=12, redun=0, int.g0=.16, stratDensity=0){
  #' Now, fit the models in parallel like in original project
  library(doParallel)
  library(foreach)
  
  #Single simulation for both fittings 
  fullsamps<-sim.bear(known = known, sig = sig, int.g0, traplocs=traplocs, behav, IH, sessions, redun, stratDensity)
  while (nrow(fullsamps)<200){ #Make sure at least 200
    fullsamps<-sim.bear(known = known, sig = sig, int.g0, traplocs=traplocs, behav, IH, sessions, redun, stratDensity)
  }
  #setup parallel backend to use all processors - note that this is not generally recommended for
  #computers that are actually in use, but since this was run primarily on a server and/or broken
  #laptop, I opted to use all cores, because I don't need any cores to run other tasks. 
  cl<-makeCluster(detectCores())
  registerDoParallel(cl)
  foreach (l=c("SimpleRandom", "Spread.one")) %dopar% {
    source('~/Google Drive/spatialMR/Rscripts/Simulation/RescueSpatail.R') ##source this script on each core so that all functions exist...
    source('~/Google Drive/spatialMR/Rscripts/BearSubsample.R') ##subsampling functions
    j<-trial
    k<-model
    library(spatstat)
    library(secr)
    library(LaplacesDemon)
    library(mosaic)
    library(sp)
    library(foreach)
    
    fullN<-nrow(fullsamps)
    starts<-get.rds.starts()
    notilde<-gsub(pattern = "~", replacement = "tilde" , x = k)
    startno<-filter(starts, trial==j, model==notilde)[,"startno"]
    hairsamps<-BearSubsample(data = fullsamps, type = l, n)
    secr.from.samples(full=fullsamps, samps = hairsamps, trapcsv = trapPath, modEval = as.formula(k),
                      trial = j, number = startno, subtype = l, size = 200, fullN=fullN)
    
  }
  stopCluster(cl)
}

bear.setup <- function(){
  for(p in 1:10000){
    print(paste("Completed ", p-1 , " iterations (", 6*(p-1), " models fitted). System time: ", Sys.time(), sep="", collapse = ""))
    rm(list=ls())#Clear memory and free RAM 
    
    ##create trap locations
    library(secr)
    setwd("~/Google Drive/spatialMR/Rscripts/Simulation")
    traplocs<-make.grid(nx=6, ny=6, spacing = 800)
    traplocs[,3]<-rownames(traplocs)
    colnames(traplocs)<-c("x","y","detectorID")
    traplocs2<-cbind(traplocs$detectorID, traplocs$x, traplocs$y)
    traplocs2<-as.data.frame(traplocs2)
    colnames(traplocs2)<-c("Detector", "X", "Y") #Mimicing efford documentation to HOPEFULLY get it to work...
    rownames(traplocs2)<-rownames(traplocs)
    traplocs<-traplocs2
    trapPath<-tempfile(fileext = ".csv")
    write.table(x = traplocs, file = trapPath, sep=",", col.names = FALSE, row.names = FALSE) #needed to drop rownames and colnames!  
    ##Genetically identified individuals (instead of 16-34-299 for eg, letters for simplicity)
    known<-c(letters, LETTERS, c("Aa", "Bb", "Cc", "Dd", "Ee", "Ff", "Gg","Hh"))[1:30] ##30 known bears
    sig<-sqrt(3/pi) / 1000
    
    bear.init(known, sig, model="g0 ~ b", trial = "t1", int.g0 = 1, behav = 0, IH=0, redun=0, sessions=4, traplocs = traplocs, trapPath = trapPath)
    bear.init(known, sig, model="g0 ~ b", trial = "t4", int.g0 = 1, behav = 0, IH=0, redun=1, sessions=4, traplocs = traplocs, trapPath = trapPath)
    bear.init(known, sig, model="g0 ~ b", trial = "t3", int.g0 = 1, behav = 0, IH=1.25, redun=0, sessions=4, traplocs = traplocs, trapPath = trapPath)
    bear.init(known, sig, model="g0 ~ b", trial = "t2", int.g0 = 1, behav = -.7, IH=0, redun=0, sessions=4, traplocs = traplocs, trapPath = trapPath)
    bear.init(known, sig, model="g0 ~ b", trial = "t5", int.g0 = 1, behav = 0, IH=1.25, redun=1, sessions=4, traplocs = traplocs, trapPath = trapPath)
    bear.init(known, sig, model="g0 ~ b", trial = "t6", int.g0 = 1, behav = -.7, IH=1.25, redun=1, sessions=4, traplocs = traplocs, trapPath = trapPath)
    bear.init(known, sig, model="g0 ~ b", trial = "t7", int.g0 = 1, behav = -.7, IH=1.25, redun=1, sessions=4, traplocs = traplocs, trapPath = trapPath, stratDensity = .75)
    bear.init(known, sig, model="g0 ~ b", trial = "t8", int.g0 = 1, behav = 0, IH=0, redun=0, sessions=4, traplocs = traplocs, trapPath = trapPath, stratDensity = .75)
  }
}
