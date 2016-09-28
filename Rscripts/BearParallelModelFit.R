desnconv<-function(x){
  100*x/3861.022
}
library(secr)
library(secrdesign)
library(scrbook)
BearParallelModelFit<-function(trapgrid="../data/detectorfileScaled.csv", subtype=NULL, iteration=1, size=450)

{
  output<-subtype
library(foreach)
#library(doMC)
library(doParallel)
#proxdata path and caphist
if(size==450){proxdata<-paste("../data/SubsamplingData/", subtype, "/TempCaphist.csv", sep="")}
if(size!=450){proxdata<-paste("../data/SizeData/", subtype, "/TempCaphist.csv", sep="")}
  
    bearCH<-read.capthist(captfile = proxdata, trapgrid,  detector= 'proximity', covnames="Sex")

#models to run within the foreach loop
models<-list(list(g0~b+t+Sex, sigma~Sex), list(g0~t+Sex, sigma~Sex), list(g0~b+t, sigma~Sex), list(g0~t, sigma~Sex), list(g0~b, sigma~Sex))
SecrFits<-vector("list", 5)
whichMod<-c("Model A", "Model B", "Model C", "Model D", "Model E")
names(SecrFits)<-whichMod

#setup parallel backend to use all processors - note that this is not generally recommended for
#computers that are actually in use, but since this was run primarily on a server and/or broken
#laptop, I opted to use all cores, because I didn't need any cores to run other tasks. 
cl<-makeCluster(detectCores())
registerDoParallel(cl)

#start time
strt<-Sys.time()

#foreach loop, parallel (%do% operator runs systematically, %dopar% is parallel)
foreach(d=1:5) %dopar% {
  #import packages, need to be loaded on each processor independently 
  library(foreach)
  library(doParallel)
  library(secr)
  library(secrdesign)
  library(scrbook)
  source('~/Google Drive/spatialMR/Rscripts/BearParallelModelFit.R')
  secrNew<-NULL
  modelEval<-models[[d]]
  secrNew <- secr.fit(bearCH, model=modelEval, buffer = 10, trace = FALSE, CL=TRUE)
  
  if (!(is.null(secrNew)))
  { print(dN<-derived(secrNew))
  ##pathNew<-paste("../data/SubsamplingData/", output,"/", whichMod[d],"/",output, whichMod[d],".csv", sep="")
  ##This was the old method of saving values from secr. I am going to keep the paths for the objects,
  ##but save all of the data in a single csv file with three additional categorical var,
  ##the model letter, the subtype, and the subsample ID. 
  
    if(size==450){
  pathNew<-("../data/SubsamplingData/FinalBearEst.csv")
  nrowSub<- nrow(read.csv("../data/SubsamplingData/SubsampleLabID.csv"))
    }
    
    if(size!=450){
  pathNew<-("../data/SizeData/FinalBearEst.csv")
  nrowSub<- nrow(read.csv("../data/SizeData/SubsampleLabID.csv"))
    }
    
    
  densNew<-desnconv(dN[2,c(1,3,4)])

  
  if(size==450){
    write.table(c(nrowSub, densNew, subtype, whichMod[d]), file =pathNew, append = TRUE, col.names = FALSE, row.names = FALSE, sep = ",")
  pathNew<-paste("../data/SubsamplingData/", output,"/", whichMod[d],"/secr", iteration, ".rds", sep="")
  }
  
  if(size!=450){
    write.table(c(nrowSub, densNew, subtype, whichMod[d], size), file =pathNew, append = TRUE, col.names = FALSE, row.names = FALSE, sep = ",")
    pathNew<-paste("../data/SizeData/", output,"/", whichMod[d],"/secr", iteration, "_", size, ".rds", sep="")
  }
  secrNew$nsamps<-size
  saveRDS(secrNew, file=pathNew)   }
}


#end time
print(Sys.time()-strt)
print(Sys.time())
stopCluster(cl)
}