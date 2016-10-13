setwd("/Users/nick/Google Drive/spatialMR/Rscripts/Simulation")
compile.secr.results <- function (){
library(secr)
compiled<-data.frame()
for(j in c("t1","t2","t3","t4","t5","t6", "t7", "t8")){
  for (k in c("g0tildeb", "g0tilde1")){
      path<-paste("~/Google Drive/spatialMR/data/SimulationData/", j, "/", k, sep="", collapse="")
      files<-list.files(path)
      whichDesktop<-which(files=="desktop.ini")
      whichIcon<-which(files=="Icon\r")
      if (length(whichDesktop)>0){files<-files[-whichDesktop]}
      if (length(whichIcon)>0){files<-files[-whichIcon]}
      files<-paste(path, files, sep="/")
      
      for (m in 1:length(files)){
        newSECR<-readRDS(files[m])
        
        for (i in 1:nrow(newSECR$fullsamps)){
          newSECR$fullsamps$uniqueID[i]<-paste(newSECR$fullsamps$Period[i], newSECR$fullsamps$site[i], sep="", collapse="")
          newSECR$fullsamps$INDuniqID[i]<-paste(newSECR$fullsamps$Period[i], newSECR$fullsamps$site[i], newSECR$fullsamps$ID[i],sep="", collapse="") 
        }
        #Redundancy info
        FullN.notRedun=length(!duplicated(newSECR$fullsamps$INDuniqID))
        SubN.notRedun=length(!duplicated(newSECR$subsamps$INDuniqID))
        SubProp.notRedun=SubN.notRedun/200
        
        #Nhat info
        nhatBoth<-region.N(newSECR)
        nhat = nhatBoth[1,1]
        nhat.SE = nhatBoth[1,2]
        
        #Basic info on which trial and subtype that I should have passed thru the functions themselves to model
        fileSplit<-strsplit(files[m], split = "/")[[1]]
        
        sim<-fileSplit[8]
        model<-fileSplit[7]
        trial<-fileSplit[6]
        
        
        if (length(newSECR$fit$par)==2){
          g0<-newSECR$fit$par[1]
          g0.bTRUE<-NA
          sigma<-newSECR$fit$par[2]
        } else {
          g0<-newSECR$fit$par[1]
          g0.bTRUE<-newSECR$fit$par[2]
          sigma<-newSECR$fit$par[3]
        }
        fullN<-newSECR$fullN
        #Compiling
        newLine<-data.frame(FullN.notRedun, SubProp.notRedun, SubN.notRedun, nhat, nhat.SE, g0, g0.bTRUE, sigma, sim, model, trial, fullN)
        compiled<-rbind(compiled, newLine)
        print(newLine)
      }
    }
  }
return(compiled)

}
#compiled<-compile.secr.results
#or read in old one
compiled<-compile.secr.results()

library(mosaic)



mean(fullN~trial, data=compiled)
bwplot(SubProp.notRedun~trial, data=compiled)

#need to split up sim into simnumber and subtype 
split<-strsplit(as.character(compiled$sim), split = "")
simNo<-NULL
subtype<-NULL
for (j in 1:nrow(compiled)){
  new<-split[[j]]
  subNew <- paste(new[1:10], sep="", collapse="")
  simNoNew <- as.numeric(paste(new[(length(new)-8):(length(new)-4)], sep="", collapse=""))
  subtype<-c(subtype, subNew)
  simNo<- c(simNo, simNoNew)
}
compiled$subtype<-subtype
compiled$simNo<-simNo

#Two weird simulations that saved as SimpleRandom10001 (1).rds ... removing those two rows
compiled<-compiled[-which(is.na(compiled$simNo)),]

#Tally our simulations
tally(trial~model, data=compiled)

bwplot(nhat~trial|subtype, data=compiled)
bwplot(SubProp.notRedun~trial|subtype, data=compiled)
bwplot(nhat.SE~trial|subtype, data=compiled)

#Now we need the discrepancy for each individual simulation
#Todo: fix for two diff models
discrep<-NULL
for (h in c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8")){
  j<-10001
  for (u in 1:(nrow(compiled))){ ##to account for number of sims (2 subtypes * 7 trials)
    nextSim<-filter(compiled, trial==h, simNo==j)
    if(nrow(nextSim)==0){break}
    newLine<-nextSim[1:2,2:5]
    newLine$nhatBias<-nextSim[,4]-30
    newLine<-cbind(newLine, nextSim[1:2,c(10:14)])
    discrep<-rbind(discrep, newLine)
    j<-j+1
  }
}

colnames(discrep)[9]<-"SubsamplingType"
ggplot(data=discrep)+
  scale_fill_grey() +
  geom_boxplot(aes(x=trial, y=nhatBias, fill=SubsamplingType)) +
  ylab("N-hat - Known Individuals") +
  xlab("Trial") +
  ggtitle("N-hat Biases vs Subsampling Type") +
  theme_light()

ggplot(data=discrep)+
  scale_fill_grey() +
  geom_boxplot(aes(x=trial, y=SubProp.notRedun, fill=SubsamplingType)) +
  ylab("Proportion of non-redundant Samples") +
  xlab("Trial") +
  ggtitle("Sample Redundancy vs Subsampling Type") +
  theme_light()

ggplot(data=compiled)+
  scale_fill_grey() +
  geom_boxplot(aes(x=trial, y=g0.bTRUE, fill=subtype)) +
  ylab("g0b beta") +
  xlab("Trial") +
  ggtitle("g0b beta vs Subsampling Type") +
  theme_light() 

ggplot(data=compiled)+
  scale_fill_grey() +
  geom_boxplot(aes(x=trial, y=g0, fill=subtype)) +
  ylab("g0 beta") +
  xlab("Trial") +
  ggtitle("g0 beta vs Subsampling Type") +
  theme_light() 

tally(~trial, data=compiled) #Number of trials completed
#'Average number of samples for each trial... didnt save pre-trimmed so doing it this way
lens<-data.frame()
for (j in 1:100) {
  t1<- data.frame(trial="t1", n=nrow(sim.bear(known = known, sig = sig, int.g0=1, traplocs=traplocs, behav=0, IH=0, sessions=4, redun=0, stratDensity=0)))
  
  t2<- data.frame(trial="t2", n=nrow(sim.bear(known = known, sig = sig, int.g0=1, traplocs=traplocs, behav = -0.7, IH=0, sessions=4, redun=0, stratDensity=0)))
  
  t3<- data.frame(trial="t3", n=nrow(sim.bear(known = known, sig = sig, int.g0=1, traplocs=traplocs, behav=0, IH=1.25, sessions=4, redun=0, stratDensity=0)))
  
  t4<- data.frame(trial="t4", n=nrow(sim.bear(known = known, sig = sig, int.g0=1, traplocs=traplocs, behav=0, IH=0, sessions=4, redun=1, stratDensity=0)))
  
  t5<- data.frame(trial="t5", n=nrow(sim.bear(known = known, sig = sig, int.g0=1, traplocs=traplocs, behav=0, IH=1.25, sessions=4, redun=1, stratDensity=0)))
  
  t6<- data.frame(trial="t6", n=nrow(sim.bear(known = known, sig = sig, int.g0=1, traplocs=traplocs, behav=-.7, IH=1.25, sessions=4, redun=1, stratDensity=0)))
  
  t7<- data.frame(trial="t7", n=nrow(sim.bear(known = known, sig = sig, int.g0=1, traplocs=traplocs, behav=-.7, IH=1.25, sessions=4, redun=1, stratDensity=.75)))
  lens<-rbind(lens, t1, t2, t3, t4, t5, t6, t7)
  print(Sys.time())
  print(j)
  }

