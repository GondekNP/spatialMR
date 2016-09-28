setwd("/Users/nick/Google Drive/spatialMR/Rscripts/Simulation")
##Figure out what the last one was, start off there (so I don't have to do it manually)

##Sep 20 progress report

#kable(starts[,c(1:3,5)])
compile.secr.results <- function (){
library(secr)
compiled<-data.frame()
for(j in c("t1","t2","t3","t4","t5","t6")){
  for (k in c("g0 tilde b")){
      path<-paste("~/Google Drive/spatialMR/data/SimulationData/", j, "/", k, sep="", collapse="")
      files<-list.files(path)
      whichDesktop<-which(files=="desktop.ini")
      whichIcon<-which(files=="Icon\r")
      files<-files[-c(whichDesktop, whichIcon)]
      files<-paste(path, files, sep="/")
      
      for (i in length(files)){
        newSECR<-readRDS(files[i])
        
        for (i in 1:nrow(newSECR$fullsamps)){
          newSECR$fullsamps$uniqueID[i]<-paste(newSECR$fullsamps$Period[i], newSECR$fullsamps$site[i], sep="", collapse="")
          newSECR$fullsamps$INDuniqID[i]<-paste(newSECR$fullsamps$Period[i], newSECR$fullsamps$site[i], newSECR$fullsamps$ID[i],sep="", collapse="") 
        }
        #Redundancy info
        FullN.notRedun=length(!duplicated(newSECR$fullsamps$INDuniqID))
        SubN.notRedun=length(!duplicated(newSECR$subsamps$INDuniqID))
        FullProp.notRedun=FullN.notRedun/nrow(newSECR$fullsamps)
        SubProp.notRedun=SubN.notRedun/nrow(newSECR$subsamps)
        
        #Nhat info
        nhat = region.N(newSECR)[1,1]
        nhat.SE = region.N()[1,2]
        
        #Basic info on which trial and subtype that I should have passed thru the functions themselves to model
        fileSplit<-strsplit(files[i], split = "/")[[1]]
        
        sim<-fileSplit[8]
        model<-fileSplit[7]
        trial<-fileSplit[6]
        
        #Compiling
        newLine<-data.frame(FullProp.notRedun, FullN.notRedun, SubProp.notRedun, SubN.notRedun, nhat, nhat.SE, sim, model, trial)
        compiled<-rbind(compiled, newLine)
      }
      
    }
  }
}
