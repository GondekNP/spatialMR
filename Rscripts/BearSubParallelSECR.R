
BearSubParallelSECR<- function(subtype, iterNo, size=450) {
  source('~/Google Drive/spatialMR/Rscripts/BearParallelModelFit.R')
  source('~/Google Drive/spatialMR/Rscripts/BearSubsample.R')
  source('~/Google Drive/spatialMR/Rscripts/BearSumFunc.R')
  
  #'Loading necessary libraries.
  
  library(dplyr) 
  library(secr) 
  library(secrdesign)
  library(scrbook)
  library(mosaic)
  
  #' Read in raw data on samples and individuals tab (saved as .csv files)
  ind<-read.csv("../data/Individuals2.csv")
  samps<-read.csv("../data/samplesCorrected.csv")

  #' Note: on July 23, David sent a file with corrections made to a few observations that were missing period
  #' or had period="6?".  This program uses the corrected "samples" tab from that spreadsheet.  This is the 
  #' main difference from the DataExploration.R file.
  #'    
  
  #' Look at event IDs & Periods. Use strsplit to determine the site associated with each sample 
  
  events<-unlist(strsplit(as.character(samps$Event.ID), "-"))
  samps$site<-as.numeric(sapply(strsplit(as.character(samps$Event.ID), "-"), `[`, 1))
  suppressWarnings(samps$cid<-as.numeric(sapply(strsplit(as.character(samps$Event.ID), "-"), `[`, 2)))
  table(samps$site)
  
  #' Focus only on observations that were sampled successfully.
  sampobs<-filter(samps, Class=="sample")
  
  #' Fix the 6? entries, turn this variable into a numeric variable
  sampobs$Period[sampobs$Period=="6?"]<-"6"
  
  #' For now, drop observations that are missing Period.  Eventually, however, we will want to reconcile two of these 
  #' observations that were highlighted earlier. The remaining 7 of these 9 observations correspond to observations from collared bears. 
  sampobs<-filter(sampobs, Period!="")
  
  #' Subsampling of the sampobs data.
  sampobs2 <- BearSubsample(sampobs, subtype, size)
 
  #' NEW 1/14/16 - saving the subsample labID for later use in huggins models, 
  #' that way the same subsample can be reused.
  if (size==450){
  write.table(matrix(sampobs2$Lab.ID, 1, size), file = "../data/SubsamplingData/SubsampleLabID.csv", sep=",", append = TRUE, col.names = FALSE, row.names = FALSE )
  }
  if (size!=450){
    write.table(matrix(sampobs2$Lab.ID, 1, size), file = "../data/SizeData/SubsampleLabID.csv", sep=",", append = TRUE, col.names = FALSE, row.names = FALSE )
  }
  #' Now, turn Period into a numeric variable  
  #sampobs2$Period<-droplevels(sampobs2$Period)  
  sampobs2$Period<-as.numeric(sampobs2$Period)
  table(sampobs2$Period)
  
  #' Drop several of the columns that we will not need. 
  sampobs3<-sampobs2[,c(1:9, 16,18,42:44)] 
  
  #' Setting csv pathways, Write this file out
  if (size==450){ ##Causing the temp files to write out to the wrong directory
  pathcsv1<-paste("../data/SubsamplingData/", subtype, "/TempSampobs.csv", sep = "")
  pathcsv2<-paste("../data/SubsamplingData/", subtype, "/TempCaphist.csv", sep = "")
  write.csv(sampobs3, file=pathcsv1, row.names=FALSE)
      }
  
  if (size!=450){
    pathcsv1<-paste("../data/SizeData/", subtype, "/TempSampobs.csv", sep = "")
    pathcsv2<-paste("../data/SizeData/", subtype, "/TempCaphist.csv", sep = "")
    write.csv(sampobs3, file=pathcsv1, row.names=FALSE)
      }
  
  #' Apply the capture history transformation necessary for secr models
  BearSumFunc(data=pathcsv1, summary = FALSE, output=pathcsv2)
  
  #' Run the model fitting and storage function.
  BearParallelModelFit(subtype = subtype, iteration=iterNo, size= size)
  print(paste(subtype,iterNo, "done!"))
}