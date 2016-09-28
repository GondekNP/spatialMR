desnconv<-function(x){
  100*x/3861.022
}
BearModelFit<- function(trapgrid="../data/detectorfileScaled.csv", subtype=NULL, iteration=1)
      {
    ## This function fits five models, chosen by J. Fieberg's progress report using the 
    ## entire dataset, which identified the SCR models with highest AICc values. I chose
    ## these five based on their low AICc value, and their relatively parsimonous nature.
  
  ### save(test, file="../data/SubsamplingData/test.rda")
  ### write.table(test,"testcsv.csv", append = TRUE, row.names = FALSE, col.names = FALSE)
  output<-subtype
  require(secr)
  require(secrdesign)
  require(scrbook)
  
  ##Setting csv path to proxdata
  proxdata<-paste("../data/SubsamplingData/", subtype, "/TempCaphist.csv", sep="")
  
  bearCH<-read.capthist(captfile = proxdata, trapgrid,  detector= 'proximity', covnames="Sex")
  print(t0<-Sys.time())
  #### Time specific detection and behavioral response  
  secrA <- secr.fit(bearCH, model =list(g0~b+t+Sex, sigma~Sex), buffer = 10, trace = FALSE, CL=TRUE) # null model
  print(dA<-derived(secrA))
  print(tA<-Sys.time()-t0)
  #### Time trend in detection 
  ## Deviation from progress report, see below. same here, T was capital. 
  secrB <- secr.fit(bearCH, model =list(g0~t+Sex, sigma~Sex), buffer = 10, trace = FALSE, CL=TRUE) # null model
  print(dB<-derived(secrB))
  print(tB<-Sys.time()-tA)
  #### Time specific detection and behavioral response  
  secrC <- secr.fit(bearCH, model =list(g0~b+t, sigma~Sex), buffer = 10, trace = FALSE, CL=TRUE) # null model
  print(dC<-derived(secrC))
  print(tC<-Sys.time()-tB)
  #### Time trend in detection 
  ##T was capital in SecrAll1.R, is that wrong or is this (discrepancy with progress report)?
  secrD <- secr.fit(bearCH, model =list(g0~t, sigma~Sex), buffer = 10, trace = FALSE, CL=TRUE) # null model
  print(dD<-derived(secrD))
  print(tD<-Sys.time()-tC)
  #### Behavioral response  
  secrE <- secr.fit(bearCH, model =list(g0~b, sigma~Sex), buffer = 10, trace = FALSE, CL=TRUE) # null model
  print(dE<-derived(secrE))
  print(tE<-Sys.time()-tD)
  
  if(!is.null(output)){
    ##Creating file paths to appropriate data folder
    pathA<-paste("../data/SubsamplingData/", output,"/Model A/",output,"A.csv", sep="")
    pathB<-paste("../data/SubsamplingData/", output,"/Model B/",output,"B.csv", sep="")
    pathC<-paste("../data/SubsamplingData/", output,"/Model C/",output,"C.csv", sep="")
    pathD<-paste("../data/SubsamplingData/", output,"/Model D/",output,"D.csv", sep="")
    pathE<-paste("../data/SubsamplingData/", output,"/Model E/",output,"E.csv", sep="")
    
  
  ##Saving/Appending existing csv files with a new line
  densA<-desnconv(dA[2,c(1,3,4)]);write.table(densA, file =pathA, append = TRUE) 
  densB<-desnconv(dB[2,c(1,3,4)]);write.table(densB, file =pathB, append = TRUE) 
  densC<-desnconv(dC[2,c(1,3,4)]);write.table(densC, file =pathC, append = TRUE)  
  densD<-desnconv(dD[2,c(1,3,4)]);write.table(densD, file =pathD, append = TRUE) 
  densE<-desnconv(dE[2,c(1,3,4)]);write.table(densE, file =pathE, append = TRUE) 
  
  
  ##Modifying path for object saving
  pathA<-paste("../data/SubsamplingData/", output,"/Model A/secrA", iteration, ".rds", sep="")
  pathB<-paste("../data/SubsamplingData/", output,"/Model B/secrB", iteration, ".rds", sep="")
  pathC<-paste("../data/SubsamplingData/", output,"/Model C/secrC", iteration, ".rds", sep="")
  pathD<-paste("../data/SubsamplingData/", output,"/Model D/secrD", iteration, ".rds", sep="")
  pathE<-paste("../data/SubsamplingData/", output,"/Model E/secrE", iteration, ".rds", sep="")
  
  ##Saving model object... just in case
  saveRDS(secrA, file=pathA)
  saveRDS(secrB, file=pathB)
  saveRDS(secrC, file=pathC)
  saveRDS(secrD, file=pathD)
  saveRDS(secrE, file=pathE)
  }
  
  
}