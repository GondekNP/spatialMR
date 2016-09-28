BearHugginsParallel <- function(treatment="Sub"){
  #' ## Fit Huggins model using RMark
  #' Load libraries
  #+ warning=FALSE, message=FALSE 
  library(dplyr) # for data manipulation
  library(RMark) # for fitting models
  library(FSA) # for converting between data types
  options(width=160)
  
  #' Set wd to HugginsData so that there arent thousands of mark files
  setwd("../data/HugginsData") ##not working on parallel comp?
  
  #' Read in data created using "Data Exploration.R" file  
  sampobs0<-read.csv("../samps1.csv")
  
  #' Choose the filepath corresponding to the treatment
  if(treatment == "Size"){ 
    pathin<- "../SizeData/SubsampleLabID.csv"
    pathout<- "../SizeData/HuggBearEst.csv"
  }
  
  if(treatment == "Sub"){ 
    pathin<- "../SubsamplingData/SubsampleLabID.csv"
    pathout<- "../SubsamplingData/HuggBearEst.csv"
  }
  
  #' Load subsampling csv
  subsamp<-read.csv(pathin)
  
for (j in 1:nrow(subsamp)){  
  #' Gender: Sex = 204.25 -> Male, Sex= 250.25 ->Female
  sampobs0$Sex2<-rep("1",nrow(sampobs0))
  sampobs0$Sex2[sampobs0$Sex==250.25]<-0  
  
  #' Filter obs without a period
  sampobs0<-filter(sampobs0, Period!="")
  sampobs<-NULL
  #' Use the existing subsample from the original data instead of using BearSubSample, so we can
  #' compare residuals with the exact same datasets
  
  sub<-subsamp[j,]
  for (i in sub){
    sampobs<-rbind(sampobs, sampobs0[which(sampobs0$Lab.ID == i),])
  }
  
  #' Create a data set that contains a count of the number of times each bear was seen for each period
  caphist<- sampobs%>%group_by(Individual, Period)%>%
    summarize(Count= n())
  
  #' Convert to Rmark file
  capMark<-capHistConvert(caphist[,c(1,2)], id="Individual", in.type="event", out.type="RMark")
  sexid<-unique(select(sampobs, Individual, Sex2))
  table(sexid$Sex2)
  capMark2<-merge(capMark, sexid, all=FALSE)  
  capMark3<-capMark2[,c(2,3)]
  names(capMark3)[2]<-"sex"
  
  #' Fit Huggins models (with sex covariate & behavioral response)
  
  run.models=function(data)
  {
    
    # Define parameter models
    mod0=list(formula=~c+time+sex, share=TRUE)
    mod1=list(formula=~time+sex, share=TRUE)
    mod2=list(formula=~c+time, share=TRUE)
    mod3=list(formula=~time, share=TRUE)
    mod4=list(formula=~c, share=TRUE)  
    
    # Run assortment of models
    #
    #
    # Capture Closed models
    #
    # time + sex
    m0<-mark(data,model="Huggins",model.parameters=list(p=mod0))
    
    # Time trend + sex
    m1<-mark(data,model="Huggins",model.parameters=list(p=mod1))
    
    # Behavioral response + sex
    m2<-mark(data,model="Huggins",model.parameters=list(p=mod2))
    
    # time + Behavioral response + sex
    m3<-mark(data,model="Huggins",model.parameters=list(p=mod3))
    
    # Time trend + Behavioral response + sex
    m4<-mark(data,model="Huggins",model.parameters=list(p=mod4))
    return(collect.models())
    
  }

  
  #' Pull off N, Se, AICwt and put in table
  modelsum<-function(models){
    mnames=models$model.table$model # Names of models
    names<-names(models) # names of model objects
    nl<-length(names)-1 # number of models fit
    sumnames<-paste(deparse(substitute(models)),names[-(nl+1)], "results", "derived", "'N Population Size'", sep="$")
    SEnames<-paste(sumnames, "se", sep="$")
    Nnames<-paste(sumnames, "estimate", sep="$")
    N<-unlist(sapply(1:nl, FUN=function(x){eval(parse(text=Nnames[x]))}))
    se<-unlist(sapply(1:nl, FUN=function(x){eval(parse(text=SEnames[x]))}))
    wt<-models$model.table$weight
    derived<-data.frame(model=mnames,AIC.wt=wt,N=N,se=se)
    Nhat<-sum(N*wt)
    sea<-sqrt(se^2+(N-Nhat)^2)
    sea<-sum(se*wt)
    derived
  }
  modout<-run.models(data=capMark3)
  pathNew<-paste(treatment,"/huggins", j, ".rds", sep="")
  saveRDS(modout, file = pathNew)
  
  est<-modelsum(modout)
  est<-mutate(est, modelAnalog = c("Model A", "Model B", "Model C", "Model D", "Model E"))
  est<-mutate(est, subNo = j)
  write.table(est, file=pathout, append = if(j>1){TRUE}else{FALSE}, col.names = if(j>1){FALSE}else{TRUE}, row.names = FALSE, sep = ",")
  print(paste("Line", j, "done!"))
  }
}