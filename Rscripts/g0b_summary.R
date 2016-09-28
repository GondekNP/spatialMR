#' This function will create relevant graphs to primarily compare g0TRUE parameters from the various
#' secr models. This will probably be sourced in an rmd that can be knitted to get the most current
#' results, assuming the models continue to fit for a while longer (4/4/16 as of now).
#' 
#' Note that this only works for models with the behavior as a parameter (pulls off g0bTRUE)

sizesum<-read.csv("../data/SizeData/FinalBearEst.csv")
subsum<-read.csv("../data/SubsamplingData/FinalBearEst.csv")
colnames(sizesum)<-c("SubsampleID", "densEstimate", "lcl", "ucl", "Subtype", "Model", "Size")
colnames(subsum)<-c("SubsampleID", "densEstimate", "lcl", "ucl", "Subtype", "Model")
tally(Subtype~Size + Model, data=sizesum)
tally(Subtype~Model, data=subsum)

g0b_summary <- function(mod, treatment){
  sizesum<-read.csv("../data/SizeData/FinalBearEst.csv")
  subsum<-read.csv("../data/SubsamplingData/FinalBearEst.csv")
  
  subtypes<-c("SimpleRandom", "Spread.one", "Spread.two", "Stratified")
  
    for(p in 1:4){
      
      for(j in 1:200){ ##eyeball this upper bound... highest secr number goes here
        
          if(treatment=="Sub"){
            pathin<-paste("../data/SubsamplingData/", subtypes[p],"/", mod,"/secr", j, ".rds", sep="")
            pathout<-paste("../data/SubsamplingData/g0bTRUE_est.csv")
          }
          
          if(treatment=="Size"){
            pathin<-paste("../data/SizeData/", subtypes[p],"/", mod,"/secr", j, ".rds", sep="")
            pathout<-paste("../data/SizeData/g0est.csv")
          }
        
        try({ ##try() used because secr###.rds numbers have gaps, this is easiest/ugliest solution
        secrMod<-readRDS(pathin)
        bTru<-secrMod$beta.vcv["g0.bTRUE", "g0"] ##g0.bTRUE by g0 on the variance covariance matrix
        csvOut<-data.frame(g0bTRUE = bTru, Model = mod, Subtype = subtypes[p])
        write.table(csvOut, file = pathout, append = if(p==1 & j==1 & mod=="Model A"){FALSE}else{TRUE},
                    col.names = if(p==1 & j==1 & mod=="Model A"){c("g0.bTRUE", "Model", "Subtype")}else{FALSE}, sep=",")
          })
      }
    }
}

g0b_size_summary <- function(mod, treatment){
  sizes<-seq(950, 250, -100)
  subtype<-c("SimpleRandom", "Spread.one")
  first<-FALSE
    for(j in 4:30){ ##eyeball this upper bound... highest secr number goes here
      for (k in 1:2){      
        for (p in 1:8){
       if(treatment=="Sub"){
        pathin<-paste("../data/SubsamplingData/", subtype[k],"/", mod,"/secr", j,"_", sizes[p],".rds", sep="")
        pathout<-paste("../data/SubsamplingData/g0bTRUE_est.csv")
      }
      
      if(treatment=="Size"){
        pathin<-paste("../data/SizeData/", subtype[k],"/", mod,"/secr", j,"_", sizes[p], ".rds", sep="")
        pathout<-paste("../data/SizeData/g0bTRUE_est.csv")
      }
          if(p==1 & j==1 & mod=="Model A" & k==1){first<-TRUE}
          
      try({ ##try() used because secr###.rds numbers have gaps, this is easiest/ugliest solution
        secrMod<-readRDS(pathin)
        bTru<-secrMod$beta.vcv["g0.bTRUE", "g0"] ##g0.bTRUE by g0 on the variance covariance matrix
        g0t<-secrMod$beta.vcv["g0", "g0"]
        csvOut<-data.frame(g0 = g0t, g0bTRUE = bTru, Model = mod, Subtype = subtype[k], n = secrMod$nsamps)
        write.table(csvOut, file = pathout, append = if(first==TRUE){FALSE}else{TRUE},
                    col.names = if(first==TRUE){c("g0","g0.bTRUE", "Model", "Subtype", "Size")}else{FALSE}, sep=",")
          })
    }
  }
}
}
