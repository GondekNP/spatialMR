#' ## Fit Huggins model using RMark
#' 
#' 
#+ echo=FALSE 
 # wd <- ifelse(basename(getwd())=="Rscripts", 
 #               gsub("/Rscripts", "", getwd()),
 #              getwd())
 #  opts_knit$set(root.dir=wd,comment=NA, fig.height=8, fig.width=12)
  #opts_knit$set(comment=NA, fig.height=8, fig.width=12)
  rm(list = ls()) # clear out memory
  
#' Load libraries
#+ warning=FALSE, message=FALSE 
   library(dplyr) # for data manipulation
   library(RMark) # for fitting models
   library(FSA) # for converting between data types
   options(width=160)
   
#' Read in data created using "Data Exploration.R" file  
  sampobs<-read.csv("../data/samps1.csv")
  
#' Gender: Sex = 204.25 -> Male, Sex= 250.25 ->Female
  sampobs$Sex2<-rep("1",nrow(sampobs))
  sampobs$Sex2[sampobs$Sex==250.25]<-0  

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
#
# Define parameter models
#
mod0=list(formula=~time+sex, share=TRUE)
mod1=list(formula=~Time+sex, share=TRUE)
mod2=list(formula=~c+sex, share=TRUE)
mod3=list(formula=~time+c+sex, share=TRUE)
mod4=list(formula=~Time+c+sex, share=TRUE)  

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
  
#' Results of models and model averaging 
  results=run.models(data=capMark3)  
  


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
    derived2<-rbind(derived, data.frame(model="Model Average", AIC.wt=NA,N=Nhat, se=sea))
    derived2
  }
  modelsum(results)
   
  
#' These models seem to be suggesting that capture probabilities
#' are lower for males than females (opposite of the original spatial mr models I fit!). This lead me to wonder 
#' if I should also be modeling go~sex in the spatial MR models (detection probability might be lower NEAR the center, 
#' but bears migth move farther distances).  This would seem to make some sense...see secrall2.html
  data.p<-data.frame(sex=c(0,1))

#' Lets estimate capture probabilities for time 1 
  est=covariate.predictions(results,data=data.p,indices=c(1))
  est