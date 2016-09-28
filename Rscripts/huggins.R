rm(list=ls()) 
data_dir<-"C:/Users/mike/Dropbox/teaching/WILD8390/spring2015/exercises/wk6"

#data_dir<-"C:/documents and settings/conroy/my documents/Dropbox/teaching/WILD8390/spring2014/week6"
setwd(data_dir)
#load in a saved capture history data object
load("capt.hist")


#use this capture history database as input into RMark and run some models

require(RMark)
#
run.models=function(data)
{
#
# Define parameter models
#
pdotshared=list(formula=~1,share=TRUE)

ptimeshared=list(formula=~time,share=TRUE)
ptimeshared.c=list(formula=~time+c,share=TRUE)
p_cov<-list(formula=~x,share=TRUE)
p_cov.b<-list(formula=~x+c,share=TRUE)

ptime.c=list(formula=~time+c)
# Run assortment of models
#
#
# Capture Closed models
#
# constant p=c
m_cov<-mark(data,model="Huggins",model.parameters=list(p=p_cov))

m0<-mark(data,model="Huggins",model.parameters=list(p=pdotshared))
mb<-mark(data,model="Huggins")
m_cov<-mark(data,model="Huggins",model.parameters=list(p=p_cov))
mt<-mark(data,model="Huggins",model.parameters=list(p=ptimeshared))

m_cov.b<-mark(data,model="Huggins",model.parameters=list(p=p_cov.b))

return(collect.models())

}
#results of models and model averaging 
results=run.models(data=capt.hist)

#display index value for p coefficients
PIMS(results$mt,parameter="p")

data.f<-data.frame(x=seq(0,1,0.10))
#just p[1] because not time specific..
est=covariate.predictions(results,data=data.f,indices=c(1))

#plot prediction and ci
x<-seq(0,1,0.10)
plot(est$estimates$covdata,est$estimates$estimate,type="b",ylim=c(0,1),xlab="Covariate",ylab="Capture probability",pch=1)
matlines(est$estimates$covdata,est$estimates$lcl,lty=2)
matlines(est$estimates$covdata,est$estimates$ucl,lty=2)




#derived estimates of N
names=results$model.table$model
N<-c(results$m_cov$results$derived$estimate,results$m_cov.b$results$derived$estimate,results$m0$results$derived$estimate,results$mb$results$derived$estimate,results$mt$results$derived$estimate)
se<-c(results$m_cov$results$derived$se,results$m_cov.b$results$derived$se,results$m0$results$derived$se,results$mb$results$derived$se,results$mt$results$derived$se)
wt<-results$model.table$weight
derived<-data.frame(model=names,AIC.wt=wt,N=N,se=se)
derived


#model averaged estimates of  N

Nhat<-sum(N*wt)
se<-sqrt(se^2+(N-Nhat)^2)
se<-sum(se*wt)
print("Model averaged N and se")
Nhat
se


