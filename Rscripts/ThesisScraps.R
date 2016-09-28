#' ## Data Cleaning/Data Development
#' 
#' 
#+ echo=FALSE 
# wd <- ifelse(basename(getwd())=="Rscripts", 
#               gsub("/Rscripts", "", getwd()),
#              getwd())
#  opts_knit$set(root.dir=wd,comment=NA, fig.height=8, fig.width=12)
opts_knit$set(comment=NA, fig.height=8, fig.width=12)
rm(list = ls()) # clear out memory

#' Load libraries
#+ warning=FALSE, message=FALSE 
library(dplyr) # for data manipulation
options(width=160)

#' Purpose:  to read in raw data from Karen's excel spradsheet, "Karen's work Copy of g1288 results.xls", 
#' look at a few data summaries, and check for errors.
#' 
#' Read in raw data on samples and individuals tab (saved as .csv files)
ind<-read.csv("../data/Individuals2.csv")
head(ind)
samps<-read.csv("../data/samples.csv")
head(samps)

#' Data quality check:  compare number of samples on the "individuals" and "samples" tabs of Karen's Excel file.  This looks good (the two agree!).
no.samps<-table(samps$Individual)
dat.no<-data.frame(names(no.samps),no.samps)[,-2]
alldat<-merge(ind, dat.no, by.x="Individual", by.y="names.no.samps.", all=TRUE )
summary(alldat$Freq- alldat$no_Samples) # Looks good

#' Look at event IDs & Periods. Use strsplit to determine the site associated with each sample 
events<-unlist(strsplit(as.character(samps$Event.ID), "-"))
samps$site<-as.numeric(sapply(strsplit(as.character(samps$Event.ID), "-"), `[`, 1))
samps$cid<-as.numeric(sapply(strsplit(as.character(samps$Event.ID), "-"), `[`, 2))
table(samps$site)
# samps$Per.check<-substr(samps$cid,1,1) # thought might be period, but it is not
# table(samps$Per.Check, samps$Period)

#' Note, there are several observations that are missing the sample Period.  A few of these appear to be samples
#' of collared bears (these should be dropped, correct?).  Many of the missing observations have 
#' Class = Xsubselect (not sent to lab?).  Two correspond to sampled observations with a non-missing Priority, so these 
#' probably(?) represent data entry errors that should be fixed.
#'

#' I will write all observations that are missing a value for Period out ot a file for later checking.  The code, below, also highlights
#' three observations that should probably be given higher priority for checking.
filter(samps, Period=="", Lab.ID%in%c(26, 93, 1031))[,c(1:9, 16,18, 42, 43)]  
miss.period<-filter(samps, Period=="")[,c(1:9, 16,18, 42, 43)]
write.csv(miss.period, file="../data/MissingPeriod.csv", row.names=FALSE) 

#' Update:  7/10/2015:  using a file named "Subsampling Minnesota bear hair_boxes 1 and 2.xls" file in directory, 
#' C:\Users\jfieberg\Documents\MNDNR\Grand Rapids\Bear Group\DNA hair sampling\draft instructions plan, we were able to determine the period for
#' 3 of these observations.  The code, below, will impute these values.
samps$Period[samps$Event.ID=="67-01" & samps$Period==""]<-1
samps$Period[samps$Event.ID=="33-04" & samps$Period==""]<-5
samps$Period[samps$Event.ID=="46-09" & samps$Period==""]<-3

#' Fix these, then turn this variable into a numeric variable
samps$Period[samps$Period=="6?"]<-"6"

#' For now, drop observations that are missing Period.  Eventually, however, we will want to reconcile two of these 
#' observations that were highlighted earlier. The remaining 7 of these 9 observations correspond to observations from collared bears. 
samps<-filter(samps, Period!="")

#'From thesis- X% of available clusters were analyzed
sum(tally(~Class, data=samps)[c(2:5, 7)])/ nrow(samps) ##number analyzed
tally(~Class, data=samps)[2] / sum(tally(~Class, data=samps)[c(2:5, 7)]) ##number successful

source('~/Google Drive/spatialMR/Rscripts/BearSubsample.R')
samps<-mutate(samps, UID=as.numeric(site)*as.numeric(Period))
samps<-samps[!is.na(samps$UID),]
samps<-reorder(samps)
ss<-as.data.frame(tally(~UID,data=samps))
ss<-ss[order(ss$Freq, decreasing = TRUE),]

rand<-BearSubsample(samps, type = "SimpleRandom", n=300)
spread<-BearSubsample(samps, type = "Spread.one", n=300)
rand<-mutate(rand, UID=as.numeric(site)*as.numeric(Period))
spread<-mutate(spread, UID=as.numeric(site)*as.numeric(Period))
rand<-as.data.frame(tally(~UID,data=rand))
spread<-as.data.frame(tally(~UID,data=spread))

ggplot(data=ss, aes(x=1:283, y=Freq)) + 
    geom_point(data=rand, aes(x=Var1, y=Freq), col="red") +
    geom_bar(stat="identity") 




