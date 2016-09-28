#' ## Create sample history files
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
   library(mosaic) # for prop
   options(width=160)
   
#' Read in data created using "Data Exploration.R" file  
  sampobs<-read.csv("../data/samps1.csv")
  
#' Gender: Sex = 204.25 -> Male, Sex= 250.25 ->Female
  sampobs$Group<-rep("M",nrow(sampobs))
  sampobs$Group[sampobs$Sex==250.25]<-"F"  
  
#' Create a data set that contains a count of the number of times each bear was seen for each unique site x period combination.  Also determine
#' sex of each individual and tabulate the number of males and femlaes
  caphist<- sampobs%>%group_by(Individual, site, Period)%>%
      summarize(Count= n())
  sexid<-unique(select(sampobs, Individual, Group))
  table(sexid$Group)
  caphist2<-merge(caphist, sexid, all=FALSE)  
  
#' #### Distribution of counts of the **same** individual at each site x period sampling occassion.  
#' 
#' The largest count was 11 (same bear, same site, same period).   
  table(caphist$Count)
  tally(~Count, data=caphist, format="proportion")

#' 53% of the time, bears were detected > 1 time at a site x Period combination.   
  prop(~caphist$Count>1) 
  hist(caphist$Count, xlab="No. Observations per Individual x Site x Period combination", main="")  
  
#' #### Number of **individuals** detected during each (site x period) combination.  
#' 
#' Report from 2013 says: "Thus 1019 samples (92%) were successfully genotyped; these were from 96 different sites and 333 site-sessions.  I see 346 unique
#' combinations here [ **update 7/10/2015** after correcting the Period for 3 observations, I now have **347** unique combinations."
  capnums<-caphist%>%group_by(site, Period)%>% summarize(ncap=n() )
  capnums
  nrow(capnums)  
  
#'   As many as 5 bears were detected at a site during the same sampling period. Most site x sampling period combinations
#'   with at least 1 detection "trapped" only 1 or 2 bears.
  table(capnums$ncap)
  
#' #### Number of different unique (site x period) combinations that each bear was detected at.  
  bearcap<-caphist%>%group_by(Individual)%>% summarize(ncap=n())
  bearcap

#' Wow! One bear was captured at 62 unique site x period combinations!  Definitely some capture heterogeneity (and, I'd be surprised
#' if it was all due to movement and placement of "home range center" relative to the trapping grid).
  table(bearcap$ncap)
  hist(bearcap$ncap, xlab="No. unique sites capturing the same bear", main="")  
  sum((table(bearcap$ncap)/43)[1:5])
 
#' #### Number of bears captured during each period
#' 
#' Number of unique bears captured during eahc time period 
  timecap<-caphist%>%group_by(Period)%>% summarize(nbears=n_distinct(Individual) )
  timecap  

#' #### Number of unique bears captured at each site (across the different sampling periods). 
#' 
  sitecap<-caphist%>%group_by(site)%>% summarize(nbears=n_distinct(Individual) )
  sitecap  

#'  100 of the sites had at least 1 capture, with one site capturing 7 unique bears.  This doesn't exactly agree with the 96 stated in the 2013
#'  report, "Thus 1019 samples (92%) were successfully genotyped; these were from 96 different sites and 333 site-sessions."
  nrow(sitecap)
  table(sitecap$nbears)
  hist(sitecap$nbears, xlab="No. Observations at each site", main="")  

#' Write out file in the format expected by SECR, proximity detector
  bearCH<-data.frame(Session="BearMR", ID=caphist$Individual, Occassion=caphist2$Period, Detector=caphist2$site, Sex=caphist2$Group)
  write.table(bearCH, file="../data/BearCH.csv", row.names=FALSE, col.names=FALSE, sep=",")

#' Write out the original sampobs data, with a line for each observation at a site and period so that we can also use a multi detector.  
  bearCHP<-data.frame(Session="BearMR", ID=sampobs$Individual, Occassion=sampobs$Period, Detector=sampobs$site, Sex=sampobs$Group)
  write.table(bearCHP, file="../data/BearCHP.csv", row.names=FALSE, col.names=FALSE, sep=",")
  
#' ### Other checks
#' 
#'Look to see if summaries here match those in CAPTURE HETEROGENEITY IN HAIR-TRAPPING OF BEARS (Wildlife Research Summary from 2013) 
#' 
#' **Report**: "individual bears were detected up to 132 times each and up to 32 times in a single sampling session."
  max(table(sampobs$Individual))
  max(table(sampobs$Individual, sampobs$Period))
  
#'  **Report**, "About a third of both males (31%) and females (29%) were detected in only 1 sampling session.
#'   A similar percent of males (31%) and females (24%) were detected at only 1 site during the study."
  sitetemp<-caphist2%>%group_by(Individual, Group)%>% summarize(nsite=n_distinct(site))
  tally(nsite~Group, data=sitetemp, format="count")
  tally(nsite~Group, data=sitetemp, format="proportion")  
  histogram(~nsite|Group, data=sitetemp) # corraborates Figure 4
   
#' Nor for number of time periods  
  ptemp<-caphist2%>%group_by(Individual, Group)%>% summarize(nperiod=n_distinct(Period))
  tally(nperiod~Group, data=ptemp, format="count")
  tally(nperiod~Group, data=ptemp, format="proportion")  

#' **Report**:The number of hair traps that yielded bear hair ranged from 30-79 per session (Table 1). Numbers, below, differ a little from Table 1, probably
#' because I required the hair to be processed OK.
#' 
  apply(tally(site~Period, data=sampobs),2, FUN=function(x){sum(x>0)})
  
#' Lastly, Table 2: number of unqiue bears detected in each Period: checks out
  phist<-caphist2%>%group_by(Period, Individual, Group)%>%summarize(nbears=n_distinct(Individual))
  tally(Period~Group, data=phist, format="count")
 
#' Max number of sites for one bear
  maxsite<-tally(Individual~site, data=sampobs, format="count")
  maxs<-NULL
  for (i in 1:43){
    maxs[i]<-sum(maxsite[i,]!=0)
  }
  max(maxs)