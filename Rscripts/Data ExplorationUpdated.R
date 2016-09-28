#' ## Data Cleaning/Data Development
#' 
#' 
#+ echo=FALSE 
 # wd <- ifelse(basename(getwd())=="Rscripts", 
#               gsub("/Rscripts", "", getwd()),
 #              getwd())
#  opts_knit$set(root.dir=wd,comment=NA, fig.height=8, fig.width=12)
 # opts_knit$set(comment=NA, fig.height=8, fig.width=12)
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
#  samps<-read.csv("../data/samples.csv")
#  head(samps)
  
  samps<-read.csv("../data/samplesCorrected.csv")
  head(samps)
  
#' Note: on July 23, David sent a file with corrections made to a few observations that were missing period
#' or had period="6?".  This program uses the corrected "samples" tab from that spreadsheet.  This is the 
#' main difference from the DataExploration.R file.
#'    
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
  
  
#' Lets look at the variable labeled **Class**.  I'm not sure what all of these categories are, but I will take a guess, below:
#'
#' - sample (sent to lab & processed without any problems)?
#' - xbomb (not sure) are these samples that were difficult to extract DNA from?
#' - Xinadequate (sent to lab, but sample was not adequate)
#' - Xmixed (sent to lab, processed, contained a mixture of hair from different individuals)
#' - Xnotsent (not sent, we'll see why, below)
#' - Xspecies (sent to lab, processed, but not black bear hair)
#' - Xsubselect (low priority, ran out of money before could process)
#'
#' Number of each type
  table(samps$Class)
  
#' Lets look at a few of these categories.  
#' 
#' **Xbomb**:  **Individual** is missing for all of the Xbomb observations, and priority is as high as 1. So,
#' my guess is that an attempt was made to process these samples, but they were problematic in some way.
  filter(samps, Class=="Xbomb")[,c(1:9, 16,18)]  
   
#' **Xinadequate**: similar story...not sure how this category differs from **xbomb** though?
  filter(samps, Class=="Xinadequate")[,c(1:9, 16,18)]  

#' **Xmixed**:  similar story...
  filter(samps, Class=="Xmixed")[,c(1:9, 16,18)]  

#' **Xnotsent**:  Looks like these observations correspond to mislabeled / misbundled envelopes.  Only 4 of these. 
  filter(samps, Class=="Xnotsent")[,c(1:9, 16,18)]  
   
#' **Xspecies**:  hair likely from different species
  filter(samps, Class=="Xspecies")[,c(1:9, 16,18)]  
   
#' **Xsubselect**:  some of these are high priority (so, not sure why they fell to end of list? maybe they were alternates for other
#' samples collected at the same site & period?). Show only the first 10 observations.
  filter(samps, Class=="Xsubselect")[1:10,c(1:9, 16)]  

#' ### Subsetting of the data   
#'
#' At this point, I think it makes to subset the data, creating 4 separate data sets:
#' 
#'  1. Samples that were processed without any problems (Class = sample).  
#'  2. Samples that were sent, but turned out to be problematic (Class = Xbomb, Xinadequate, Xmixed)
#'  3. Samples that likely did not represent bear hair (Class = Xnotsent, Xspecies)
#'  4. Samples that were not processed (Xsubselect)
#'  
#'  We will focus mostly on [1] going forward, but the other data sets may still prove useful, particularly if /
#'  when we try to construct a reasonable simulation model.
  sampobs<-filter(samps, Class=="sample")
  samp.prob<-filter(samps, Class%in%c("Xbomb", "Xinadequate", "Xmixed"))
  samp.nobhair<-filter(samps, Class%in%c("Xnotsent", "Xspecies"))
  samp.noprocess<-filter(samps, Class=="Xsubselect")
   
#' ### Further exploration of Sampled Observations
#'    
#' At least 2 observations with Period = "?6".  Look at these:
  filter(sampobs, Period=="6?")
  
#' Fix these, then turn this variable into a numeric variable
  sampobs$Period[sampobs$Period=="6?"]<-"6"
  
#' For now, drop observations that are missing Period.  Eventually, however, we will want to reconcile two of these 
#' observations that were highlighted earlier. The remaining 7 of these 9 observations correspond to observations from collared bears. 
  sampobs2<-filter(sampobs, Period!="")

#' Now, turn Period into a numeric variable  
  sampobs2$Period<-droplevels(sampobs2$Period)  
  sampobs2$Period<-as.numeric(sampobs2$Period)
  table(sampobs2$Period)
  
#' Drop several of the columns that we will not need. 
  sampobs3<-sampobs2[,c(1:9, 16,18,42:44)] 

#' Write this file out for later use
   write.csv(sampobs3, file="../data/samps1.csv", row.names=FALSE)  
   