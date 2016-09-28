source('~/Google Drive/spatialMR/Rscripts/BearParallelModelFit.R')
source('~/Google Drive/spatialMR/Rscripts/BearSubsample.R')
source('~/Google Drive/spatialMR/Rscripts/BearSumFunc.R')

#'Loading necessary libraries.

library(dplyr) 
library(secr) 
library(secrdesign)
library(scrbook)
library(ggplot2)
library(mosaic)

#' Read in raw data on samples and individuals tab (saved as .csv files)
ind<-read.csv("../data/Individuals2.csv")
head(ind)

samps<-read.csv("../data/samplesCorrected.csv")
head(samps)

#' Note: on July 23, David sent a file with corrections made to a few observations that were missing period
#' or had period="6?".  This program uses the corrected "samples" tab from that spreadsheet.  This is the 
#' main difference from the DataExploration.R file.
#'    

#' Look at event IDs & Periods. Use strsplit to determine the site associated with each sample 

events<-unlist(strsplit(as.character(samps$Event.ID), "-"))
samps$site<-as.numeric(sapply(strsplit(as.character(samps$Event.ID), "-"), `[`, 1))
suppressWarnings(samps$cid<-as.numeric(sapply(strsplit(as.character(samps$Event.ID), "-"), `[`, 2)))
table(samps$site)
# samps$Per.check<-substr(samps$cid,1,1) # thought might be period, but it is not
# table(samps$Per.Check, samps$Period)

#' Focus only on observations that were sampled successfully.
sampobs<-filter(samps, Class=="sample")

#' Fix the 6? entries, turn this variable into a numeric variable
sampobs$Period[sampobs$Period=="6?"]<-"6"

#' For now, drop observations that are missing Period.  Eventually, however, we will want to reconcile two of these 
#' observations that were highlighted earlier. The remaining 7 of these 9 observations correspond to observations from collared bears. 
sampobs<-filter(sampobs, Period!="")

#' Subsampling of the sampobs data.
sampobs2 <- BearSubsample(sampobs, "SimpleRandom", 450)
