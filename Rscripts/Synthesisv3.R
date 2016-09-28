##'This is version 3 of a synthesis file, meant to run over many iterations (eventually), 
##'across all four subsampling types. Here (Jan 2 2015), I use only random sampling and one
##'iteration, in order to make sure object and csv saving is working properly. 
source('~/Google Drive/spatialMR/Rscripts/BearSubSECR.R')
BearSubSECR(subtype="SimpleRandom", iter=3)
