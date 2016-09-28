sizesum<-read.csv("../data/SizeData/FinalBearEst.csv")
library(mosaic)
colnames(sizesum)<-c("SubsampleID", "densEstimate", "lcl", "ucl", "Subtype", "Model", "Size")
tally(Subtype~Size, data=sizesum)
