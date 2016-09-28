setwd("/Users/nick/Google Drive/spatialMR/data/SubsamplingData/")
library(dplyr)

SimpleRandomA<-read.csv("SimpleRandom/Model A/SimpleRandomModel A.csv")
StratifiedA<-read.csv("Stratified/Model A/StratifiedModel A.csv")
Spread.oneA<-read.csv("Spread.one/Model A/Spread.oneModel A.csv")
Spread.twoA<-read.csv("Spread.two/Model A/Spread.twoModel A.csv")

colNames<-c("Estimate", "lcl", "ucl")
colnames(SimpleRandomA)<-colNames
colnames(StratifiedA)<-colNames
colnames(Spread.oneA)<-colNames
colnames(Spread.twoA)<-colNames

SimpleRandomA <- mutate(SimpleRandomA, subtype="SimpleRandom")
StratifiedA <- mutate(StratifiedA, subtype="Stratified")
Spread.oneA <- mutate(Spread.oneA, subtype="Spread.one")
Spread.twoA <- mutate(Spread.twoA, subtype="Spread.two")

ModelASum<-rbind(SimpleRandomA, StratifiedA, Spread.oneA, Spread.twoA)
