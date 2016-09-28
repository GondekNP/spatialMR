#'This script will read in the existing Bear Est data and compare them, using a bwplot. 
#'(1/21/16)
library(mosaic)

BearEst<-read.csv("../data/SubsamplingData/FinalBearEst.csv")
colnames(BearEst)<-c("SubID", "densEstimate", "ucl", "lcl", "Subtype", "Model")

#'Number of Observations for each subtype X model. As of 1/21, there are ~16 for each. 
#'As of 1/22, almost 25 for each. 100 should be attainable by around the 30th of Jan. 
tally(Subtype~Model, data=BearEst)

#'Full model values from VerificationSECR.Rmd
fullModel<-c(14,15,14,13,14)
names(fullModel)<-c("Model A", "Model B", "Model C", "Model D", "Model E")

#'A box-whisker plot function to streamline the process for each model. 
BearBWplot<-function(model="Model A"){
  ggplot(filter(BearEst, Model==model), aes(x=Subtype, y=densEstimate))+
    geom_boxplot() +
    ylab("Density Estimate (Bears per Sq Mile)")+
    ggtitle(model) +
    geom_hline(yintercept=fullModel[model], col="red") 
    }
#'Treating all models the same, how does the subtype affect estimate
ggplot(BearEst, aes(x=Subtype, y=densEstimate))+
  geom_boxplot() +
  ylab("Density Estimate (Bears per Sq Mile)")+
  ggtitle("All Models")

#'Constructing a bwplot to see the difference between the different subtypes for each model
BearBWplot("Model A")
BearBWplot("Model B")
BearBWplot("Model C")
BearBWplot("Model D")
BearBWplot("Model E")

#'Look at the residuals between the full model and the means of the subtypes. 
BearResid<-function(model="Model A"){
  resids<-mosaic::mean(densEstimate~Subtype, data=filter(BearEst, Model==model))-fullModel[model]
  barplot(resids, ylim=c(-1,1))
}
BearResid("Model A")
BearResid("Model B")
BearResid("Model C")
BearResid("Model D")
BearResid("Model E")

#'Another useful look at the data: ANOVA to see if the estimates are significantly
#'different from one another
bearmod<-lm(densEstimate~Subtype, data=BearEst)
Anova(bearmod)

#'Now by model - Interesting, they are only significantly different in A, C, E (as of
#'1/22/16) and those are the three that have behavior as a parameter for g0!
Anova(lm(densEstimate~Subtype, data=filter(BearEst, Model=="Model A")))
Anova(lm(densEstimate~Subtype, data=filter(BearEst, Model=="Model B")))
Anova(lm(densEstimate~Subtype, data=filter(BearEst, Model=="Model C")))
Anova(lm(densEstimate~Subtype, data=filter(BearEst, Model=="Model D")))
Anova(lm(densEstimate~Subtype, data=filter(BearEst, Model=="Model E")))

#'Spinning into md document
#'   spin("EstimateSummary.R")

