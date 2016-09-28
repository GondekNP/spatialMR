#'This script is going to reshape our sample observations into a dataframe that will allow us to look at redundant samples versus time.
#'(and hopefuly fit a glm model to that dataframe).

library(dplyr)
library(reshape2)
library(lme4)
library(doBy)
library(ggplot2)

#' read in sampobs from DataExploration.R
sampobs<-read.csv("../data/samps1.csv")

#' Create dataframe of interest
test2<-summaryBy(Sample~Individual+Period+site, FUN=length, data=sampobs)
firstcap<-summaryBy(Period~Individual, FUN=min,data=test2)
behav.obs<-test2 %>% left_join(firstcap)
behav.obs$firstcap<-I(behav.obs$Period==behav.obs$Period.min)
behav.obs$Period<-as.factor(behav.obs$Period)

#' Fit glmm model
mod<-glmer(Sample.length~Period+firstcap+(1|Individual), data=behav.obs, family=poisson())
summary(mod)

#' Not what I hoped...in terms of firstcap, but it does appear that the there were fewer captures
#' for the later sampling periods (which would have a similar effect - i.e., fewer recaptures
#' if the data were sampled randomly)... 
#' 
#' Lets look at when the first captures occurred by period & also overall # samples by period
#' - Most first captures occured during period 1, 2, or 3.
table(firstcap$Period.min) # distribution of first captures (which period?)

#' What about total number of captures by period
table(sampobs$Period)

#' Now, number of unique bears in each sampling period
ubears<-summaryBy(Individual~Period, FUN=function(x){length(unique(x))}, data=behav.obs)


#' So, the first sampling period has the most first time captures, but the fewest overall
#'  number of hair samples. 
plot(1:6, as.numeric(table(firstcap$Period.min)/sum(table(firstcap$Period.min))), lty=1, ylim=c(0,0.5), 
     xlab="Period", ylab="Proportion", type="l")

lines(1:6, table(sampobs$Period)/sum(table(sampobs$Period)), lty=2) 
#lines(1:6, ubears[,2]/sum(ubears[,2]), col="blue")
legend(2, 0.5, lty=c(1,2), bty="n", 
       legend=c("Distribution of First Captures", "Distribution of Samples"))


#' Nick's code
test2<-as.data.frame(tally(site~Period + Individual, data=melt(sampobs, id.vars = c("Individual","site","Period"))[1:3]))
test2$Freq<-test2$Freq/11 #They're all off by a factor of 11...?
colnames(test2)[4]<-"nsamps"
test2<-test2[test2$nsamps>0,]

test3<-melt(test2, id.vars = c("Individual", "Period"))[1:2]
test3<-test3[!duplicated(test3),]
test3<-mutate(test3, firstCap = !duplicated(Individual))

#' Here it is, John
behav.obs<-test2 %>% left_join(test3)
