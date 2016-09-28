sizesum<-read.csv("../data/SizeData/FinalBearEst.csv")
library(mosaic)
library(ggplot2)
colnames(sizesum)<-c("SubsampleID", "densEstimate", "lcl", "ucl", "Subtype", "Model", "Size")
tally(Subtype~Size + Model, data=sizesum)


#less significant slope for simple random
summary(lm(densEstimate~Size, data=filter(sizesum, Model=="Model D", Subtype=="SimpleRandom")))
xyplot(densEstimate~Size, data=filter(sizesum, Model=="Model D", Subtype=="SimpleRandom"), type=c("p","r"))
mdD<-sd(densEstimate~Size, data=filter(sizesum, Model=="Model D", Subtype=="SimpleRandom"))
plot(mdD~names(mdD))

summary(lm(densEstimate~Size, data=filter(sizesum, Model=="Model E", Subtype=="SimpleRandom")))
xyplot(densEstimate~Size, data=filter(sizesum, Model=="Model E", Subtype=="SimpleRandom"), type=c("p","r"))
mdE<-sd(densEstimate~Size, data=filter(sizesum, Model=="Model E", Subtype=="SimpleRandom"))
plot(mdE~names(mdE))

#more significant slope for spread.one
summary(lm(densEstimate~Size, data=filter(sizesum, Model=="Model D", Subtype=="Spread.one")))
xyplot(densEstimate~Size, data=filter(sizesum, Model=="Model D", Subtype=="Spread.one"), type=c("p","r"))
mdD<-sd(densEstimate~Size, data=filter(sizesum, Model=="Model D", Subtype=="Spread.one"))
plot(mdD~names(mdD))

summary(lm(densEstimate~Size, data=filter(sizesum, Model=="Model E", Subtype=="Spread.one")))
xyplot(densEstimate~Size, data=filter(sizesum, Model=="Model E", Subtype=="Spread.one"), type=c("p","r"))
mdE<-sd(densEstimate~Size, data=filter(sizesum, Model=="Model E", Subtype=="Spread.one"))
plot(mdE~names(mdE))

#Output for slide - four linear models showing the effect of size of the sample on the density estimate
par(mfrow=c(2,2))

  d1<-ggplot(data=filter(sizesum, Model=="Model D", Subtype=="Spread.one"), aes(x=Size, y=densEstimate)) +
  geom_point() +
  geom_smooth(method=lm, se=TRUE, col="red", size=1) +
  ggtitle("Model D - Spread") +
  ylab("DensEstimate")+
  xlab("Size") +
  theme(plot.title = element_text(size=22))
  
  d2<-ggplot(data=filter(sizesum, Model=="Model D", Subtype=="SimpleRandom"), aes(x=Size, y=densEstimate)) +
  geom_point() +
  geom_smooth(method=lm, se=TRUE, col="red", size=1) +
  ggtitle("Model D - Simple Random") +
  ylab("DensEstimate")+
  xlab("Size") +
  theme(plot.title = element_text(size=22))
  
  e1<-ggplot(data=filter(sizesum, Model=="Model E", Subtype=="Spread.one"), aes(x=Size, y=densEstimate)) +
    geom_point() +
    geom_smooth(method=lm, se=TRUE, col="red", size=1) +
    ggtitle("Model E - Spread") +
    ylab("DensEstimate")+
    xlab("Size") +
    theme(plot.title = element_text(size=22))
  
  e2<-ggplot(data=filter(sizesum, Model=="Model E", Subtype=="SimpleRandom"), aes(x=Size, y=densEstimate)) +
    geom_point() +
    geom_smooth(method=lm, se=TRUE, col="red", size=1) +
    ggtitle("Model E - Simple Random") +
    ylab("DensEstimate")+
    xlab("Size") +
    theme(plot.title = element_text(size=22))
  
#' Aggregating and plotting all of the size x subtype versus density estimates.   

  library(cowplot)
  plot_grid(d1, d2, e1, e2, ncol = 2, nrow = 2)
  
  new<-NULL
  sizesum2<-NULL
  for(x in 1:8){
  new<-filter(sizesum, Size==(150 + 100*x))
  sizesum2<-rbind(sizesum2, new)
  }
  
a1<-  ggplot(filter(sizesum2, Model=="Model A", Subtype=="SimpleRandom"), aes(x=as.factor(Size), y=densEstimate))+
    geom_boxplot() +
    ggtitle("g0 ~ b + t + Sex") +
    geom_hline(yintercept=FullModels$estimate[1], col="red")+
    theme(plot.title = element_text(size=16)) +
    ylab("Density Estimate (Bear / mi^2)") +
    xlab("")+
    ylim(10,16) +
    theme_gray() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
a2<-  ggplot(filter(sizesum2, Model=="Model B", Subtype=="SimpleRandom"), aes(x=as.factor(Size), y=densEstimate))+
    geom_boxplot() +
    ggtitle("g0 ~ t + Sex") +
    geom_hline(yintercept=FullModels$estimate[2], col="red")+
    theme(plot.title = element_text(size=16)) +
    ylab("") +
    xlab("")+
    ylim(10,16) +
    theme_gray() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
a3<-  ggplot(filter(sizesum2, Model=="Model C", Subtype=="SimpleRandom"), aes(x=as.factor(Size), y=densEstimate))+
    geom_boxplot() +
    ggtitle("g0 ~ b + t") +
    geom_hline(yintercept=FullModels$estimate[3], col="red")+
    theme(plot.title = element_text(size=16)) +
    ylab("") +
    xlab("SimpleRandom")+
    ylim(10,16) +
    theme_gray() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
a4<-  ggplot(filter(sizesum2, Model=="Model D", Subtype=="SimpleRandom"), aes(x=as.factor(Size), y=densEstimate))+
    geom_boxplot() +
    ggtitle("g0 ~ t") +
    geom_hline(yintercept=FullModels$estimate[4], col="red")+
    theme(plot.title = element_text(size=16)) +
    ylab("") +
    xlab("")+
    ylim(10,16) +
    theme_gray() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

a5<-  ggplot(filter(sizesum2, Model=="Model E", Subtype=="SimpleRandom"), aes(x=as.factor(Size), y=densEstimate))+
    geom_boxplot() +
    ggtitle("g0 ~ b") +
    geom_hline(yintercept=FullModels$estimate[5], col="red")+
    theme(plot.title = element_text(size=16)) +
    ylab("") +
    xlab("")+
    ylim(10,16) +
    theme_gray() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

b1<-  ggplot(filter(sizesum2, Model=="Model A", Subtype=="Spread.one"), aes(x=as.factor(Size), y=densEstimate))+
  geom_boxplot() +
  ggtitle("g0 ~ b + t + Sex") +
  geom_hline(yintercept=FullModels$estimate[1], col="red")+
  theme(plot.title = element_text(size=16)) +
  ylab("Density Estimate (Bear / mi^2)") +
  xlab("")+
  ylim(10,16) +
  theme_gray() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

b2<-  ggplot(filter(sizesum2, Model=="Model B", Subtype=="Spread.one"), aes(x=as.factor(Size), y=densEstimate))+
  geom_boxplot() +
  ggtitle("g0 ~ t + Sex") +
  geom_hline(yintercept=FullModels$estimate[2], col="red")+
  theme(plot.title = element_text(size=16)) +
  ylab("") +
  xlab("")+
  ylim(10,16) +
  theme_gray() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

b3<-  ggplot(filter(sizesum2, Model=="Model C", Subtype=="Spread.one"), aes(x=as.factor(Size), y=densEstimate))+
  geom_boxplot() +
  ggtitle("g0 ~ b + t") +
  geom_hline(yintercept=FullModels$estimate[3], col="red")+
  theme(plot.title = element_text(size=16)) +
  ylab("") +
  xlab("Spread.one")+
  ylim(10,16) +
  theme_gray() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

b4<-  ggplot(filter(sizesum2, Model=="Model D", Subtype=="Spread.one"), aes(x=as.factor(Size), y=densEstimate))+
  geom_boxplot() +
  ggtitle("g0 ~ t") +
  geom_hline(yintercept=FullModels$estimate[4], col="red")+
  theme(plot.title = element_text(size=16)) +
  ylab("") +
  xlab("")+
  ylim(10,16) +
  theme_gray() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

b5<-  ggplot(filter(sizesum2, Model=="Model E", Subtype=="Spread.one"), aes(x=as.factor(Size), y=densEstimate))+
  geom_boxplot() +
  ggtitle("g0 ~ b") +
  geom_hline(yintercept=FullModels$estimate[5], col="red")+
  theme(plot.title = element_text(size=16)) +
  ylab("") +
  xlab("")+
  ylim(10,16) +
  theme_gray() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


plot_grid(a1,a2,a3,a4,a5,b1,b2,b3,b4,b5, nrow = 2, ncol=5)
