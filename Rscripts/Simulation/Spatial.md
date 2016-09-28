First step is to create a systematic grid of traps 8x8 in dataframe


```r
library(secr)
traplocs<-make.grid(nx=10, ny=10, spacing = .8)
```

Genetically identified individuals (instead of 16-34-299, a letter for simplicity)


```r
known<-c(letters, LETTERS, c("Aa", "Bb", "Cc", "Dd", "Ee", "Ff", "Gg","Hh", "Ii", "Jj"))
sig<-sqrt(10/pi) #avg homerange 10 sq km (Sollman, Gardner, Belant 2012)
```

Defining 'observation window' in spatstat


```r
library(spatstat)
traprange<-owin(xrange=c(min(traplocs$x)-1,max(traplocs$x)+1),
                yrange=c(min(traplocs$y)-1,max(traplocs$y)+1))
traprange$units$singular<-"kilometer"
traprange$units$plural<-"kilometers"
```

Example simple sequential inhibition


```r
plot(traprange, main="Example Simulated Activity Centers \n(X), and Trap Locations (o)")
points(traplocs)
points(rSSI(r = sig/2, n=length(known), win = traprange, giveup = 10000), pch="X")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

```r
sim.bear<- function (known, sig, trapsxy, int.g0=.08, behav= -.02, IH=0, sessions=2, redun = 0){
  library(sp)
  library(mosaic)
  library(LaplacesDemon)
  #Simulating AC's for 15 bears, with inhibition range 'r' defined by homerange radius 
  ACs<-data.frame(AC=rSSI(r = sig/2, n=length(known), win = traprange, giveup = 10000),
                          ID=known, captured=rep(FALSE,length(known)), IHconstant = abs(rnorm(n = length(known), mean = 0, sd = IH)))
  BearSamps<-data.frame()
  
  
  for (s in 1:sessions){ #Captures for each session
    
     for (b in known){ #Captures for each bear in each session
       bAC<-as.numeric(filter(ACs, ID==b)[,c("AC.x","AC.y")])
       #Euclidean distance between AC for this bear and each trap location, and subsequent half-normal capture prob
       dists<-data.frame(dist=spDistsN1(pts=as.matrix(trapsxy), pt = bAC), trapID=rownames(trapsxy))
       # intercept capture prob + behavior effect + effect from individual heterogeneity
       log.g0<- log(int.g0 + behav*filter(ACs, ID==b)[,"captured"]) + filter(ACs, ID==b)[,"IHconstant"]
       g0<-exp(log.g0)
       dists<-mutate(dists, g = g0 * dhalfnorm(scale = 1.5*sig, x = dist)) 
       
       for (h in 1:nrow(dists)){ #For each individual trap
         capProb <- dists$g[h] #Default capture probability
         if ( rbinom(n=1, size=1, prob=capProb) == 1 ){ ##Coin flip - if captured (evals to 1), add a row to the samps
           newSamp<-data.frame(type="BearMR", ID = b, Period=s, Trap=dists$trapID[h]) #first (non-redundant) sample
            for (v in (1:(rpois(1, redun) + 1))) {BearSamps<-rbind(BearSamps, newSamp)} #if redun is 0, evals to 1, only one samp
           ACs$captured[which(known==b)] <- TRUE ##Bear is captured, next time the cap prob will change depending on 'behav'
         }
         
       }
       
     }
  
  }
  return(BearSamps)
}
```

Testing it out - four scenarios. 

i) samples determined solely by half-normal

ii) detections determined by halfnormal with variable g0 (which is CONSTANT for the simulation for each individual)

iii) same as i or ii, but including a behavioral effect with captures being more (or less) likely following initial capture

iv) same as above (i - iii), but with redundant data introduced by way of poisson distribution. 

i) samples determined solely by half-normal


```r
t1<-sim.bear(known = known, sig = sig, int.g0 = .16, trapsxy = traplocs, behav=0, IH=0, sessions=6, redun=0)
tally(~ID,data=t1)
```

```
## ID
##  a  b  c  e  f  j  m  p  r  t  w  B  D  F  H  K  N  S  U  Z Cc Dd Ee Gg Jj 
##  9  7  4  6  4  4  4  5  4  3  6  4  1  3  4  5  3  3  6  3  2  3  3  4  5 
##  g  h  k  v  x  y  z  A  L  Y Aa Ff Ii  s  C  E  G  I  O  d  M  J  P  T 
##  1  7  1  2  3  1  5  3  3  4  2  7  3  2  2  2  3  2  2  3  3  1  1  1
```

```r
tally(~Period, data=t1)
```

```
## Period
##  1  2  3  4  5  6 
## 29 31 32 25 25 27
```

ii) detections determined by halfnormal with variable g0 (which is CONSTANT for the simulation for each individual)

IH = .1 creates a random normal value on a normal dist with an sd of .1. That constant becomes an additive contstant for the log function g0.


```r
t2<-sim.bear(known = known, sig = sig, int.g0 = .16, trapsxy = traplocs, behav=0, IH=.1, sessions=6, redun=0)
tally(~ID,data=t2)
```

```
## ID
##  c  d  i  k  l  p  v  z  B  G  H  J  N  Q  T  V  X  Y  Z Aa Ff Hh Ii Jj  a 
##  3  3  8  3  2  7  1  1  5  2  4  6  2  3  3  4  3  5  2  2  6  6  5  7  3 
##  e  f  g  q  s  C  F  I  O  S  j  o  A  P  W Bb Dd  b  t  L Ee  u  x  U  h 
##  1  4  1  1  3  4  3  7  3  3  4  3  2  2  3  2  1  1  2  1  1  1  1  1  1
```

```r
tally(~Period, data=t2)
```

```
## Period
##  1  2  3  4  5  6 
## 27 23 27 23 26 26
```

iii) same as i or ii, but including a behavioral effect with captures being more (or less) likely following initial capture

If the bear has been captured previously (ie left a single sample already), the behavioral constant becomes a multiplier for the half-normal distribution of capture probabilties.


```r
t3<-sim.bear(known = known, sig = sig, int.g0 = .16, trapsxy = traplocs, behav= -.04, IH=0, sessions=6, redun=0)
tally(~ID,data=t3)
```

```
## ID
##  a  b  d  e  i  j  k  n  s  u  B  E  F  Q  S  U  W Bb Dd Ee Ff  f  h  o  v 
##  4  2  2  2  5  3  2  8  6  4  2  3  4  5  3  1  5  2  1  4  2  5  3  2  2 
##  w  x  J  K  N  O  R Ii Jj  c  y  L  V  Y  D  H  P  X  Z Cc Gg  l  p  q  A 
##  4  1  1  4  7  2  3  1  2  2  4  3  1  1  1  2  2  1  1  1  2  1  1  1  1 
##  I  g 
##  1  1
```

```r
tally(~Period, data=t3)
```

```
## Period
##  1  2  3  4  5  6 
## 26 25 23 23 20 17
```

iv) same as above (i - iii), but with redundant data introduced by way of poisson distribution.

After a bear is detected at a trap, redundant samples are added by way of a random value taken from a poisson distribution with lambda equal to 'redun'


```r
t4<-sim.bear(known = known, sig = sig, int.g0 = .16, trapsxy = traplocs, behav=0, IH=0, sessions=6, redun=1.5)
tally(~ID,data=t4)
```

```
## ID
##  a  b  h  j  t  E  H  L  N  P  T  V  X Aa Dd Hh  n  q  y  G  J  K  M  Q  S 
##  4 15  5 11 11 15 11  8  6  6  5 18 18  9 17 11  1  3  4 12  2 14  7 10  7 
##  W  Z Jj  c  k  m  s  v  w  x  R  Y Ee Gg Ii  e  p  z  I Bb Cc  d  i  o  u 
## 15 17  5  9 17  2  1  9  7  1  4  4  4  5 11  7  1  7  4  5  2  4  6  4  7 
##  O 
##  2
```

```r
tally(~Period, data=t4)
```

```
## Period
##  1  2  3  4  5  6 
## 52 58 89 69 63 59
```

Figuring out how to fit to sim data - start by building capture history from samps and detector locations


```r
#first need the names of traps as a col and not as rownames
traplocs<-make.grid(nx=10, ny=10, spacing = .8)
traplocs[,3]<-rownames(traplocs)
colnames(traplocs)<-c("x","y","detectorID")
traplocs2<-cbind(traplocs$detectorID, traplocs$x, traplocs$y)
traplocs<-as.data.frame(traplocs2)
colnames(traplocs)<-c("Detector", "X", "Y") #Mimicing efford documentation to HOPEFULLY get it to work...

trapPath<-tempfile(fileext = ".csv")
write.table(x = traplocs, file = trapPath, sep=",", col.names = FALSE, row.names = FALSE) #needed to drop rownames and colnames!

head(read.csv(trapPath))
```

```
##   A.1  X0 X0.1
## 1 A 2 0.8    0
## 2 A 3 1.6    0
## 3 A 4 2.4    0
## 4 A 5 3.2    0
## 5 A 6 4.0    0
## 6 A 7 4.8    0
```

Now, fit the models in parallel like in original project


```r
library(doParallel)
library(foreach)


secr.from.samples <- function (samps, trapcsv)  { 
  
  #use all but one processor
  cl<-makeCluster(detectCores())
  registerDoParallel(cl)
  times<-data.frame()
  
  for (d in 1:3){
  #foreach(d=1:3) %dopar% {
      #in parallel, libraries need to be loaded independently
      library(secr)
      fitted<-list(NULL, NULL, NULL)
      models<-list(list(g0~b+t), list(g0~t), list(g0~b))
      modelEval<-models[[d]]
      strt<-Sys.time()
      patht0<-tempfile(fileext = ".csv")
      write.table(samps, file=patht0, sep = ",") 
      
      t0caphist<-read.capthist(captfile = patht0, trapfile = trapcsv, detector = 'proximity')
      try({fitted[[d]]<-secr.fit(t0caphist, model = modelEval, buffer = 10, trace = FALSE, CL=TRUE, detectfn = 0)})
      
      outcome<-TRUE
      if(is.null(fitted[[d]])){outcome<-FALSE; print("Model fit failed.")}
      time<-Sys.time() - strt
      newTime<-data.frame(time, as.character(modelEval), outcome)
      times<-rbind(times, newTime)
      
    }
  
  print(times)
  return(fitted)
}


secr.from.samples(t1, trapcsv = trapPath)
```

```
## No errors found :-)
```

```
## Warning: closing unused connection 34 (<-Nick-VAIO:11013)
```

```
## Warning: closing unused connection 33 (<-Nick-VAIO:11013)
```

```
## Warning: closing unused connection 32 (<-Nick-VAIO:11013)
```

```
## Warning: closing unused connection 31 (<-Nick-VAIO:11013)
```

```
## Warning: closing unused connection 30 (<-Nick-VAIO:11013)
```

```
## Warning: closing unused connection 29 (<-Nick-VAIO:11013)
```

```
## Warning: closing unused connection 28 (<-Nick-VAIO:11013)
```

```
## Warning: closing unused connection 27 (<-Nick-VAIO:11013)
```

```
## No errors found :-)
## No errors found :-)
##             time as.character.modelEval. outcome
## 1  6.620532 mins              g0 ~ b + t    TRUE
## 2  5.839181 mins                  g0 ~ t    TRUE
## 3 53.496895 mins                  g0 ~ b    TRUE
```

```
## [[1]]
## NULL
## 
## [[2]]
## NULL
## 
## [[3]]
## 
## secr.fit(capthist = t0caphist, model = modelEval, buffer = 10, 
##     CL = TRUE, detectfn = 0, trace = FALSE)
## secr 2.10.3, 11:15:07 13 Sep 2016
## 
## Detector type     proximity 
## Detector number   100 
## Average spacing   0.8 m 
## x-range           0 7.2 m 
## y-range           0 7.2 m 
## 
## N animals       :  49  
## N detections    :  169 
## N occasions     :  6 
## Mask area       :  0.06553075 ha 
## 
## Model           :  g0~b sigma~1 
## Fixed (real)    :  none 
## Detection fn    :  halfnormal
## N parameters    :  3 
## Log likelihood  :  -564.0315 
## AIC             :  1134.063 
## AICc            :  1134.596 
## 
## Beta parameters (coefficients) 
##                beta    SE.beta        lcl        ucl
## g0       -0.7575678 0.25013797 -1.2478292 -0.2673064
## g0.bTRUE  0.0197212 0.25407243 -0.4782516  0.5176940
## sigma    -0.7906698 0.04534672 -0.8795478 -0.7017919
## 
## Variance-covariance matrix of beta parameters 
##                    g0      g0.bTRUE         sigma
## g0        0.062569002 -0.0486660788 -0.0052337894
## g0.bTRUE -0.048666079  0.0645527993  0.0002113205
## sigma    -0.005233789  0.0002113205  0.0020563251
## 
## Fitted (real) parameters evaluated at base levels of covariates 
##        link  estimate SE.estimate       lcl       ucl
## g0    logit 0.3191745  0.05435552 0.2230761 0.4335685
## sigma   log 0.4535409  0.02057717 0.4149705 0.4956963
```

```r
secr.from.samples(t2, trapcsv = trapPath)
```

```
## No errors found :-)
```

```
## Warning: closing unused connection 13 (<-Nick-VAIO:11013)
```

```
## Warning: closing unused connection 12 (<-Nick-VAIO:11013)
```

```
## Warning: closing unused connection 11 (<-Nick-VAIO:11013)
```

```
## Warning: closing unused connection 10 (<-Nick-VAIO:11013)
```

```
## Warning: closing unused connection 9 (<-Nick-VAIO:11013)
```

```
## Warning: closing unused connection 8 (<-Nick-VAIO:11013)
```

```
## Warning: closing unused connection 7 (<-Nick-VAIO:11013)
```

```
## Warning: closing unused connection 6 (<-Nick-VAIO:11013)
```

```
## No errors found :-)
## No errors found :-)
##             time as.character.modelEval. outcome
## 1  9.008773 mins              g0 ~ b + t    TRUE
## 2  5.278936 mins                  g0 ~ t    TRUE
## 3 58.634305 mins                  g0 ~ b    TRUE
```

```
## [[1]]
## NULL
## 
## [[2]]
## NULL
## 
## [[3]]
## 
## secr.fit(capthist = t0caphist, model = modelEval, buffer = 10, 
##     CL = TRUE, detectfn = 0, trace = FALSE)
## secr 2.10.3, 11:30:20 13 Sep 2016
## 
## Detector type     proximity 
## Detector number   100 
## Average spacing   0.8 m 
## x-range           0 7.2 m 
## y-range           0 7.2 m 
## 
## N animals       :  50  
## N detections    :  152 
## N occasions     :  6 
## Mask area       :  0.06553075 ha 
## 
## Model           :  g0~b sigma~1 
## Fixed (real)    :  none 
## Detection fn    :  halfnormal
## N parameters    :  3 
## Log likelihood  :  -544.175 
## AIC             :  1094.35 
## AICc            :  1094.872 
## 
## Beta parameters (coefficients) 
##                 beta    SE.beta        lcl        ucl
## g0       -1.10961627 0.26359572 -1.6262544 -0.5929781
## g0.bTRUE -0.07338612 0.26033653 -0.5836363  0.4368641
## sigma    -0.68458106 0.05073162 -0.7840132 -0.5851489
## 
## Variance-covariance matrix of beta parameters 
##                    g0     g0.bTRUE        sigma
## g0        0.069482704 -0.054125604 -0.006226105
## g0.bTRUE -0.054125604  0.067775107  0.000572753
## sigma    -0.006226105  0.000572753  0.002573697
## 
## Fitted (real) parameters evaluated at base levels of covariates 
##        link  estimate SE.estimate       lcl       ucl
## g0    logit 0.2479424   0.0491519 0.1643441 0.3559518
## sigma   log 0.5043015   0.0256005 0.4565700 0.5570229
```

```r
secr.from.samples(t3, trapcsv = trapPath)
```

```
## No errors found :-)
```

```
## Warning: closing unused connection 21 (<-Nick-VAIO:11013)
```

```
## Warning: closing unused connection 20 (<-Nick-VAIO:11013)
```

```
## Warning: closing unused connection 19 (<-Nick-VAIO:11013)
```

```
## Warning: closing unused connection 18 (<-Nick-VAIO:11013)
```

```
## Warning: closing unused connection 17 (<-Nick-VAIO:11013)
```

```
## Warning: closing unused connection 16 (<-Nick-VAIO:11013)
```

```
## Warning: closing unused connection 15 (<-Nick-VAIO:11013)
```

```
## Warning: closing unused connection 14 (<-Nick-VAIO:11013)
```

```
## No errors found :-)
## No errors found :-)
##             time as.character.modelEval. outcome
## 1  9.150566 mins              g0 ~ b + t    TRUE
## 2  6.008701 mins                  g0 ~ t    TRUE
## 3 52.322298 mins                  g0 ~ b    TRUE
```

```
## [[1]]
## NULL
## 
## [[2]]
## NULL
## 
## [[3]]
## 
## secr.fit(capthist = t0caphist, model = modelEval, buffer = 10, 
##     CL = TRUE, detectfn = 0, trace = FALSE)
## secr 2.10.3, 11:46:29 13 Sep 2016
## 
## Detector type     proximity 
## Detector number   100 
## Average spacing   0.8 m 
## x-range           0 7.2 m 
## y-range           0 7.2 m 
## 
## N animals       :  52  
## N detections    :  134 
## N occasions     :  6 
## Mask area       :  0.06553075 ha 
## 
## Model           :  g0~b sigma~1 
## Fixed (real)    :  none 
## Detection fn    :  halfnormal
## N parameters    :  3 
## Log likelihood  :  -484.1173 
## AIC             :  974.2345 
## AICc            :  974.7345 
## 
## Beta parameters (coefficients) 
##                beta    SE.beta        lcl         ucl
## g0       -0.8599486 0.25587951 -1.3614633 -0.35843402
## g0.bTRUE -0.5773021 0.25288675 -1.0729510 -0.08165315
## sigma    -0.7303306 0.05628136 -0.8406401 -0.62002122
## 
## Variance-covariance matrix of beta parameters 
##                    g0      g0.bTRUE         sigma
## g0        0.065474323 -0.0469851771 -0.0076940956
## g0.bTRUE -0.046985177  0.0639517090  0.0009978858
## sigma    -0.007694096  0.0009978858  0.0031675911
## 
## Fitted (real) parameters evaluated at base levels of covariates 
##        link  estimate SE.estimate       lcl       ucl
## g0    logit 0.2973501  0.05346168 0.2040026 0.4113387
## sigma   log 0.4817497  0.02713501 0.4314343 0.5379330
```

```r
secr.from.samples(t4, trapcsv = trapPath)
```

```
## Session BearMR 
## More than one detection per detector per occasion at proximity detector(s)
## Session BearMR 
## More than one detection per detector per occasion at proximity detector(s)
## [1] "Model fit failed."
## Session BearMR 
## More than one detection per detector per occasion at proximity detector(s)
## Session BearMR 
## More than one detection per detector per occasion at proximity detector(s)
## [1] "Model fit failed."
## Session BearMR 
## More than one detection per detector per occasion at proximity detector(s)
## Session BearMR 
## More than one detection per detector per occasion at proximity detector(s)
## [1] "Model fit failed."
##              time as.character.modelEval. outcome
## 1 0.03119993 secs              g0 ~ b + t   FALSE
## 2 0.03220010 secs                  g0 ~ t   FALSE
## 3 0.04780006 secs                  g0 ~ b   FALSE
```

```
## [[1]]
## NULL
## 
## [[2]]
## NULL
## 
## [[3]]
## NULL
```

