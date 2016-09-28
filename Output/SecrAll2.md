# Initial SECR models 




Load libraries


```r
   library(dplyr) # for data manipulation
   library(secr) 
   library(secrdesign)
   library(scrbook)
   library(ggplot2)
   options(width=160)
```

## Initial data explorations
Lets look at the trapping grid.   
  


```r
  trap.gr<-read.csv("../data/detectorfileScaled.csv", header=FALSE)
  names(trap.gr)<-c("Trap", "X", "Y")
  plot(trap.gr$X, trap.gr$Y, main="Trap Grid", xlab="X", ylab="Y") 
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

Read in data created using "Data Exploration.R" file, names(bearCH)<-c("Session", "ID", "Occasion", "Detector", "Count", "Sex")


```r
  bearCH<-read.capthist("../data/BearCH.csv", "../data/detectorfileScaled.csv",  detector= 'proximity', covnames="Sex")
```

```
## No errors found :-)
```

Try plotting method from scrbook package 


```r
  enc<-read.csv("../data/BearCH.csv", header=F) # Encounter data
  enc[,1]<-as.numeric(1) # Treat individuals and periods as factor variables
  enc[,2]<-as.numeric(enc[,2])
  traps<-read.csv("../data/detectorfileScaled.csv", header=FALSE) # Trap data
  traps.3d<-cbind(traps, matrix(1, nrow(traps),6))  # add columns to say when traps were active
  y3d<-SCR23darray(enc[,1:4], traps.3d) # Create 3d array from capture history
  spiderplot(y3d, traps[,2:3]) # plot centroids along with recpatures for each individual
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

```
## $xcent
##  [1]  6.7879342  4.7523061  3.8101265  8.7284770  5.7085273  6.2290399  6.6124957  6.7781150  8.3458890  7.1316006  8.1768162  8.9864519  4.0063365  3.2587474
## [15]  8.6366540  8.4282984  7.8533623  6.9462871  9.8142318  7.8373251  6.9821324  3.1551494  1.2375379  0.3893351  6.7128891  3.7510853 10.0896873  8.7284770
## [29]  4.9545165  4.1802003  8.0468022  6.8244947  9.3026417  4.0909616  7.2882166  8.8738312  3.3888988  5.5522468  9.5560640  2.3204930  8.7004208  8.7872839
## [43]  3.5282345
## 
## $avg.s
##              [,1]        [,2]
##  [1,] -0.62976213  6.75865748
##  [2,]  4.34596964  1.92274823
##  [3,]  2.67358208 -2.71459437
##  [4,]  0.21479718  8.72583365
##  [5,] -3.99783112 -4.07487792
##  [6,]  2.48749144 -5.71080773
##  [7,] -1.26822052  6.48973933
##  [8,]  0.06587713  6.77779482
##  [9,]  7.22379217  4.17979538
## [10,] -4.42207240 -5.59508741
## [11,]  1.09953178  8.10255225
## [12,]  7.03015501 -5.59761000
## [13,]  3.96863606 -0.54832438
## [14,] -0.72255556 -3.17763245
## [15,] -6.69593390  5.45493003
## [16,] -7.67086560  3.49199590
## [17,] -7.84975089  0.23813835
## [18,] -6.66932653  1.94190331
## [19,]  1.72712845  9.66106481
## [20,]  0.45082255  7.82434813
## [21,] -5.41963003  4.40202041
## [22,]  0.05637236 -3.15464573
## [23,] -0.24354523 -1.21333661
## [24,]  0.16181416  0.35411580
## [25,] -4.26628044 -5.18283040
## [26,] -3.74436059 -0.22450927
## [27,] -9.98628044 -1.44083040
## [28,]  0.21479718  8.72583365
## [29,] -0.80214912 -4.88915031
## [30,]  0.47355096 -4.15329070
## [31,]  6.49753406 -4.74690180
## [32,]  6.76452732  0.90271721
## [33,]  7.20769533 -5.88117940
## [34,]  3.40642523  2.26544340
## [35,]  1.95744564 -7.02043503
## [36,]  7.42470872  4.85989507
## [37,] -0.47509209  3.35543181
## [38,] -5.49573151  0.79017725
## [39,]  7.81029424  5.50614768
## [40,]  2.06858650  1.05149298
## [41,] -8.55228044 -1.59869339
## [42,] -8.24589634  3.03670070
## [43,] -3.52761784  0.06596146
## 
## $center
## [1] -2.479339e-10  1.735537e-10
```

### Summary of the data provided by SECR

- n = number of individuals detected on each occasion
- u = number of individuals detected for the first time on each occasion
- f =  number of individuals detected exactly f times
- M(t+1) =  cumulative number of individuals detected
- losses	= number of individuals reported as not released on each occasion
- detections	= number of detections, including within-occasion `recaptures'
- detectors visited = number of detectors at which at least one detection was recorded
- detectors used =  number of detectors, excluding any `not set' in usage attribute of traps attribute


```r
  summary(bearCH)
```

```
## Object class      capthist 
## Detector type     proximity 
## Detector number   121 
## Average spacing   1.078547 m 
## x-range           -10.24721 8.170471 m 
## y-range           -10.32148 9.661065 m 
## Counts by occasion 
##                     1   2   3   4   5   6 Total
## n                  14  23  24  28  25  21   135
## u                  14  13   8   3   1   4    43
## f                  13   6   3  10   5   6    43
## M(t+1)             14  27  35  38  39  43    43
## losses              0   0   0   0   0   0     0
## detections         29  92  84 122 104  76   507
## detectors visited  22  64  59  75  74  53   347
## detectors used    121 121 121 121 121 121   726
```

```r
  str(bearCH)
```

```
##  capthist [1:43, 1:6, 1:121] 0 0 0 0 0 0 0 0 0 0 ...
##  - attr(*, "dimnames")=List of 3
##   ..$ : chr [1:43] "0056" "10-86-01" "12-106-02" "120-4-2" ...
##   ..$ : chr [1:6] "1" "2" "3" "4" ...
##   ..$ : chr [1:121] "1" "2" "3" "4" ...
##  - attr(*, "covariates")='data.frame':	43 obs. of  1 variable:
##   ..$ Sex: Factor w/ 2 levels "F","M": 1 2 2 2 1 1 2 2 1 2 ...
##  - attr(*, "traps")=Classes 'traps' and 'data.frame':	121 obs. of  2 variables:
##   ..$ x: num [1:121] -3.428 -2.414 -0.233 1.863 2.893 ...
##   ..$ y: num [1:121] -8.99 -9.42 -9.85 -9.04 -10.13 ...
##   ..- attr(*, "detector")= chr "proximity"
##   ..- attr(*, "spacex")= num 0.000643
##   ..- attr(*, "spacey")= num 0.00263
##   ..- attr(*, "spacing")= num 1.08
##  - attr(*, "session")= chr "BearMR"
##  - attr(*, "inject.time")= num [1:507] 0 0 0 0 0 0 0 0 0 0 ...
```

## Models 
  
 First, explore the effect of scaling the trapping grid coordinates
 
 - Model detection as a function of period, movement as function of sex
 - Compare models fit using scaled and unscaled grid
    
Model detection as a function of capture Period and movement as a function of sex of the animal.
 Buffer if scaled = 10, if unscaled = 10*10000 = 10,000


```r
  secr0 <- secr.fit(bearCH, model =list(g0~t+Sex, sigma~Sex), buffer = 10, trace = FALSE, CL=TRUE) # null model
```

For some reason, I am getting different answers depending on whether I "knit" the file or run from the console.  Try to calculate
the logL's for each set of parameters.  Secr0.A uses the parameters I obtained when "knitting" the file.  Secr0.B when running 
in the console.  Interesting, the logL's don't match those from secr0 above.  


```r
  secr0.A<- secr.fit(bearCH, model =list(g0~t+Sex, sigma~Sex), buffer = 10, trace = FALSE, CL=TRUE,
                     details=list(LLonly=TRUE), start=c( -2.1684004,  0.9448443,  0.7901649,  1.5231371,  1.2350439,  0.4311504, -0.1298321,  0.6217320,  0.6001064))
  secr0.B<- secr.fit(bearCH, model =list(g0~t+Sex, sigma~Sex), buffer = 10, trace = FALSE, CL=TRUE,
                     details=list(LLonly=TRUE), start=c( 2.0106951,  1.3681691,  1.2842788,  1.7731416,  1.5592968 , 1.1002393, -0.6942304,  0.8666626,  0.6107980))
  secr0.A
```

```
##    logLik 
## -1820.714
```

```r
  secr0.B
```

```
##    logLik 
## -2110.349
```

```r
  secr0
```

```
## 
## secr.fit(capthist = bearCH, model = list(g0 ~ t + Sex, sigma ~ 
##     Sex), buffer = 10, CL = TRUE, trace = FALSE)
## secr 2.9.5, 19:00:21 10 Aug 2015
## 
## Detector type     proximity 
## Detector number   121 
## Average spacing   1.078547 m 
## x-range           -10.24721 8.170471 m 
## y-range           -10.32148 9.661065 m 
## N animals       :  43  
## N detections    :  507 
## N occasions     :  6 
## Mask area       :  0.1224767 ha 
## 
## Model           :  g0~t + Sex sigma~Sex 
## Fixed (real)    :  none 
## Detection fn    :  halfnormal
## N parameters    :  9 
## Log likelihood  :  -1736.066 
## AIC             :  3490.131 
## AICc            :  3495.586 
## 
## Beta parameters (coefficients) 
##                  beta    SE.beta        lcl        ucl
## g0         -2.0106973 0.23787241 -2.4769187 -1.5444760
## g0.t2       1.3681730 0.23830774  0.9010984  1.8352476
## g0.t3       1.2842830 0.24071727  0.8124858  1.7560802
## g0.t4       1.7731450 0.23515591  1.3122479  2.2340421
## g0.t5       1.5593030 0.23707802  1.0946386  2.0239674
## g0.t6       1.1002431 0.24165554  0.6266069  1.5738793
## g0.SexM    -0.6942333 0.18326776 -1.0534315 -0.3350351
## sigma       0.8666622 0.04754932  0.7734673  0.9598572
## sigma.SexM  0.6107982 0.06388215  0.4855915  0.7360050
## 
## Variance-covariance matrix of beta parameters 
##                      g0         g0.t2         g0.t3         g0.t4         g0.t5         g0.t6       g0.SexM         sigma    sigma.SexM
## g0          0.056583283 -0.0378366539 -0.0374997075 -0.0372530714 -0.0369371410 -3.900974e-02 -0.0220323142 -0.0037731738  3.929829e-03
## g0.t2      -0.037836654  0.0567905794  0.0396717436  0.0397247132  0.0397256596  3.952080e-02 -0.0017253620 -0.0002829010  1.320385e-04
## g0.t3      -0.037499707  0.0396717436  0.0579448024  0.0397724905  0.0397795165  3.953504e-02 -0.0021910691 -0.0004340371  2.691102e-04
## g0.t4      -0.037253071  0.0397247132  0.0397724905  0.0552983016  0.0398610618  3.957255e-02 -0.0021884092 -0.0004701124  1.367033e-04
## g0.t5      -0.036937141  0.0397256596  0.0397795165  0.0398610618  0.0562059875  3.955442e-02 -0.0028789704 -0.0005364804  3.698606e-04
## g0.t6      -0.039009744  0.0395208038  0.0395350403  0.0395725490  0.0395544183  5.839740e-02 -0.0002800397 -0.0001047984 -6.665050e-06
## g0.SexM    -0.022032314 -0.0017253620 -0.0021910691 -0.0021884092 -0.0028789704 -2.800397e-04  0.0335870730  0.0041188605 -6.087975e-03
## sigma      -0.003773174 -0.0002829010 -0.0004340371 -0.0004701124 -0.0005364804 -1.047984e-04  0.0041188605  0.0022609382 -2.257430e-03
## sigma.SexM  0.003929829  0.0001320385  0.0002691102  0.0001367033  0.0003698606 -6.665050e-06 -0.0060879749 -0.0022574301  4.080929e-03
## 
## Fitted (real) parameters evaluated at base levels of covariates 
##        link  estimate SE.estimate        lcl       ucl
## g0    logit 0.1180843  0.02477213 0.07749219 0.1758855
## sigma   log 2.3789572  0.11318177 2.16726774 2.6113235
```

```r
  plot(secr0$mask)
  plot(bearCH, tracks=T, add=T)
```

```
## Warning in plot.capthist(bearCH, tracks = T, add = T): track for repeat detections on same occasion joins points in arbitrary sequence
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

Calculate density estimate (a derived parameter)


```r
  ds<-derived(secr0)
```

Compare density estimates (just different scales)  


```r
   ds  # scaled
```

```
##         estimate SE.estimate      lcl      ucl       CVn        CVa       CVD
## esa   0.08790324          NA       NA       NA        NA         NA        NA
## D   489.17422519    77.67074 359.0466 666.4635 0.1571663 0.02257494 0.1587793
```

## Additional Models

In all of these models, I will assume sigma~sex (males move more than females)

- g0 ~ T (time trend in detection probabilities)
- g0 ~ b (behavioral response following initial detection)
- g0 ~ b + t (time specific detections & behavioral response)
- g0 ~ b + T (time trend and behavioral response)


#### Time trend in detection  


```r
  secr1 <- secr.fit(bearCH, model =list(g0~T+Sex, sigma~Sex), buffer = 10, trace = FALSE, CL=TRUE) # null model
  (d1<-derived(secr1))
```

```
##         estimate SE.estimate      lcl      ucl       CVn       CVa       CVD
## esa   0.08801502          NA       NA       NA        NA        NA        NA
## D   488.55297431    77.57281 358.5896 665.6189 0.1571612 0.0226207 0.1587808
```

#### Behavioral response  


```r
  secr2 <- secr.fit(bearCH, model =list(g0~b+Sex, sigma~Sex), buffer = 10, trace = FALSE, CL=TRUE) # null model
  (d2<-derived(secr2))
```

```
##         estimate SE.estimate      lcl      ucl       CVn        CVa       CVD
## esa   0.07854264          NA       NA       NA        NA         NA        NA
## D   547.47334173    87.69765 400.7514 747.9128 0.1572178 0.03069458 0.1601862
```

#### Time specific detection and behavioral response  


```r
  secr3 <- secr.fit(bearCH, model =list(g0~b+t+Sex, sigma~Sex), buffer = 10, trace = FALSE, CL=TRUE) # null model
  (d3<-derived(secr3))
```

```
##         estimate SE.estimate      lcl      ucl       CVn        CVa       CVD
## esa   0.08248825          NA       NA       NA        NA         NA        NA
## D   521.28638488    84.00143 380.8813 713.4492 0.1572561 0.03517719 0.1611426
```

#### Time trend and behavioral response


```r
  secr4 <- secr.fit(bearCH, model =list(g0~b+T+Sex, sigma~Sex), buffer = 10, trace = FALSE, CL=TRUE) # null model
  (d4<-derived(secr4))
```

```
##         estimate SE.estimate      lcl      ucl       CVn        CVa       CVD
## esa   0.07681376          NA       NA       NA        NA         NA        NA
## D   559.79557283    90.26901 408.9311 766.3176 0.1572051 0.03590626 0.1612535
```

Compare models using AIC  


```r
  AIC(secr0, secr1, secr2, secr3, secr4)
```

```
##                          model   detectfn npar    logLik      AIC     AICc  dAICc AICcwt
## secr3 g0~b + t + Sex sigma~Sex halfnormal   10 -1732.707 3485.415 3492.290  0.000 0.8386
## secr0     g0~t + Sex sigma~Sex halfnormal    9 -1736.066 3490.131 3495.586  3.296 0.1614
## secr2     g0~b + Sex sigma~Sex halfnormal    5 -1747.610 3505.220 3506.842 14.552 0.0000
## secr4 g0~b + T + Sex sigma~Sex halfnormal    6 -1746.727 3505.454 3507.787 15.497 0.0000
## secr1     g0~T + Sex sigma~Sex halfnormal    5 -1765.917 3541.834 3543.455 51.165 0.0000
```

## Parameter estimates  

#### Time specific detection 


```r
  secr0
```

```
## 
## secr.fit(capthist = bearCH, model = list(g0 ~ t + Sex, sigma ~ 
##     Sex), buffer = 10, CL = TRUE, trace = FALSE)
## secr 2.9.5, 19:00:21 10 Aug 2015
## 
## Detector type     proximity 
## Detector number   121 
## Average spacing   1.078547 m 
## x-range           -10.24721 8.170471 m 
## y-range           -10.32148 9.661065 m 
## N animals       :  43  
## N detections    :  507 
## N occasions     :  6 
## Mask area       :  0.1224767 ha 
## 
## Model           :  g0~t + Sex sigma~Sex 
## Fixed (real)    :  none 
## Detection fn    :  halfnormal
## N parameters    :  9 
## Log likelihood  :  -1736.066 
## AIC             :  3490.131 
## AICc            :  3495.586 
## 
## Beta parameters (coefficients) 
##                  beta    SE.beta        lcl        ucl
## g0         -2.0106973 0.23787241 -2.4769187 -1.5444760
## g0.t2       1.3681730 0.23830774  0.9010984  1.8352476
## g0.t3       1.2842830 0.24071727  0.8124858  1.7560802
## g0.t4       1.7731450 0.23515591  1.3122479  2.2340421
## g0.t5       1.5593030 0.23707802  1.0946386  2.0239674
## g0.t6       1.1002431 0.24165554  0.6266069  1.5738793
## g0.SexM    -0.6942333 0.18326776 -1.0534315 -0.3350351
## sigma       0.8666622 0.04754932  0.7734673  0.9598572
## sigma.SexM  0.6107982 0.06388215  0.4855915  0.7360050
## 
## Variance-covariance matrix of beta parameters 
##                      g0         g0.t2         g0.t3         g0.t4         g0.t5         g0.t6       g0.SexM         sigma    sigma.SexM
## g0          0.056583283 -0.0378366539 -0.0374997075 -0.0372530714 -0.0369371410 -3.900974e-02 -0.0220323142 -0.0037731738  3.929829e-03
## g0.t2      -0.037836654  0.0567905794  0.0396717436  0.0397247132  0.0397256596  3.952080e-02 -0.0017253620 -0.0002829010  1.320385e-04
## g0.t3      -0.037499707  0.0396717436  0.0579448024  0.0397724905  0.0397795165  3.953504e-02 -0.0021910691 -0.0004340371  2.691102e-04
## g0.t4      -0.037253071  0.0397247132  0.0397724905  0.0552983016  0.0398610618  3.957255e-02 -0.0021884092 -0.0004701124  1.367033e-04
## g0.t5      -0.036937141  0.0397256596  0.0397795165  0.0398610618  0.0562059875  3.955442e-02 -0.0028789704 -0.0005364804  3.698606e-04
## g0.t6      -0.039009744  0.0395208038  0.0395350403  0.0395725490  0.0395544183  5.839740e-02 -0.0002800397 -0.0001047984 -6.665050e-06
## g0.SexM    -0.022032314 -0.0017253620 -0.0021910691 -0.0021884092 -0.0028789704 -2.800397e-04  0.0335870730  0.0041188605 -6.087975e-03
## sigma      -0.003773174 -0.0002829010 -0.0004340371 -0.0004701124 -0.0005364804 -1.047984e-04  0.0041188605  0.0022609382 -2.257430e-03
## sigma.SexM  0.003929829  0.0001320385  0.0002691102  0.0001367033  0.0003698606 -6.665050e-06 -0.0060879749 -0.0022574301  4.080929e-03
## 
## Fitted (real) parameters evaluated at base levels of covariates 
##        link  estimate SE.estimate        lcl       ucl
## g0    logit 0.1180843  0.02477213 0.07749219 0.1758855
## sigma   log 2.3789572  0.11318177 2.16726774 2.6113235
```

#### Time trend in detection  


```r
  secr1
```

```
## 
## secr.fit(capthist = bearCH, model = list(g0 ~ T + Sex, sigma ~ 
##     Sex), buffer = 10, CL = TRUE, trace = FALSE)
## secr 2.9.5, 19:42:29 10 Aug 2015
## 
## Detector type     proximity 
## Detector number   121 
## Average spacing   1.078547 m 
## x-range           -10.24721 8.170471 m 
## y-range           -10.32148 9.661065 m 
## N animals       :  43  
## N detections    :  507 
## N occasions     :  6 
## Mask area       :  0.1224767 ha 
## 
## Model           :  g0~T + Sex sigma~Sex 
## Fixed (real)    :  none 
## Detection fn    :  halfnormal
## N parameters    :  5 
## Log likelihood  :  -1765.917 
## AIC             :  3541.834 
## AICc            :  3543.455 
## 
## Beta parameters (coefficients) 
##                  beta    SE.beta         lcl        ucl
## g0         -1.1517650 0.17012705 -1.48520790 -0.8183221
## g0.T        0.1400500 0.03301478  0.07534221  0.2047578
## g0.SexM    -0.6417341 0.17755430 -0.98973414 -0.2937341
## sigma       0.8714236 0.04764701  0.77803716  0.9648100
## sigma.SexM  0.6069122 0.06402737  0.48142086  0.7324036
## 
## Variance-covariance matrix of beta parameters 
##                      g0          g0.T       g0.SexM         sigma    sigma.SexM
## g0          0.028943212 -2.761055e-03 -2.180067e-02 -3.855353e-03  3.948931e-03
## g0.T       -0.002761055  1.089976e-03 -5.858831e-05 -3.777348e-05  8.318089e-07
## g0.SexM    -0.021800672 -5.858831e-05  3.152553e-02  3.953068e-03 -5.910450e-03
## sigma      -0.003855353 -3.777348e-05  3.953068e-03  2.270237e-03 -2.268957e-03
## sigma.SexM  0.003948931  8.318089e-07 -5.910450e-03 -2.268957e-03  4.099504e-03
## 
## Fitted (real) parameters evaluated at base levels of covariates 
##        link  estimate SE.estimate       lcl       ucl
## g0    logit 0.2401668  0.03104593 0.1846421 0.3061199
## sigma   log 2.3903112  0.11395585 2.1771946 2.6242890
```

#### Behavioral response    


```r
  secr2
```

```
## 
## secr.fit(capthist = bearCH, model = list(g0 ~ b + Sex, sigma ~ 
##     Sex), buffer = 10, CL = TRUE, trace = FALSE)
## secr 2.9.5, 19:54:41 10 Aug 2015
## 
## Detector type     proximity 
## Detector number   121 
## Average spacing   1.078547 m 
## x-range           -10.24721 8.170471 m 
## y-range           -10.32148 9.661065 m 
## N animals       :  43  
## N detections    :  507 
## N occasions     :  6 
## Mask area       :  0.1224767 ha 
## 
## Model           :  g0~b + Sex sigma~Sex 
## Fixed (real)    :  none 
## Detection fn    :  halfnormal
## N parameters    :  5 
## Log likelihood  :  -1747.61 
## AIC             :  3505.22 
## AICc            :  3506.842 
## 
## Beta parameters (coefficients) 
##                  beta    SE.beta        lcl        ucl
## g0         -1.6458181 0.19200866 -2.0221482 -1.2694880
## g0.bTRUE    1.0383027 0.14850409  0.7472400  1.3293654
## g0.SexM    -0.5875912 0.17864933 -0.9377375 -0.2374450
## sigma       0.8598876 0.04364657  0.7743419  0.9454333
## sigma.SexM  0.6075204 0.06104874  0.4878671  0.7271738
## 
## Variance-covariance matrix of beta parameters 
##                     g0      g0.bTRUE       g0.SexM         sigma    sigma.SexM
## g0          0.03686733 -1.805503e-02 -0.0224093393 -0.0036862099  3.997320e-03
## g0.bTRUE   -0.01805503  2.205346e-02  0.0003952051 -0.0003122689 -6.773903e-05
## g0.SexM    -0.02240934  3.952051e-04  0.0319155833  0.0039362658 -5.944234e-03
## sigma      -0.00368621 -3.122689e-04  0.0039362658  0.0019050229 -1.899642e-03
## sigma.SexM  0.00399732 -6.773903e-05 -0.0059442343 -0.0018996422  3.726949e-03
## 
## Fitted (real) parameters evaluated at base levels of covariates 
##        link  estimate SE.estimate      lcl       ucl
## g0    logit 0.1616749  0.02602412 0.116897 0.2193449
## sigma   log 2.3628950  0.10318140 2.169164 2.5739284
```

#### Time specific detection and behavioral response  


```r
  secr3
```

```
## 
## secr.fit(capthist = bearCH, model = list(g0 ~ b + t + Sex, sigma ~ 
##     Sex), buffer = 10, CL = TRUE, trace = FALSE)
## secr 2.9.5, 20:02:55 10 Aug 2015
## 
## Detector type     proximity 
## Detector number   121 
## Average spacing   1.078547 m 
## x-range           -10.24721 8.170471 m 
## y-range           -10.32148 9.661065 m 
## N animals       :  43  
## N detections    :  507 
## N occasions     :  6 
## Mask area       :  0.1224767 ha 
## 
## Model           :  g0~b + t + Sex sigma~Sex 
## Fixed (real)    :  none 
## Detection fn    :  halfnormal
## N parameters    :  10 
## Log likelihood  :  -1732.707 
## AIC             :  3485.415 
## AICc            :  3492.29 
## 
## Beta parameters (coefficients) 
##                  beta    SE.beta        lcl        ucl
## g0         -2.0689050 0.23881657 -2.5369768 -1.6008331
## g0.bTRUE    0.6160296 0.23816768  0.1492295  1.0828296
## g0.t2       1.0447628 0.26964114  0.5162758  1.5732497
## g0.t3       0.7793892 0.30887264  0.1740100  1.3847685
## g0.t4       1.1714826 0.32797630  0.5286609  1.8143044
## g0.t5       0.9488472 0.33177623  0.2985777  1.5991166
## g0.t6       0.5030087 0.33261741 -0.1489094  1.1549269
## g0.SexM    -0.6443136 0.18361595 -1.0041943 -0.2844329
## sigma       0.8581330 0.04424482  0.7714147  0.9448512
## sigma.SexM  0.6130436 0.06150971  0.4924868  0.7336004
## 
## Variance-covariance matrix of beta parameters 
##                      g0      g0.bTRUE         g0.t2         g0.t3         g0.t4         g0.t5         g0.t6      g0.SexM         sigma    sigma.SexM
## g0          0.057033354 -0.0049296713 -3.516140e-02 -0.0335611782 -0.0325488766 -0.0321795310 -3.416468e-02 -0.022368070 -3.709695e-03  3.938494e-03
## g0.bTRUE   -0.004929671  0.0567238453 -2.988321e-02 -0.0462903679 -0.0547263434 -0.0555537445 -5.462180e-02  0.003759912 -6.284833e-04 -3.890180e-05
## g0.t2      -0.035161399 -0.0298832063  7.270634e-02  0.0638511940  0.0683251157  0.0687582214  6.810417e-02 -0.003583824 -4.415373e-05  2.411800e-04
## g0.t3      -0.033561178 -0.0462903679  6.385119e-02  0.0954023082  0.0841936664  0.0848698701  8.392563e-02 -0.004987523  1.082043e-04  2.185670e-04
## g0.t4      -0.032548877 -0.0547263434  6.832512e-02  0.0841936664  0.1075684552  0.0932087210  9.207815e-02 -0.005518358  1.798552e-04  1.333576e-04
## g0.t5      -0.032179531 -0.0555537445  6.875822e-02  0.0848698701  0.0932087210  0.1100754680  9.285773e-02 -0.006239277  1.466142e-04  3.397690e-04
## g0.t6      -0.034164677 -0.0546218023  6.810417e-02  0.0839256283  0.0920781512  0.0928577285  1.106343e-01 -0.003813881  5.130821e-04  4.664143e-06
## g0.SexM    -0.022368070  0.0037599117 -3.583824e-03 -0.0049875233 -0.0055183581 -0.0062392768 -3.813881e-03  0.033714819  4.056099e-03 -6.094649e-03
## sigma      -0.003709695 -0.0006284833 -4.415373e-05  0.0001082043  0.0001798552  0.0001466142  5.130821e-04  0.004056099  1.957604e-03 -1.947101e-03
## sigma.SexM  0.003938494 -0.0000389018  2.411800e-04  0.0002185670  0.0001333576  0.0003397690  4.664143e-06 -0.006094649 -1.947101e-03  3.783444e-03
## 
## Fitted (real) parameters evaluated at base levels of covariates 
##        link estimate SE.estimate        lcl       ucl
## g0    logit 0.112156  0.02378065 0.07330628 0.1678652
## sigma   log 2.358753  0.10441368 2.16282394 2.5724307
```

#### Time trend and behavioral response


```r
   secr4
```

```
## 
## secr.fit(capthist = bearCH, model = list(g0 ~ b + T + Sex, sigma ~ 
##     Sex), buffer = 10, CL = TRUE, trace = FALSE)
## secr 2.9.5, 21:08:05 10 Aug 2015
## 
## Detector type     proximity 
## Detector number   121 
## Average spacing   1.078547 m 
## x-range           -10.24721 8.170471 m 
## y-range           -10.32148 9.661065 m 
## N animals       :  43  
## N detections    :  507 
## N occasions     :  6 
## Mask area       :  0.1224767 ha 
## 
## Model           :  g0~b + T + Sex sigma~Sex 
## Fixed (real)    :  none 
## Detection fn    :  halfnormal
## N parameters    :  6 
## Log likelihood  :  -1746.727 
## AIC             :  3505.454 
## AICc            :  3507.787 
## 
## Beta parameters (coefficients) 
##                   beta    SE.beta        lcl         ucl
## g0         -1.63106364 0.19298880 -2.0093147 -1.25281255
## g0.bTRUE    1.21715041 0.20078636  0.8236164  1.61068444
## g0.T       -0.06127307 0.04610125 -0.1516299  0.02908371
## g0.SexM    -0.58038174 0.17896470 -0.9311461 -0.22961738
## sigma       0.85817061 0.04336996  0.7731671  0.94317417
## sigma.SexM  0.60761977 0.06081642  0.4884218  0.72681776
## 
## Variance-covariance matrix of beta parameters 
##                       g0      g0.bTRUE          g0.T       g0.SexM         sigma    sigma.SexM
## g0          0.0372446755 -1.652372e-02 -5.706754e-04 -0.0224388042 -3.714902e-03  3.999870e-03
## g0.bTRUE   -0.0165237171  4.031516e-02 -6.210626e-03  0.0009784059 -5.188872e-04 -5.286741e-05
## g0.T       -0.0005706754 -6.210626e-03  2.125325e-03 -0.0001974397  7.776284e-05 -8.667275e-06
## g0.SexM    -0.0224388042  9.784059e-04 -1.974397e-04  0.0320283626  3.920864e-03 -5.937866e-03
## sigma      -0.0037149024 -5.188872e-04  7.776284e-05  0.0039208642  1.880953e-03 -1.873559e-03
## sigma.SexM  0.0039998699 -5.286741e-05 -8.667275e-06 -0.0059378661 -1.873559e-03  3.698637e-03
## 
## Fitted (real) parameters evaluated at base levels of covariates 
##        link  estimate SE.estimate       lcl       ucl
## g0    logit 0.1636847  0.02641863 0.1182284 0.2222137
## sigma   log 2.3588415  0.10235098 2.1666172 2.5681201
```

## Density estimates
  
Convert density esitmate to N/sq mile.  As reported on the help page for secr.fit:

- Density for the unscaled analysis is reported in terms of animals/ha (with distances measured in m). So...
- For the scaled analysis, density will be reported in terms of animals/(1000x1000)ha (with distances measured 
in terms of km).  

There are 3861.022 square miles in 1000x1000 ha.  To convert, we use:

D (per 100 $mi^2$) = (N/ha)*(1000ha/3.861022 mi^2)*100  



```r
 desnconv<-function(x){
   100*x/3861.022
   }
  desnconv(ds[2,c(1,3,4)]) # point estimate anbd 95% CU **time varying detection model**
```

```
##   estimate      lcl      ucl
## D 12.66955 9.299262 17.26132
```

```r
  desnconv(d1[2,c(1,3,4)]) # point estimate anbd 95% CU **trend in detection model**
```

```
##   estimate      lcl      ucl
## D 12.65346 9.287426 17.23945
```

```r
  desnconv(d2[2,c(1,3,4)]) # point estimate anbd 95% CU **Behav response**
```

```
##   estimate      lcl      ucl
## D 14.17949 10.37941 19.37085
```

```r
  desnconv(d3[2,c(1,3,4)]) # point estimate anbd 95% CU **Time specific detection and behavioral response**
```

```
##   estimate     lcl      ucl
## D 13.50125 9.86478 18.47825
```

```r
  desnconv(d4[2,c(1,3,4)]) # point estimate anbd 95% CU **Time trend and behavioral response**
```

```
##   estimate      lcl      ucl
## D 14.49864 10.59127 19.84753
```

#### Calculate detection functions for the go~t+Sex, sigma~sex model



```r
  x<-seq(0,5, length=100)
  lx<-length(x)
  bet<-secr0$fit$par
  g0s.F<-plogis(rep(bet[1],6)+c(0,bet[2:6]))
  g0s.M<-plogis(rep(bet[1],6)+c(0,bet[2:6])+rep(bet[7],6))
  sigs<-c(exp(bet[8]), exp(bet[8]+bet[9]))
  phat<-NULL
  for(i in 1:6){
    pfemale<-g0s.F[i]*exp(-x^2/(2*sigs[1]))
    pmale<-g0s.M[i]*exp(-x^2/(2*sigs[2]))
    phat<-rbind(phat, data.frame(x=rep(x,2),sex=rep(c("F","M"),each=lx), p=c(pfemale, pmale), period=as.factor(rep(i,each=2*lx))))  
  }
```

Plot fitted detection functions.  Looks like the "time effect" is much bigger than then sex effect. 


```r
  ggplot(phat, aes(x, p, shape=sex, colour=period)) +geom_line()+facet_wrap(~sex)+xlab("Distance from HR center (km)")+ ylab("Detection Probability")
```

![plot of chunk unnamed-chunk-23](figure/unnamed-chunk-23-1.png) 

Some functions to look at
- reduce
- RMarkInput (to allow fitting of traditional M-R models in Program Mark, but from R)
- write.DA 
- write.SPACECAP (to allow Bayesian fitting)
 
Could also try:

 - Other detection functions (exponential, etc)
 - Resource selection map
 - SPACECAP/JAGS models   
 - Fit of models to data pooled across sessions (counts of individuals across occassions)
 - Fit of models using count data at each session  

 For how to deal with "non-habitat" in JAGS, see: http://www.mikemeredith.net/blog/1309_SECR_in_JAGS_patchy_habitat.htm#fold  
