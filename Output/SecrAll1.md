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
  secr0 <- secr.fit(bearCH, model =list(g0~t, sigma~Sex), buffer = 10, trace = FALSE, CL=TRUE) # null model
  plot(secr0$mask)
  plot(bearCH, tracks=T, add=T)
```

```
## Warning in plot.capthist(bearCH, tracks = T, add = T): track for repeat detections on same occasion joins points in arbitrary sequence
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

Calculate density estimate (a derived parameter)


```r
  ds<-derived(secr0)
```

What if we use the unscaled and uncentered trap locations?


```r
 bearCH.us<-read.capthist("../data/BearCH.csv", "../data/detectorfileUnScaled.csv",  detector= 'proximity', covnames="Sex")
```

```
## No errors found :-)
```

```r
 summary(bearCH.us)
```

```
## Object class      capthist 
## Detector type     proximity 
## Detector number   121 
## Average spacing   1078.547 m 
## x-range           444166.1 462583.8 m 
## y-range           5253349 5273332 m 
## Usage range by occasion
##     1 2 3 4 5 6 7 8
## min 1 1 1 1 1 1 1 1
## max 1 1 1 1 1 1 1 1
## Counts by occasion 
##                     1   2   3   4   5   6   7   8 Total
## n                  14  23  24  28  25  21   0   0   135
## u                  14  13   8   3   1   4   0   0    43
## f                  13   6   3  10   5   6   0   0    43
## M(t+1)             14  27  35  38  39  43  43  43    43
## losses              0   0   0   0   0   0   0   0     0
## detections         29  92  84 122 104  76   0   0   507
## detectors visited  22  64  59  75  74  53   0   0   347
## detectors used    121 121 121 121 121 121 121 121   968
```

```r
 secr0.US <- secr.fit(bearCH.us, model =list(g0~t, sigma~Sex), buffer = 10000, trace = FALSE, CL=TRUE) # null model
 dUS<-derived(secr0.US)
```

Compare density estimates (just different scales)  


```r
 dUS # unscaled
```

```
##         estimate  SE.estimate          lcl          ucl       CVn        CVa       CVD
## esa 8.874491e+04           NA           NA           NA        NA         NA        NA
## D   4.845348e-04 7.670603e-05 0.0003559635 0.0006595451 0.1564504 0.02418448 0.1583086
```

```r
 ds  # scaled
```

```
##         estimate SE.estimate     lcl      ucl       CVn        CVa       CVD
## esa   0.08874528          NA      NA       NA        NA         NA        NA
## D   484.53281519    76.70293 355.966 659.5351 0.1564502 0.02414779 0.1583029
```

 Now, N in the region (these depend critically on the size of the buffer!).  Early on, I used a buffer size of 10 (for scaled) and a buffer
 size of 50,000 for the unscaled. The latter buffer was much bigger (proporationally) and resulted in a much bigger N! Here, buffers are similar
 and there is little to no difference between the estimates.


```r
  region.N(secr0)
```

```
##     estimate SE.estimate      lcl      ucl  n
## E.N 59.34399    9.394324 43.59755 80.77770 43
## R.N 68.83806    5.376740 60.25854 81.68261 43
```

```r
  region.N(secr0.US)
```

```
##     estimate SE.estimate      lcl      ucl  n
## E.N 59.34424    9.394704 43.59725 80.77893 43
## R.N 68.83843    5.377380 60.25808 81.68477 43
```

Before moving forward, lets check the buffer = 10


```r
  mask.check(secr0) #looks OK
```

```
## Computing log likelihoods... 
## beta vector : -2.473702 0.9554292 
## beta vector : -2.473702 0.9554292 
## beta vector : -2.473702 0.9554292 
## beta vector : -2.473702 0.9554292 
## beta vector : -2.473702 0.9554292 
## beta vector : -2.473702 0.9554292 
## beta vector : -2.473702 0.9554292 
## beta vector : -2.473702 0.9554292 
## beta vector : -2.473702 0.9554292
```

```
##          spacing
## buffer    0.600276317453125 0.450207238089844 0.300138158726563
##   9.8842              1e+10             1e+10             1e+10
##   14.8263             1e+10             1e+10             1e+10
##   19.7684             1e+10             1e+10             1e+10
```

## Additional Models

In all of these models, I will assume sigma~sex (males move more than females)

- g0 ~ T (time trend in detection probabilities)
- g0 ~ b (behavioral response following initial detection)
- g0 ~ b + t (time specific detections & behavioral response)
- g0 ~ b + T (time trend and behavioral response)


#### Time trend in detection  


```r
  secr1 <- secr.fit(bearCH, model =list(g0~T, sigma~Sex), buffer = 10, trace = FALSE, CL=TRUE) # null model
  (d1<-derived(secr1))
```

```
##         estimate SE.estimate      lcl      ucl       CVn        CVa       CVD
## esa   0.08880523          NA       NA       NA        NA         NA        NA
## D   484.20572910    76.66908 355.7003 659.1368 0.1564894 0.02413686 0.1583399
```

#### Behavioral response  


```r
  secr2 <- secr.fit(bearCH, model =list(g0~b, sigma~Sex), buffer = 10, trace = FALSE, CL=TRUE) # null model
  (d2<-derived(secr2))
```

```
##         estimate SE.estimate     lcl      ucl       CVn        CVa       CVD
## esa   0.07823719          NA      NA       NA        NA         NA        NA
## D   549.61076933    88.21128 402.075 751.2828 0.1570845 0.03292395 0.1604977
```

#### Time specific detection and behavioral response  


```r
  secr3 <- secr.fit(bearCH, model =list(g0~b+t, sigma~Sex), buffer = 10, trace = FALSE, CL=TRUE) # null model
  (d3<-derived(secr3))
```

```
##         estimate SE.estimate      lcl      ucl       CVn        CVa      CVD
## esa   0.08186511          NA       NA       NA        NA         NA       NA
## D   525.25429871    84.88374 383.4395 719.5193 0.1569256 0.03860761 0.161605
```

#### Time trend and behavioral response


```r
  secr4 <- secr.fit(bearCH, model =list(g0~b+T, sigma~Sex), buffer = 10, trace = FALSE, CL=TRUE) # null model
  (d4<-derived(secr4))
```

```
##         estimate SE.estimate      lcl      ucl       CVn        CVa      CVD
## esa   0.07623517          NA       NA       NA        NA         NA       NA
## D   564.04414241    91.27419 411.5854 772.9764 0.1571768 0.03849026 0.161821
```

Compare models using AIC  


```r
  AIC(secr0, secr1, secr2, secr3, secr4)
```

```
##                    model   detectfn npar    logLik      AIC     AICc  dAICc AICcwt
## secr3 g0~b + t sigma~Sex halfnormal    9 -1739.065 3496.131 3501.585  0.000  0.945
## secr0     g0~t sigma~Sex halfnormal    8 -1743.518 3503.036 3507.271  5.686  0.055
## secr2     g0~b sigma~Sex halfnormal    4 -1753.140 3514.280 3515.332 13.747  0.000
## secr4 g0~b + T sigma~Sex halfnormal    5 -1752.099 3514.198 3515.820 14.235  0.000
## secr1     g0~T sigma~Sex halfnormal    4 -1772.627 3553.254 3554.306 52.721  0.000
```

## Parameter estimates  

#### Time specific detection 


```r
  secr0
```

```
## 
## secr.fit(capthist = bearCH, model = list(g0 ~ t, sigma ~ Sex), 
##     buffer = 10, CL = TRUE, trace = FALSE)
## secr 2.9.5, 15:45:36 10 Aug 2015
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
## Model           :  g0~t sigma~Sex 
## Fixed (real)    :  none 
## Detection fn    :  halfnormal
## N parameters    :  8 
## Log likelihood  :  -1743.518 
## AIC             :  3503.036 
## AICc            :  3507.271 
## 
## Beta parameters (coefficients) 
##                  beta    SE.beta        lcl        ucl
## g0         -2.4737024 0.20408735 -2.8737062 -2.0736985
## g0.t2       1.3528903 0.23627718  0.8897955  1.8159851
## g0.t3       1.2594920 0.23840972  0.7922175  1.7267664
## g0.t4       1.7544987 0.23306718  1.2976955  2.2113020
## g0.t5       1.5244840 0.23447043  1.0649304  1.9840376
## g0.t6       1.1071518 0.24007726  0.6366090  1.5776946
## sigma       0.9554292 0.04457587  0.8680621  1.0427963
## sigma.SexM  0.4795969 0.05552869  0.3707627  0.5884311
## 
## Variance-covariance matrix of beta parameters 
##                       g0         g0.t2         g0.t3         g0.t4         g0.t5
## g0          4.165165e-02 -3.853638e-02 -3.853983e-02 -3.824232e-02 -3.844502e-02
## g0.t2      -3.853638e-02  5.582690e-02  3.907747e-02  3.912482e-02  3.908825e-02
## g0.t3      -3.853983e-02  3.907747e-02  5.683919e-02  3.913277e-02  3.909363e-02
## g0.t4      -3.824232e-02  3.912482e-02  3.913277e-02  5.432031e-02  3.915940e-02
## g0.t5      -3.844502e-02  3.908825e-02  3.909363e-02  3.915940e-02  5.497638e-02
## g0.t6      -3.869011e-02  3.904610e-02  3.905049e-02  3.908676e-02  3.906097e-02
## sigma      -1.127670e-03 -1.635756e-04 -2.173578e-04 -3.343171e-04 -2.524477e-04
## sigma.SexM  2.281779e-05 -2.618182e-05 -7.768298e-06 -4.566275e-05  1.110511e-05
##                    g0.t6         sigma    sigma.SexM
## g0         -3.869011e-02 -0.0011276700  2.281779e-05
## g0.t2       3.904610e-02 -0.0001635756 -2.618182e-05
## g0.t3       3.905049e-02 -0.0002173578 -7.768298e-06
## g0.t4       3.908676e-02 -0.0003343171 -4.566275e-05
## g0.t5       3.906097e-02 -0.0002524477  1.110511e-05
## g0.t6       5.763709e-02 -0.0001431474  3.705375e-05
## sigma      -1.431474e-04  0.0019870079 -1.725578e-03
## sigma.SexM  3.705375e-05 -0.0017255785  3.083436e-03
## 
## Fitted (real) parameters evaluated at base levels of covariates 
##        link   estimate SE.estimate        lcl       ucl
## g0    logit 0.07772243  0.01462932 0.05346877 0.1116796
## sigma   log 2.59978627  0.11594532 2.38228985 2.8371395
```

#### Time trend in detection  


```r
  secr1
```

```
## 
## secr.fit(capthist = bearCH, model = list(g0 ~ T, sigma ~ Sex), 
##     buffer = 10, CL = TRUE, trace = FALSE)
## secr 2.9.5, 17:39:53 10 Aug 2015
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
## Model           :  g0~T sigma~Sex 
## Fixed (real)    :  none 
## Detection fn    :  halfnormal
## N parameters    :  4 
## Log likelihood  :  -1772.627 
## AIC             :  3553.254 
## AICc            :  3554.306 
## 
## Beta parameters (coefficients) 
##                  beta    SE.beta         lcl        ucl
## g0         -1.5920143 0.11656864 -1.82048461 -1.3635440
## g0.T        0.1406189 0.03275643  0.07641748  0.2048203
## sigma       0.9551351 0.04465460  0.86761368  1.0426565
## sigma.SexM  0.4824436 0.05568054  0.37331174  0.5915754
## 
## Variance-covariance matrix of beta parameters 
##                       g0          g0.T         sigma    sigma.SexM
## g0          1.358825e-02 -2.749647e-03 -1.203079e-03  5.815161e-06
## g0.T       -2.749647e-03  1.072984e-03 -4.512833e-05  1.009177e-05
## sigma      -1.203079e-03 -4.512833e-05  1.994034e-03 -1.733117e-03
## sigma.SexM  5.815161e-06  1.009177e-05 -1.733117e-03  3.100323e-03
## 
## Fitted (real) parameters evaluated at base levels of covariates 
##        link  estimate SE.estimate       lcl       ucl
## g0    logit 0.1691007  0.01637855 0.1393757 0.2036649
## sigma   log 2.5990217  0.11611616 2.3812217 2.8367428
```

#### Behavioral response    


```r
  secr2
```

```
## 
## secr.fit(capthist = bearCH, model = list(g0 ~ b, sigma ~ Sex), 
##     buffer = 10, CL = TRUE, trace = FALSE)
## secr 2.9.5, 17:47:18 10 Aug 2015
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
## Model           :  g0~b sigma~Sex 
## Fixed (real)    :  none 
## Detection fn    :  halfnormal
## N parameters    :  4 
## Log likelihood  :  -1753.14 
## AIC             :  3514.28 
## AICc            :  3515.332 
## 
## Beta parameters (coefficients) 
##                  beta    SE.beta        lcl        ucl
## g0         -2.0601159 0.14467798 -2.3436796 -1.7765523
## g0.bTRUE    1.0563123 0.14799011  0.7662570  1.3463676
## sigma       0.9351576 0.04124569  0.8543176  1.0159977
## sigma.SexM  0.4943951 0.05272727  0.3910516  0.5977387
## 
## Variance-covariance matrix of beta parameters 
##                       g0      g0.bTRUE         sigma    sigma.SexM
## g0          2.093172e-02 -0.0176505217 -0.0009698759 -8.499418e-05
## g0.bTRUE   -1.765052e-02  0.0219010727 -0.0004578631  1.322172e-04
## sigma      -9.698759e-04 -0.0004578631  0.0017012065 -1.431424e-03
## sigma.SexM -8.499418e-05  0.0001322172 -0.0014314235  2.780165e-03
## 
## Fitted (real) parameters evaluated at base levels of covariates 
##        link  estimate SE.estimate        lcl       ucl
## g0    logit 0.1130342  0.01450505 0.08756947 0.1447294
## sigma   log 2.5476150  0.10512283 2.34977026 2.7621177
```

#### Time specific detection and behavioral response  


```r
  secr3
```

```
## 
## secr.fit(capthist = bearCH, model = list(g0 ~ b + t, sigma ~ 
##     Sex), buffer = 10, CL = TRUE, trace = FALSE)
## secr 2.9.5, 17:52:28 10 Aug 2015
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
## Model           :  g0~b + t sigma~Sex 
## Fixed (real)    :  none 
## Detection fn    :  halfnormal
## N parameters    :  9 
## Log likelihood  :  -1739.065 
## AIC             :  3496.131 
## AICc            :  3501.585 
## 
## Beta parameters (coefficients) 
##                  beta    SE.beta         lcl        ucl
## g0         -2.5034042 0.20449860 -2.90421414 -2.1025943
## g0.bTRUE    0.6962092 0.23501397  0.23559023  1.1568281
## g0.t2       0.9909011 0.26667275  0.46823212  1.5135701
## g0.t3       0.6941049 0.30524959  0.09582672  1.2923831
## g0.t4       1.0807758 0.32298173  0.44774329  1.7138084
## g0.t5       0.8425335 0.32612488  0.20334048  1.4817265
## g0.t6       0.4328228 0.32918930 -0.21237642  1.0780219
## sigma       0.9387331 0.04193858  0.85653498  1.0209312
## sigma.SexM  0.4919806 0.05329331  0.38752765  0.5964336
## 
## Variance-covariance matrix of beta parameters 
##                       g0      g0.bTRUE         g0.t2         g0.t3         g0.t4
## g0          4.181968e-02 -0.0025282315 -0.0371522598 -0.0364564342 -0.0357642893
## g0.bTRUE   -2.528232e-03  0.0552315681 -0.0288987854 -0.0448166999 -0.0527475347
## g0.t2      -3.715226e-02 -0.0288987854  0.0711143554  0.0624173452  0.0665859470
## g0.t3      -3.645643e-02 -0.0448166999  0.0624173452  0.0931773107  0.0818034380
## g0.t4      -3.576429e-02 -0.0527475347  0.0665859470  0.0818034380  0.1043171984
## g0.t5      -3.593606e-02 -0.0534516959  0.0669165571  0.0823368997  0.0900735205
## g0.t6      -3.618335e-02 -0.0530511430  0.0666820449  0.0819880889  0.0896323562
## sigma      -1.062673e-03 -0.0011927098  0.0003330237  0.0007282303  0.0008208827
## sigma.SexM -3.119493e-05  0.0007541858 -0.0002855317 -0.0006581436 -0.0007733562
##                    g0.t5         g0.t6         sigma    sigma.SexM
## g0         -0.0359360563 -0.0361833541 -0.0010626725 -3.119493e-05
## g0.bTRUE   -0.0534516959 -0.0530511430 -0.0011927098  7.541858e-04
## g0.t2       0.0669165571  0.0666820449  0.0003330237 -2.855317e-04
## g0.t3       0.0823368997  0.0819880889  0.0007282303 -6.581436e-04
## g0.t4       0.0900735205  0.0896323562  0.0008208827 -7.733562e-04
## g0.t5       0.1063574389  0.0902884309  0.0009278173 -7.373038e-04
## g0.t6       0.0902884309  0.1083655940  0.0010090406 -7.039741e-04
## sigma       0.0009278173  0.0010090406  0.0017588442 -1.485112e-03
## sigma.SexM -0.0007373038 -0.0007039741 -0.0014851124  2.840177e-03
## 
## Fitted (real) parameters evaluated at base levels of covariates 
##        link   estimate SE.estimate        lcl       ucl
## g0    logit 0.07561987  0.01429476 0.05194564 0.1088449
## sigma   log 2.55674018  0.10727321 2.35498647 2.7757783
```

#### Time trend and behavioral response


```r
   secr4
```

```
## 
## secr.fit(capthist = bearCH, model = list(g0 ~ b + T, sigma ~ 
##     Sex), buffer = 10, CL = TRUE, trace = FALSE)
## secr 2.9.5, 18:44:40 10 Aug 2015
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
## Model           :  g0~b + T sigma~Sex 
## Fixed (real)    :  none 
## Detection fn    :  halfnormal
## N parameters    :  5 
## Log likelihood  :  -1752.099 
## AIC             :  3514.198 
## AICc            :  3515.82 
## 
## Beta parameters (coefficients) 
##                   beta    SE.beta        lcl         ucl
## g0         -2.03911101 0.14609280 -2.3254476 -1.75277438
## g0.bTRUE    1.24899143 0.19982068  0.8573501  1.64063276
## g0.T       -0.06604832 0.04578481 -0.1557849  0.02368826
## sigma       0.93193285 0.04093490  0.8517019  1.01216377
## sigma.SexM  0.49641560 0.05243123  0.3936523  0.59917893
## 
## Variance-covariance matrix of beta parameters 
##                       g0      g0.bTRUE          g0.T         sigma    sigma.SexM
## g0          2.134311e-02 -0.0157997812 -6.885010e-04 -0.0010209951 -6.549599e-05
## g0.bTRUE   -1.579978e-02  0.0399283032 -6.123245e-03 -0.0007871069  3.195423e-04
## g0.T       -6.885010e-04 -0.0061232450  2.096249e-03  0.0001195718 -6.639277e-05
## sigma      -1.020995e-03 -0.0007871069  1.195718e-04  0.0016756658 -1.404030e-03
## sigma.SexM -6.549599e-05  0.0003195423 -6.639277e-05 -0.0014040295  2.749034e-03
## 
## Fitted (real) parameters evaluated at base levels of covariates 
##        link  estimate SE.estimate        lcl       ucl
## g0    logit 0.1151573  0.01488628 0.08903721 0.1476976
## sigma   log 2.5394127  0.10399416 2.34363214 2.7515483
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
## D 12.54934 9.219475 17.08188
```

```r
  desnconv(d1[2,c(1,3,4)]) # point estimate anbd 95% CU **trend in detection model**
```

```
##   estimate      lcl      ucl
## D 12.54087 9.212595 17.07156
```

```r
  desnconv(d2[2,c(1,3,4)]) # point estimate anbd 95% CU **Behav response**
```

```
##   estimate      lcl      ucl
## D 14.23485 10.41369 19.45813
```

```r
  desnconv(d3[2,c(1,3,4)]) # point estimate anbd 95% CU **Time specific detection and behavioral response**
```

```
##   estimate      lcl      ucl
## D 13.60402 9.931035 18.63546
```

```r
  desnconv(d4[2,c(1,3,4)]) # point estimate anbd 95% CU **Time trend and behavioral response**
```

```
##   estimate      lcl      ucl
## D 14.60867 10.66001 20.01999
```

#### Calculate detection functions for the go~t, sigma~sex model



```r
  x<-seq(0,5, length=100)
  lx<-length(x)
  bet<-secr0$fit$par
  g0s<-plogis(rep(bet[1],6)+c(0,bet[2:6]))
  sigs<-c(exp(bet[7]), exp(bet[7]+bet[8]))
  phat<-NULL
  for(i in 1:6){
    pfemale<-g0s[i]*exp(-x^2/(2*sigs[1]))
    pmale<-g0s[i]*exp(-x^2/(2*sigs[2]))
    phat<-rbind(phat, data.frame(x=rep(x,2),sex=rep(c("F","M"),each=lx), p=c(pfemale, pmale), period=as.factor(rep(i,each=2*lx))))  
  }
```

Plot fitted detection functions.  Looks like the "time effect" is much bigger than then sex effect. 


```r
  ggplot(phat, aes(x, p, shape=sex, colour=period)) +geom_line()+facet_wrap(~sex)+xlab("Distance from HR center (km)")+ ylab("Detection Probability")
```

![plot of chunk unnamed-chunk-25](figure/unnamed-chunk-25-1.png) 

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
