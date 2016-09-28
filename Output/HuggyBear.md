## Fit Huggins model using RMark





Load libraries


```r
   library(dplyr) # for data manipulation
   library(RMark) # for fitting models
   library(FSA) # for converting between data types
   options(width=160)
```

Read in data created using "Data Exploration.R" file  


```r
  sampobs<-read.csv("../data/samps1.csv")
```

Gender: Sex = 204.25 -> Male, Sex= 250.25 ->Female


```r
  sampobs$Sex2<-rep("1",nrow(sampobs))
  sampobs$Sex2[sampobs$Sex==250.25]<-0  
```

Create a data set that contains a count of the number of times each bear was seen for each period


```r
  caphist<- sampobs%>%group_by(Individual, Period)%>%
      summarize(Count= n())
```

Convert to Rmark file


```r
  capMark<-capHistConvert(caphist[,c(1,2)], id="Individual", in.type="event", out.type="RMark")
  sexid<-unique(select(sampobs, Individual, Sex2))
  table(sexid$Sex2)
```

```
## 
##  0  1 
## 17 26
```

```r
  capMark2<-merge(capMark, sexid, all=FALSE)  
  capMark3<-capMark2[,c(2,3)]
  names(capMark3)[2]<-"sex"
```

Fit Huggins models (with sex covariate & behavioral response)


```r
run.models=function(data)
{
#
# Define parameter models
#
mod0=list(formula=~time+sex, share=TRUE)
mod1=list(formula=~Time+sex, share=TRUE)
mod2=list(formula=~c+sex, share=TRUE)
mod3=list(formula=~time+c+sex, share=TRUE)
mod4=list(formula=~Time+c+sex, share=TRUE)  

# Run assortment of models
#
#
# Capture Closed models
#
# time + sex
m0<-mark(data,model="Huggins",model.parameters=list(p=mod0))

# Time trend + sex
m1<-mark(data,model="Huggins",model.parameters=list(p=mod1))

# Behavioral response + sex
m2<-mark(data,model="Huggins",model.parameters=list(p=mod2))

# time + Behavioral response + sex
m3<-mark(data,model="Huggins",model.parameters=list(p=mod3))
 
# Time trend + Behavioral response + sex
m4<-mark(data,model="Huggins",model.parameters=list(p=mod4))
return(collect.models())

}
```

Results of models and model averaging 


```r
  results=run.models(data=capMark3)  
```

```
## 
## Output summary for Huggins model
## Name : p(~time + sex)c() 
## 
## Npar :  7
## -2lnL:  344.4843
## AICc :  358.9323
## 
## Beta
##                 estimate        se        lcl       ucl
## p:(Intercept) -0.5896799 0.3613297 -1.2978862 0.1185263
## p:time2        0.8635172 0.4454631 -0.0095905 1.7366250
## p:time3        0.9564107 0.4462907  0.0816810 1.8311405
## p:time4        1.3413508 0.4547746  0.4499926 2.2327091
## p:time5        1.0501931 0.4475954  0.1729061 1.9274800
## p:time6        0.6787909 0.4451688 -0.1937398 1.5513217
## p:sex         -0.2624783 0.2692325 -0.7901741 0.2652174
## 
## 
## Real Parameter p
##          1         2        3         4         5         6
##  0.3211727 0.5287506 0.551819 0.6440447 0.5748838 0.4826078
## 
## 
## Real Parameter c
##          2        3         4         5         6
##  0.5287506 0.551819 0.6440447 0.5748838 0.4826078
## 
## Output summary for Huggins model
## Name : p(~Time + sex)c() 
## 
## Npar :  3
## -2lnL:  354.4803
## AICc :  360.5747
## 
## Beta
##                 estimate        se        lcl       ucl
## p:(Intercept)  0.0872502 0.2612113 -0.4247240 0.5992244
## p:Time         0.0695271 0.0865951 -0.1001994 0.2392536
## p:sex         -0.2492631 0.2638999 -0.7665070 0.2679807
## 
## 
## Real Parameter p
##          1        2         3        4         5         6
##  0.4841386 0.501515 0.5188878 0.536215 0.5534552 0.5705678
## 
## 
## Real Parameter c
##          2        3         4        5         6
##  0.4841386 0.501515 0.5188878 0.536215 0.5534552
## 
## Output summary for Huggins model
## Name : p(~c + sex)c() 
## 
## Npar :  3
## -2lnL:  340.9505
## AICc :  347.045
## 
## Beta
##                 estimate        se        lcl       ucl
## p:(Intercept) -0.5343850 0.3439730 -1.2085722 0.1398021
## p:c            1.1927448 0.3708732  0.4658333 1.9196563
## p:sex         -0.3921986 0.3064557 -0.9928517 0.2084545
## 
## 
## Real Parameter p
##          1         2         3         4         5         6
##  0.3161486 0.3161486 0.3161486 0.3161486 0.3161486 0.3161486
## 
## 
## Real Parameter c
##          2         3         4         5         6
##  0.6037743 0.6037743 0.6037743 0.6037743 0.6037743
## 
## Output summary for Huggins model
## Name : p(~time + c + sex)c() 
## 
## Npar :  8
## -2lnL:  335.2536
## AICc :  351.8319
## 
## Beta
##                 estimate        se        lcl       ucl
## p:(Intercept) -0.8625429 0.6210593 -2.0798191 0.3547333
## p:time2        0.2261521 0.5397531 -0.8317641 1.2840683
## p:time3       -0.2207129 0.7149261 -1.6219680 1.1805423
## p:time4       -0.1876165 0.8340843 -1.8224218 1.4471889
## p:time5       -0.5554870 0.8460808 -2.2138054 1.1028314
## p:time6       -0.9285552 0.8400684 -2.5750894 0.7179790
## p:c            2.0375187 1.2683475 -0.4484423 4.5234798
## p:sex         -0.4961310 0.3566989 -1.1952608 0.2029988
## 
## 
## Real Parameter p
##         1         2         3         4         5         6
##  0.238208 0.2816328 0.2004888 0.2058466 0.1521269 0.1099664
## 
## 
## Real Parameter c
##         2         3         4        5         6
##  0.750474 0.6579736 0.6653823 0.579203 0.4866118
## 
## Output summary for Huggins model
## Name : p(~Time + c + sex)c() 
## 
## Npar :  4
## -2lnL:  336.4998
## AICc :  344.6579
## 
## Beta
##                 estimate        se        lcl        ucl
## p:(Intercept) -1.2030261 1.2880590 -3.7276218  1.3215696
## p:Time        -0.2721739 0.1282173 -0.5234799 -0.0208679
## p:c            2.6352827 1.5730224 -0.4478413  5.7184067
## p:sex         -0.5574543 0.3566816 -1.2565502  0.1416417
## 
## 
## Real Parameter p
##         1         2         3         4         5         6
##  0.176522 0.1403646 0.1106185 0.0865416 0.0673086 0.0521061
## 
## 
## Real Parameter c
##         2         3         4       5         6
##  0.749358 0.6948765 0.6343313 0.56922 0.5016239
```

Pull off N, Se, AICwt and put in table


```r
 modelsum<-function(models){
    mnames=models$model.table$model # Names of models
    names<-names(models) # names of model objects
    nl<-length(names)-1 # number of models fit
    sumnames<-paste(deparse(substitute(models)),names[-(nl+1)], "results", "derived", "'N Population Size'", sep="$")
    SEnames<-paste(sumnames, "se", sep="$")
    Nnames<-paste(sumnames, "estimate", sep="$")
    N<-unlist(sapply(1:nl, FUN=function(x){eval(parse(text=Nnames[x]))}))
    se<-unlist(sapply(1:nl, FUN=function(x){eval(parse(text=SEnames[x]))}))
    wt<-models$model.table$weight
    derived<-data.frame(model=mnames,AIC.wt=wt,N=N,se=se)
    Nhat<-sum(N*wt)
    sea<-sqrt(se^2+(N-Nhat)^2)
    sea<-sum(se*wt)
    derived2<-rbind(derived, data.frame(model="Model Average", AIC.wt=NA,N=Nhat, se=sea))
    derived2
  }
  modelsum(results)
```

```
##                   model       AIC.wt        N         se
## 1 p(~Time + c + sex)c() 0.7507701102 43.52116  0.7611775
## 2        p(~c + sex)c() 0.2275890937 43.50875  0.7572501
## 3 p(~time + c + sex)c() 0.0207814186 48.20319  4.3808124
## 4     p(~time + sex)c() 0.0005968353 59.15993 27.4885726
## 5     p(~Time + sex)c() 0.0002625423 89.67909 89.8663117
## 6         Model Average           NA 43.63709  0.8748506
```

These models seem to be suggesting that capture probabilities
are lower for males than females (opposite of the original spatial mr models I fit!). This lead me to wonder 
if I should also be modeling go~sex in the spatial MR models (detection probability might be lower NEAR the center, 
but bears migth move farther distances).  This would seem to make some sense...see secrall2.html


```r
  data.p<-data.frame(sex=c(0,1))
```

Lets estimate capture probabilities for time 1 


```r
  est=covariate.predictions(results,data=data.p,indices=c(1))
  est
```

```
## $estimates
##   vcv.index model.index par.index covdata  estimate        se        lcl       ucl fixed
## 1         1           1         1       0 0.2639918 0.2109624 0.04095736 0.7507764      
## 2         2           1         1       1 0.1792571 0.1634801 0.02414381 0.6584758      
## 
## $vcv
##            [,1]       [,2]
## [1,] 0.04450514 0.03063782
## [2,] 0.03063782 0.02672576
## 
## $reals
## $reals$`1`
##    estimate         se
## 1 0.3567083 0.08291342
## 2 0.2989803 0.07236296
## 
## $reals$`2`
##    estimate         se
## 1 0.5217987 0.06517871
## 2 0.4595851 0.05719294
## 
## $reals$`3`
##    estimate         se
## 1 0.3694947 0.08013484
## 2 0.2836183 0.07577651
## 
## $reals$`4`
##    estimate        se
## 1 0.2968083 0.1296232
## 2 0.2044559 0.1182113
## 
## $reals$`5`
##    estimate        se
## 1 0.2309373 0.2287661
## 2 0.1467302 0.1704376
```

