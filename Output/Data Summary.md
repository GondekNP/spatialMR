## Create sample history files





Load libraries


```r
   library(dplyr) # for data manipulation
   library(mosaic) # for prop
   options(width=160)
```

Read in data created using "Data Exploration.R" file  


```r
  sampobs<-read.csv("../data/samps1.csv")
```

Gender: Sex = 204.25 -> Male, Sex= 250.25 ->Female


```r
  sampobs$Group<-rep("M",nrow(sampobs))
  sampobs$Group[sampobs$Sex==250.25]<-"F"  
```

Create a data set that contains a count of the number of times each bear was seen for each unique site x period combination.  Also determine
sex of each individual and tabulate the number of males and femlaes


```r
  caphist<- sampobs%>%group_by(Individual, site, Period)%>%
      summarize(Count= n())
  sexid<-unique(select(sampobs, Individual, Group))
  table(sexid$Group)
```

```
## 
##  F  M 
## 17 26
```

```r
  caphist2<-merge(caphist, sexid, all=FALSE)  
```

#### Distribution of counts of the **same** individual at each site x period sampling occassion.  

The largest count was 11 (same bear, same site, same period).   


```r
  table(caphist$Count)
```

```
## 
##   1   2   3   4   5   6   7   8   9  11 
## 237 139  75  29  13   7   4   1   1   1
```

```r
  tally(~Count, data=caphist, format="proportion")
```

```
## 
##           1           2           3           4           5           6           7           8           9          11 
## 0.467455621 0.274161736 0.147928994 0.057199211 0.025641026 0.013806706 0.007889546 0.001972387 0.001972387 0.001972387
```

53% of the time, bears were detected > 1 time at a site x Period combination.   


```r
  prop(~caphist$Count>1) 
```

```
##      TRUE 
## 0.5325444
```

```r
  hist(caphist$Count, xlab="No. Observations per Individual x Site x Period combination", main="")  
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

#### Number of **individuals** detected during each (site x period) combination.  

Report from 2013 says: "Thus 1019 samples (92%) were successfully genotyped; these were from 96 different sites and 333 site-sessions.  I see 346 unique
combinations here [ **update 7/10/2015** after correcting the Period for 3 observations, I now have **347** unique combinations."


```r
  capnums<-caphist%>%group_by(site, Period)%>% summarize(ncap=n() )
  capnums
```

```
## Source: local data frame [347 x 3]
## Groups: site
## 
##    site Period ncap
## 1     1      2    2
## 2     1      4    1
## 3     1      5    1
## 4     2      1    2
## 5     2      2    1
## 6     2      3    1
## 7     2      4    2
## 8     2      5    2
## 9     3      4    1
## 10    4      3    2
## ..  ...    ...  ...
```

```r
  nrow(capnums)  
```

```
## [1] 347
```

  As many as 5 bears were detected at a site during the same sampling period. Most site x sampling period combinations
  with at least 1 detection "trapped" only 1 or 2 bears.


```r
  table(capnums$ncap)
```

```
## 
##   1   2   3   4   5 
## 229  85  26   5   2
```

#### Number of different unique (site x period) combinations that each bear was detected at.  


```r
  bearcap<-caphist%>%group_by(Individual)%>% summarize(ncap=n())
  bearcap
```

```
## Source: local data frame [43 x 2]
## 
##    Individual ncap
## 1        0056   19
## 2    10-86-01   35
## 3   12-106-02   62
## 4     120-4-2    1
## 5   121-22-04    5
## 6    13-27-05   49
## 7  131-103-04    4
## 8  133-116-01    5
## 9  135-108-03    2
## 10    17-2-02   14
## ..        ...  ...
```

Wow! One bear was captured at 62 unique site x period combinations!  Definitely some capture heterogeneity (and, I'd be surprised
if it was all due to movement and placement of "home range center" relative to the trapping grid).


```r
  table(bearcap$ncap)
```

```
## 
##  1  2  3  4  5  6  7  8  9 11 13 14 19 24 35 36 37 41 49 62 
## 10  4  3  4  2  2  3  1  1  1  1  2  1  1  1  1  1  1  2  1
```

```r
  hist(bearcap$ncap, xlab="No. unique sites capturing the same bear", main="")  
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

#### Number of bears captured during each period

Number of unique bears captured during eahc time period 


```r
  timecap<-caphist%>%group_by(Period)%>% summarize(nbears=n_distinct(Individual) )
  timecap  
```

```
## Source: local data frame [6 x 2]
## 
##   Period nbears
## 1      1     14
## 2      2     23
## 3      3     24
## 4      4     28
## 5      5     25
## 6      6     21
```

#### Number of unique bears captured at each site (across the different sampling periods). 



```r
  sitecap<-caphist%>%group_by(site)%>% summarize(nbears=n_distinct(Individual) )
  sitecap  
```

```
## Source: local data frame [100 x 2]
## 
##    site nbears
## 1     1      3
## 2     2      4
## 3     3      1
## 4     4      2
## 5     5      3
## 6     7      1
## 7     8      4
## 8     9      1
## 9    10      4
## 10   11      3
## ..  ...    ...
```

 100 of the sites had at least 1 capture, with one site capturing 7 unique bears.  This doesn't exactly agree with the 96 stated in the 2013
 report, "Thus 1019 samples (92%) were successfully genotyped; these were from 96 different sites and 333 site-sessions."


```r
  nrow(sitecap)
```

```
## [1] 100
```

```r
  table(sitecap$nbears)
```

```
## 
##  1  2  3  4  5  6  7 
## 26 28 21 14  8  2  1
```

```r
  hist(sitecap$nbears, xlab="No. Observations at each site", main="")  
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png) 

Write out file in the format expected by SECR, proximity detector


```r
  bearCH<-data.frame(Session="BearMR", ID=caphist$Individual, Occassion=caphist2$Period, Detector=caphist2$site, Sex=caphist2$Group)
  write.table(bearCH, file="../data/BearCH.csv", row.names=FALSE, col.names=FALSE, sep=",")
```

Write out the original sampobs data, with a line for each observation at a site and period so that we can also use a multi detector.  


```r
  bearCHP<-data.frame(Session="BearMR", ID=sampobs$Individual, Occassion=sampobs$Period, Detector=sampobs$site, Sex=sampobs$Group)
  write.table(bearCHP, file="../data/BearCHP.csv", row.names=FALSE, col.names=FALSE, sep=",")
```

### Other checks

Look to see if summaries here match those in CAPTURE HETEROGENEITY IN HAIR-TRAPPING OF BEARS (Wildlife Research Summary from 2013) 

**Report**: "individual bears were detected up to 132 times each and up to 32 times in a single sampling session."


```r
  max(table(sampobs$Individual))
```

```
## [1] 132
```

```r
  max(table(sampobs$Individual, sampobs$Period))
```

```
## [1] 32
```

 **Report**, "About a third of both males (31%) and females (29%) were detected in only 1 sampling session.
  A similar percent of males (31%) and females (24%) were detected at only 1 site during the study."


```r
  sitetemp<-caphist2%>%group_by(Individual, Group)%>% summarize(nsite=n_distinct(site))
  tally(nsite~Group, data=sitetemp, format="count")
```

```
##      Group
## nsite F M
##    1  4 8
##    2  3 3
##    3  0 2
##    4  3 4
##    5  3 0
##    6  0 1
##    7  0 1
##    9  1 0
##    10 1 1
##    14 0 1
##    15 1 2
##    18 0 1
##    21 0 1
##    23 1 0
##    24 0 1
```

```r
  tally(nsite~Group, data=sitetemp, format="proportion")  
```

```
##      Group
## nsite          F          M
##    1  0.23529412 0.30769231
##    2  0.17647059 0.11538462
##    3  0.00000000 0.07692308
##    4  0.17647059 0.15384615
##    5  0.17647059 0.00000000
##    6  0.00000000 0.03846154
##    7  0.00000000 0.03846154
##    9  0.05882353 0.00000000
##    10 0.05882353 0.03846154
##    14 0.00000000 0.03846154
##    15 0.05882353 0.07692308
##    18 0.00000000 0.03846154
##    21 0.00000000 0.03846154
##    23 0.05882353 0.00000000
##    24 0.00000000 0.03846154
```

```r
  histogram(~nsite|Group, data=sitetemp) # corraborates Figure 4
```

![plot of chunk unnamed-chunk-18](figure/unnamed-chunk-18-1.png) 

Nor for number of time periods  


```r
  ptemp<-caphist2%>%group_by(Individual, Group)%>% summarize(nperiod=n_distinct(Period))
  tally(nperiod~Group, data=ptemp, format="count")
```

```
##        Group
## nperiod F M
##       1 5 8
##       2 1 5
##       3 2 1
##       4 4 6
##       5 2 3
##       6 3 3
```

```r
  tally(nperiod~Group, data=ptemp, format="proportion")  
```

```
##        Group
## nperiod          F          M
##       1 0.29411765 0.30769231
##       2 0.05882353 0.19230769
##       3 0.11764706 0.03846154
##       4 0.23529412 0.23076923
##       5 0.11764706 0.11538462
##       6 0.17647059 0.11538462
```

**Report**:The number of hair traps that yielded bear hair ranged from 30-79 per session (Table 1). Numbers, below, differ a little from Table 1, probably
because I required the hair to be processed OK.



```r
  apply(tally(site~Period, data=sampobs),2, FUN=function(x){sum(x>0)})
```

```
##  1  2  3  4  5  6 
## 22 64 59 75 74 53
```

Lastly, Table 2: number of unqiue bears detected in each Period: checks out


```r
  phist<-caphist2%>%group_by(Period, Individual, Group)%>%summarize(nbears=n_distinct(Individual))
  tally(Period~Group, data=phist, format="count")
```

```
##       Group
## Period  F  M
##      1  7  7
##      2  9 14
##      3  9 15
##      4 12 16
##      5 10 15
##      6 10 11
```

