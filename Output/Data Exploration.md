## Data Cleaning/Data Development





Load libraries


```r
   library(dplyr) # for data manipulation
   options(width=160)
```

Purpose:  to read in raw data from Karen's excel spradsheet, "Karen's work Copy of g1288 results.xls", 
look at a few data summaries, and check for errors.

Read in raw data on samples and individuals tab (saved as .csv files)


```r
  ind<-read.csv("../data/Individuals2.csv")
  head(ind)
```

```
##   Individual Sex no_Samples Loci CXX20   G1D  G10M MSUT2  MU50  G10X  G10P REN144.A06   G1A  G10C   D1A  G10J  G10L  CPH9 CXX110  D123  MU26  G10U  MU59
## 1         56   F         65   24 123.1 176.2 214.2 205.2 134.1 133.1 163.2      129.1 194.2 209.2 157.2 207.2 137.2 143.1  155.2 141.1 183.2 179.2 233.2
## 2       1435   F          1   24 123.1 172.2 208.2 205.2 132.1 147.1 163.2      125.1 194.2 215.2 167.2 187.2 137.1 143.1  155.2 143.1 185.2 175.2 233.2
## 3       2213   F        117   24 135.1 174.2 206.2 197.2 120.1 141.1 153.2      121.1 198.2 205.2 157.2 187.2 137.1 143.1  137.2 145.2 185.2 179.2 231.2
## 4       3002   F         26   24 135.1 174.2 206.2 197.2 120.1 141.2 153.2      119.1 196.2 205.2 163.2 203.2 137.1 143.1  137.2 155.2 185.2 177.2 231.2
## 5       3101   M          1   24 123.1 172.2 208.2 197.2 120.1 141.1 151.2      119.1 194.2 215.2 175.2 187.2 137.2 149.2  157.2 141.1 185.2 175.2 233.2
## 6       3103   F          1   24 123.1 172.2 208.2 197.2 120.1 141.1 157.2      119.1 194.2 213.2 167.2 187.2 141.2 143.1  157.2 141.1 185.2 173.2 239.2
##   REN145.P07  G10B  MU23  G10H Sex.1
## 1      157.2 156.2 203.2 239.2     F
## 2      157.2 156.2 187.2 241.3     F
## 3      157.2 156.2 189.2 241.3     F
## 4      157.2 158.2 189.2 241.3     F
## 5      159.2 156.2 187.2 241.2     M
## 6      159.2 156.2 203.2 241.3     F
```

```r
  samps<-read.csv("../data/samples.csv")
  head(samps)
```

```
##       Sample Lab.ID  Class Individual Bundle.No. Period Event.ID Priority Selected Bear.No.  X X.G X.U Left U.L     Comments X.1 Loci   CXX20     G1D    G10M
## 1    120-1-1    885 sample       0056        130      1   120-01        4        1       NA NA      30    A   L 1st envelope  NA    7 123.137 176.186 214.218
## 2 380-120-03    732 sample       0056        380      1   120-03        3        1       NA NA  10  NA    A   L               NA    7 123.137 176.186 214.218
## 3    120-1-6   1004 sample       0056        254      1   120-06        4        2       NA NA  10  NA    A   L 1st envelope  NA    7 123.137 176.186 214.218
## 4 380-120-07    731 sample       0056        380      1   120-07        2        1       NA NA  10  NA    A   L               NA    7 123.137 176.186 214.218
## 5  20-120-09     20 sample       0056         20      1   120-09        2        3       NA NA      30    A   L 1st envelope  NA   15 123.137 176.186 214.218
## 6  20-120-10     87 sample       0056         20      1   120-10        3        1       NA NA  10  NA    A   U               NA   12 123.137 176.186 214.218
##     MSUT2    MU50    G10X    G10P REN144.A06   G1A G10C   D1A    G10J    G10L CPH9 CXX110  D123 MU26 G10U    MU59 REN145.P07 G10B  MU23  G10H   Sex
## 1 205.207 134.136 133.147                 NA    NA   NA    NA                   NA     NA    NA   NA   NA                 NA   NA    NA    NA 250.2
## 2 205.207 134.136 133.147                 NA    NA   NA    NA                   NA     NA    NA   NA   NA                 NA   NA    NA    NA 250.2
## 3 205.207 134.136 133.147                 NA    NA   NA    NA                   NA     NA    NA   NA   NA                 NA   NA    NA    NA 250.2
## 4 205.207 134.136 133.147                 NA    NA   NA    NA                   NA     NA    NA   NA   NA                 NA   NA    NA    NA 250.2
## 5 205.207 134.136 133.147              129.1 194.2   NA 157.2 207.211 137.159   NA     NA 141.1   NA   NA                 NA   NA 203.2 239.2 250.2
## 6 205.207 134.136 133.147 163.163         NA    NA   NA    NA 207.211 137.159   NA     NA    NA   NA   NA 233.237         NA   NA    NA 239.2 250.2
```

Data quality check:  compare number of samples on the "individuals" and "samples" tabs of Karen's Excel file.  This looks good (the two agree!).


```r
  no.samps<-table(samps$Individual)
  dat.no<-data.frame(names(no.samps),no.samps)[,-2]
  alldat<-merge(ind, dat.no, by.x="Individual", by.y="names.no.samps.", all=TRUE )
  summary(alldat$Freq- alldat$no_Samples) # Looks good
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##       0       0       0       0       0       0       3
```

Look at event IDs & Periods. Use strsplit to determine the site associated with each sample 


```r
   events<-unlist(strsplit(as.character(samps$Event.ID), "-"))
   samps$site<-as.numeric(sapply(strsplit(as.character(samps$Event.ID), "-"), `[`, 1))
   samps$cid<-as.numeric(sapply(strsplit(as.character(samps$Event.ID), "-"), `[`, 2))
```

```
## Warning: NAs introduced by coercion
```

```r
   table(samps$site)
```

```
## 
##   1   2   3   4   5   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  32  33  34  35  36  38  39  40  41  42  45  46 
##  11  18   2  14  15   2  10   1  27  21   1   1  28  29  28   8  18   1   7  20  29  13   9   7  33  25  17  13   4  25  34  18  26   6   8   1  12  16  18  35 
##  48  49  50  51  52  53  54  55  56  57  59  60  61  62  63  64  66  67  69  70  71  72  73  74  75  77  78  80  81  82  83  84  85  86  87  88  89  90  91  92 
##   9   6  23   4   8   8   1  21  22   7  10  30   4  29   1  11  12  20  17   1  26  20  12   9  26  11   7   1   4   2  21   6  11  20  17  12  20  20  16  17 
##  93  94  95  96  98  99 100 101 102 103 104 105 106 107 108 109 113 114 115 116 117 118 119 120 121 
##  10   7   5  21  14  15  24  10   7  18  10  12  22   4  12   9   9  23   5   1   6   2   8  38  18
```

```r
  # samps$Per.check<-substr(samps$cid,1,1) # thought might be period, but it is not
  # table(samps$Per.Check, samps$Period)
```

Note, there are several observations that are missing the sample Period.  A few of these appear to be samples
of collared bears (these should be dropped, correct?).  Many of the missing observations have 
Class = Xsubselect (not sent to lab?).  Two correspond to sampled observations with a non-missing Priority, so these 
probably(?) represent data entry errors that should be fixed.

I will write all observations that are missing a value for Period out ot a file for later checking.  The code, below, also highlights
three observations that should probably be given higher priority for checking.


```r
  filter(samps, Period=="", Lab.ID%in%c(26, 93, 1031))[,c(1:9, 16,18, 42, 43)]  
```

```
##     Sample Lab.ID  Class Individual Bundle.No. Period Event.ID Priority Selected     Comments Loci   Sex site
## 1 26-67-01     26 sample   25-55-01         26           67-01        1        1 1st envelope   12 204.2   67
## 2 26-33-04     93 sample    7-34-11         26           33-04        2        1                 9 204.2   33
## 3 26-46-09   1031  Xbomb                    26           46-09        3        2                 6 204.2   46
```

```r
  miss.period<-filter(samps, Period=="")[,c(1:9, 16,18, 42, 43)]
  write.csv(miss.period, file="../data/MissingPeriod.csv", row.names=FALSE) 
```

Update:  7/10/2015:  using a file named "Subsampling Minnesota bear hair_boxes 1 and 2.xls" file in directory, 
C:\Users\jfieberg\Documents\MNDNR\Grand Rapids\Bear Group\DNA hair sampling\draft instructions plan, we were able to determine the period for
3 of these observations.  The code, below, will impute these values.


```r
  samps$Period[samps$Event.ID=="67-01" & samps$Period==""]<-1
  samps$Period[samps$Event.ID=="33-04" & samps$Period==""]<-5
  samps$Period[samps$Event.ID=="46-09" & samps$Period==""]<-3
```

Lets look at the variable labeled **Class**.  I'm not sure what all of these categories are, but I will take a guess, below:

- sample (sent to lab & processed without any problems)?
- xbomb (not sure) are these samples that were difficult to extract DNA from?
- Xinadequate (sent to lab, but sample was not adequate)
- Xmixed (sent to lab, processed, contained a mixture of hair from different individuals)
- Xnotsent (not sent, we'll see why, below)
- Xspecies (sent to lab, processed, but not black bear hair)
- Xsubselect (low priority, ran out of money before could process)

Number of each type


```r
  table(samps$Class)
```

```
## 
##                  sample       Xbomb Xinadequate      Xmixed    Xnotsent    Xspecies  Xsubselect 
##           1        1026          80         112          14           4           9         396
```

Lets look at a few of these categories.  

**Xbomb**:  **Individual** is missing for all of the Xbomb observations, and priority is as high as 1. So,
my guess is that an attempt was made to process these samples, but they were problematic in some way.


```r
  filter(samps, Class=="Xbomb")[,c(1:9, 16,18)]  
```

```
##        Sample Lab.ID Class Individual Bundle.No. Period Event.ID Priority Selected                                         Comments Loci
## 1   23-100-10     92 Xbomb                    23      1   100-10        3        1                                                     1
## 2   331-14-03    638 Xbomb                   331      1    14-03        2       NA                                                     6
## 3     3-14-06   1099 Xbomb                     3      1    14-06        3        3                                     2nd envelope    3
## 4     4-15-01      4 Xbomb                     4      1    15-01        2       NA                                                     1
## 5    15-22-01     15 Xbomb                    15      1    22-01        1        1               maybe not bear? uf brown and white    2
## 6     5-26-18     70 Xbomb                     5      1    26-18        3        1                                                     3
## 7     9-39-01      9 Xbomb                     9      1    39-01        1        1                                                     0
## 8     8-40-01      8 Xbomb                     8      1    40-01        1        1                                                     0
## 9     7-52-02      7 Xbomb                     7      1    52-02        1        1                                  maybe not bear?    0
## 10   21-70-01     21 Xbomb                    21      1    70-01        1        1               maybe not bear? roots looked weird    1
## 11    2-71-02   1029 Xbomb                     2      1    71-02        3        2                                       dirty hair    0
## 12   22-80-01     22 Xbomb                    22      1    80-01        1        1                                                     3
## 13   57-88-01   1101 Xbomb                    57      1    88-01        2        3                                                     2
## 14  382-88-05    736 Xbomb                   382      1    88-05        1        1                                     1st envelope    1
## 15   27-88-06     27 Xbomb                    27      1    88-06        3        1                                                     1
## 16     16-1-1    794 Xbomb                    33      1                 4        1                                     1st envelope    2
## 17     26-1-1    883 Xbomb                   128      1                 4        1                                                     3
## 18     26-1-2    944 Xbomb                   193      1                 4        1                                     1st envelope    2
## 19    26-1-22    968 Xbomb                   217      1                 4        1                                     1st envelope    2
## 20     26-1-5    985 Xbomb                   235      1                 4        2                                     1st envelope    6
## 21  73-107-01    156 Xbomb                    73      2   107-01        1        1 blonde underfur, kind of weird, not sure if bear    2
## 22  67-114-03    145 Xbomb                    67      2   114-03        3        1                               roots looked weird    1
## 23  72-119-06    155 Xbomb                    72      2   119-06        3        1                                     1st envelope    2
## 24  384-36-03    740 Xbomb                   384      2    36-03        3        1                                     1st envelope    3
## 25   37-45-01     37 Xbomb                    37      2    45-01        1        1                                                     1
## 26   55-49-02    126 Xbomb                    55      2    49-02        2        1                                     1st envelope    4
## 27   48-52-01     48 Xbomb                    48      2    52-01        1        1                                                     1
## 28   54-62-02    125 Xbomb                    54      2    62-02        3        1                                     1st envelope    6
## 29   91-90-04    196 Xbomb                    91      2    90-04        1        1                                                     0
## 30    119-2-1    803 Xbomb                    43      2                 4        1                                                     6
## 31    26-2-10    830 Xbomb                    70      2                 4        1                                     1st envelope    1
## 32     77-2-2    921 Xbomb                   168      2                 4        1                                                     2
## 33     84-2-2    978 Xbomb                   228      2                 4        1                                                     2
## 34    108-2-4    986 Xbomb                   236      2                 4        2                                                     2
## 35    26-2-11   1022 Xbomb                   273      2                 4        2                    coarse underfur, 1st envelope    0
## 36 131-103-01    276 Xbomb                   131      3   103-01        3        1                                                     1
## 37 131-103-02    275 Xbomb                   131      3   103-02        2        1                                 not sure if bear    1
## 38 130-114-02    272 Xbomb                   130      3   114-02        2        1                                                     5
## 39  127-17-02    268 Xbomb                   127      3    17-02        1        1                                                     6
## 40  111-19-01    234 Xbomb                   111      3    19-01        1        1                                                     3
## 41  117-50-06   1046 Xbomb                   117      3    50-06        3        2                                                     3
## 42  155-77-01    319 Xbomb                   155      3    77-01        2        1                                     1st envelope    1
## 43     92-3-2    818 Xbomb                    58      3                 4        1                                                     0
## 44     29-3-2    888 Xbomb                   133      3                 4        1                                                     2
## 45     14-3-7   1003 Xbomb                   253      3                 4        2                                                     2
## 46 203-113-02    408 Xbomb                   203      4   113-02        1        1                                                     5
## 47 202-120-04    407 Xbomb                   202      4   120-04        3        1                                                     1
## 48  194-16-02   1058 Xbomb                   194      4    16-02        2        2                                                     6
## 49  166-45-01    337 Xbomb                   166      4    45-01        2        2                                                     1
## 50  182-51-01    371 Xbomb                   182      4    51-01        2        1                                                     1
## 51  179-52-04    366 Xbomb                   179      4    52-04        2        1                                  maybe not bear?    2
## 52  160-57-01    327 Xbomb                   160      4    57-01        1        1                                                     0
## 53  160-57-03    328 Xbomb                   160      4    57-03        3        1                                  maybe not bear?    2
## 54  218-93-05    435 Xbomb                   218      4    93-05        3        1                                                     3
## 55     86-4-7    782 Xbomb                    21      4                 4        1                                                     2
## 56     28-4-5    788 Xbomb                    27      4                 4        1                                                     3
## 57     36-4-4    853 Xbomb                    95      4                 4        1                       coarse underfur, not bear?    2
## 58  330-22-04    636 Xbomb                   330      5    22-04        2       NA                                                     4
## 59  248-36-02    494 Xbomb                   248      5    36-02        1        1                                                     2
## 60  257-39-05    509 Xbomb                   257      5    39-05        3        1                                                     1
## 61  305-42-02    595 Xbomb                   305      5    42-02        3        1                                                     0
## 62  243-45-04    484 Xbomb                   243      5    45-04        1        1                                                     1
## 63  265-50-01    524 Xbomb                   265      5    50-01        1        1                                                     1
## 64  268-62-04    528 Xbomb                   268      5    62-04        1        1                                                     3
## 65  241-71-05   1111 Xbomb                   241      5    71-05        3        3                                  maybe not bear?    2
## 66  297-82-01    580 Xbomb                   297      5    82-01        1        1                                     2nd envelope    2
## 67  256-87-01    507 Xbomb                   256      5    87-01        2        1                                                     4
## 68     56-5-9    822 Xbomb                    62      5                 4        1                                     2nd envelope    1
## 69     50-5-8    933 Xbomb                   181      5                 4        1                                                     6
## 70     71-5-1   1017 Xbomb                   268      5                 4        2                                  coarse underfur    6
## 71  344-16-01    667 Xbomb                   344      6    16-01        1        1                                                     0
## 72  321-34-02    622 Xbomb                   321      6    34-02        2        1                                                     4
## 73  314-45-02    607 Xbomb                   314      6    45-02        3        1                                                     6
## 74  336-50-03    649 Xbomb                   336      6    50-03        1        1                                                     2
## 75  318-59-01    614 Xbomb                   318      6    59-01        1        1                                                     1
## 76  315-69-01    608 Xbomb                   315      6    69-01        1        1                                                     0
## 77  338-75-04    654 Xbomb                   338      6    75-04        3        1                                                     1
## 78   349-8-01    673 Xbomb                   349      6     8-01        1        1                                                     3
## 79  337-86-03   1086 Xbomb                   337      6    86-03        2        2                                                     6
## 80   26-46-09   1031 Xbomb                    26      3    46-09        3        2                                                     6
```

**Xinadequate**: similar story...not sure how this category differs from **xbomb** though?


```r
  filter(samps, Class=="Xinadequate")[,c(1:9, 16,18)]  
```

```
##         Sample Lab.ID       Class Individual Bundle.No. Period Event.ID Priority Selected                              Comments Loci
## 1    23-100-02     NA Xinadequate                    23      1   100-02        2        1                     no roots on hairs    0
## 2    19-113-01     NA Xinadequate                    19      1   113-01        1        1                              no roots    0
## 3    20-120-04     NA Xinadequate                    20      1   120-04        1        1                                 <5 uf    0
## 4      4-15-02     NA Xinadequate                     4      1    15-02        1        1                           just shafts    0
## 5     14-16-03     NA Xinadequate                    14      1    16-03        1        1                     no roots on hairs    0
## 6    378-71-07     NA Xinadequate                   378      1    71-07        2        1  neither envelope had sufficient hair    0
## 7      18-8-02     NA Xinadequate                    18      1     8-02        3        1               2 hairs - dubious roots    0
## 8      6-87-03     NA Xinadequate                     6      1    87-03        1        1                     only one uf  root    0
## 9     27-88-02     NA Xinadequate                    27      1    88-02        2        1                                          0
## 10   382-88-03     NA Xinadequate                   382      1    88-03        2        1                              no roots    0
## 11    27-88-04     NA Xinadequate                    27      1    88-04        1        1                <5 uf in each envelope    0
## 12    30-91-02     NA Xinadequate                    30      1    91-02        2        2                               < 20 uf    0
## 13      4-2-04     NA Xinadequate                     4      2     2-04        3        1                                  2 gh    0
## 14    41-26-01     NA Xinadequate                    41      2    26-01        2        1                     no roots on hairs    0
## 15    39-36-07     NA Xinadequate                    39      2    36-07        2        1                                <20 uf    0
## 16    79-84-05     NA Xinadequate                    79      2    84-05        1        1                     no roots on hairs    0
## 17    70-99-02     NA Xinadequate                    70      2    99-02        1        1                                  1 gh    0
## 18    122-1-01     NA Xinadequate                   122      3     1-01        1        1                                  5 uf    0
## 19  135-108-05     NA Xinadequate                   135      3   108-05        2        1                     no roots on hairs    0
## 20  137-109-06     NA Xinadequate                   137      3   109-06        1        1                                <20 uf    0
## 21   129-11-06     NA Xinadequate                   129      3    11-06        1        1                                  1 gh    0
## 22   102-26-01     NA Xinadequate                   102      3    26-01        1        1                                1 hair    0
## 23   112-29-04     NA Xinadequate                   112      3    29-04        2        2                             < 5 hairs    0
## 24   150-42-01     NA Xinadequate                   150      3    42-01        2       NA      both envelopes had too few hairs    0
## 25   150-42-03     NA Xinadequate                   150      3    42-03        1        1                     no roots on hairs    0
## 26   151-56-02     NA Xinadequate                   151      3    56-02        2        2                      not enough hairs    0
## 27   151-56-07     NA Xinadequate                   151      3    56-07        3       NA                     no roots on hairs    0
## 28   151-56-08     NA Xinadequate                   151      3    56-08        1        1      both envelopes had too few hairs    0
## 29   142-61-01     NA Xinadequate                   142      3    61-01        1        1                          1 guard hair    0
## 30   149-72-01     NA Xinadequate                   149      3    72-01        1        1                                1 hair    0
## 31   143-73-01     NA Xinadequate                   143      3    73-01        1        1                                 ~5 uf    0
## 32   155-77-02     NA Xinadequate                   155      3    77-02        1        1                                 <5 uf    0
## 33   351-83-03     NA Xinadequate                   351      3    83-03        3        2                                1 root    0
## 34   148-83-06     NA Xinadequate                   148      3    83-06        3        1                                 <20uf    0
## 35   148-83-07     NA Xinadequate                   148      3    83-07        1        1              both envelopes had <20uf    0
## 36   390-83-10     NA Xinadequate                   390      3    83-10        1        1  neither envelope has sufficient hair    0
## 37   145-84-01     NA Xinadequate                   145      3    84-01        1        1           neither envelope had enough    0
## 38   312-85-02     NA Xinadequate                   312      3    85-02        3        2                                <20 uf    0
## 39   147-94-01     NA Xinadequate                   147      3    94-01        2        1   only 3 hairs, probably Xspecies too    0
## 40      55-3-8     NA Xinadequate                   221      3                 4        1                               2 roots    0
## 41  207-106-09     NA Xinadequate                   207      4   106-09        2        1                                 <5 uf    0
## 42  215-117-01     NA Xinadequate                   215      4   117-01        1        1                                          0
## 43  215-117-03     NA Xinadequate                   215      4   117-03        2        1                                          0
## 44  212-119-05     NA Xinadequate                   212      4   119-05        2        1                               3 hairs    0
## 45  281-120-06     NA Xinadequate                   281      4   120-06        3        1                                1 hair    0
## 46   48-120-07     NA Xinadequate                    48      4   120-07        3        3                                <20 uf    0
## 47   188-18-02     NA Xinadequate                   188      4    18-02        2        1          envelope appears to be empty    0
## 48   190-28-01     NA Xinadequate                   190      4    28-01        2        2                                 <20uf    0
## 49   166-45-06     NA Xinadequate                   166      4    45-06        1        1                              <5 hairs    0
## 50   181-50-03     NA Xinadequate                   181      4    50-03        3        1                              <5 hairs    0
## 51   181-50-06     NA Xinadequate                   181      4    50-06        1        1                               2 hairs    0
## 52   182-51-02     NA Xinadequate                   182      4    51-02        1        1                               2 hairs    0
## 53   227-56-02     NA Xinadequate                   227      4    56-02        1        1                    2 hairs with roots    0
## 54   183-62-01     NA Xinadequate                   183      4    62-01        1        1                                 <20uf    0
## 55   184-63-01     NA Xinadequate                   184      4    63-01        1        1 both samples didn’t have enough hairs    0
## 56   159-69-01     NA Xinadequate                   159      4    69-01        1        1                         too few hairs    0
## 57   159-69-02     NA Xinadequate                   159      4    69-02        3       NA                         too few hairs    0
## 58   159-69-04     NA Xinadequate                   159      4    69-04        2        1                         too few hairs    0
## 59   111-72-03     NA Xinadequate                   111      4    72-03        2        2            <5 hairs in both envelopes    0
## 60   185-74-01     NA Xinadequate                   185      4    74-01        1        1                only a couple of hairs    0
## 61   186-75-02     NA Xinadequate                   186      4    75-02        3        1                               2 hairs    0
## 62   178-87-01     NA Xinadequate                   178      4    87-01        2        1                                  3 gh    0
## 63   225-94-02     NA Xinadequate                   225      4    94-02        1        1                                 <5 uf    0
## 64   226-95-01     NA Xinadequate                   226      4    95-01        1        1                              <5 hairs    0
## 65      86-4-5     NA Xinadequate                     8      4                 4        1                                1 hair    0
## 66      75-4-8     NA Xinadequate                    19      4                 4        1                              <5 hairs    0
## 67     106-4-7     NA Xinadequate                    84      4                 4        1                               3 roots    0
## 68     119-4-6     NA Xinadequate                    98      4                 4        1                               3 hairs    0
## 69      62-4-7     NA Xinadequate                    99      4                 4        1                              <5 hairs    0
## 70      26-4-1     NA Xinadequate                   112      4                 4        1                              <5 hairs    0
## 71      27-4-1     NA Xinadequate                   142      4                 4        1                                1 hair    0
## 72     120-4-8     NA Xinadequate                   169      4                 4        1                                <20 uf    0
## 73      52-4-2     NA Xinadequate                   264      4                 4        2                                <20 uf    0
## 74  284-108-02     NA Xinadequate                   284      5   108-02        2        2                               2 hairs    0
## 75   255-16-03     NA Xinadequate                   255      5    16-03        1        1                               2 hairs    0
## 76   254-18-01     NA Xinadequate                   254      5    18-01        2        1                               3 hairs    0
## 77   262-27-01     NA Xinadequate                   262      5    27-01        1        1                                <20 uf    0
## 78   262-27-02     NA Xinadequate                   262      5    27-02        2        2                               3 roots    0
## 79   258-29-02     NA Xinadequate                   258      5    29-02        2        1                               2 roots    0
## 80   257-39-01     NA Xinadequate                   257      5    39-01        1        1                                  3 uf    0
## 81   302-41-04     NA Xinadequate                   302      5    41-04        1        1            < 5 uf envelope says 41-14    0
## 82    35-56-04     NA Xinadequate                    35      5    56-04        3        1                                <5 uf     0
## 83   311-56-08     NA Xinadequate                   311      5    56-08        1        1                                  <5uf    0
## 84   268-62-01     NA Xinadequate                   268      5    62-01        2        1                             < 5 hairs    0
## 85   313-77-01     NA Xinadequate                   313      5    77-01        2        1                                  <5uf    0
## 86   313-77-02     NA Xinadequate                   313      5    77-02        1        1                                  <5uf    0
## 87   266-86-01     NA Xinadequate                   266      5    86-01        1        1                                  <5uf    0
## 88   256-87-02     NA Xinadequate                   256      5    87-02        3        1                                  5 uf    0
## 89   312-88-01     NA Xinadequate                   312      5    88-01        1        1   less than 5 roots in both envelopes    0
## 90   307-89-02     NA Xinadequate                   307      5    89-02        2        1                                  <5uf    0
## 91   307-89-04     NA Xinadequate                   307      5    89-04        3        1                               2 roots    0
## 92   306-91-04     NA Xinadequate                   306      5    91-04        1        1                                  <5uf    0
## 93   290-93-02     NA Xinadequate                   290      5    93-02        1        1                                 <5 uf    0
## 94   283-98-03     NA Xinadequate                   283      5    98-03        1        1                               2 roots    0
## 95      16-5-1     NA Xinadequate                    38      5                 4        1                               3 roots    0
## 96      89-5-3     NA Xinadequate                    74      5                 4        1                                <20 uf    0
## 97      56-5-2     NA Xinadequate                   152      5                 4        1                                          0
## 98      67-5-3     NA Xinadequate                   192      5                 4        1                               < 20 uf    0
## 99    348-1-01     NA Xinadequate                   348      6     1-01        1        1                               2 roots    0
## 100 377-120-02     NA Xinadequate                   377      6   120-02        2        1                                1 root    0
## 101  343-17-03     NA Xinadequate                   343      6    17-03        2        1                               3 hairs    0
## 102  345-18-01     NA Xinadequate                   345      6    18-01        1        1                             1 gh 5 uf    0
## 103  345-18-02     NA Xinadequate                   345      6    18-02        2        1                                 <5 uf    0
## 104  329-39-01     NA Xinadequate                   329      6    39-01        1        1                               3 hairs    0
## 105  351-55-01     NA Xinadequate                   351      6    55-01        1        1                               2 hairs    0
## 106  357-66-01     NA Xinadequate                   357      6    66-01        2        1                               3 hairs    0
## 107  366-73-01     NA Xinadequate                   366      6    73-01        1        1                               4 hairs    0
## 108  366-73-03     NA Xinadequate                   366      6    73-03        3        1           both envelopes had <4 hairs    0
## 109  339-74-01     NA Xinadequate                   339      6    74-01        1        1                               3 hairs    0
## 110  330-87-01     NA Xinadequate                   330      6    87-01        1        1                                 <10uf    0
## 111  354-89-01     NA Xinadequate                   354      6    89-01        1        1                               2 hairs    0
## 112  368-93-03     NA Xinadequate                   368      6    93-03        1        1                              <5 hairs    0
```

**Xmixed**:  similar story...


```r
  filter(samps, Class=="Xmixed")[,c(1:9, 16,18)]  
```

```
##        Sample Lab.ID  Class Individual Bundle.No. Period Event.ID Priority Selected                                                              Comments Loci
## 1    50-50-02    123 Xmixed                    50      2    50-02        3        1                                                                          2
## 2    54-62-05     54 Xmixed                    54      2    62-05        1        1                                                     kinky guard hairs    1
## 3  138-106-04    290 Xmixed                   138      3   106-04        3        1 different ID in second spreadsheet - (10-4) - envelope actually 106-4    3
## 4  207-106-10    416 Xmixed                   207      4   106-10        1        1                                                                          1
## 5   145-60-12    302 Xmixed                   145      4    60-12        2        1                                                                          2
## 6   177-64-01    362 Xmixed                   177      4    64-01        2        1                                                                          0
## 7      34-4-6    798 Xmixed                    37      4                 4        1                                                                          1
## 8      86-4-8    826 Xmixed                    66      4                 4        1                                                          1st envelope    0
## 9   246-14-02    489 Xmixed                   246      5    14-02        1        1                                                                          2
## 10  300-21-05    587 Xmixed                   300      5    21-05        3        1                                                                          0
## 11  348-50-03    671 Xmixed                   348      5    50-03        2       NA                                                                          0
## 12 377-120-05    728 Xmixed                   377      6   120-05        1        1                                                          1st envelope    1
## 13  334-27-02    645 Xmixed                   334      6    27-02        2        1                                                                          0
## 14  357-66-02    690 Xmixed                   357      6    66-02        1        1                                                                          1
```

**Xnotsent**:  Looks like these observations correspond to mislabeled / misbundled envelopes.  Only 4 of these. 


```r
  filter(samps, Class=="Xnotsent")[,c(1:9, 16,18)]  
```

```
##      Sample Lab.ID    Class Individual Bundle.No. Period Event.ID Priority Selected                                                      Comments Loci
## 1 68-121-02     NA Xnotsent                    68      2   121-02        1        1                        no envelope with this number in bundle    0
## 2 354-89-02     NA Xnotsent                   354      3    89-02        2        1                                           not found in bundle    0
## 3 393-33-02     NA Xnotsent                   393      4    33-02        3        1 envelope actually in bundle - 33-03 (should be in bundle 244)    0
## 4 326-25-04     NA Xnotsent                   326      6    25-04        2        1    This sample not included in bundle (25-01 present instead)    0
```

**Xspecies**:  hair likely from different species


```r
  filter(samps, Class=="Xspecies")[,c(1:9, 16,18)]  
```

```
##      Sample Lab.ID    Class Individual Bundle.No. Period Event.ID Priority Selected                                           Comments Loci
## 1   7-52-01     NA Xspecies                     7      1    52-01        2        1                              not bear, banded hair    0
## 2  24-66-04     NA Xspecies                    24      1    66-04        3        1                      banded hairs, all 3 envelopes    0
## 3   16-7-01     NA Xspecies                    16      1     7-01        1        1 samples appeared to be ungulate hair (4 envelopes)    0
## 4 104-25-03     NA Xspecies                   104      3    25-03        1        1                                        banded hair    0
## 5 119-27-03     NA Xspecies                   119      3    27-03        3        1                                        banded hair    0
## 6 110-39-01     NA Xspecies                   110      3    39-01        1        1                                        banded hair    0
## 7 109-52-01     NA Xspecies                   109      3    52-01        1        1                                        banded hair    0
## 8 263-49-01     NA Xspecies                   263      5    49-01        3        1                                        banded hair    0
## 9 331-52-01     NA Xspecies                   331      6    52-01        1        1                                       banded hairs    0
```

**Xsubselect**:  some of these are high priority (so, not sure why they fell to end of list? maybe they were alternates for other
samples collected at the same site & period?). Show only the first 10 observations.


```r
  filter(samps, Class=="Xsubselect")[1:10,c(1:9, 16)]  
```

```
##        Sample Lab.ID      Class Individual Bundle.No. Period Event.ID Priority Selected Comments
## 1  308-100-01     NA Xsubselect                   308      1   100-01        3       NA         
## 2  380-120-13     NA Xsubselect                   380      1   120-13        1       NA         
## 3   373-16-02     NA Xsubselect                   373      1    16-02        3       NA         
## 4    15-22-03     NA Xsubselect                    15      1    22-03        2       NA         
## 5     5-26-24     NA Xsubselect                     5      1    26-24        2       NA         
## 6   379-62-02     NA Xsubselect                   379      1    62-02        1       NA         
## 7   379-62-03     NA Xsubselect                   379      1    62-03        2       NA         
## 8     1-69-03     NA Xsubselect                     1      1    69-03        2       NA         
## 9   249-71-03     NA Xsubselect                   249      1    71-03        2       NA         
## 10  378-71-04     NA Xsubselect                   378      1    71-04        3       NA
```

### Subsetting of the data   

At this point, I think it makes to subset the data, creating 4 separate data sets:

 1. Samples that were processed without any problems (Class = sample).  
 2. Samples that were sent, but turned out to be problematic (Class = Xbomb, Xinadequate, Xmixed)
 3. Samples that likely did not represent bear hair (Class = Xnotsent, Xspecies)
 4. Samples that were not processed (Xsubselect)
 
 We will focus mostly on [1] going forward, but the other data sets may still prove useful, particularly if /
 when we try to construct a reasonable simulation model.


```r
  sampobs<-filter(samps, Class=="sample")
  samp.prob<-filter(samps, Class%in%c("Xbomb", "Xinadequate", "Xmixed"))
  samp.nobhair<-filter(samps, Class%in%c("Xnotsent", "Xspecies"))
  samp.noprocess<-filter(samps, Class=="Xsubselect")
```

### Further exploration of Sampled Observations
   
At least 2 observations with Period = "?6".  Look at these:


```r
  filter(sampobs, Period=="6?")
```

```
##      Sample Lab.ID  Class Individual Bundle.No. Period Event.ID Priority Selected Bear.No.  X X.G X.U Left U.L     Comments X.1 Loci   CXX20     G1D    G10M
## 1 365-85-01    704 sample   10-86-01        365     6?    85-01        1        1       NA NA   2  20    C   L               NA    7 123.135 176.176 208.214
## 2 364-96-03    702 sample   31-60-03        364     6?    96-03        1        1       NA NA  10  NA    A   L 1st envelope  NA    7 141.145 186.186 206.218
##     MSUT2    MU50    G10X G10P REN144.A06 G1A G10C D1A G10J G10L CPH9 CXX110 D123 MU26 G10U MU59 REN145.P07 G10B MU23 G10H   Sex site cid
## 1 197.207 130.138 141.151              NA  NA   NA  NA             NA     NA   NA   NA   NA              NA   NA   NA   NA 204.2   85   1
## 2 197.201 134.140 151.153              NA  NA   NA  NA             NA     NA   NA   NA   NA              NA   NA   NA   NA 204.2   96   3
```

Fix these, then turn this variable into a numeric variable


```r
  sampobs$Period[sampobs$Period=="6?"]<-"6"
```

For now, drop observations that are missing Period.  Eventually, however, we will want to reconcile two of these 
observations that were highlighted earlier. The remaining 7 of these 9 observations correspond to observations from collared bears. 


```r
  sampobs2<-filter(sampobs, Period!="")
```

Now, turn Period into a numeric variable  


```r
  sampobs2$Period<-droplevels(sampobs2$Period)  
  sampobs2$Period<-as.numeric(sampobs2$Period)
  table(sampobs2$Period)
```

```
## 
##   1   2   3   4   5   6 
##  78 209 175 235 182 140
```

Drop several of the columns that we will not need. 


```r
  sampobs3<-sampobs2[,c(1:9, 16,18,42:44)] 
```

Write this file out for later use


```r
   write.csv(sampobs3, file="../data/samps1.csv", row.names=FALSE)  
```

