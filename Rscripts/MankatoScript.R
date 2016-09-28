source('~/Google Drive/spatialMR/Rscripts/BearSubParallelSECR.R')

for (s in 4:1000)
  {
testSizes<-seq(950, 250, -100)
    for (x in 1:8){
      #try({BearSubParallelSECR("Spread.one", iterNo=s, size = testSizes[x])})
      try({BearSubParallelSECR("SimpleRandom", iterNo=s, size = testSizes[x])})
    }
  }
