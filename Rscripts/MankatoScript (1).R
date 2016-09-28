source('~/Google Drive/spatialMR/Rscripts/BearSubParallelSECR.R')

for (s in 80:1000)
{
testSizes<-seq(400, 160, -10)
    for (x in 1:17){
      BearSubParallelSECR("Spread.one", iterNo=s, size = testSizes[x])
      #BearSubParallelSECR("SimpleRandom", iterNo=s, size = testSizes[x])
    }
}