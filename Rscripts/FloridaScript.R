source('~/Google Drive/spatialMR/Rscripts/BearSubParallelSECR.R')
for (iter in 150:1000){
  BearSubParallelSECR("Stratified", iterNo=iter)
  BearSubParallelSECR("Spread.one", iterNo=iter)
  BearSubParallelSECR("Spread.two", iterNo=iter)
  BearSubParallelSECR("SimpleRandom", iterNo=iter)
}