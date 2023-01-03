require(gdata)
require(parallel)
require(doParallel)
require(dtw)



#####################

get.dtw.distances<-function(ts, dataset){
  #apply(dataset, 1, function(x) dtw(x, ts)$normalizedDistance)
  apply(dataset, 1, function(x) dtw(x, ts)$distance)
}

dtw.matrix<-function(dataset, numCores=detectCores()){
  
  dtw.distances<-matrix(0, nrow=nrow(dataset), ncol=nrow(dataset))
  
  registerDoParallel(numCores)
  
  out<-foreach(i = 2:nrow(dataset), .combine=c) %dopar% {
    get.dtw.distances(dataset[i,], dataset[1:i,])
  }
  
  stopImplicitCluster()
  
  lowerTriangle(dtw.distances, diag=T, byrow=T)<-c(0,out)
  upperTriangle(dtw.distances) <- NA
  dtw.distances
}