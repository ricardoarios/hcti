library(crqa)
library(gdata)
library(parallel)
library(doParallel)

#####################

delay = 1; embed = 2; rescale = 1; radius = 1;
normalize = 0; mindiagline = 2; minvertline = 2;
tw = 0; 

get.rentr.similarity<-function(ts, dataset){
  apply(dataset, 1, function(x) crqa(x, ts, delay, embed, rescale, radius, 
                                     normalize, mindiagline, minvertline, tw)$rENTR)
}

crqa.matrix<-function(dataset, numCores=detectCores()){
  
  crqa.distances<-matrix(0, nrow=nrow(dataset), ncol=nrow(dataset))
  
  registerDoParallel(numCores)
  
  out<-foreach(i = 2:nrow(dataset), .combine=c) %dopar% {
    get.rentr.similarity(dataset[i,], dataset[1:i,])
  }
  
  stopImplicitCluster()
  
  lowerTriangle(crqa.distances, diag=T, byrow=T)<-c(0,out)
  #upperTriangle(crqa.distances) <- lowerTriangle(crqa.distances, byrow=TRUE)
  upperTriangle(crqa.distances)  <- NA
  crqa.distances
}