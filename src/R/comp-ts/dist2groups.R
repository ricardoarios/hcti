calculateDistances<-function(dataset, clust){
  num.group<-(clust %>% unique())
  resp<-matrix(nrow=nrow(dataset), ncol=(length(num.group)+1))
  resp[,1]<-clust
  centroids<-list()
  for(c in 1:length(num.group)){
    index<-which(clust == c)
    if(length(index) > 1){
      centroids[[c]]<-apply(dataset[index,], 2, mean)
    }else{
      centroids[[c]]<-dataset[index,]
    }
  }
  
  for(i in 1:nrow(dataset)){
    for(j in 1:length(centroids)){
      resp[i, j+1]<-dtw(dataset[i, ], centroids[[j]])$distance
    }
  }
  
  resp
}