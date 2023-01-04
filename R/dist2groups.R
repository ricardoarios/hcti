#' Function used to calculate the distance between each time series and
#'    the centroids of all clusters
#'
#' This code is part of the HCTI package
#' Date: 2020/2021
#'
#' Developer: Ricardo Rios
#'
#' GNU General Public License v3.0
#'
#' Permissions of this strong copyleft license are
#'    conditioned on making available complete
#'    source code of licensed works and
#'    modifications, which include larger works
#'    using a licensed work, under the same license.
#'    Copyright and license notices must be
#'    preserved. Contributors provide an express
#'    grant of patent rights.
#'
#' @param dataset a set of time series
#' @param clust a vector containing the cluster index for each time series
#' @return the distance of each time series to each centroid
#' @examples
#' calculateDistances(dataset, clust)
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
