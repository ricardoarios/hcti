require(gdata)
require(parallel)
require(doParallel)
require(dtw)

#' Function used to get the DTW distance
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
#' @param ts A single time series
#' @param dataset a set of time series
#' @param normalized if TRUE, then the normalized DTW is calculated
#' @examples
#' # calculating the DTW distance between the i-th time series and others
#' get.dtw.distances(dataset[i,], dataset[1:i,])
get.dtw.distances<-function(ts, dataset, normalized=FALSE){
  if (normalized)
    apply(dataset, 1, function(x) dtw(x, ts)$normalizedDistance)
  else
    apply(dataset, 1, function(x) dtw(x, ts)$distance)
}

#' Function used to get the distance matrix between all time series using DTW
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
#' @param numCores number of available cores
#' @examples
#' # calculating the DTW distance between the i-th time series and others
#' dtw.matrix(dataset)
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
