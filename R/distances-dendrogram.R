#' Function used to extract information from dendrograms
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
#' @param dend a dendrogram produced by a hierachical clustering
#' @param country a country ID to measure its transition
#' @return distances from dendrogram clades
#' @examples
#' See
calc.dend.dists<-function(dend, country = NA){
  if(is.na(country))
    stop("Please, inform the country's ID.")

  if(length(dend) == 1 && (dend %>% get_nodes_attr("label") %>% na.omit() == country))
    return(NA)

  result<-(dend %>% get_nodes_attr("height") %>% as.numeric())[1]


  for(i in 1:length(dend)){
    if(!(country %in% (dend[[i]] %>% get_nodes_attr("label") %>% na.omit()))){
      next
    }else if(length(dend[[i]]) == 1 && (dend[[i]] %>% get_nodes_attr("label") %>% na.omit() != country)){
      result<-c(result,  Recall(dend[[(2-i)+1]], country))
    }else if(country %in% (dend[[i]] %>% get_nodes_attr("label") %>% na.omit())){
      result<-c(result,  Recall(dend[[i]], country))
    }else{
      cat("Not Possible!")
    }
  }
  return(result)
}

#' Function that calculates HCTI (Hierarchical clustering transition index)
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
#' @param hc output from a hierarchical clustering algorithm
#' @param country.ID country ID to get its HCTI
#' @return HCTI
#' @examples
#' calculateDistances(dataset, clust)
hcti<-function(hc, country.ID){
  dend <- hc %>% as.dendrogram
  distances<-calc.dend.dists(dend, country.ID) %>% na.omit()
  result <- 0
  if(length(distances) > 2){
    distances<-distances[length(distances):(length(distances)-2)] %>% sort(decreasing = T)
    result<-(distances[2]-distances[3])/(distances[1]-distances[3])
  }
  result
}



