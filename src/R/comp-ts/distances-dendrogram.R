
#which(row.names(all.ts.country) == "Spain")

calc.dend.dists<-function(dend, country = NA){
  if(is.na(country))
    stop("Please, inform the country's name.")

  if(length(dend) == 1 && (dend %>% get_nodes_attr("label") %>% na.omit() == country))
    return(NA)
  
  
  cat("Tamanho: ", length(dend), "\n")
  cat("Teste", (dend %>% get_nodes_attr("label") %>% na.omit() == country) %>% all(), "\n")
  
  
  result<-(dend %>% get_nodes_attr("height") %>% as.numeric())[1]
  cat(result, "\n")
  for(i in 1:length(dend)){
    if(!(country %in% (dend[[i]] %>% get_nodes_attr("label") %>% na.omit())))
      next
    if(length(dend[[i]]) == 1 && (dend[[i]] %>% get_nodes_attr("label") %>% na.omit() == country))
      result<-c(result,  Recall(dend[[(2-i)+1]], country))
    if(country %in% (dend[[i]] %>% get_nodes_attr("label") %>% na.omit()))
      result<-c(result,  Recall(dend[[i]], country))
  }
  return(result)
}

get.movement<-function(distances){
  distances<-distances[length(distances):(length(distances)-2)] %>% sort(decreasing = T)  
  (distances[2]-distances[3])/(distances[1]-distances[3])
}

#distances<-calc.dend.dists(dend, 51) %>% na.omit() #USA
#distances<-distances[length(distances):(length(distances)-1)]
#distances<-calc.dend.dists(dend, 9) %>% na.omit() #BR
#distances[length(distances):(length(distances)-2)]
#distances<-calc.dend.dists(dend, 24) %>% na.omit() #India
#distances[length(distances):(length(distances)-2)]
#distances<-calc.dend.dists(dend, 38) %>% na.omit() #Spain
#distances[length(distances):(length(distances)-2)]

