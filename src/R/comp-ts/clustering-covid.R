rm(list=ls())
library(readr)
library(dplyr)
library(ggplot2)
library(cluster)

#load(file="data/confirmed-cumulative-by-country.Rdata")
#load(file="data/confirmed-day-by-country.Rdata")
load(file="data/death-day-by-country.Rdata")

source("R/map-cluster.R")

####
summarised_countries$Country <- recode(summarised_countries$Country,
                                       'North Macedonia' = 'Macedonia',
                                       'Korea, South' = 'South Korea',
                                       'Taiwan*' = 'Taiwan',
                                       'US' = 'USA',
                                       'United Kingdom' = 'UK'
)
###


#max.window<-(ncol(summarised_countries)-1)
max.window<-20

final.k<-c()
final.sil<-c()
final.resp<-list()

first.death.BR<-which(summarised_countries[which(summarised_countries$Country=="Brazil"), -c(1)]>0)[1]
br.index<-which(summarised_countries$Country=="Brazil")

step<-6
iter.time<-1

while(first.death.BR+(step*(iter.time-1))+max.window <= ncol(summarised_countries)){
  
  cat("*****Analyzing: ", iter.time, "\n")
  
  
  analyzed.country<-c()
  
  all.ts.country<-matrix(NA, nrow = 1, ncol=(max.window+1))
  
  for(i in 1:nrow(summarised_countries)){
    first.death<-which(summarised_countries[i, -c(1)]>0)[1]
    if(is.na(first.death) || (first.death > first.death.BR)){
      next;
    }
    start<-first.death+(step*(iter.time-1))
    end<-start+max.window
    all.ts.country <- rbind(all.ts.country, summarised_countries[i, start:end] %>% as.numeric())
    analyzed.country<-c(analyzed.country, i)  
  }
  
  
  all.ts.country<-all.ts.country[-c(1),]
  

  ##############cal dist
  
  source("R/parDTW.R")
  diss<-dtw.matrix(all.ts.country) %>% as.dist()
  #source("R/parCRQA.R")
  #diss<-crqa.matrix(all.ts.country) %>% as.dist()
  
  best.k<-0
  best.sil<-0
  best.clust<-c()
  
  for(k.it in 5:10){
    #result<-pam(diss, k = k.it, diss=T)
    hc <- hclust(diss, method = "average")
    clust <- cutree(hc, k = k.it)
    validity<-silhouette(clust, diss)[,3] %>% mean()
    #validity<-silhouette(result)[,3] %>% mean()
    cat("validation: ", validity, "\n")
    if(validity > best.sil){
      best.k = k.it
      best.sil = validity
      best.clust<-clust
      #best.clust<-result$clustering
    }
  }
  
  new.index<-which(analyzed.country==br.index)
  group<-best.clust[new.index]
  
  final.resp[[iter.time]]<-analyzed.country[which(best.clust == group)]
  cat("Result: ", summarised_countries[final.resp[[iter.time]],1] %>% as.character(), "\n")
  #createMapCluster(group.country = summarised_countries[final.resp[[iter.time]],1] %>% as.character(), 
  #                 plot.file = paste(sep="", "result/cluster-", iter.time, ".pdf"))
  createMapAllClusters(group.country = best.clust, 
                 plot.file = paste(sep="", "result/cluster-", iter.time, ".pdf"))
  final.sil<-c(final.sil, best.sil)
  final.k<-c(final.k, best.k)
  iter.time<-iter.time+1
  

}

#checking groups
#summarised_countries[analyzed.country[which(best.clust == 5)],1]

