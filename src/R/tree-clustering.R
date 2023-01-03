rm(list=ls())
library(readr)
library(dplyr)
library(ggplot2)
library(cluster)
library(reshape2)
library(dendextend)

#load(file="data/confirmed-cumulative-by-country.Rdata")

load(file="data/confirmed-day-by-country.Rdata")
#load(file="data/death-day-by-country.Rdata")

source("R/map-cluster.R")
source("R/comp-ts/dist2groups.R")
source("R/comp-ts/distances-dendrogram.R")
source("R/parDTW.R")

####
summarised_countries$Country <- recode(summarised_countries$Country,
                                       'North Macedonia' = 'Macedonia',
                                       'Korea, South' = 'South Korea',
                                       'Taiwan*' = 'Taiwan',
                                       'US' = 'USA',
                                       'United Kingdom' = 'UK')


summarised_countries<-summarised_countries[-which(summarised_countries$Country=="Diamond Princess"),]

monitor.countries<-c("Brazil", "France", "Italy",
                     "UK", "USA", "China",
                     "Belgium", "Germany", "Iran",
                     "Spain", "Turkey", "India", "Mexico", 
                     "Argentina", "Russia", "Canada", "Colombia")

population<-read.csv("data/pop.csv")
population$country <- recode(population$country,
                                       'North Macedonia' = 'Macedonia',
                                       'United States' = 'USA',
                                       'United Kingdom' = 'UK')
###

file.base<-"result/cluster-monitor.out"

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
  out.line<-paste(sep="", "Window: ", iter.time)
  write(out.line, file=file.base, append = TRUE)
  
  analyzed.country<-c()
  
  all.ts.country<-matrix(NA, nrow = 1, ncol=(max.window+1))
  
  for(i in 1:nrow(summarised_countries)){
    first.death<-which(summarised_countries[i, -c(1)]>0)[1]
    #if(is.na(first.death) || (first.death > first.death.BR)){
    if(is.na(first.death) || (first.death > first.death.BR) || !(summarised_countries[i,1] %in% monitor.countries)){
      next;
    }
    start<-first.death+(step*(iter.time-1))
    end<-start+max.window
    ## using population
    factor.pop<-population[which(population$country == (summarised_countries[i,1] %>% as.character())), 2]
    new.total<-((summarised_countries[i, start:end] %>% as.numeric())/factor.pop)*1000000
    all.ts.country <- rbind(all.ts.country, new.total)
    ## not using population
#    all.ts.country <- rbind(all.ts.country, (summarised_countries[i, start:end] %>% as.numeric()))
    
    analyzed.country<-c(analyzed.country, i)  
  }
  
  
  all.ts.country<-all.ts.country[-c(1),]
  row.names(all.ts.country)<-summarised_countries$Country[analyzed.country]
  
  ##############cal dist
  diss<-dtw.matrix(all.ts.country) %>% as.dist()
  #source("R/parCRQA.R")
  #diss<-crqa.matrix(all.ts.country) %>% as.dist()
  
  best.k<-3
  #result<-pam(diss, k = best.k, diss=T)
  hc <- hclust(diss, method = "average")
  clust <- cutree(hc, k = best.k)
  #locator(1)
  
  
  ######ploting dendogram
  pdf(file=paste(sep="", file.base, "-DEND-", iter.time, ".pdf"))
  plot(hc, labels = row.names(all.ts.country), 
       xlab="", ylab="", sub="", main="", cex=2.5)
  dev.off()
  
# France - 5   
# Spain - 11
#which(row.names(all.ts.country) == "Spain")
#dend <- hc %>% as.dendrogram
#dend %>% get_nodes_attr("height")
#distances<-calc.dend.dists(dend, 5) %>% na.omit() #Spain
#distances[length(distances):(length(distances)-2)] %>% sort(decreasing = T)  
#get.movement(distances)

#row.names(all.ts.country)[dend %>% get_nodes_attr("label")]
  
#  Sys.sleep(1)
  
  validity<-silhouette(clust, diss)[,3] %>% mean()
  #validity<-silhouette(result)[,3] %>% mean()
  cat("validation: ", validity, "\n")
  
  ####
  out.line<-paste(sep="", "Validity: ", validity)
  write(out.line, file=file.base, append = TRUE)
  out.line<-(silhouette(clust, diss) %>% summary(., FUN = mean))$clus.avg.widths
  write(out.line, file=file.base, append = TRUE)
  temp.k<-which(summarised_countries[analyzed.country, 1] %in% monitor.countries)
  out.line<-paste(cbind(summarised_countries[analyzed.country[temp.k], 1] %>% as.character(), clust[temp.k]))
  write.table(cbind(summarised_countries[analyzed.country[temp.k], 1] %>% as.character(), clust[temp.k]), 
              file=file.base, append = T, col.names = F, row.names = F)
  
  centroid.distances<-cbind(summarised_countries[analyzed.country, 1] %>% as.character(), 
                            calculateDistances(all.ts.country, clust) %>% round(.,2), 
                            apply(all.ts.country, 1, mean) %>% round(digits = 2))
  temp.k<-which(centroid.distances %in% monitor.countries)
  write.table(centroid.distances[temp.k, ], 
              file=paste(sep="", file.base, "-dist2k-", iter.time, ".csv"), 
              append = F, col.names = F, row.names = F, sep = ",")
  

  ### ploting groups
  closest.cent<-matrix(nrow = ncol(all.ts.country), ncol=length(unique(centroid.distances[,2])))
  closest.country<-c()
  for(cent.index in unique(centroid.distances[,2])){
    sub.group<-subset(centroid.distances, centroid.distances[,2] == cent.index)
    index.closest<-sub.group[, as.numeric(cent.index)+2] %>% which.min()
    
    closest.cent[,as.numeric(cent.index)]<-all.ts.country[which(summarised_countries[analyzed.country,1] == sub.group[index.closest,1]), ]
    closest.country<-c(closest.country, sub.group[index.closest,1])

  }
  paste(sep="", file.base, "-DEND-", iter.time, ".pdf")
  write.table(closest.cent, sep=",", file=paste(sep="", file.base, "-med-", iter.time, ".csv"), row.names = F, col.names = F)

  closest.cent<-data.frame(cbind(1:nrow(closest.cent),closest.cent))
  
  colnames(closest.cent)<-c("Weeks", paste(sep = ".", closest.country, unique(centroid.distances[,2])))
  closest.cent = melt(closest.cent, id=c("Weeks"))
  colnames(closest.cent)<-c("Days", "Cluster", "value")
  
  
  pdf(file=paste(sep="", file.base, "-dist2k-", iter.time, ".pdf"))
  newmap<-ggplot(closest.cent) + geom_line(aes(x=Days, y=value, colour=Cluster)) +
    scale_colour_manual(values=c("yellow", "green", "blue", "red", "black")) +
    theme(axis.title = element_blank(), text = element_text(size=20))
  print(newmap)
  dev.off()

  
  ###
  
  
  new.index<-which(analyzed.country==br.index)
  group<-clust[new.index]
  
  final.resp[[iter.time]]<-analyzed.country[which(clust == group)]
  cat("Result: ", summarised_countries[final.resp[[iter.time]],1] %>% as.character(), "\n")
#  createMapCluster(group.country = summarised_countries[final.resp[[iter.time]],1] %>% as.character(), 
#                   plot.file = paste(sep="", "result/cluster-", iter.time, ".pdf"))
  cbind(summarised_countries[analyzed.country,1] %>% as.character(), clust) %>% 
    write.table(sep=",", file=paste(sep="", "/tmp/cluster-", iter.time, ".csv"), row.names = F, col.names = F)
  createMapAllClusters(group.country = clust, 
                       plot.file = paste(sep="", "result/cluster-", iter.time, ".pdf"))
  final.sil<-c(final.sil, validity)
  final.k<-c(final.k, best.k)
  iter.time<-iter.time+1
}

pdf(file=paste(sep="", file.base, "-box.pdf"))
v.df<-data.frame(Clustering=rep(paste(sep="", "k=", best.k), length(final.sil)), MSW=final.sil)
ggplot(v.df, aes(x=Clustering, y=MSW)) + 
  geom_boxplot(color="blue", fill="blue", alpha=0.2,
               notch=TRUE,notchwidth = 0.8,
               outlier.colour="red", outlier.fill="red", outlier.size=3)+
  theme(axis.title = element_blank(), text = element_text(size=20))
dev.off()




