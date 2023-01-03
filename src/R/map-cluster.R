library(tidyverse)
library(rvest)
library(magrittr)
library(ggmap)
library(stringr)

createMapCluster<-function(group.country, plot.file){
  
  #map.world <- map_data("world")
  #save(map.world, file="data/map-world.Rdata")
  load(file="data/map-world.Rdata")
  
  names.temp<-names(map.world)
  map.world<-cbind(map.world, rep(FALSE, nrow(map.world)))
  names(map.world)<-c(names.temp, "fill_flg")
  
  #group.country<-c("Brazil", "France", "Germany", "UK")
  
  for(country in group.country){
    ind.temp<-which(map.world$region == country)
    if(length(ind.temp) > 0){
      map.world[ind.temp, "fill_flg"]<-TRUE
    }else{
      cat("Error! Country (", country, ") not found.\n")
    }
  }
  
  pdf(file=plot.file)
  newmap<-ggplot() +
    geom_polygon(data = map.world, aes(x = long, y = lat, group = group, fill = fill_flg)) +
    #geom_point(data = df.country_points, aes(x = lon, y = lat), color = "#e60000") +
    scale_fill_manual(values = c("#CCCCCC","#e60000")) +
    theme(text = element_text(family = "Gill Sans", color = "#FFFFFF")
          #,panel.background = element_rect(fill = "#444444")
          #,plot.background = element_rect(fill = "#444444")
          #,panel.grid = element_blank()
          ,plot.title = element_text(size = 30)
          ,plot.subtitle = element_text(size = 10)
          ,axis.text = element_blank()
          ,axis.title = element_blank()
          ,axis.ticks = element_blank()
          ,legend.position = "none"
    )
  print(newmap)
  dev.off()
}


createMapAllClusters<-function(group.country, plot.file){
  
  #map.world <- map_data("world")
  #save(map.world, file="data/map-world.Rdata")
  load(file="data/map-world.Rdata")
  
  names.temp<-names(map.world)
  map.world<-cbind(map.world, rep(0, nrow(map.world)))
  names(map.world)<-c(names.temp, "fill_flg")
  
  #group.country<-c("Brazil", "France", "Germany", "UK")
  
  for(i in unique(group.country)){
    countries<-summarised_countries[analyzed.country[which(group.country == i)],1] %>% as.character()
    for(country in countries){
      ind.temp<-which(map.world$region == country)
      if(length(ind.temp) > 0){
        map.world[ind.temp, "fill_flg"]<-i
      }else{
        cat("Error! Country (", country, ") not found.\n")
      }
    }
  }
  
  pdf(file=plot.file)
  newmap<-ggplot() +
    geom_polygon(data = map.world, aes(x = long, y = lat, group = group, fill = as.factor(fill_flg))) +
    #geom_point(data = df.country_points, aes(x = lon, y = lat), color = "#e60000") +
    scale_fill_manual(values = c("#CCCCCC", "yellow", "green", "blue", "red", "black")) +
    theme(text = element_text(family = "Gill Sans", color = "#FFFFFF")
          #,panel.background = element_rect(fill = "#444444")
          #,plot.background = element_rect(fill = "#444444")
          #,panel.grid = element_blank()
          ,plot.title = element_text(size = 30)
          ,plot.subtitle = element_text(size = 10)
          ,axis.text = element_blank()
          ,axis.title = element_blank()
          ,axis.ticks = element_blank()
          ,legend.position = "none"
    )
  print(newmap)
  dev.off()
}

