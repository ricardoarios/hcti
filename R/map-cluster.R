library(tidyverse)
library(rvest)
library(magrittr)
library(ggmap)
library(stringr)

#' Function used to visualize on a global map all countries from a single cluster
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
#' @param group.country A cluster of countries
#' @param plot.file A file (name and path) where the plot will be saved
#' @param world.map A dataset containing all countries (see 'data' fold)
#' @examples
#' # See clustering-covid.R
createMapCluster<-function(group.country, plot.file=NA,
                           world.map="data/map-world.Rdata"){

  load(file=world.map)

  names.temp<-names(map.world)
  map.world<-cbind(map.world, rep(FALSE, nrow(map.world)))
  names(map.world)<-c(names.temp, "fill_flg")

  for(country in group.country){
    ind.temp<-which(map.world$region == country)
    if(length(ind.temp) > 0){
      map.world[ind.temp, "fill_flg"]<-TRUE
    }else{
      cat("Error! Country (", country, ") not found.\n")
    }
  }

  if (!is.na(plot.file))
    pdf(file=plot.file)

  newmap<-ggplot() +
    geom_polygon(data = map.world, aes(x = long, y = lat, group = group, fill = fill_flg)) +
    scale_fill_manual(values = c("#CCCCCC","#e60000")) +
    theme(text = element_text(family = "Gill Sans", color = "#FFFFFF")
          ,plot.title = element_text(size = 30)
          ,plot.subtitle = element_text(size = 10)
          ,axis.text = element_blank()
          ,axis.title = element_blank()
          ,axis.ticks = element_blank()
          ,legend.position = "none"
    )
  print(newmap)

  if (!is.na(plot.file))
    dev.off()
}

#' Function used to visualize on a global map all clusters of countries
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
#' @param group.country Clusters containing all countries
#' @param plot.file A file (name and path) where the plot will be saved
#' @param world.map A dataset containing all countries (see 'data' fold)
#' @examples
#' # See clustering-covid.R
createMapAllClusters<-function(group.country, plot.file=NA,
                               world.map="data/map-world.Rdata"){

  load(file=world.map)

  names.temp<-names(map.world)
  map.world<-cbind(map.world, rep(0, nrow(map.world)))
  names(map.world)<-c(names.temp, "fill_flg")

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

  if (!is.na(plot.file))
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

  if (!is.na(plot.file))
    dev.off()
}

