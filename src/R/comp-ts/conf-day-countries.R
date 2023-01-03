rm(list=ls())
library(readr)
library(dplyr)

load(file="data/confirmed-day-by-country.Rdata")


##############cal dist

start<-Sys.time()
source("R/parDTW.R")
diss<-dtw.matrix(summarised_countries[,-c(1)] %>% as.matrix())
end<-Sys.time()

##############plot results

#heatmap(diss)


countries<-summarised_countries[,1]

base.country<-"Brazil"
# "United Kingdom"

dev.off()
plot.ts(diss[which(summarised_countries$Country==base.country), ], main=base.country, ylab="DTW", xlab="Countries")

countries[which.max(diss[which(summarised_countries$Country==base.country), ])]

min.v<-sort.list(diss[which(summarised_countries$Country==base.country), ], decreasing = F)[2:6]
nearest.countries<-countries[min.v]

par(mfrow=c(3,2))
plot(summarised_countries[which(summarised_countries$Country==base.country), -c(1)] %>% as.numeric(), main=base.country, ylab="#cases", xlab="Time")
for(i in nearest.countries){
  plot(summarised_countries[which(summarised_countries$Country==i), -c(1)] %>% as.numeric(), main=i, ylab="#cases", xlab="Time")  
}
