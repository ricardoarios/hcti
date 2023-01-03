rm(list=ls())
library(readr)
library(dplyr)
library(ggplot2)

#load(file="data/confirmed-cumulative-by-country.Rdata")
#load(file="data/confirmed-day-by-country.Rdata")
load(file="data/death-day-by-country.Rdata")

##############cal dist

#start<-Sys.time()
#source("R/parCRQA.R")
#diss<-crqa.matrix(summarised_countries[,-c(1)] %>% as.matrix())
#end<-Sys.time()

plotRPgraph<-function (RP, par) 
{
  unit = cols = labelx = labely = pcex = NA
  for (v in 1:length(par)) assign(names(par)[v], par[[v]])
  xdim = nrow(RP)
  ydim = ncol(RP)
  RP = matrix(as.numeric(RP), nrow = xdim, ncol = ydim)
  tstamp = seq(0, xdim, unit)
  par(mar = c(3.8, 3.8, 1, 2), font.axis = 1.5, cex.axis = 1.5, 
      font.lab = 1, cex.lab = 1.2)
  plot(tstamp, tstamp, type = "n", xlab = "", ylab = "")
  l = 1
  for (l in 1:ydim) {
    ind = which(RP[, l] == 1)
    points(rep(l, length(ind)), ind, cex = pcex, col = cols, 
           pch = 20)
  }
  mtext(labelx, at = mean(tstamp), side = 1, line = 2.2, cex = 1.2, 
        font = 1)
  mtext(labely, at = mean(tstamp), side = 2, line = 2.2, cex = 1.2, 
        font = 1)
}

##############plot results

base.country<-"Brazil"

compared.countries<-c("United Kingdom", "US", "Spain", "Italy", "France", "China")
letter.labels<-c("(a) UK", "(b) US", "(c) ES", "(d) IT", "(e) FR", "(f) CN")

delay = 1; embed = 2; rescale = 1; radius = 1;
normalize = 0; mindiagline = 2; minvertline = 2;
tw = 0; 

resp.crqa<-c()

par(mfrow=c(3,2))
for(i in 1:length(compared.countries)){
  res = crqa(summarised_countries[which(summarised_countries$Country==base.country), -c(1)] %>% as.numeric(), 
             summarised_countries[which(summarised_countries$Country==compared.countries[i]), -c(1)] %>% as.numeric(),
             delay, embed, rescale, radius, normalize, mindiagline, minvertline, tw)
  resp.crqa<-c(resp.crqa, res$rENTR)
  par = list(unit = 1, labelx = letter.labels[i], labely = "", cols = "black", pcex = 0.8, mar = c(5.1, 4.1, 4.1, 1.1))
  plotRPgraph(res$RP, par)
  
}

dados<-data.frame(Country=compared.countries, rENTR=resp.crqa)
ggplot(data=dados, aes(x=Country, y=rENTR)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=rENTR %>% round(2)), vjust=1.6, color="white", size=3.5)+
  theme_minimal()




