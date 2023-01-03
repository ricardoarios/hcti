rm(list=ls())
library(readr)
library(dplyr)

source("R/normalize.R")

covid_data <- read_csv("data/COVID-19-master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
covid_data <- covid_data[,-c(1,3,4)]
names.temp <- colnames(covid_data)[-c(1)] %>% as.Date(format="%m/%d/%y") %>% format(format="%d/%m/%Y")
names.temp<-c("Country", names.temp[-c(1)])
colnames(covid_data)<-names.temp

countries<-factor(covid_data$Country) %>% unique()

summarised_countries<-matrix(NA, ncol=(ncol(covid_data)-2))
remove.countries<-c()
for(i in 1:length(countries)){
  index<-which(covid_data$Country == countries[i])
  new.values<-colSums(covid_data[c(index),-c(1)]) %>% as.numeric()  %>% diff()# %>% normalization()
  if(!all(new.values %>% is.nan())){
    summarised_countries<-rbind(summarised_countries, new.values)
  }else{
    remove.countries<-c(remove.countries, i)
  }
}

summarised_countries<-summarised_countries[-c(1),] 
colnames(summarised_countries)<-names.temp[-c(1)]
summarised_countries<-tbl_df(summarised_countries)

if(length(remove.countries)>0){
  summarised_countries<-cbind(countries[-c(remove.countries)], summarised_countries)
}else{
  summarised_countries<-cbind(countries, summarised_countries)
}

colnames(summarised_countries)<-names.temp

save(summarised_countries, file="data/death-day-by-country.Rdata")

