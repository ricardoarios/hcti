rm(list=ls())
library(readr)
library(dplyr)

covid_data <- read_csv("data/COVID-19-master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
covid_data <- covid_data[,-c(1,3,4)]
names.temp <- colnames(covid_data)[-c(1)] %>% as.Date(format="%m/%d/%y") %>% format(format="%d/%m/%Y")
names.temp<-c("Country", names.temp[-c(1)])
colnames(covid_data)<-names.temp

countries<-factor(covid_data$Country) %>% unique()

summarised_countries<-matrix(NA, ncol=(ncol(covid_data)-2))

for(i in 1:length(countries)){
  index<-which(covid_data$Country == countries[i])
  summarised_countries<-rbind(summarised_countries, colSums(covid_data[c(index),-c(1)]) %>% diff() %>% as.numeric()) #scale(center = T, scale = T) %>% as.numeric())
}

summarised_countries<-summarised_countries[-c(1),]
colnames(summarised_countries)<-names.temp[-c(1)]
summarised_countries<-tbl_df(summarised_countries)

summarised_countries<-cbind(countries, summarised_countries)
colnames(summarised_countries)<-names.temp

save(summarised_countries, file="data/confirmed-day-by-country.Rdata")