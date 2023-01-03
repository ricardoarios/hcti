library(readr)
library(dplyr)


load(file="data/violence-BA-stock.Rdata")

###confirmed_cases
confirmed_covid <- read_csv("data/COVID-19-master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
confirmed_covid_brazil <- confirmed_covid[which(confirmed_covid$`Country/Region`=="Brazil"),-c(1,2,3,4)]

names.temp <- colnames(confirmed_covid_brazil) %>% as.Date(format="%m/%d/%y")
confirmed_covid_brazil <- confirmed_covid_brazil %>% as.numeric() %>% diff()
names(confirmed_covid_brazil) <- names.temp[2:length(names.temp)]

names.temp <- colnames(violence.monitor)
violence.monitor <- cbind(violence.monitor,rep(0,nrow(violence.monitor)))
colnames(violence.monitor) <- c(names.temp,"Casos confirmados")

day_confirmed <- names(confirmed_covid_brazil)

for (i in 1:length(day_confirmed)) {
  index_temp <- which(violence.monitor$Data == day_confirmed[i])
  violence.monitor[index_temp,ncol(violence.monitor)] <- confirmed_covid_brazil[i]
}


###death_cases
death_covid <- read_csv("data/COVID-19-master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
death_covid_brazil <- death_covid[which(death_covid$`Country/Region`=="Brazil"),-c(1,2,3,4)]

names.temp <- colnames(death_covid_brazil) %>% as.Date(format="%m/%d/%y")
death_covid_brazil <- death_covid_brazil %>% as.numeric() %>% diff()
names(death_covid_brazil) <- names.temp[2:length(names.temp)]

names.temp <- colnames(violence.monitor)
violence.monitor <- cbind(violence.monitor,rep(0,nrow(violence.monitor)))
colnames(violence.monitor) <- c(names.temp,"Casos com óbitos")

day_death <- names(death_covid_brazil)

for (i in 1:length(day_death)) {
  index_temp <- which(violence.monitor$Data == day_death[i])
  violence.monitor[index_temp,ncol(violence.monitor)] <- death_covid_brazil[i]
}

###recovered_cases
recovered_covid <- read_csv("data/COVID-19-master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
recovered_covid_brazil <- recovered_covid[which(recovered_covid$`Country/Region`=="Brazil"),-c(1,2,3,4)]

names.temp <- colnames(recovered_covid_brazil) %>% as.Date(format="%m/%d/%y")
recovered_covid_brazil <- recovered_covid_brazil %>% as.numeric() %>% diff()
names(recovered_covid_brazil) <- names.temp[2:length(names.temp)]

names.temp <- colnames(violence.monitor)
violence.monitor <- cbind(violence.monitor,rep(0,nrow(violence.monitor)))
colnames(violence.monitor) <- c(names.temp,"Casos recuperados")

day_recovered <- names(recovered_covid_brazil)

for (i in 1:length(day_death)) {
  index_temp <- which(violence.monitor$Data == day_recovered[i])
  violence.monitor[index_temp,ncol(violence.monitor)] <- recovered_covid_brazil[i]
}

save(violence.monitor, file = "data/violence-BA-stock-covid.Rdata")

par(mfrow=c(2,1))
plot(violence.monitor$`Casos com óbitos`,t="l", col="red")
grid()
#plot(violence.monitor$`Óbito` %>% as.numeric(), t="l", col="red")
#plot(violence.monitor$`stk_^BVSP` %>% as.numeric(), t="p", col="red")
plot(violence.monitor$`Casos recuperados`,t="l", col="red")
grid()



