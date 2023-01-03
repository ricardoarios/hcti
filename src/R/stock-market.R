rm(list=ls())
library('quantmod')

#### IBOV, S&P 500, Dow Jones, DAX (Alemanha), Shangai (comp.), Espanha, Dolar/Real, ouro, bitcoin (US)
monitor.symb<-c("^BVSP", "^GSPC", "^DJI", "^GDAXI", "000001.ss", "^IBEX", "BRL=X", "GC=F", "BTC-USD")

#testing...
#ind=5
#var.temp<-new.env()
#getSymbols(monitor.symb[ind], env = var.temp, src = "yahoo", from = as.Date("2020-01-01"), to = Sys.Date())
#chartSeries(get(ls(var.temp), envir = var.temp), type = "candlesticks", theme = "white", name = monitor.symb[ind])

load("data/violence-monitor-BA.Rdata")
violence.monitor<-violence.monitor %>% as.data.frame()
violence.monitor$Data<-as.Date(violence.monitor$Data, format="%d/%m/%Y")# %>% format(format = "%d/%m/%Y") 
violence.monitor<-violence.monitor %>% dplyr::arrange(Data)

#get(ls(var.temp), envir = var.temp)

for(ind in 1:length(monitor.symb)){
  
  var.temp<-new.env()
  getSymbols(monitor.symb[ind], env = var.temp, src = "yahoo", from = as.Date("2020-01-01"), to = Sys.Date())
  stock.data<-get(ls(var.temp), envir = var.temp)
  
  date.stock<-as.matrix(stock.data) %>% row.names() %>% 
                    as.Date(format = "%Y-%m-%d")
  
  names.tmp<-names(violence.monitor)
  violence.monitor<-cbind(violence.monitor, rep(0,nrow(violence.monitor)))
  names(violence.monitor)<-c(names.tmp, paste(sep="", "stk_", monitor.symb[ind]))
  
  for(j in 1:length(date.stock)){
    pos.temp<-which(violence.monitor$Data==date.stock[j])
    violence.monitor[pos.temp, ncol(violence.monitor)]<-stock.data[j, 1] %>% as.numeric() #using OPEN values
  }
  
}

save(violence.monitor, file="data/violence-BA-stock.Rdata")
