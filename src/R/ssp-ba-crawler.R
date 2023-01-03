rm(list=ls())

require("Rcrawler")
require("stringr")
require("dplyr")

get.all.links<-function(base.url="http://200.187.8.90/boletim-stelecom/?", links){
  all.links<-str_match(links, "bo_cod")
  next.page<-links[which(is.na(all.links))] 
  next.index<-1
  
  final.links<-c()
  
  while(TRUE){
    active.links<-which(!is.na(all.links))
    final.links<-c(final.links, unlist(str_split(links[active.links], '\\?'))[seq(3,3*length(active.links),3)] %>% paste(sep="", base.url, .))
    
    
    ###
    #
    # UNFORTUNATELY USING BREAK, MUST IMPROVE THIS PART.
    #
    ###
    if(next.index > length(next.page)){ break }
    
    temp<-unlist(next.page[next.index] %>% str_split(., '\\?'))[3]%>% str_remove_all(.,"amp;")
    url=paste(sep="", base.url, temp)
    cat("Parsing (", url, ")...\n")
    page<-LinkExtractor(url=url)
    links<-page[["InternalLinks"]]
    all.links<-str_match(links, "bo_cod")
    next.index<-next.index+1
  }
  
  final.links
  
}

page<-LinkExtractor(url="http://200.187.8.90/boletim-stelecom/?dt_inicio=01%2F01%2F2020&dt_fim=07%2F05%2F2020")
links<-page[["InternalLinks"]]
resp<-get.all.links(links=links)

violence.monitor<-matrix(0, nrow=length(resp), ncol=1)
colnames(violence.monitor)<-c("Data")

for(i in 1:length(resp)){
  cat("\n Getting data from: ", resp[i], "\n")
  data.table<-ContentScraper(Url = resp[i] ,CssPatterns = c(".corUm")) %>% str_extract(pattern="[0-9]+\\/[0-9]+\\/[0-9]+")
  names.table<-ContentScraper(Url = resp[i] ,XpathPatterns = "//*/h1", ManyPerPattern = TRUE) %>% unlist()
  values.table<-ContentScraper(Url = resp[i] ,XpathPatterns = "//*/p", ManyPerPattern = TRUE) %>% unlist() %>% 
    str_split(pattern="Total: ") %>% unlist() %>% as.integer() %>% na.exclude() %>% as.vector()
  violence.monitor[i,1]<-data.table
  
  for(j in 1:length(names.table)){
    if(!names.table[j] %in% colnames(violence.monitor)){
      temp.names<-colnames(violence.monitor)
      violence.monitor<-cbind(violence.monitor, rep(0,length(resp)))
      colnames(violence.monitor)<-c(temp.names, names.table[j])
    }
    violence.monitor[i, which(colnames(violence.monitor) == names.table[j])] = values.table[j]
  }
}

save(violence.monitor, file="data/violence-monitor-BA.Rdata")
