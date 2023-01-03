normalization<-function(series){
  (series-min(series))/(max(series)-min(series))
}