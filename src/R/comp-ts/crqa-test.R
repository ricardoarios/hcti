library(crqa)
library(tseriesChaos)
library(rgl)

embedd(summarised_countries[which(summarised_countries$Country=="Italy"), -c(1)] %>% as.numeric(), m = 3, d=5) %>% plot3d(t="l")



delay = 1; embed = 1; rescale = 1; radius = 1;
normalize = 0; mindiagline = 2; minvertline = 2;
tw = 0; 

res = crqa(summarised_countries[which(summarised_countries$Country=="Albania"), -c(1)] %>% as.numeric(), 
           summarised_countries[which(summarised_countries$Country=="Italy"), -c(1)] %>% as.numeric(),
            delay, embed, rescale, radius, normalize, mindiagline, minvertline, tw)
                                

par = list(unit = 2, labelx = "Time", labely = "Time", cols = "black", pcex = 1)

RP = res$RP
dev.off()
plotRP(RP, par)

res$rENTR


res = crqa(summarised_countries[which(summarised_countries$Country=="Spain"), -c(1)] %>% as.numeric(), 
           summarised_countries[which(summarised_countries$Country=="France"), -c(1)] %>% as.numeric(),
           delay, embed, rescale, radius, normalize, mindiagline, minvertline, tw)


par = list(unit = 2, labelx = "Time", labely = "Time", cols = "black", pcex = 1)

RP = res$RP
plotRP(RP, par)

res$rENTR

