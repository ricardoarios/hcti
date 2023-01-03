library(ggplot2)

df <- data.frame(Cluster=c("C.1", "C.2", "C.3", "C.4", "C.5"),
                 Distance=c(438.49,92.83,284,340,1172))

ggplot(data=df, aes(x=Cluster, y=Distance)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=Distance), vjust=1.6, color="white", size=3.5)+
  theme_minimal()
