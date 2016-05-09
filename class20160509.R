library(ggmap)
if(!require('ggmap')){
  
  install.packages("ggmap")
  
  library(ggmap)#forget_map()
  
}

twmap<-get_map(location='Taipei',zoom=7,language="zh-TW")

ggmap(twmap)

#heatmap
nba<-read.csv("http://datasets.flowingdata.com/ppg2008.csv")

head(nba)
 ##寬表轉長表
library(reshape2)#formelt()

nba.m<-melt(nba,id.vars="Name")#寬表轉長表,以名字作依據

head(nba.m,10)