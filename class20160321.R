##從上次的解答開始
if (!require('SportsAnalytics')){
  install.packages("SportsAnalytics")
  library(SportsAnalytics)
}
NBA1415<-fetch_NBAPlayerStatistics("14-15")
for(i in 1:nrow(NBA1415)){
  if(NBA1415[i,"GamesPlayed"]>70&NBA1415[i,"TotalPoints"]>1500){
    print(NBA1415[i,c("Name","Team","Position")])
  }
}

## 都說了for不好用
subset(NBA1415,GamesPlayed>70&TotalPoints>1500)[,c("Name","Team","Position")]
## 都說了for不好用-2
NBA1415[NBA1415$GamesPlayed>70&NBA1415$TotalPoints>1500,c("Name","Team","Position")]

## 可是....還是有想用的時候啊....
San<-subset(NBA1415,Team=='SAN')
order(San$TotalPoints,decreasing = T)
San[order(San$TotalPoints,decreasing = T)[1],
    c("Name","Team","TotalPoints")]

##想要每一隊的最高分
##我們還是用for+unique()試試看好了
unique(NBA1415$Team) #unique可以取不重複的隊名
for(team in unique(NBA1415$Team)){
  selectTeam<-subset(NBA1415,Team==team) #取每一隊的球員(隊伍要等於其中一隊)
  print(selectTeam[order(selectTeam$TotalPoints,decreasing = T)[1],
                   c("Name","Team","TotalPoints")]) #每選一次就印得分第一的球員
}

##用rbind()把輸出整理成一個Data Frame
FinalOutput<-NULL
for(team in unique(NBA1415$Team)){
  selectTeam<-subset(NBA1415,Team==team)
  FinalOutput<-rbind(FinalOutput,
                     selectTeam[
                       order(selectTeam$TotalPoints,decreasing = T)[1],
                       c("Name","Team","TotalPoints")]) #跑完一行，下一行會再塞進去
}
FinalOutput



##apply()
apply(NBA1415,2,max)
##apply()範例2
## NBA1415球季,出場數、出場總分鐘數、總分的平均值各是多少
apply(NBA1415[,c("GamesPlayed","TotalMinutesPlayed","TotalPoints")],2,mean)


##sapply()
sapply(iris, mean)
##sapply()範例2
sapply(NBA1415[,c("GamesPlayed","TotalMinutesPlayed","TotalPoints")],mean)


##lapply(): List
lapply(iris, mean)

##lapply()範例2
lapply(NBA1415[,c("GamesPlayed","TotalMinutesPlayed","TotalPoints")],mean)


##tapply()
tapply(NBA1415$Name,NBA1415$Team,length) ##以隊伍作依據來分類每一隊裡有幾個球員
tapply(NBA1415$TotalPoints,NBA1415$Team,max) #每一隊得分最多的是幾分
tapply(NBA1415$TotalPoints,NBA1415$Team,mean) #每一隊的平均總得分數
tapply(NBA1415$TotalPoints,NBA1415$Team,range) #每一隊的總得分range

##split()
split(1:30,gl(3, 10))

##split()+lapply()
lapply(split(1:30,gl(3, 10)),mean)
tapply(1:30,gl(3, 10),mean)


##Data Frame也可以split()
NBA1415Team<-split(NBA1415[,c("TotalPoints","GamesPlayed")],NBA1415$Team)
##split()搭配apply()家族使用
##也有逗號前跟逗號後
lapply(NBA1415Team, colMeans)
sapply(NBA1415Team, colMeans)

##split()：用多個欄位來分群
NBA1415TP<-split(NBA1415[,c("TotalPoints","GamesPlayed")],list(NBA1415$Team,NBA1415$Position))
lapply(NBA1415TP, colMeans)
sapply(NBA1415TP, colMeans)

##類似apply家族的函數：aggregate()
aggregate(NBA1415$TotalPoints, by=list(NBA1415$Team,NBA1415$Position), FUN=mean, na.rm=TRUE)
##類似apply家族的函數：aggregate()-2
aggregate(TotalPoints ~ Team+Position, data = NBA1415, mean) #要先打~才能做運算
#如果只要算隊伍平均就把position刪除

#Missing Value
x<-c(1,2,3,4,5,NA)
mean(x)
mean(x, na.rm=T)
sum(x)
sum(x, na.rm=T)

##Removing NA
x <- c(1, 2, NA, 4, NA, 5)
x[! is.na(x)] #只執行is.na(x)會出現是NA就是TRUE
x[! complete.cases(x)] #只取出NA


##定義一個新的函數
round2<-function(v){
  round(v,digits = 2)
}
round2(3.886)
##Lazy Evaluation
round2Lazy<-function(vector,nothing){
  round(vector,digits = 2)
}
round2(3.886)
##Lazy Evaluation 2
f <- function(a, b) {
  print(a)
  print(b)
}
f(45)
##…參數
roundmean<-function(vector, ...){
  round(mean(vector,...),digits=2)
}
roundmean(c(1.1,2,3,4,5))
roundmean(c(1.1,2,3,4,5,NA))
roundmean(c(1.1,2,3,4,5,NA),na.rm=T)

##參數預設值
roundDe<-function(v=1.111:10.111){
  round(v,digits = 2)
}
roundDe(1.66:6.66)
roundDe()
##retrun()
round2<-function(v){
  if(!is.numeric(v)){
    print("輸入數字")
    return()
  }
  round(v,digits = 2)
}
round2("a")


##函數也可以當作參數來用
apply(iris,2,max)

RoundNumber2<-function(v,XFun){
  round(XFun(v),digits = 2)
}
RoundNumber2(1.1:10.1,mean)

##從網路上下載檔案 download.file
if (!require('RCurl')){
  install.packages("RCurl")
  library(RCurl)
}
download.file("https://raw.githubusercontent.com/yijutseng/
              BigDataCGUIM/master/files/opendata10401.csv", 
              destfile = "open.csv", method = "curl")

## read.csv使用範例
data <- read.csv('open.csv')
data

#作業得分王
MaxPoint<-aggregate(TotalPoints~Team,NBA1415,max)
tapply(NBA1415$TotalPoints,NBA1415$Team,max)
NBA1415MaxPoint<-merge(NBA1415,MaxPoint)
output<-NBA1415MaxPoint[order(NBA1415MaxPoint$TotalPoints,decreasing = T),c("Team","Name","TotalPoints")]
library(knitr)
kable(output, digits=2)

#作業最辛苦的球員TotalMinutesPlayed 
install.packages("SportsAnalytics")
library(SportsAnalytics)
NBA1415<-fetch_NBAPlayerStatistics("14-15")

MaxTime<-aggregate(TotalMinutesPlayed~Team,NBA1415,max)
tapply(NBA1415$TotalMinutesPlayed,NBA1415$Team,max)
NBA1415MaxTime<-merge(NBA1415,MaxTime)
output<-NBA1415MaxTime[order(NBA1415MaxTime$TotalMinutesPlayed,decreasing = T),c("Team","Name","TotalMinutesPlayed")]
library(knitr)
kable(output, digits=2)

