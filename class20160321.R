##�q�W�����ѵ��}�l
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

## �����Ffor���n��
subset(NBA1415,GamesPlayed>70&TotalPoints>1500)[,c("Name","Team","Position")]
## �����Ffor���n��-2
NBA1415[NBA1415$GamesPlayed>70&NBA1415$TotalPoints>1500,c("Name","Team","Position")]

## �i�O....�٬O���Q�Ϊ��ɭ԰�....
San<-subset(NBA1415,Team=='SAN')
order(San$TotalPoints,decreasing = T)
San[order(San$TotalPoints,decreasing = T)[1],
    c("Name","Team","TotalPoints")]

##�Q�n�C�@�����̰���
##�ڭ��٬O��for+unique()�ոլݦn�F
unique(NBA1415$Team) #unique�i�H�������ƪ����W
for(team in unique(NBA1415$Team)){
  selectTeam<-subset(NBA1415,Team==team) #���C�@�����y��(����n����䤤�@��)
  print(selectTeam[order(selectTeam$TotalPoints,decreasing = T)[1],
                   c("Name","Team","TotalPoints")]) #�C��@���N�L�o���Ĥ@���y��
}

##��rbind()���X��z���@��Data Frame
FinalOutput<-NULL
for(team in unique(NBA1415$Team)){
  selectTeam<-subset(NBA1415,Team==team)
  FinalOutput<-rbind(FinalOutput,
                     selectTeam[
                       order(selectTeam$TotalPoints,decreasing = T)[1],
                       c("Name","Team","TotalPoints")]) #�]���@��A�U�@��|�A��i�h
}
FinalOutput



##apply()
apply(NBA1415,2,max)
##apply()�d��2
## NBA1415�y�u,�X���ơB�X���`�����ơB�`���������ȦU�O�h��
apply(NBA1415[,c("GamesPlayed","TotalMinutesPlayed","TotalPoints")],2,mean)


##sapply()
sapply(iris, mean)
##sapply()�d��2
sapply(NBA1415[,c("GamesPlayed","TotalMinutesPlayed","TotalPoints")],mean)


##lapply(): List
lapply(iris, mean)

##lapply()�d��2
lapply(NBA1415[,c("GamesPlayed","TotalMinutesPlayed","TotalPoints")],mean)


##tapply()
tapply(NBA1415$Name,NBA1415$Team,length) ##�H����@�̾ڨӤ����C�@���̦��X�Ӳy��
tapply(NBA1415$TotalPoints,NBA1415$Team,max) #�C�@���o���̦h���O�X��
tapply(NBA1415$TotalPoints,NBA1415$Team,mean) #�C�@���������`�o����
tapply(NBA1415$TotalPoints,NBA1415$Team,range) #�C�@�����`�o��range

##split()
split(1:30,gl(3, 10))

##split()+lapply()
lapply(split(1:30,gl(3, 10)),mean)
tapply(1:30,gl(3, 10),mean)


##Data Frame�]�i�Hsplit()
NBA1415Team<-split(NBA1415[,c("TotalPoints","GamesPlayed")],NBA1415$Team)
##split()�f�tapply()�a�ڨϥ�
##�]���r���e��r����
lapply(NBA1415Team, colMeans)
sapply(NBA1415Team, colMeans)

##split()�G�Φh�����Ӥ��s
NBA1415TP<-split(NBA1415[,c("TotalPoints","GamesPlayed")],list(NBA1415$Team,NBA1415$Position))
lapply(NBA1415TP, colMeans)
sapply(NBA1415TP, colMeans)

##����apply�a�ڪ���ơGaggregate()
aggregate(NBA1415$TotalPoints, by=list(NBA1415$Team,NBA1415$Position), FUN=mean, na.rm=TRUE)
##����apply�a�ڪ���ơGaggregate()-2
aggregate(TotalPoints ~ Team+Position, data = NBA1415, mean) #�n����~�~�వ�B��
#�p�G�u�n�ⶤ����N��position�R��

#Missing Value
x<-c(1,2,3,4,5,NA)
mean(x)
mean(x, na.rm=T)
sum(x)
sum(x, na.rm=T)

##Removing NA
x <- c(1, 2, NA, 4, NA, 5)
x[! is.na(x)] #�u����is.na(x)�|�X�{�ONA�N�OTRUE
x[! complete.cases(x)] #�u���XNA


##�w�q�@�ӷs�����
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
##�K�Ѽ�
roundmean<-function(vector, ...){
  round(mean(vector,...),digits=2)
}
roundmean(c(1.1,2,3,4,5))
roundmean(c(1.1,2,3,4,5,NA))
roundmean(c(1.1,2,3,4,5,NA),na.rm=T)

##�Ѽƹw�]��
roundDe<-function(v=1.111:10.111){
  round(v,digits = 2)
}
roundDe(1.66:6.66)
roundDe()
##retrun()
round2<-function(v){
  if(!is.numeric(v)){
    print("��J�Ʀr")
    return()
  }
  round(v,digits = 2)
}
round2("a")


##��Ƥ]�i�H���@�Ѽƨӥ�
apply(iris,2,max)

RoundNumber2<-function(v,XFun){
  round(XFun(v),digits = 2)
}
RoundNumber2(1.1:10.1,mean)

##�q�����W�U���ɮ� download.file
if (!require('RCurl')){
  install.packages("RCurl")
  library(RCurl)
}
download.file("https://raw.githubusercontent.com/yijutseng/
              BigDataCGUIM/master/files/opendata10401.csv", 
              destfile = "open.csv", method = "curl")

## read.csv�ϥνd��
data <- read.csv('open.csv')
data

#�@�~�o����
MaxPoint<-aggregate(TotalPoints~Team,NBA1415,max)
tapply(NBA1415$TotalPoints,NBA1415$Team,max)
NBA1415MaxPoint<-merge(NBA1415,MaxPoint)
output<-NBA1415MaxPoint[order(NBA1415MaxPoint$TotalPoints,decreasing = T),c("Team","Name","TotalPoints")]
library(knitr)
kable(output, digits=2)

#�@�~�̨��W���y��TotalMinutesPlayed 
install.packages("SportsAnalytics")
library(SportsAnalytics)
NBA1415<-fetch_NBAPlayerStatistics("14-15")

MaxTime<-aggregate(TotalMinutesPlayed~Team,NBA1415,max)
tapply(NBA1415$TotalMinutesPlayed,NBA1415$Team,max)
NBA1415MaxTime<-merge(NBA1415,MaxTime)
output<-NBA1415MaxTime[order(NBA1415MaxTime$TotalMinutesPlayed,decreasing = T),c("Team","Name","TotalMinutesPlayed")]
library(knitr)
kable(output, digits=2)
