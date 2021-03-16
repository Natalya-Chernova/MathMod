rm(list=ls())
rm(list=ls())
getwd()
setwd("D:/thesis/core/rwl")
#подключим пакеты
library(dplR)
library(graphics)
library(stats)
library(utils)
#попробуем объединить наши результаты в один датафрейм
data <- read.rwl("1s.rwl")
data2 <-read.rwl("2n.rwl")
data3 <- read.rwl("3n.rwl")
data4 <-read.rwl("3s.rwl")
#посмотрим, что из себя представляют данные
colnames(data)
rownames(data)
class(data)
dim(data)
dim(data2)
dim(data3)
dim(data4)
#зададим переменный
x1<-data
x2<-data2
x3<-data3
x4<-data4
#попробуем засунуть всё в один датафрейм
x1
x2<-c(x2,rep(NA,length(x1)-length(x2)))
x3<-c(x3,rep(NA,length(x1)-length(x3)))
x4<-c(x4,rep(NA,length(x1)-length(x4)))
df<-cbind(x1,x2,x3,x4)
# на этом месте он начинает материться, что аргумень подразумевает разное количество строк
data.frame=cbind(x1,
x2<-c(x2,rep(NA,length(x1)-length(x2))),
x3<-c(x3,rep(NA,length(x1)-length(x3))),
x4<-c(x4,rep(NA,length(x1)-length(x4))))
# и так тоже
df=cbind(data,data2,data3,data4)
#в теории, тут можно построить какой-то график, но это после победы над датафреймом
plot(df.crn, add.spline=TRUE, nyrs=20)

