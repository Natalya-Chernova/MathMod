rm(list=ls())
rm(list=ls())
getwd()
setwd("D:/thesis/core/csv")
#подключим пакет
library(dplR)
#попробуем прочитать данные
data<-read.csv("1n.csv")
dat <- data
dat.sum <- summary(dat)
mean(dat.sum$year)
mean(dat.sum$stdev)
mean(dat.sum$ar1)
mean(interseries.cor(dat)[, 1])
plot(dat, plot.type="spag")
RNGversion("2.15.0")
set.seed(4576)
i <- sample(x=nrow(dat), size=1)
j <- sample(x=ncol(dat), size=1)
tmp <- dat[, j]
tmp <- c(NA, tmp[-i])
dat[, j] <- tmp
rwl.60 <- corr.rwl.seg(dat, seg.length=60, pcrit=0.01)
seg.60 <- corr.series.seg(rwl<-dat, series<-"643114",
                          + seg.length<-60)
win <- 1800:1960
dat.yrs <- time(dat)
dat.trunc <- dat[dat.yrs %in% win, ]
ccf.30 <- ccf.series.rwl(rwl<-dat.trunc, series<-"643114",
                           + seg.length<-30, bin.floor<-5)
win <- 1850:1900
dat.trunc <- dat[dat.yrs %in% win, ]
ccf.20 <- ccf.series.rwl(rwl<-dat.trunc, series<-"643114",
                           + seg.length<-20, bin.floor<-0)
xskel.ccf.plot(rwl<-dat, series<-"643114",
               + win.start<-1865, win.width<-40)
j
colnames(co021)[j]
i
rownames(co021)[i]

