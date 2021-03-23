rm(list=ls())
getwd()
setwd("D:/thesis/core/rwl/dplr")
library(dplR)
library(dplyr)
library(graphics)
library(stats)
library(utils)

#1 Превращаем деревья в лес
core_list = list()
core_desc = data.frame()
for(file in dir(path="rwl/")){
  core = read.rwl(paste("rwl", file, sep = "/"))
  core_list = c(core_list, core)
  maxy = max(rownames(core) %>% as.integer(),na.rm = T)
  miny = min(rownames(core) %>% as.integer(),na.rm = T)
  dfr = data.frame(coren = names(core),maxy,miny)
  core_desc = rbind(core_desc,dfr)
  #dplR::rwl.report(core)
  
}

starty = min(core_desc$miny)

for(n in 1:length(core_list)){
  upd_core = data.frame(
    temp= c(rep(NA, core_desc$miny[n]-starty),core_list[[n]]))
  names(upd_core) = core_desc$coren[n]
  rownames(upd_core) = starty:core_desc$maxy[n]
  if(n==1){
    core_pack = upd_core
  } else {
    core_pack = cbind(core_pack, upd_core)  
  }
  
}

core_pack = dplR::as.rwl(core_pack)

# 2. Проведение перекрёстной датировки

#Посмотрим возраст образцов
plot(core_pack)
#Посмотрим статистику по образцам
summary(core_pack)
rwl.stats(core_pack)
# Выведем абсолютные значения радиального прироста
plot(core_pack, plot.type="spag")
read.ids(core_pack, stc = c(3, 2, 3))
autoread.ids(core_pack)
# Посмотрим корреляцию с мастером
crs <- corr.rwl.seg(core_pack, seg.length=60, pcrit=0.01, label.cex = 1.25)
# pcrit -критическое значение для теста, стоит ли его поменять?
## Average correlation and p-value for the first few series
head(crs$overall)
## Average correlation for each bin
crs$avg.seg.rho

# 3. Проведём стандартизацию индивидуальных рядов абсолютного прироста и построение обобщенных хронологий

#3.1. Классическая ниленейная модель биологического роста вида f(t) = a exp(bt) + k
core_pack.rwi <- detrend(rwl = core_pack, method = "ModNegExp")

# теперь убедимся, что core_pack core_pack.rwi имеют одинаковые размеры и каждая серия стандартизована
dim(core_pack)
dim(core_pack.rwi)
names(core_pack)
names(core_pack.rwi)
colMeans(core_pack.rwi, na.rm=TRUE)


# 3.2.  Building a Mean Value Chronology
# building a chronology by averaging across the years of the rwi object
core_pacK.crn <- chron(core_pack.rwi, prefix = "CAM")
# Теперь убедимся, что они имеют одинаковое количество строчек
dim(core_pacK.crn)
dim(core_pack.rwi)
plot(core_pacK.crn, add.spline=TRUE, nyrs=20)
plot(core_pacK.crn,xlab="Year",ylab="RWI")
# 3.3. Build Mean Value Chronology with Stabilized Variance

core_pack1.rwi <- detrend(core_pack,method = "Spline")
core_pack1.crn <- chron(core_pack1.rwi)
core_pack1.crn2 <- chron.stabilized(core_pack.rwi,
                               winLength=101,
                               biweight = TRUE,
                               running.rbar = FALSE)
yrs <- time(core_pack)
plot(yrs,core_pack1.crn$xxxstd,type="l",col="grey")
lines(yrs,core_pack1.crn2$adj.crn,col="red")

# 4. Проведём оценку качества хронологии (Соломина, 2012)

# Рассчитаем коэффициент чувствительности (оценивает величину межгодовой изменчивости прироста)
sens1(core_pack.rwi[, 1])
# Чувствительность серии -  0.3204442
#Посчитаем стандартное отклонение (показывает амплитуду изменчивости прироста)
standart_deviation <- sd(core_pack.rwi)
summary(core_pack.rwi)
# Рассчитаем индекс EPS (Expressed Population Signal).
# EPS показывает насколько хорошо реальная хронология, полученная из ограниченного количества образцов, отражает гипотетическую, пред  ставленную неограниченным числом деревьев (Wigley et al., 1984).
core_pack.ids <- read.ids(core_pack, stc = c(4, 3, 1))
srwl <- strip.rwl(core_pack, ids = core_pack.ids, verbose = TRUE)
tail(srwl)


# Выявление климатического сигнала в древесно-кольцевых хронологиях
#Анализ климатического отклика обобщенных хронологий проводится с помощью расчета коэффициентов корреляций (Rs) между индексами прироста и ежемесячными значениями осадков и температуры воздуха за период, в течение которого возможно влияние климатических факторов на годичный радиальный прирост древесины.



#6. А теперь можно просто немного поиграться =)
# Очень интересная функция, показывающая реакцию на эктремальные погодные события
data(core_pack)
event.years <- c(2010, 2014)
core_pack.sea <- sea(core_pack, event.years)
foo <- core_pack.sea$se.unscaled
names(foo) <- core_pack.sea$lag
barplot(foo, col = ifelse(core_pack.sea$p < 0.05, "grey30", "grey75"),
        ylab = "RWI", xlab = "Superposed Epoch")
# К сожалению, не сработало:Ошибка в quantile.default(newX[, i], ...) :
#missing values and NaN's not allowed if 'na.rm' is FALSE



# Я не совсем понимаю, что тут происходит, но почему бы не попробовать
# Применение низкочастотной, высокочастотной и полосовой фильтрации к у с частотами или периодами.
data("core_pack")
x <- na.omit(core_pack[, 1])
# 20-year low-pass filter -- note freq is passed in
bSm <- pass.filt(x, W=0.05, type="low", method="Butterworth")
cSm <- pass.filt(x, W=0.05, type="low", method="ChebyshevI")
plot(x, type="l", col="grey")
lines(bSm, col="red")
lines(cSm, col="blue")
# 20-year high-pass filter -- note period is passed in
bSm <- pass.filt(x, W=20, type="high")
plot(x, type="l", col="grey")
lines(bSm, col="red")
# 20 to 100-year band-pass filter -- note freqs are passed in
bSm <- pass.filt(x, W=c(0.01, 0.05), type="pass")
cSm <- pass.filt(x, W=c(0.01, 0.05), type="pass", method="ChebyshevI")
plot(x, type="l", col="grey")
lines(bSm, col="red")
lines(cSm, col="blue")
# 20 to 100-year stop-pass filter -- note periods are passed in
cSm <- pass.filt(x, W=c(20, 100), type="stop", method="ChebyshevI")
plot(x, type="l", col="grey")
lines(cSm, col="red")
