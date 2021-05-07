rm(list=ls())
getwd()
setwd("D:/thesis/core")
library(dplR)
library(dplyr)
library(graphics)
library(stats)
library(utils)

#1 Превращаем деревья в лес
#1.1. Лес
f_core_list = list()
f_core_desc = data.frame()
for(file in dir(path="forrest/")){
  f_core = read.rwl(paste("forrest", file, sep = "/"))
  f_core_list = c(f_core_list, f_core)
  maxy = max(rownames(f_core) %>% as.integer(),na.rm = T)
  miny = min(rownames(f_core) %>% as.integer(),na.rm = T)
  dfr = data.frame(coren = names(f_core),maxy,miny)
  f_core_desc = rbind(f_core_desc,dfr)
  #dplR::rwl.report(f_core)
  
}

starty = min(f_core_desc$miny)

for(n in 1:length(f_core_list)){
  upd_f_core = data.frame(
    temp= c(rep(NA, f_core_desc$miny[n]-starty),f_core_list[[n]]))
  names(upd_f_core) = f_core_desc$coren[n]
  rownames(upd_f_core) = starty:f_core_desc$maxy[n]
  if(n==1){
    forest = upd_f_core
  } else {
    forest = cbind(forest, upd_f_core)  
  }
  
}

forest = dplR::as.rwl(forest)
#1.2. Город
c_core_list = list()
c_core_desc = data.frame()
for(file in dir(path="city_new/")){
  c_core = read.rwl(paste("city_new", file, sep = "/"))
  c_core_list = c(c_core_list, c_core)
  maxy = max(rownames(c_core) %>% as.integer(),na.rm = T)
  miny = min(rownames(c_core) %>% as.integer(),na.rm = T)
  dfr = data.frame(coren = names(c_core),maxy,miny)
  c_core_desc = rbind(c_core_desc,dfr)
  #dplR::rwl.report(c_core)
  
}

starty = min(c_core_desc$miny)

for(n in 1:length(c_core_list)){
  upd_c_core = data.frame(
    temp= c(rep(NA, c_core_desc$miny[n]-starty),c_core_list[[n]]))
  names(upd_c_core) = c_core_desc$coren[n]
  rownames(upd_c_core) = starty:c_core_desc$maxy[n]
  if(n==1){
    city = upd_c_core
  } else {
    city = cbind(city, upd_c_core)  
  }
  
}

city = dplR::as.rwl(city)


# 2. Проведение перекрёстной датировки

#Посмотрим возраст образцов
plot(forest)
plot(city)
#Посмотрим статистику по образцам
summary(forest)
rwl.stats(city)
# Выведем абсолютные значения радиального прироста
plot(forest, plot.type="spag")
plot(city, plot.type="spag")

# Посмотрим корреляцию с мастером
f_crs <- corr.rwl.seg(forest, seg.length=60, pcrit=0.05, label.cex = 1.25)
c_crs <- corr.rwl.seg(city, seg.length=60, pcrit=0.05, label.cex = 1.25)

#Средняя корреляция и p-value для первых нескольких значений
head(f_crs$overall)
head(c_crs$overall)
## Average correlation for each bin
f_crs$avg.seg.rho
c_crs$avg.seg.rho
# Проверим гляйхлойфихькайт
# Для леса
Glk_forest<-glk(forest, overlap = 50, prob = TRUE)
Glk_forest$p_mat
Glk_forest$overlap
Glk_forest$glk_mat
glk.legacy(forest)
# Для города
Glk_city<-glk(city, overlap = 50, prob = TRUE)
Glk_city$p_mat
Glk_city$overlap
Glk_city$glk_mat
glk.legacy(city)

# 3. Проведём стандартизацию индивидуальных рядов абсолютного прироста

#3.1. Детрендинг с использованием метода modified exponential decay
forest.rwi <- detrend(rwl = forest, method = "ModNegExp")

# Теперь убедимся, что  имеют одинаковые размеры и каждая серия стандартизована
# В лесу
dim(forest)
dim(forest.rwi)
names(forest)
names(forest.rwi)
colMeans(forest.rwi, na.rm=TRUE)
# В городе
dim(city)
dim(city.rwi)
names(city)
names(city.rwi)
colMeans(city.rwi, na.rm=TRUE)
# 3.2. Проведём детрендинг методом Stabilized Variance
# Для леса
forest1.rwi <- detrend(forest,method = "Spline")
# Для города
city1.rwi <- detrend(city,method = "Spline")

# 4  Построение обобщённой хронологии
#4.1. Построение хронологии с modified exponential decay
# Для леса
forest.crn <- chron(forest.rwi, prefix = "CAM")
# Для города
city.crn <- chron(city.rwi, prefix = "CAM")
# Теперь убедимся, что они имеют одинаковое количество строчек
#Лес
dim(forest.crn)
dim(forest.rwi)
plot(forest.crn, add.spline=TRUE, nyrs=20)
plot(forest.crn,xlab="Year",ylab="RWI")
# Город
dim(city.crn)
dim(city.rwi)
plot(city.crn, add.spline=TRUE, nyrs=20)
plot(city.crn,xlab="Year",ylab="RWI")
# 4.2. Построение обобщённой хронологии со Stabilized Variance
#Лес
forest1.crn <- chron(forest1.rwi)
forest1.crn2 <- chron.stabilized(forest.rwi,
                               winLength=101,
                               biweight = TRUE,
                               running.rbar = FALSE)
yrs <- time(forest)
plot(yrs,forest1.crn$xxxstd,type="l",col="grey")
lines(yrs,forest1.crn2$adj.crn,col="red")
# Город
city1.crn <- chron(city1.rwi)
city1.crn2 <- chron.stabilized(city.rwi,
                                 winLength=101,
                                 biweight = TRUE,
                                 running.rbar = FALSE)
yrs <- time(city)
plot(yrs,city1.crn$xxxstd,type="l",col="grey")
lines(yrs,city1.crn2$adj.crn,col="red")

# 5. Проведём оценку качества хронологии (Соломина, 2012)

# 5.1. Рассчитаем коэффициент чувствительности (оценивает величину межгодовой изменчивости прироста)
#Для леса
sens1(forest.rwi[, 1])
#Для города
sens1(city.rwi[, 1])
# Для второго метода детрендинга
# Лес
sens1(forest1.rwi[, 1])
# Город
sens1(city1.rwi[, 1])

#5.2. Рассчитаем индекс EPS (Expressed Population Signal).
# EPS показывает насколько хорошо реальная хронология, полученная из ограниченного количества образцов, отражает гипотетическую, представленную неограниченным числом деревьев (Wigley et al., 1984).
forest.ids <- read.ids(forest, stc = c(4, 3, 1))
f_srwl <- strip.rwl(forest, ids = forest.ids, verbose = TRUE)
tail(f_srwl)
city.ids <- read.ids(city, stc = c(4, 3, 1))
c_srwl <- strip.rwl(core_pack, ids = core_pack.ids, verbose = TRUE)
tail(c_srwl)
#5.3. Подсчитаем коэффициент Джини для леса (Biondi and Qeadan, 2008).
gini.coef(forest.crn)
#И для города
gini.coef(city.crn)


#6 Выявление климатического отклика в древесно-кольцевых хронологиях

#6.1 Реакция на экстримальные погодные события
#6.1.1. Засуха
# Лес
forest
event.years <- c(1964, 1972, 2010, 2014)
forest.sea <- sea(forest, event.years, lag=5, resample=1000)
foo1 <- forest.sea$se.unscaled
names(foo1) <- forest.sea$lag
barplot(foo, col = ifelse(forest.sea$p < 0.05, "grey30", "grey75"),
        ylab = "RWI", xlab = "Superposed Epoch")
# Город
city
event.years <- c(1964, 1972, 2010, 2014)
city.sea <- sea(city, event.years, lag=5, resample=1000)
foo2 <- city.sea$se.unscaled
names(foo2) <- city.sea$lag
barplot(foo2, col = ifelse(city.sea$p < 0.05, "grey30", "grey75"),
        ylab = "RWI", xlab = "Superposed Epoch")
#6.1.2. Переувлажнение
# Лес
event.years <- c(1973, 1991, 1998, 2004)
forest1.sea <- sea(forest, event.years, lag=5, resample=1000)
foo3 <- forest1.sea$se.unscaled
names(foo3) <- forest1.sea$lag
barplot(foo3, col = ifelse(forest1.sea$p < 0.05, "grey30", "grey75"),
        ylab = "RWI", xlab = "Superposed Epoch")
# Город
event.years <- c(1973, 1991, 1998, 2004)
city1.sea <- sea(city, event.years, lag=5, resample=1000)
foo4 <- city1.sea$se.unscaled
names(foo4) <- city1.sea$lag
barplot(foo4, col = ifelse(city1.sea$p < 0.05, "grey30", "grey75"),
        ylab = "RWI", xlab = "Superposed Epoch")
#6.1.3. Аномально холодные зимы
# Лес

event.years <- c(1941,2003, 2006, 2011)
forest2.sea <- sea(forest, event.years, lag=5, resample=1000)
foo5 <- forest2.sea$se.unscaled
names(foo5) <- forest2.sea$lag
barplot(foo5, col = ifelse(forest2.sea$p < 0.05, "grey30", "grey75"),
        ylab = "RWI", xlab = "Superposed Epoch")
# Город
event.years <- c(1941,2003, 2006, 2011)
city2.sea <- sea(city, event.years, lag=5, resample=1000)
foo6 <- city2.sea$se.unscaled
names(foo6) <- city2.sea$lag
barplot(foo6, col = ifelse(city2.sea$p < 0.05, "grey30", "grey75"),
        ylab = "RWI", xlab = "Superposed Epoch")
#6.1.4. Бесснежные зимы
# Лес
event.years <- c(1954, 1964, 1972, 1985)
forest3.sea <- sea(forest, event.years, lag=5, resample=1000)
foo7 <- forest3.sea$se.unscaled
names(foo7) <- forest3.sea$lag
barplot(foo7, col = ifelse(forest3.sea$p < 0.05, "grey30", "grey75"),
        ylab = "RWI", xlab = "Superposed Epoch")
# Город

event.years <- c(1954, 1964, 1972, 1985)
city3.sea <- sea(city, event.years, lag=5, resample=1000)
foo8 <- city3.sea$se.unscaled
names(foo8) <- city3.sea$lag
barplot(foo8, col = ifelse(city3.sea$p < 0.05, "grey30", "grey75"),
        ylab = "RWI", xlab = "Superposed Epoch")
#6.1.5. Зимняя оттепель
# Лес

event.years <- c(1961, 2008)
forest4.sea <- sea(forest, event.years, lag=5, resample=1000)
foo9 <- forest4.sea$se.unscaled
names(foo9) <- forest4.sea$lag
barplot(foo9, col = ifelse(forest4.sea$p < 0.05, "grey30", "grey75"),
        ylab = "RWI", xlab = "Superposed Epoch")
# Город

event.years <- c(1961, 2008)
city4.sea <- sea(city, event.years, lag=5, resample=1000)
foo10 <- city4.sea$se.unscaled
names(foo10) <- city4.sea$lag
barplot(foo10, col = ifelse(city4.sea$p < 0.05, "grey30", "grey75"),
        ylab = "RWI", xlab = "Superposed Epoch")

#6.2. Тут будет корреляция с осадками и суммой активных температур, но чуть позже:)




