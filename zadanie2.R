#создайте модель множественной линейной регрессии ночных потоков паров воды за летний период 2013 года по данным измерений методом турбулентной пульсации
rm(list=ls())
getwd()
setwd("D:/group124/Chernova/MathMod")
# Запустим пакет readr
library("tidyverse")
library(readr)
# Считаем данные и приведём их в адекватный вид
tbl = read_csv("eddypro.csv", skip = 1, na =c("","NA","-9999","-9999.0"),  comment=c("["))
tbl
tbl=tbl[-1,]
# Посмотрим наши переменные
glimpse(tbl)
# избавимся от переменной roll
tbl = select(tbl, -(roll))
# Преобразуем переменные типа char в факторы
tbl = tbl %>% mutate_if(is.character, factor)
library(stringr)
# избавимся от ненужных символов
names(tbl) =  str_replace_all(names(tbl), "[!]","_emph_")
# Ещё раз посмотрим наши переменные
glimpse(tbl)
# Выберем все переменные типа numeric
sapply(tbl,is.numeric)
# Сделаем таблицу, состоящую из нужных нам колонок
tbl_numeric = tbl[,sapply(tbl,is.numeric) ]
# Получим таблицу, содержащую все остальные колонки (пока не понимаю, зачем она, но пусть будет)
tbl_non_numeric = tbl[,!sapply(tbl,is.numeric) ]
cor_td = cor(tbl_numeric)
cor_td
# Избавимся от строк, где есть NA
cor_td = cor(drop_na(tbl_numeric))
cor_td
cor_td = cor(drop_na(tbl_numeric)) %>% as.data.frame %>% select(h2o_flux)
vars = row.names(cor_td)[cor_td$h2o_flux^2 > .1] %>% na.exclude
vars
# Попробуем перейти к анализу
formula = as.formula(paste("h2o_flux~", paste(vars,collapse = "+"), sep=""))
formula
# Сделаем обучающую и и тестирующую выборки
teaching_tbl = sample_n(tbl, floor(length(tbl$date)*.7))
testing_tbl = sample_n(tbl, floor(length(tbl$date)*.3))
teaching_tbl
testing_tbl
