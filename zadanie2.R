#создайте модель множественной линейной регрессии ночных потоков паров воды за летний период 2013 года по данным измерений методом турбулентной пульсации
rm(list=ls())
getwd()
setwd("D:/group124/Chernova/MathMod")
# Запустим все нужные пакеты
library(tidyverse)
library(readr)
library(stringr)
library(lubridate)
library(tidyr)
library(dplyr)
library(tibble)
library(rnoaa)
#1 Приводим данные в удобный для работы вид

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

# избавимся от ненужных символов
names(tbl) =  str_replace_all(names(tbl), "[!]","_emph_")
str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>%
  str_replace_all("[-]","_minus_") %>%
  str_replace_all("[@]","_at_") %>%
  str_replace_all("[$]","_dollar_") %>%
  str_replace_all("[#]","_hash_") %>%
  str_replace_all("[/]","_div_") %>%
  str_replace_all("[%]","_perc_") %>%
  str_replace_all("[&]","_amp_") %>%
  str_replace_all("[\\^]","_power_") %>%
  str_replace_all("[()]","_") 
# Ещё раз посмотрим наши переменные
glimpse(tbl)
# Выберем все переменные типа numeric
sapply(tbl,is.numeric)
# Отфильтруем данные ночных потоков за летний период
tbl = filter(tbl, DOY >= 151 & DOY <= 242, daytime==FALSE)
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
#Cделаем непересекающиеся подвыборки 
row_numbers = 1:length(tbl$date)
teach = sample(row_numbers, floor(length(tbl$date)*.7))
test = row_numbers[-teach]
Teaching_tbl = tbl_numeric[teach,]
Testing_tbl = tbl_numeric[test,]
# 2 Ура-ура, мы можем перейти к построению моделей!
# Модель  по обучающей выборке
mod1 = lm(h2o_flux~ (.) , data = Teaching_tbl)
# А теперь узнаем всё, что мы хотим знать о модели
# посмотрим коэффициенты
coefficients(mod1)
# посчитаем предсказанные моделью значения
predict(mod1)
# потом узнаем остатки
resid(mod1)
# и доверительный интервал, конечно же
confint(mod1, level = 0.95)
#общие значения по модели будут не лишними
summary(mod1)
#дисперсионный анализ - это тоже важно
anova(mod1)
#и графики, куда же без них
plot(mod1)
# Ура-ура, мы получили кучу графиков!