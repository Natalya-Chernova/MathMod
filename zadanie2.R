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

# А теперь создадим вторую модель, "выкинув как можно больше переменных, которые не вносят почти ничего в качество итоговой модели"
formula2 = h2o_flux ~ (DOY+file_records+Tau+qc_Tau+rand_err_Tau+H+qc_H+rand_err_H+LE+qc_LE+rand_err_LE+co2_flux+qc_co2_flux+rand_err_co2_flux+rand_err_h2o_flux+`co2_v-adv`+`h2o_v-adv`+co2_mole_fraction+co2_mixing_ratio+h2o_molar_density+h2o_mole_fraction+h2o_mixing_ratio+h2o_time_lag+sonic_temperature+air_temperature+air_pressure+air_density+air_heat_capacity+air_molar_volume+water_vapor_density+e+es+specific_humidity+RH+VPD+Tdew+u_unrot+v_unrot+ w_unrot+u_rot +v_rot +w_rot +max_speed+wind_dir+yaw+pitch                  +TKE +L +bowen_ratio +`T*` + x_peak +x_offset +`x_10%`+`x_30%`+`x_50%`+`x_70%`+`x_90%`+un_Tau+Tau_scf+un_H+H_scf+un_LE+LE_scf+un_co2_flux+un_h2o_flux+w_spikes                  +ts_var+h2o_var+`w/ts_cov`+co2)
mod2 = lm(formula2, data = Teaching_tbl)
# а теперь узнаем то, что хотим знать о второй модели
# посмотрим коэффициенты
coefficients(mod2)
# посчитаем предсказанные моделью значения
predict(mod2)
# потом узнаем остатки
resid(mod2)
# и доверительный интервал, конечно же
confint(mod2, level = 0.95)
#общие значения по модели будут не лишними
summary(mod2)
#дисперсионный анализ - это тоже важно
anova(mod2)
#и графики, куда же без них
plot(mod2)
# Больше графиков богу графиков :)
#А теперь мы создадим третью модель, скомпактив вторую
formula3 = h2o_flux ~ (DOY+file_records+Tau+qc_Tau+rand_err_Tau+H+qc_H+rand_err_H+LE+qc_LE+rand_err_LE+co2_flux+qc_co2_flux+rand_err_co2_flux+rand_err_h2o_flux+`co2_v-adv`+`h2o_v-adv`+co2_mole_fraction+co2_mixing_ratio+h2o_molar_density+h2o_mole_fraction+h2o_mixing_ratio+h2o_time_lag+sonic_temperature+air_temperature+air_pressure+air_density+air_heat_capacity+water_vapor_density+e+es+RH+VPD+Tdew+u_unrot+v_unrot+ w_unrot+u_rot +v_rot+max_speed+wind_dir+yaw+pitch +TKE +L +bowen_ratio +`T*` + x_peak +x_offset +`x_10%`+`x_30%`+`x_50%`+`x_70%`+`x_90%`+un_Tau+Tau_scf+un_H+H_scf+un_LE+LE_scf+un_co2_flux+un_h2o_flux+ts_var)
mod3 = lm(formula3, data = Teaching_tbl)
# И повторим всё остальное
# коэффициенты
coefficients(mod3)
# предсказанные моделью значения
predict(mod3)
# потом узнаем остатки
resid(mod3)
# доверительный интервал
confint(mod3, level = 0.95)
#общие значения по модели
summary(mod3)
#дисперсионный анализ 
anova(mod3)
#и графики
plot(mod3)

# и скомпактим нашу модель ещё сильнее...
formula4 = h2o_flux ~ (DOY+file_records+Tau+qc_Tau+rand_err_Tau+H+qc_H+rand_err_H+LE+qc_LE+rand_err_LE+co2_flux+qc_co2_flux+rand_err_co2_flux+rand_err_h2o_flux+`co2_v-adv`+`h2o_v-adv`+co2_mole_fraction+co2_mixing_ratio+h2o_molar_density+h2o_mole_fraction+h2o_mixing_ratio+h2o_time_lag+sonic_temperature+air_temperature+air_pressure+air_density+air_heat_capacity+water_vapor_density+e+es+VPD+u_unrot+v_unrot+u_rot +v_rot+max_speed+wind_dir+yaw+pitch +TKE +L +`T*` + x_peak +x_offset +`x_10%`+`x_30%`+`x_50%`+`x_70%`+un_Tau+Tau_scf+un_H+H_scf+un_LE+LE_scf+un_co2_flux+un_h2o_flux+ts_var)
mod4 = lm(formula4, data = Teaching_tbl)
# И повторим всё остальное
# коэффициенты
coefficients(mod4)
# предсказанные моделью значения
predict(mod4)
# потом узнаем остатки
resid(mod4)
# доверительный интервал
confint(mod4, level = 0.95)
#общие значения по модели
summary(mod4)
#дисперсионный анализ 
anova(mod4)
#и графики
plot(mod4)

# Так, конечно, можно продолжать очень долго, но можно я закончу скомпакчивание модели?
# Лучше сделать корреляционный анализ :)
cor_Teaching_tbl = select(Teaching_tbl, DOY,file_records,Tau,qc_Tau,rand_err_Tau,H,qc_H,rand_err_H,LE,qc_LE,rand_err_LE,co2_flux,qc_co2_flux,rand_err_co2_flux,rand_err_h2o_flux,`co2_v-adv`,`h2o_v-adv`,co2_mole_fraction,co2_mixing_ratio,h2o_molar_density,h2o_mole_fraction,h2o_mixing_ratio,h2o_time_lag,sonic_temperature,air_temperature,air_pressure,air_density,air_heat_capacity,water_vapor_density,e,es,VPD,u_unrot,v_unrot,u_rot,v_rot,max_speed,wind_dir,yaw,pitch,TKE,L,`T*`,x_peak,x_offset,`x_10%`,`x_30%`,`x_50%`,`x_70%`,un_Tau,Tau_scf,un_H,H_scf,un_LE,LE_scf,un_co2_flux,un_h2o_flux,ts_var)
cor_tbl = cor(cor_Teaching_tbl) %>% as.data.frame
#Построим графики по полученной модели
#Построим график h2o_flux от h2o_flux, использовав значения, полученные на модели 4, и на основе обучающей выборки
qplot(h2o_flux, h2o_flux, data = Teaching_tbl) + geom_line(aes(y = predict(mod4, Teaching_tbl)))
#Построим график h2o_flux от h2o_flux, использовав значения, полученные на модели 4, и на основе тестирующей выборки
qplot(h2o_flux, h2o_flux, data = Testing_tbl) + geom_line(aes(y = predict(mod4, Testing_tbl)))
#Примеры зависимости зависимости от зависимости
#Больше графиков богу графиков
qplot(DOY, h2o_flux, data = Testing_tbl) + geom_line(aes(y = predict(mod4, Testing_tbl)))
qplot(Tau, h2o_flux, data = Testing_tbl) + geom_line(aes(y = predict(mod4, Testing_tbl)))
qplot(co2_flux, h2o_flux, data = Testing_tbl) + geom_line(aes(y = predict(mod4, Testing_tbl)))
#Не стреляйте в хим-токсика, он программирует, как умеет
