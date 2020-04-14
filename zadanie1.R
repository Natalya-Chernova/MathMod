#Наверное, лучше это всё-таки как-то комментировать, чтобы вы поверили, 
#что я понимаю, что делаю
#для региона 82 рассчитать урожайность пшеницы в период с 2009 по 2016 год
#взяв для рассчета средние суммы активных температур за эти годы, 
#с 18 ближайших метеостанций

rm(list=ls())
library(tidyverse)
library(rnoaa)
# Скачаем данные с метеостанций и сохраним результат

station_data = ghcnd_stations() 

write.csv(station_data, file="station_data.csv")

station_data = read.csv("station_data.csv")

#список станций ближайших к столице моего региона,создав таблицу с именем региона и координатами его столицы

simferopol = data.frame(id = "SIMFEROPOL",  latitude = 34.101626,  longitude = 44.955197)

#выбираем количество станций, имеющих необходимые данные за заданный временной период

simferopol_around = meteo_nearby_stations(lat_lon_df = simferopol, station_data = station_data,
                                    limit = 18, var = c("PRCP", "TAVG"),
                                    year_min = 2009, year_max = 2016)

#simferpol_around это список единственным элементом которого является таблица, 
#содержащая идентификаторы метеостанций отсортированных по их 
# удаленности от Симферополя, первым элементом таблицы будет идентификатор метеостанции Тулы, 
#его то мы и попытаемся получить

simferopol_id = simferopol_around[["SIMFEROPOL"]][["id"]][1]

# Получаем данные с одной метеостанции

all_simferopol_data = meteo_tidy_ghcnd(stationid = simferopol_id)

# Смотрим все данные с метеостанции

summary(simferopol_id)

# промежуточный объект, куда мы будем загружать данные с метеостанции

all_i=data.frame()

# а сюда со всех метеостанций

all_simferopol_meteodata = data.frame()

# а теперь делаем цикл, чтобы не делать так 18 раз

for(i in 1:18)
{
simferopol_id=simferopol_around[["SIMFEROPOL"]][["id"]]
print(i)
print(simferopol_id)
all_i = meteo_tidy_ghcnd(stationid = simferopol_id[i], var="TAVG", date_min = "2009-01-01", date_max = "2016-12-31")
all_i  =  all_i [, c ( " id " , " date " , " tavg "  )]
all_simferopol_meteodata=rbind(all_simferopol_meteodata, all_i)
}
# Записываем все результаты
write.csv(all_simferopol_meteodata, "all_simferopol_meteodata.csv")
# посмотрим таблички
view(all_simferopol_data)
view(all_simferopol_meteodata)
str(all_simferopol_meteodata)
# добавим колонку со средней температурой
all_simferopol_meteodata = mutate(all_simferopol_data, 
                               tavg = (all_simferopol_data$tmax + all_simferopol_data$tmin)/2)
#посмотрим табличку ещё раз
view(all_simferopol_meteodata)
#и сохраним её, чтобы не потерять :)
write.csv(all_simferopol_meteodata, "all_simferopol_meteodata.csv")
# а теперь откроем
all_simferopol_meteodata=read.csv("all_simferopol_meteodata.csv")
# и посмотрим
str(all_simferopol_meteodata)
library(lubridate)
# а теперь добавим день, месяц, год
all_simferopol_meteodata = mutate(all_simferopol_meteodata, year = year(date), 
                               month = month(date), day = day(date))

# и посмотрим, что получилось
str(all_simferopol_meteodata)
#отфильтруем данные за 2009-2016 года
years_simferopol_meteodata =filter(all_simferopol_meteodata, year %in% c(2009:2016))
#проверим результат
str(years_simferopol_meteodata)
summary(years_simferopol_meteodata)
# Разделим температуру на 10, чтобы привести в нормальный вид
years_simferopol_meteodata[,"tavg"] = years_simferopol_meteodata$tavg/10
summary (years_simferopol_meteodata)
# Превратим в нули все NA и где tavg<5 
years_simferopol_meteodata[is.na(years_simferopol_meteodata$tavg),"tavg"] = 0
years_simferopol_meteodata[years_simferopol_meteodata$tavg<5, "tavg"] = 0
#проверяем
summary(years_simferopol_meteodata)
# группируем по метеостанциям, годам и месяцам
alldays = group_by(years_simferopol_meteodata,id,year,month)
#просуммируем температуру по этим группам
sumT_alldays_simferopol = summarize(alldays, tsum = sum(tavg))
sumT_alldays_simferopol
# максимальная температура за месяц = 859
859/31
# то есть, средняя дневная температура равна 27,7, что вполне адекватно
summary(sumT_alldays_simferopol) 
# Сгруппируем данные по месяцам  
groups_simferopol_months = group_by(sumT_alldays_simferopol,month)
groups_simferopol_months
# найдем для всех метеостанций и всех лет среднее значение температуры по месяцам
sumT_months = summarize(groups_simferopol_months, St = mean(tsum))
sumT_months

#Зададим все необходимые для расчёта переменные
y=1
afi=c(0.00, 0.00, 0.00, 32.11, 26.34, 25.64, 23.20, 18.73, 16.30, 13.83, 0.00, 0.00)
bfi=c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03, 8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
di=c(0.00, 0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)
Kf=300
Qj=1600
Lj=2.2
Ej=25
i={1;2;3;4;5;6;7;8;9;10;11;12}
# Рассчитаем Fi по месяцам
sumT_months = mutate(sumT_months, Fi = afi+bfi*y*St)
#Рассчитаем Yi
sumT_months = mutate(sumT_months, Yi = ((Fi*di)*Kf)/(Qj*Lj*(100 - Ej)))
##  Расчитываем урожай 
Yield = sum(sumT_months$Yi);  
Yield
# Полученное значение - 17,66532 ц/га. Средняя урожайность пшеницы в Крыму - 17,2 ц/га.
# И да прибудет с нами Лиссоз!=)