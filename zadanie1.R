#Наверное, лучше это всё-таки как-то комментировать, чтобы вы поверили, 
#что я понимаю, что делаю
#для региона 82 рассчитать урожайность пшеницы в период с 2009 по 2016 год
#взяв для рассчета средние суммы активных температур за эти годы, 
#с 18 ближайших метеостанций
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
#сюда мы будем загружать данные с метеостанции

all_i=data.frame()

# а сюда со всех метеостанций

all_simferopol_meteodata = data.frame()

# а теперь делаем цикл, чтобы не делать так 18 раз

for(i in 1:18)
{
simferopol_id=simferopol_around[["SIMFEROPOL"]][["id"]]
print(i)
print(simferopol_id)
}
#Зададим интересующие нас даты
all_i = meteo_tidy_ghcnd(stationid = simferopol_id[i], var="TAVG", date_min = "2009-01-01", date_max = "2016-12-31")

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
St>5

