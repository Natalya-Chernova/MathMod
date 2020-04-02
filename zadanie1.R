library(tidyverse)
library(rnoaa)
station_data = ghcnd_stations() 
write.csv(station_data, file="station_data.csv")
station_data = read.csv("station_data.csv")
simferopol = data.frame(id = "SIMFEROPOL",  latitude = 34.101626,  longitude = 44.955197)
simferopol_around = meteo_nearby_stations(lat_lon_df = simferopol, station_data = station_data,
                                    limit = 18, var = c("PRCP", "TAVG"),
                                    year_min = 2009, year_max = 2016)
simferopol_id = simferopol_around[["SIMFEROPOL"]][["id"]][1]
all_simferopol_data = meteo_tidy_ghcnd(stationid = simferopol_id)
summary(simferopol_id)
y=1
Kf=300
Qj =1600
Lj=2.2
Ej=25

