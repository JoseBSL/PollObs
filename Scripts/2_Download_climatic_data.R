
#Install packages
install.packages("rdwd")
#Load library
library(rdwd)
library(dplyr)
library(tibble)

#Leipzig----
#Find possible weather stations for Leipzig
findID("Leipzig", exactmatch=FALSE)
#Retrieve data link with variables of interest
link <- selectDWD("Leipzig-Holzhausen", res="hourly",  per="recent",
                  var=c("air_temperature","precipitation"))
#Dowload data
file = dataDWD(link, read=FALSE)
#Generate climatic file
leipzig = readDWD(file, varnames = T)
leipzig_temp = as_tibble(leipzig$hourly_air_temperature_recent_stundenwerte_TU_02928_akt)
leipzig_rain = as_tibble(leipzig$hourly_precipitation_recent_stundenwerte_RR_02928_akt)

#Merge files
leipzig_weather = left_join(leipzig_temp, leipzig_rain)

#select cols of interest and rename
leipzig_weather = leipzig_weather %>% 
select(!starts_with("QN")) %>% 
select(!c("eor", "RS_IND.Niederschlagsindikator",
          "WRTR.Niederschlagsform")) %>% 
rename(Weather_station = STATIONS_ID) %>% 
rename(Date_time = MESS_DATUM) %>% 
rename(Temperature = TT_TU.Lufttemperatur) %>% 
rename(Humidity = RF_TU.Relative_Feuchte) %>% 
rename(Rainfall = R1.Niederschlagshoehe) %>% 
mutate(Weather_station = "Leipzig")
#Save 
saveRDS(leipzig_weather, "Data/Working_files/leipzig_weather.rds")

#Halle----
#Find possible weather stations for Halle
findID("Halle", exactmatch=FALSE)
#For Halle this seems the best one (located in the airport...)
link <- selectDWD("Leipzig/Halle", res="hourly",  per="recent",
                  var=c("air_temperature","precipitation"))
#Dowload data
file = dataDWD(link, read=FALSE)
#Generate climatic file
halle = readDWD(file, varnames = T)
halle_temp = as_tibble(halle$hourly_air_temperature_recent_stundenwerte_TU_02932_akt)
halle_rain = as_tibble(halle$hourly_precipitation_recent_stundenwerte_RR_02932_akt)

#Merge files
halle_weather = left_join(halle_temp, halle_rain)

#select cols of interest and rename
halle_weather = halle_weather %>% 
select(!starts_with("QN")) %>% 
select(!c("eor", "RS_IND.Niederschlagsindikator",
          "WRTR.Niederschlagsform")) %>% 
rename(Weather_station = STATIONS_ID) %>% 
rename(Date_time = MESS_DATUM) %>% 
rename(Temperature = TT_TU.Lufttemperatur) %>% 
rename(Humidity = RF_TU.Relative_Feuchte) %>% 
rename(Rainfall = R1.Niederschlagshoehe) %>% 
mutate(Weather_station = "Halle")
#Save 
saveRDS(halle_weather, "Data/Working_files/halle_weather.rds")

#Jena----
#Find possible weather stations for Jena
findID("Jena", exactmatch=FALSE)
#For Jena this seems the best one (located in the city)
link <- selectDWD("Jena (Sternwarte)", res="hourly",  per="recent",
                  var=c("air_temperature","precipitation"))
#Dowload data
file = dataDWD(link, read=FALSE)
#Generate climatic file
jena = readDWD(file, varnames = T)
jena_temp = as_tibble(jena$hourly_air_temperature_recent_stundenwerte_TU_02444_akt)
jena_rain = as_tibble(jena$hourly_precipitation_recent_stundenwerte_RR_02444_akt)
#jena_wind = as_tibble(jena$hourly_wind_recent_stundenwerte_FF_02932_akt)
#Merge files
jena_weather = left_join(jena_temp, jena_rain)

#select cols of interest and rename
jena_weather = jena_weather %>% 
select(!starts_with("QN")) %>% 
select(!c("eor", "RS_IND.Niederschlagsindikator",
          "WRTR.Niederschlagsform")) %>% 
rename(Weather_station = STATIONS_ID) %>% 
rename(Date_time = MESS_DATUM) %>% 
rename(Temperature = TT_TU.Lufttemperatur) %>% 
rename(Humidity = RF_TU.Relative_Feuchte) %>% 
rename(Rainfall = R1.Niederschlagshoehe) %>% 
mutate(Weather_station = "Jena")
#Save 
saveRDS(jena_weather, "Data/Working_files/jena_weather.rds")



