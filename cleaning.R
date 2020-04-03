library(dplyr)
library(lubridate)

#' Identify suitable flying conditions for FLIR using hourly weather data from
#' 
#' @param wind dataframe field storing wind speed
#' @param dewT dataframe field storing dewpoint temperature
#' @param bublT dataframe field storing dry bulb temperature
#' @param type dataframe field storing weather type
#' @return boolean 
test_flir <- function(wind, dewT, bulbT, type){
  windspeed <- as.numeric(wind) < 11
  temp <- (as.numeric(bulbT) - as.numeric(dewT)) > 3
  precip <- !grepl("SN|RA|FG", type)
  goodConditions <- windspeed & temp & precip
  return(goodConditions)
}

file1 <- read.csv(file='data-raw/Hourly_01Jan11_08Mar20.csv', header = TRUE, na.strings = "", stringsAsFactors = FALSE)%>%
  mutate(DATE = ymd_hms(DATE),
         FLIR = test_flir(HourlyWindSpeed, HourlyDewPointTemperature, HourlyDryBulbTemperature, HourlyPresentWeatherType)
         )

file2 <- read.csv(file='data-raw/Hourly_01Sep02_31Dec10.csv', header = TRUE, na.strings = "", stringsAsFactors = FALSE)%>%
  mutate(DATE = ymd_hms(DATE),
         FLIR = test_flir(HourlyWindSpeed, HourlyDewPointTemperature, HourlyDryBulbTemperature, HourlyPresentWeatherType)
  )

file3 <- read.csv(file='data-raw/Hourly_01Jan97_31Dec99.csv', header = TRUE, na.strings = "", stringsAsFactors = FALSE)%>%
  mutate(DATE = ymd_hms(DATE),
         FLIR = test_flir(HourlyWindSpeed, HourlyDewPointTemperature, HourlyDryBulbTemperature, HourlyPresentWeatherType)
  )

dat <- bind_rows(select(file1, DATE, FLIR), select(file2, DATE, FLIR), select(file3, DATE, FLIR))

cleandata <- dat%>%
  mutate(Month = month(DATE),
         Year = year(DATE),
         Hour = hour(DATE))


