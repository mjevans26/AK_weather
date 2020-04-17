library(dplyr)
library(lubridate)

# https://www.ncdc.noaa.gov/cdo-web/datasets/LCD/stations/WBAN:27401/detail

#' Identify suitable flying conditions for FLIR using hourly weather data
#' 
#' @description Wilson & Durner (2019) describe optimal FLIR conditions as
#' "surface wind speeds <11 km/hr;
#'  dew point‐ambient temp spread of >3.0°C,
#'  and no visible mois- ture such as fog or precipitation"
#' 
#' @param wind dataframe field storing wind speed
#' @param dewT dataframe field storing dewpoint temperature
#' @param bublT dataframe field storing dry bulb temperature
#' @param type dataframe field storing weather type
#' @return boolean 
test_flir <- function(wind, dewT, bulbT, type){
  windspeed <- as.numeric(wind) < 11
  temp <- (as.numeric(bulbT) - as.numeric(dewT)) > 3
  precip <- !grepl("SN|RA|FG|PL|GR|HZ|DZ|BR", type)
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
  mutate(Day = day(DATE),
         Month = month(DATE),
         Year = year(DATE),
         Hour = hour(DATE))

# Define Date-Hour combos during which there is daylight, which is unsuitable
# https://www.timeanddate.com/sun/usa/kaktovik

# daylight hours during january and december at kaktovik AK
daylight <- list('jan31'= seq(10, 14),
                 'jan25'= seq(11, 14),
                 'jan22'= seq(11, 13),
                 'jan18'= seq(12,13)
)


daylight <- function(Month, Day, Hour){
  if(Month == 1 & Day >= 18 & Day < 22 & Hour %in% daylight[['jan18']]){
    'yes'
  }else if(Month == 1 & Day >= 22 & Day <25 & Hour %in% daylight[['jan22']]){
    'yes'
  }else if(Month == 1 & Day >= 25 & Day <31 & Hour %in% daylight[['jan25']]){
    'yes'
  }else if(Month == 1 & Day == 31 & Hour %in% daylight[['jan31']]){
    'yes'
  }else{
    'no'
  }
}

cleandata$daylight <- FALSE
cleandata$daylight[cleandata$Month == 1 & cleandata$Day >= 18 & cleandata$Day < 22 & cleandata$Hour %in% seq(12,13)] <- TRUE
cleandata$daylight[cleandata$Month == 1 & cleandata$Day >= 22 & cleandata$Day <25 & cleandata$Hour %in% seq(11,13)] <- TRUE
cleandata$daylight[cleandata$Month == 1 & cleandata$Day >= 25 & cleandata$Day <31 & cleandata$Hour %in% seq(11,14)] <- TRUE
cleandata$daylight[cleandata$Month == 1 & cleandata$Day == 31 & cleandata$Hour %in% seq(10,14)] <- TRUE

saveRDS(cleandata, file = 'data/cleandata.rds')
