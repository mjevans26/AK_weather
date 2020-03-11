library(dplyr)
library(lubridate)

file1 <- read.csv(file='data-raw/Hourly_01Jan11_08Mar20.csv', header = TRUE, na.strings = "", stringsAsFactors = FALSE)%>%
  mutate(DATE = ymd_hms(DATE))
file2 <- read.csv(file='data-raw/Hourly_01Sep02_31Dec10.csv', header = TRUE, na.strings = "", stringsAsFactors = FALSE)%>%
  mutate(DATE = ymd_hms(DATE))
file3 <- read.csv(file='data-raw/Hourly_01Jan97_31Dec99.csv', header = TRUE, na.strings = "", stringsAsFactors = FALSE)%>%
  mutate(DATE = ymd_hms(DATE))

dat <- bind_rows(file1, file3)

cleandata <- select(file1, grep("Hourly", colnames(file1)))

cleandata <- bind_cols(file1[,1:4], cleandata)%>%
  mutate(Month = month(DATE))%>%
  filter(Month == 1|Month == 12)

file3 <- mutate(file3, FLIR = as.numeric(HourlyWindSpeed)<11 &
                  (as.numeric(HourlyDewPointTemperature) - as.numeric(HourlyDryBulbTemperature)) >3 &
                  !c("RA", "SN", 'FG') %in% HourlyPresentWeatherType)
file1 <- mutate(file1, FLIR = as.numeric(HourlyWindSpeed)<11 &
                  (as.numeric(HourlyDewPointTemperature) - as.numeric(HourlyDryBulbTemperature)) >3 &
                  !c("RA", "SN", 'FG') %in% HourlyPresentWeatherType)
