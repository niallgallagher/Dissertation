library(dplyr)
library(lubridate)
library(readxl)
library(tidyverse)
library(plyr)

#Load in dataset
mls.fixtures = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Match_Updated.xlsx',guess_max = 100000)

#Filter Fixtures after 2012
#mls.fixtures.new <- filter(mls.fixtures, year >= 2012)

#mls.fixtures.new <- mls.fixtures.new[ c(1:6,8) ]

mls.fixtures$time <- substr(mls.fixtures$Time, 12, 16)

mls.fixtures$Date <- as.character(mls.fixtures$Date)

mls.stadiums = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Data/stadiums.xlsx')

mls.fixtures$Venue[mls.fixtures$Venue == 'Stevens Stadium'] <- 'Buck Shaw Stadium'

mls.fixtures$Venue[mls.fixtures$Venue == 'Oracle Park'] <- 'Buck Shaw Stadium'

mls.fixtures$Venue[mls.fixtures$Venue == 'America First Field'] <- 'Rio Tinto Stadium'

mls.fixtures$Venue[mls.fixtures$Venue == 'Avaya Stadium'] <- 'PayPal Park'

mls.fixtures$Venue[mls.fixtures$Venue == 'Robert F. Kennedy Memorial Stadium'] <- 'RFK Stadium'

mls.fixtures$Venue[mls.fixtures$Venue == 'Olympic Stadium'] <- 'Stade Olympique'

mls.fixtures$Venue[mls.fixtures$Venue == 'MAPFRE Stadium'] <- 'Historic Crew Stadium'

mls.fixtures$Venue[mls.fixtures$Venue == 'PNC Stadium'] <- 'BBVA Stadium'

mls.fixtures$Venue[mls.fixtures$Venue == 'Saputo Stadium'] <- 'Stade Saputo'

mls.fixtures$Venue[mls.fixtures$Venue == 'Geodis Park'] <- 'GEODIS Park'

mls.fixtures$Venue[mls.fixtures$Venue == 'Shell Energy Stadium'] <- 'BBVA Stadium'

mls.fixtures$Venue[mls.fixtures$Venue == 'Citypark'] <- 'CityPark'

mls.fixtures$Venue[mls.fixtures$Venue == 'Citi Field'] <- 'Citi Field Stadium'

mls.fixtures$Venue[mls.fixtures$Venue == 'BC Place Stadium'] <- 'BC Place'

mls.fixtures$Venue[mls.fixtures$Venue == 'Banc of California Stadium'] <- 'BMO Stadium'

mls.fixtures$Venue[mls.fixtures$Venue == 'Lockhart Stadium'] <- 'Bank of America Stadium'

mls.fixtures$Venue[mls.fixtures$Venue == 'West End Stadium'] <- 'Nippert Stadium'

mls.fixtures$Venue[mls.fixtures$Venue == 'Pratt & Whitney Stadium at Rentschler Fi...'] <- 'Pratt & Whitney Stadium at Rentschler Field'

mls.fixtures$Venue[mls.fixtures$Venue == 'Rose Bowl'] <- 'Dignity Health Sports Park'

mls.fixtures$Venue[mls.fixtures$Venue == 'Maureen Hendricks Field Maryland SoccerP...'] <- 'Maryland SoccerPlex'

mls.fixtures$Venue[mls.fixtures$Venue == 'Navy–Marine Corps Memorial Stadium'] <- 'Navy-Marine Corps Memorial Stadium'

#Changing 
# mls.fixtures.new$month <- sub('.*,', '', mls.fixtures.new$date)
# 
# mls.fixtures.new$month <- strsplit(paste(mls.fixtures.new$month, collapse=', '), ' ')[[1]]     
# 
# mls.fixtures.new$date2 <- paste(mls.fixtures.new$month,mls.fixtures.new$year)
# 
# mls.fixtures.new <- mls.fixtures.new %>% 
#   mutate(converted_date = parse_date_time(date2, 
#                                           orders = c("mdy", "dmy", "ymd")))
# 
# mls.fixtures.final <- mls.fixtures.new[ c(1:3,6,7,10) ]
# 
# colnames(mls.fixtures.final)[4] ="time"
# 
# mls.fixtures.final$converted_date <- as.character(mls.fixtures.final$converted_date)

mls.fixtures.dates <- sort(unique(mls.fixtures$Date))

#trial <- left_join(mls.fixtures, Allianz.weather, by=c('Venue'='Stadium', 'time'='time','Date'='date'))



#Allianz Field Weather Stadium Number:1
Allianz.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/AllianzField.xlsx')

Allianz.weather$date <- substr(Allianz.weather$time, 1, 10)

Allianz.weather$time <- substr(Allianz.weather$time, 12, 16)

Allianz.weather <- subset(Allianz.weather, date %in% mls.fixtures.dates)

#trial <- left_join(mls.fixtures, weatherstadium2, by=c('Venue'='Stadium', 'time'='time','Date'='date'))

#trial2 <- left_join(trial, mls.stadiums, by=c('Venue'='Stadium Name'))

sum(is.na(trial2$Roof))

print("Location of missing values in runs column") 
which(is.na(trial2$Roof)) 


#Audi Field Weather Stadium Number:2
Audi.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/AudiField.xlsx')

Audi.weather$date <- substr(Audi.weather$time, 1, 10)

Audi.weather$time <- substr(Audi.weather$time, 12, 16)

Audi.weather <- subset(Audi.weather, date %in% mls.fixtures.dates)


#colnames(trial3)

#trial2 <- left_join(trial, Audi.weather, by=c('Venue'='Stadium', 'time'='time','Date'='date', 'temperature_2m (°C)'='temperature_2m (°C)',
#                                               'relative_humidity_2m (%)'= 'relative_humidity_2m (%)','dew_point_2m (°C)' = 'dew_point_2m (°C)',
#                                               'apparent_temperature (°C)' = 'apparent_temperature (°C)','precipitation (mm)' = 'precipitation (mm)',
#                                               'rain (mm)'='rain (mm)','snowfall (cm)'='snowfall (cm)','cloud_cover (%)'= 'cloud_cover (%)',
#                                               'wind_speed_10m (km/h)'='wind_speed_10m (km/h)','soil_temperature_0_to_7cm (°C)' = 'soil_temperature_0_to_7cm (°C)',
#                                               'soil_moisture_0_to_7cm (m³/m³)' = 'soil_moisture_0_to_7cm (m³/m³)','is_day ()'='is_day ()'))

#weatherstadium2 <- rbind(Audi.weather,Allianz.weather)


#test23 <- left_join(mls.fixtures, weatherstadium2, by=c('Venue'='Stadium'))

#BBVA Weather Stadium Number:3
BBVA.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/BBVAStadium.xlsx')

BBVA.weather$date <- substr(BBVA.weather$time, 1, 10)

BBVA.weather$time <- substr(BBVA.weather$time, 12, 16)

BBVA.weather <- subset(BBVA.weather, date %in% mls.fixtures.dates)


Stadium3 <- rbind(BBVA.weather,Allianz.weather,Audi.weather)
#trial <- left_join(mls.fixtures, Stadium3, by=c('Venue'='Stadium', 'time'='time','Date'='date'))


#BC Place Stadium Number:4
BCPlace.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/BCPlace.xlsx')

BCPlace.weather$date <- substr(BCPlace.weather$time, 1, 10)

BCPlace.weather$time <- substr(BCPlace.weather$time, 12, 16)

BCPlace.weather <- subset(BCPlace.weather, date %in% mls.fixtures.dates)


#Stadium4 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather)
#trial <- left_join(mls.fixtures, Stadium4, by=c('Venue'='Stadium', 'time'='time','Date'='date'))

#Bank of America Stadium Number:5
BOA.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/BankofAmerica.xlsx')

BOA.weather$Stadium <- rep("Bank of America Stadium",84768)
BOA.weather$date <- substr(BOA.weather$time, 1, 10)

BOA.weather$time <- substr(BOA.weather$time, 12, 16)

BOA.weather <- subset(BOA.weather, date %in% mls.fixtures.dates)

#Stadium5 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather)
#trial <- left_join(mls.fixtures, Stadium5, by=c('Venue'='Stadium', 'time'='time','Date'='date'))

#BMO Stadium Number:6
BMO.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/BMOField.xlsx')

BMO.weather$date <- substr(BMO.weather$time, 1, 10)

BMO.weather$time <- substr(BMO.weather$time, 12, 16)

BMO.weather <- subset(BMO.weather, date %in% mls.fixtures.dates)




#Bobby Dodd Stadium Number:7
BbD.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/BobbyDoddStadium.xlsx')

BbD.weather$date <- substr(BbD.weather$time, 1, 10)

BbD.weather$time <- substr(BbD.weather$time, 12, 16)

BbD.weather <- subset(BbD.weather, date %in% mls.fixtures.dates)

Stadium7 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather,BMO.weather,BbD.weather)
trial <- left_join(mls.fixtures, Stadium7, by=c('Venue'='Stadium', 'time'='time','Date'='date'))

#Buck Shaw Stadium Number:8
Buck.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/BuckShawStadium.xlsx')

Buck.weather$date <- substr(Buck.weather$time, 1, 10)

Buck.weather$time <- substr(Buck.weather$time, 12, 16)

Buck.weather <- subset(Buck.weather, date %in% mls.fixtures.dates)

Stadium8 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather,BMO.weather,BbD.weather,Buck.weather)
trial <- left_join(mls.fixtures, Stadium8, by=c('Venue'='Stadium', 'time'='time','Date'='date'))

#BMO Stadium Number:9
BMOStad.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/BMOStadium.xlsx')

BMOStad.weather$date <- substr(BMOStad.weather$time, 1, 10)

BMOStad.weather$time <- substr(BMOStad.weather$time, 12, 16)

BMOStad.weather <- subset(BMOStad.weather, date %in% mls.fixtures.dates)

Stadium9 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather,BMO.weather,BbD.weather,Buck.weather,BMOStad.weather)
trial <- left_join(mls.fixtures, Stadium9, by=c('Venue'='Stadium', 'time'='time','Date'='date'))

#Camping World Stadium Number:10
Camp.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/CampingWorldStadium.xlsx')

Camp.weather$date <- substr(Camp.weather$time, 1, 10)

Camp.weather$time <- substr(Camp.weather$time, 12, 16)

Camp.weather <- subset(Camp.weather, date %in% mls.fixtures.dates)

Stadium10 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather,BMO.weather,BbD.weather,Buck.weather,BMOStad.weather,Camp.weather)
trial <- left_join(mls.fixtures, Stadium10, by=c('Venue'='Stadium', 'time'='time','Date'='date'))

#Lumen Field Stadium Number:11
Lumen.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/LumenField.xlsx')

Lumen.weather$date <- substr(Lumen.weather$time, 1, 10)

Lumen.weather$time <- substr(Lumen.weather$time, 12, 16)

Lumen.weather <- subset(Lumen.weather, date %in% mls.fixtures.dates)

Stadium11 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather,BMO.weather,BbD.weather,Buck.weather,BMOStad.weather,Camp.weather,
                   Lumen.weather)
trial <- left_join(mls.fixtures, Stadium11, by=c('Venue'='Stadium', 'time'='time','Date'='date'))

#Kansas Stadium Number:12
ChildrenMercyStad.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/ChildrenMercyPark.xlsx')

ChildrenMercyStad.weather$date <- substr(ChildrenMercyStad.weather$time, 1, 10)

ChildrenMercyStad.weather$time <- substr(ChildrenMercyStad.weather$time, 12, 16)

ChildrenMercyStad.weather <- subset(ChildrenMercyStad.weather, date %in% mls.fixtures.dates)

Stadium12 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather,BMO.weather,BbD.weather,Buck.weather,BMOStad.weather,Camp.weather,
                   Lumen.weather,ChildrenMercyStad.weather)
trial <- left_join(mls.fixtures, Stadium12, by=c('Venue'='Stadium', 'time'='time','Date'='date'))

#Citi Field Stadium Number:13
CitiField.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/CitiStadium.xlsx')

CitiField.weather$date <- substr(CitiField.weather$time, 1, 10)

CitiField.weather$time <- substr(CitiField.weather$time, 12, 16)

CitiField.weather <- subset(CitiField.weather, date %in% mls.fixtures.dates)

Stadium13 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather,BMO.weather,BbD.weather,Buck.weather,BMOStad.weather,Camp.weather,
                   Lumen.weather,ChildrenMercyStad.weather,CitiField.weather)
trial <- left_join(mls.fixtures, Stadium13, by=c('Venue'='Stadium', 'time'='time','Date'='date'))

#Dick's Sporting Goods Park Number:14
SportingGoodPark.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/DickSportingStadium.xlsx')

SportingGoodPark.weather$date <- substr(SportingGoodPark.weather$time, 1, 10)

SportingGoodPark.weather$time <- substr(SportingGoodPark.weather$time, 12, 16)

SportingGoodPark.weather <- subset(SportingGoodPark.weather, date %in% mls.fixtures.dates)

Stadium14 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather,BMO.weather,BbD.weather,Buck.weather,BMOStad.weather,Camp.weather,
                   Lumen.weather,ChildrenMercyStad.weather,CitiField.weather,SportingGoodPark.weather)

trial <- left_join(mls.fixtures, Stadium14, by=c('Venue'='Stadium', 'time'='time','Date'='date'))

#Dignity Health Sports Park Number:15
DignityHealthPark.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/DignityHealthPark.xlsx')

DignityHealthPark.weather$date <- substr(DignityHealthPark.weather$time, 1, 10)

DignityHealthPark.weather$time <- substr(DignityHealthPark.weather$time, 12, 16)

DignityHealthPark.weather <- subset(DignityHealthPark.weather, date %in% mls.fixtures.dates)

Stadium15 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather,BMO.weather,BbD.weather,Buck.weather,BMOStad.weather,Camp.weather,
                   Lumen.weather,ChildrenMercyStad.weather,CitiField.weather,SportingGoodPark.weather,DignityHealthPark.weather)

trial <- left_join(mls.fixtures, Stadium15, by=c('Venue'='Stadium', 'time'='time','Date'='date'))

#DRV PNK Stadium Number:16
DRVPNKStadium.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/DRVPNKStadium.xlsx')

DRVPNKStadium.weather$date <- substr(DRVPNKStadium.weather$time, 1, 10)

DRVPNKStadium.weather$time <- substr(DRVPNKStadium.weather$time, 12, 16)

DRVPNKStadium.weather <- subset(DRVPNKStadium.weather, date %in% mls.fixtures.dates)

Stadium16 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather,BMO.weather,BbD.weather,Buck.weather,BMOStad.weather,Camp.weather,
                   Lumen.weather,ChildrenMercyStad.weather,CitiField.weather,SportingGoodPark.weather,DignityHealthPark.weather,DRVPNKStadium.weather)

trial <- left_join(mls.fixtures, Stadium16, by=c('Venue'='Stadium', 'time'='time','Date'='date'))

#PayPal Park Number:17
PayPal.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/PayPalPark.xlsx')

PayPal.weather$date <- substr(PayPal.weather$time, 1, 10)

PayPal.weather$time <- substr(PayPal.weather$time, 12, 16)

PayPal.weather <- subset(PayPal.weather, date %in% mls.fixtures.dates)

Stadium17 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather,BMO.weather,BbD.weather,Buck.weather,BMOStad.weather,Camp.weather,
                   Lumen.weather,ChildrenMercyStad.weather,CitiField.weather,SportingGoodPark.weather,DignityHealthPark.weather,DRVPNKStadium.weather,
                   PayPal.weather)

trial <- left_join(mls.fixtures, Stadium17, by=c('Venue'='Stadium', 'time'='time','Date'='date'))

#ESPN Sports Complex Number:18
ESPN.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/ESPNComplex.xlsx')

ESPN.weather$date <- substr(ESPN.weather$time, 1, 10)

ESPN.weather$time <- substr(ESPN.weather$time, 12, 16)

ESPN.weather <- subset(ESPN.weather, date %in% mls.fixtures.dates)

Stadium18 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather,BMO.weather,BbD.weather,Buck.weather,BMOStad.weather,Camp.weather,
                   Lumen.weather,ChildrenMercyStad.weather,CitiField.weather,SportingGoodPark.weather,DignityHealthPark.weather,DRVPNKStadium.weather,
                   PayPal.weather,ESPN.weather)

trial <- left_join(mls.fixtures, Stadium18, by=c('Venue'='Stadium', 'time'='time','Date'='date'))

#Exploria Stadium Number:19
Exploria.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/ExploriaStadium.xlsx')

Exploria.weather$date <- substr(Exploria.weather$time, 1, 10)

Exploria.weather$time <- substr(Exploria.weather$time, 12, 16)

Exploria.weather <- subset(Exploria.weather, date %in% mls.fixtures.dates)

Stadium19 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather,BMO.weather,BbD.weather,Buck.weather,BMOStad.weather,Camp.weather,
                   Lumen.weather,ChildrenMercyStad.weather,CitiField.weather,SportingGoodPark.weather,DignityHealthPark.weather,DRVPNKStadium.weather,
                   PayPal.weather,ESPN.weather,Exploria.weather)

trial <- left_join(mls.fixtures, Stadium19, by=c('Venue'='Stadium', 'time'='time','Date'='date'))

#FedEx Field Number:20
FedExField.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/FedExField.xlsx')

FedExField.weather$date <- substr(FedExField.weather$time, 1, 10)

FedExField.weather$time <- substr(FedExField.weather$time, 12, 16)

FedExField.weather <- subset(FedExField.weather, date %in% mls.fixtures.dates)

Stadium20 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather,BMO.weather,BbD.weather,Buck.weather,BMOStad.weather,Camp.weather,
                   Lumen.weather,ChildrenMercyStad.weather,CitiField.weather,SportingGoodPark.weather,DignityHealthPark.weather,DRVPNKStadium.weather,
                   PayPal.weather,ESPN.weather,Exploria.weather,FedExField.weather)

trial <- left_join(mls.fixtures, Stadium20, by=c('Venue'='Stadium', 'time'='time','Date'='date'))

#GEODIS Park Number:21
GEODIS.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/GEODISPark.xlsx')

GEODIS.weather$date <- substr(GEODIS.weather$time, 1, 10)

GEODIS.weather$time <- substr(GEODIS.weather$time, 12, 16)

GEODIS.weather <- subset(GEODIS.weather, date %in% mls.fixtures.dates)

Stadium21 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather,BMO.weather,BbD.weather,Buck.weather,BMOStad.weather,Camp.weather,
                   Lumen.weather,ChildrenMercyStad.weather,CitiField.weather,SportingGoodPark.weather,DignityHealthPark.weather,DRVPNKStadium.weather,
                   PayPal.weather,ESPN.weather,Exploria.weather,FedExField.weather,GEODIS.weather)

trial <- left_join(mls.fixtures, Stadium21, by=c('Venue'='Stadium', 'time'='time','Date'='date'))

#Gillete Stadium Number:22
Gillete.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/GillteteStadium.xlsx')

Gillete.weather$date <- substr(Gillete.weather$time, 1, 10)

Gillete.weather$time <- substr(Gillete.weather$time, 12, 16)

Gillete.weather <- subset(Gillete.weather, date %in% mls.fixtures.dates)

Stadium22 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather,BMO.weather,BbD.weather,Buck.weather,BMOStad.weather,Camp.weather,
                   Lumen.weather,ChildrenMercyStad.weather,CitiField.weather,SportingGoodPark.weather,DignityHealthPark.weather,DRVPNKStadium.weather,
                   PayPal.weather,ESPN.weather,Exploria.weather,FedExField.weather,GEODIS.weather,Gillete.weather)

trial <- left_join(mls.fixtures, Stadium22, by=c('Venue'='Stadium', 'time'='time','Date'='date'))

#Historic Crew Stadium:23
Crew.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/CrewsStadium.xlsx')

Crew.weather$date <- substr(Crew.weather$time, 1, 10)

Crew.weather$time <- substr(Crew.weather$time, 12, 16)

Crew.weather <- subset(Crew.weather, date %in% mls.fixtures.dates)

Stadium23 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather,BMO.weather,BbD.weather,Buck.weather,BMOStad.weather,Camp.weather,
                   Lumen.weather,ChildrenMercyStad.weather,CitiField.weather,SportingGoodPark.weather,DignityHealthPark.weather,DRVPNKStadium.weather,
                   PayPal.weather,ESPN.weather,Exploria.weather,FedExField.weather,GEODIS.weather,Gillete.weather,Crew.weather)

trial <- left_join(mls.fixtures, Stadium23, by=c('Venue'='Stadium', 'time'='time','Date'='date'))

#Levi's Stadium:24
Levi.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/LeviStadium.xlsx')

Levi.weather$date <- substr(Levi.weather$time, 1, 10)

Levi.weather$time <- substr(Levi.weather$time, 12, 16)

Levi.weather <- subset(Levi.weather, date %in% mls.fixtures.dates)

Stadium24 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather,BMO.weather,BbD.weather,Buck.weather,BMOStad.weather,Camp.weather,
                   Lumen.weather,ChildrenMercyStad.weather,CitiField.weather,SportingGoodPark.weather,DignityHealthPark.weather,DRVPNKStadium.weather,
                   PayPal.weather,ESPN.weather,Exploria.weather,FedExField.weather,GEODIS.weather,Gillete.weather,Crew.weather,Levi.weather)

trial <- left_join(mls.fixtures, Stadium24, by=c('Venue'='Stadium', 'time'='time','Date'='date'))

#Levi's Stadium:24
Levi.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/LeviStadium.xlsx')

Levi.weather$date <- substr(Levi.weather$time, 1, 10)

Levi.weather$time <- substr(Levi.weather$time, 12, 16)

Levi.weather <- subset(Levi.weather, date %in% mls.fixtures.dates)

Stadium24 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather,BMO.weather,BbD.weather,Buck.weather,BMOStad.weather,Camp.weather,
                   Lumen.weather,ChildrenMercyStad.weather,CitiField.weather,SportingGoodPark.weather,DignityHealthPark.weather,DRVPNKStadium.weather,
                   PayPal.weather,ESPN.weather,Exploria.weather,FedExField.weather,GEODIS.weather,Gillete.weather,Crew.weather,Levi.weather)

trial <- left_join(mls.fixtures, Stadium24, by=c('Venue'='Stadium', 'time'='time','Date'='date'))

#Lower.com Field:25
LowerField.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/LowerField.xlsx')

LowerField.weather$date <- substr(LowerField.weather$time, 1, 10)

LowerField.weather$time <- substr(LowerField.weather$time, 12, 16)

LowerField.weather <- subset(LowerField.weather, date %in% mls.fixtures.dates)

Stadium25 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather,BMO.weather,BbD.weather,Buck.weather,BMOStad.weather,Camp.weather,
                   Lumen.weather,ChildrenMercyStad.weather,CitiField.weather,SportingGoodPark.weather,DignityHealthPark.weather,DRVPNKStadium.weather,
                   PayPal.weather,ESPN.weather,Exploria.weather,FedExField.weather,GEODIS.weather,Gillete.weather,Crew.weather,Levi.weather,
                   LowerField.weather)

trial <- left_join(mls.fixtures, Stadium25, by=c('Venue'='Stadium', 'time'='time','Date'='date'))

#Merceedes-Benz Stadium:26
MerceedesBenz.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/MerceedesBenz.xlsx')

MerceedesBenz.weather$date <- substr(MerceedesBenz.weather$time, 1, 10)

MerceedesBenz.weather$time <- substr(MerceedesBenz.weather$time, 12, 16)

MerceedesBenz.weather <- subset(MerceedesBenz.weather, date %in% mls.fixtures.dates)

Stadium26 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather,BMO.weather,BbD.weather,Buck.weather,BMOStad.weather,Camp.weather,
                   Lumen.weather,ChildrenMercyStad.weather,CitiField.weather,SportingGoodPark.weather,DignityHealthPark.weather,DRVPNKStadium.weather,
                   PayPal.weather,ESPN.weather,Exploria.weather,FedExField.weather,GEODIS.weather,Gillete.weather,Crew.weather,Levi.weather,
                   LowerField.weather,MerceedesBenz.weather)

trial <- left_join(mls.fixtures, Stadium26, by=c('Venue'='Stadium', 'time'='time','Date'='date'))

#Nippert Stadium:27
Nippert.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/NippertStadium.xlsx')

Nippert.weather$date <- substr(Nippert.weather$time, 1, 10)

Nippert.weather$time <- substr(Nippert.weather$time, 12, 16)

Nippert.weather <- subset(Nippert.weather, date %in% mls.fixtures.dates)

Stadium27 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather,BMO.weather,BbD.weather,Buck.weather,BMOStad.weather,Camp.weather,
                   Lumen.weather,ChildrenMercyStad.weather,CitiField.weather,SportingGoodPark.weather,DignityHealthPark.weather,DRVPNKStadium.weather,
                   PayPal.weather,ESPN.weather,Exploria.weather,FedExField.weather,GEODIS.weather,Gillete.weather,Crew.weather,Levi.weather,
                   LowerField.weather,MerceedesBenz.weather, Nippert.weather)

trial <- left_join(mls.fixtures, Stadium27, by=c('Venue'='Stadium', 'time'='time','Date'='date'))

#Nissan Stadium:28
Nissan.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/NissanStadium.xlsx')

Nissan.weather$date <- substr(Nissan.weather$time, 1, 10)

Nissan.weather$time <- substr(Nissan.weather$time, 12, 16)

Nissan.weather <- subset(Nissan.weather, date %in% mls.fixtures.dates)

Stadium28 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather,BMO.weather,BbD.weather,Buck.weather,BMOStad.weather,Camp.weather,
                   Lumen.weather,ChildrenMercyStad.weather,CitiField.weather,SportingGoodPark.weather,DignityHealthPark.weather,DRVPNKStadium.weather,
                   PayPal.weather,ESPN.weather,Exploria.weather,FedExField.weather,GEODIS.weather,Gillete.weather,Crew.weather,Levi.weather,
                   LowerField.weather,MerceedesBenz.weather, Nippert.weather, Nissan.weather)

trial <- left_join(mls.fixtures, Stadium28, by=c('Venue'='Stadium', 'time'='time','Date'='date'))


#Pratt Stadium:29
Pratt.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/PrattStadium.xlsx')

Pratt.weather$date <- substr(Pratt.weather$time, 1, 10)

Pratt.weather$time <- substr(Pratt.weather$time, 12, 16)

Pratt.weather <- subset(Pratt.weather, date %in% mls.fixtures.dates)

Stadium29 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather,BMO.weather,BbD.weather,Buck.weather,BMOStad.weather,Camp.weather,
                   Lumen.weather,ChildrenMercyStad.weather,CitiField.weather,SportingGoodPark.weather,DignityHealthPark.weather,DRVPNKStadium.weather,
                   PayPal.weather,ESPN.weather,Exploria.weather,FedExField.weather,GEODIS.weather,Gillete.weather,Crew.weather,Levi.weather,
                   LowerField.weather,MerceedesBenz.weather, Nippert.weather, Nissan.weather, Pratt.weather)

trial <- left_join(mls.fixtures, Stadium29, by=c('Venue'='Stadium', 'time'='time','Date'='date'))

#Providence Park:30
ProvidencePark.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/ProvidencePark.xlsx')

ProvidencePark.weather$date <- substr(ProvidencePark.weather$time, 1, 10)

ProvidencePark.weather$time <- substr(ProvidencePark.weather$time, 12, 16)

ProvidencePark.weather <- subset(ProvidencePark.weather, date %in% mls.fixtures.dates)

Stadium30 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather,BMO.weather,BbD.weather,Buck.weather,BMOStad.weather,Camp.weather,
                   Lumen.weather,ChildrenMercyStad.weather,CitiField.weather,SportingGoodPark.weather,DignityHealthPark.weather,DRVPNKStadium.weather,
                   PayPal.weather,ESPN.weather,Exploria.weather,FedExField.weather,GEODIS.weather,Gillete.weather,Crew.weather,Levi.weather,
                   LowerField.weather,MerceedesBenz.weather, Nippert.weather, Nissan.weather, Pratt.weather, ProvidencePark.weather)

trial <- left_join(mls.fixtures, Stadium30, by=c('Venue'='Stadium', 'time'='time','Date'='date'))

#Q2 Stadium:31
q2stadium.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/Q2Stadium.xlsx')

q2stadium.weather$date <- substr(q2stadium.weather$time, 1, 10)

q2stadium.weather$time <- substr(q2stadium.weather$time, 12, 16)

q2stadium.weather <- subset(q2stadium.weather, date %in% mls.fixtures.dates)

Stadium31 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather,BMO.weather,BbD.weather,Buck.weather,BMOStad.weather,Camp.weather,
                   Lumen.weather,ChildrenMercyStad.weather,CitiField.weather,SportingGoodPark.weather,DignityHealthPark.weather,DRVPNKStadium.weather,
                   PayPal.weather,ESPN.weather,Exploria.weather,FedExField.weather,GEODIS.weather,Gillete.weather,Crew.weather,Levi.weather,
                   LowerField.weather,MerceedesBenz.weather, Nippert.weather, Nissan.weather, Pratt.weather, ProvidencePark.weather, q2stadium.weather)

trial <- left_join(mls.fixtures, Stadium31, by=c('Venue'='Stadium', 'time'='time','Date'='date'))

#Red Bull Arena Stadium:32
redBull.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/RedBullArena.xlsx')

redBull.weather$date <- substr(redBull.weather$time, 1, 10)

redBull.weather$time <- substr(redBull.weather$time, 12, 16)

redBull.weather <- subset(redBull.weather, date %in% mls.fixtures.dates)


Stadium32 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather,BMO.weather,BbD.weather,Buck.weather,BMOStad.weather,Camp.weather,
                   Lumen.weather,ChildrenMercyStad.weather,CitiField.weather,SportingGoodPark.weather,DignityHealthPark.weather,DRVPNKStadium.weather,
                   PayPal.weather,ESPN.weather,Exploria.weather,FedExField.weather,GEODIS.weather,Gillete.weather,Crew.weather,Levi.weather,
                   LowerField.weather,MerceedesBenz.weather, Nippert.weather, Nissan.weather, Pratt.weather, ProvidencePark.weather, q2stadium.weather,
                   redBull.weather)

trial <- left_join(mls.fixtures, Stadium32, by=c('Venue'='Stadium', 'time'='time','Date'='date'))

#RFK Stadium:33
rfk.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/RFKStadium.xlsx')

rfk.weather$date <- substr(rfk.weather$time, 1, 10)

rfk.weather$time <- substr(rfk.weather$time, 12, 16)

rfk.weather <- subset(rfk.weather, date %in% mls.fixtures.dates)

Stadium33 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather,BMO.weather,BbD.weather,Buck.weather,BMOStad.weather,Camp.weather,
                   Lumen.weather,ChildrenMercyStad.weather,CitiField.weather,SportingGoodPark.weather,DignityHealthPark.weather,DRVPNKStadium.weather,
                   PayPal.weather,ESPN.weather,Exploria.weather,FedExField.weather,GEODIS.weather,Gillete.weather,Crew.weather,Levi.weather,
                   LowerField.weather,MerceedesBenz.weather, Nippert.weather, Nissan.weather, Pratt.weather, ProvidencePark.weather, q2stadium.weather,
                   redBull.weather,rfk.weather)

trial <- left_join(mls.fixtures, Stadium33, by=c('Venue'='Stadium', 'time'='time','Date'='date'))

#Rino Tinto:34
rioTinto.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/RioTintoStadium.xlsx')

rioTinto.weather$date <- substr(rioTinto.weather$time, 1, 10)

rioTinto.weather$time <- substr(rioTinto.weather$time, 12, 16)

rioTinto.weather <- subset(rioTinto.weather, date %in% mls.fixtures.dates)

Stadium34 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather,BMO.weather,BbD.weather,Buck.weather,BMOStad.weather,Camp.weather,
                   Lumen.weather,ChildrenMercyStad.weather,CitiField.weather,SportingGoodPark.weather,DignityHealthPark.weather,DRVPNKStadium.weather,
                   PayPal.weather,ESPN.weather,Exploria.weather,FedExField.weather,GEODIS.weather,Gillete.weather,Crew.weather,Levi.weather,
                   LowerField.weather,MerceedesBenz.weather, Nippert.weather, Nissan.weather, Pratt.weather, ProvidencePark.weather, q2stadium.weather,
                   redBull.weather,rfk.weather,rioTinto.weather)

trial <- left_join(mls.fixtures, Stadium34, by=c('Venue'='Stadium', 'time'='time','Date'='date'))

#SeatGeek Stadium:35
seatGeek.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/SeatGeekStadium.xlsx')

seatGeek.weather$date <- substr(seatGeek.weather$time, 1, 10)

seatGeek.weather$time <- substr(seatGeek.weather$time, 12, 16)

seatGeek.weather <- subset(seatGeek.weather, date %in% mls.fixtures.dates)

Stadium35 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather,BMO.weather,BbD.weather,Buck.weather,BMOStad.weather,Camp.weather,
                   Lumen.weather,ChildrenMercyStad.weather,CitiField.weather,SportingGoodPark.weather,DignityHealthPark.weather,DRVPNKStadium.weather,
                   PayPal.weather,ESPN.weather,Exploria.weather,FedExField.weather,GEODIS.weather,Gillete.weather,Crew.weather,Levi.weather,
                   LowerField.weather,MerceedesBenz.weather, Nippert.weather, Nissan.weather, Pratt.weather, ProvidencePark.weather, q2stadium.weather,
                   redBull.weather,rfk.weather,rioTinto.weather,seatGeek.weather)

trial <- left_join(mls.fixtures, Stadium35, by=c('Venue'='Stadium', 'time'='time','Date'='date'))

#Soldier Field Stadium:36
soldier.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/SoldierField.xlsx')

soldier.weather$Stadium <- rep("Soldier Field",207504)

soldier.weather$date <- substr(soldier.weather$time, 1, 10)

soldier.weather$time <- substr(soldier.weather$time, 12, 16)

soldier.weather <- subset(soldier.weather, date %in% mls.fixtures.dates)

Stadium36 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather,BMO.weather,BbD.weather,Buck.weather,BMOStad.weather,Camp.weather,
                   Lumen.weather,ChildrenMercyStad.weather,CitiField.weather,SportingGoodPark.weather,DignityHealthPark.weather,DRVPNKStadium.weather,
                   PayPal.weather,ESPN.weather,Exploria.weather,FedExField.weather,GEODIS.weather,Gillete.weather,Crew.weather,Levi.weather,
                   LowerField.weather,MerceedesBenz.weather, Nippert.weather, Nissan.weather, Pratt.weather, ProvidencePark.weather, q2stadium.weather,
                   redBull.weather,rfk.weather,rioTinto.weather,seatGeek.weather,soldier.weather)

trial <- left_join(mls.fixtures, Stadium36, by=c('Venue'='Stadium', 'time'='time','Date'='date'))

#Stade Olympique Stadium:37
stadeolympique.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/StadeOlympique.xlsx')

stadeolympique.weather$Stadium <- rep("Stade Olympique",207504)

stadeolympique.weather$date <- substr(stadeolympique.weather$time, 1, 10)

stadeolympique.weather$time <- substr(stadeolympique.weather$time, 12, 16)

stadeolympique.weather <- subset(stadeolympique.weather, date %in% mls.fixtures.dates)

Stadium37 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather,BMO.weather,BbD.weather,Buck.weather,BMOStad.weather,Camp.weather,
                   Lumen.weather,ChildrenMercyStad.weather,CitiField.weather,SportingGoodPark.weather,DignityHealthPark.weather,DRVPNKStadium.weather,
                   PayPal.weather,ESPN.weather,Exploria.weather,FedExField.weather,GEODIS.weather,Gillete.weather,Crew.weather,Levi.weather,
                   LowerField.weather,MerceedesBenz.weather, Nippert.weather, Nissan.weather, Pratt.weather, ProvidencePark.weather, q2stadium.weather,
                   redBull.weather,rfk.weather,rioTinto.weather,seatGeek.weather,soldier.weather,stadeolympique.weather)

trial <- left_join(mls.fixtures, Stadium37, by=c('Venue'='Stadium', 'time'='time','Date'='date'))

#Stade Saputo Stadium:38
stadesaputo.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/StadeSaputo.xlsx')

stadesaputo.weather$Stadium <- rep("Stade Saputo",207504)

stadesaputo.weather$date <- substr(stadesaputo.weather$time, 1, 10)

stadesaputo.weather$time <- substr(stadesaputo.weather$time, 12, 16)

stadesaputo.weather <- subset(stadesaputo.weather, date %in% mls.fixtures.dates)

Stadium38 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather,BMO.weather,BbD.weather,Buck.weather,BMOStad.weather,Camp.weather,
                   Lumen.weather,ChildrenMercyStad.weather,CitiField.weather,SportingGoodPark.weather,DignityHealthPark.weather,DRVPNKStadium.weather,
                   PayPal.weather,ESPN.weather,Exploria.weather,FedExField.weather,GEODIS.weather,Gillete.weather,Crew.weather,Levi.weather,
                   LowerField.weather,MerceedesBenz.weather, Nippert.weather, Nissan.weather, Pratt.weather, ProvidencePark.weather, q2stadium.weather,
                   redBull.weather,rfk.weather,rioTinto.weather,seatGeek.weather,soldier.weather,stadeolympique.weather,stadesaputo.weather)

trial <- left_join(mls.fixtures, Stadium38, by=c('Venue'='Stadium', 'time'='time','Date'='date'))

#Stanford Stadium:39
stanford.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/StanfordStadium.xlsx')

stanford.weather$Stadium <- rep("Stanford Stadium",207504)

stanford.weather$date <- substr(stanford.weather$time, 1, 10)

stanford.weather$time <- substr(stanford.weather$time, 12, 16)

stanford.weather <- subset(stanford.weather, date %in% mls.fixtures.dates)

Stadium39 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather,BMO.weather,BbD.weather,Buck.weather,BMOStad.weather,Camp.weather,
                   Lumen.weather,ChildrenMercyStad.weather,CitiField.weather,SportingGoodPark.weather,DignityHealthPark.weather,DRVPNKStadium.weather,
                   PayPal.weather,ESPN.weather,Exploria.weather,FedExField.weather,GEODIS.weather,Gillete.weather,Crew.weather,Levi.weather,
                   LowerField.weather,MerceedesBenz.weather, Nippert.weather, Nissan.weather, Pratt.weather, ProvidencePark.weather, q2stadium.weather,
                   redBull.weather,rfk.weather,rioTinto.weather,seatGeek.weather,soldier.weather,stadeolympique.weather,stadesaputo.weather,
                   stanford.weather)

trial <- left_join(mls.fixtures, Stadium39, by=c('Venue'='Stadium', 'time'='time','Date'='date'))

#Subaru Park:40
subarupark.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/SubaruPark.xlsx')

subarupark.weather$Stadium <- rep("Subaru Park",207504)

subarupark.weather$date <- substr(subarupark.weather$time, 1, 10)

subarupark.weather$time <- substr(subarupark.weather$time, 12, 16)

subarupark.weather <- subset(subarupark.weather, date %in% mls.fixtures.dates)

Stadium40 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather,BMO.weather,BbD.weather,Buck.weather,BMOStad.weather,Camp.weather,
                   Lumen.weather,ChildrenMercyStad.weather,CitiField.weather,SportingGoodPark.weather,DignityHealthPark.weather,DRVPNKStadium.weather,
                   PayPal.weather,ESPN.weather,Exploria.weather,FedExField.weather,GEODIS.weather,Gillete.weather,Crew.weather,Levi.weather,
                   LowerField.weather,MerceedesBenz.weather, Nippert.weather, Nissan.weather, Pratt.weather, ProvidencePark.weather, q2stadium.weather,
                   redBull.weather,rfk.weather,rioTinto.weather,seatGeek.weather,soldier.weather,stadeolympique.weather,stadesaputo.weather,
                   stanford.weather,subarupark.weather)

trial <- left_join(mls.fixtures, Stadium40, by=c('Venue'='Stadium', 'time'='time','Date'='date'))

#TCF Bank Stadium:41
tcfbank.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/TCFBankStadium.xlsx')

tcfbank.weather$Stadium <- rep("TCF Bank Stadium",34080)

tcfbank.weather$date <- substr(tcfbank.weather$time, 1, 10)

tcfbank.weather$time <- substr(tcfbank.weather$time, 12, 16)

tcfbank.weather <- subset(tcfbank.weather, date %in% mls.fixtures.dates)

Stadium41 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather,BMO.weather,BbD.weather,Buck.weather,BMOStad.weather,Camp.weather,
                   Lumen.weather,ChildrenMercyStad.weather,CitiField.weather,SportingGoodPark.weather,DignityHealthPark.weather,DRVPNKStadium.weather,
                   PayPal.weather,ESPN.weather,Exploria.weather,FedExField.weather,GEODIS.weather,Gillete.weather,Crew.weather,Levi.weather,
                   LowerField.weather,MerceedesBenz.weather, Nippert.weather, Nissan.weather, Pratt.weather, ProvidencePark.weather, q2stadium.weather,
                   redBull.weather,rfk.weather,rioTinto.weather,seatGeek.weather,soldier.weather,stadeolympique.weather,stadesaputo.weather,
                   stanford.weather,subarupark.weather,tcfbank.weather)

trial <- left_join(mls.fixtures, Stadium41, by=c('Venue'='Stadium', 'time'='time','Date'='date'))

#Toyota Stadium:42
toyota.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/ToyotaStadium.xlsx')

toyota.weather$Stadium <- rep("Toyota Stadium",207504)

toyota.weather$date <- substr(toyota.weather$time, 1, 10)

toyota.weather$time <- substr(toyota.weather$time, 12, 16)

toyota.weather <- subset(toyota.weather, date %in% mls.fixtures.dates)

Stadium42 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather,BMO.weather,BbD.weather,Buck.weather,BMOStad.weather,Camp.weather,
                   Lumen.weather,ChildrenMercyStad.weather,CitiField.weather,SportingGoodPark.weather,DignityHealthPark.weather,DRVPNKStadium.weather,
                   PayPal.weather,ESPN.weather,Exploria.weather,FedExField.weather,GEODIS.weather,Gillete.weather,Crew.weather,Levi.weather,
                   LowerField.weather,MerceedesBenz.weather, Nippert.weather, Nissan.weather, Pratt.weather, ProvidencePark.weather, q2stadium.weather,
                   redBull.weather,rfk.weather,rioTinto.weather,seatGeek.weather,soldier.weather,stadeolympique.weather,stadesaputo.weather,
                   stanford.weather,subarupark.weather,tcfbank.weather,toyota.weather)

trial <- left_join(mls.fixtures, Stadium42, by=c('Venue'='Stadium', 'time'='time','Date'='date'))

#TQL Stadium:43
tql.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/TQLStadium.xlsx')

tql.weather$Stadium <- rep("TQL Stadium",32160)

tql.weather$date <- substr(tql.weather$time, 1, 10)

tql.weather$time <- substr(tql.weather$time, 12, 16)

tql.weather <- subset(tql.weather, date %in% mls.fixtures.dates)

Stadium43 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather,BMO.weather,BbD.weather,Buck.weather,BMOStad.weather,Camp.weather,
                   Lumen.weather,ChildrenMercyStad.weather,CitiField.weather,SportingGoodPark.weather,DignityHealthPark.weather,DRVPNKStadium.weather,
                   PayPal.weather,ESPN.weather,Exploria.weather,FedExField.weather,GEODIS.weather,Gillete.weather,Crew.weather,Levi.weather,
                   LowerField.weather,MerceedesBenz.weather, Nippert.weather, Nissan.weather, Pratt.weather, ProvidencePark.weather, q2stadium.weather,
                   redBull.weather,rfk.weather,rioTinto.weather,seatGeek.weather,soldier.weather,stadeolympique.weather,stadesaputo.weather,
                   stanford.weather,subarupark.weather,tcfbank.weather,toyota.weather,tql.weather)

trial <- left_join(mls.fixtures, Stadium43, by=c('Venue'='Stadium', 'time'='time','Date'='date'))

#Yankee Stadium:44
yankee.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/YankeeStadium.xlsx')

yankee.weather$Stadium <- rep("Yankee Stadium",154896)

yankee.weather$date <- substr(yankee.weather$time, 1, 10)

yankee.weather$time <- substr(yankee.weather$time, 12, 16)

yankee.weather <- subset(yankee.weather, date %in% mls.fixtures.dates)

Stadium44 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather,BMO.weather,BbD.weather,Buck.weather,BMOStad.weather,Camp.weather,
                   Lumen.weather,ChildrenMercyStad.weather,CitiField.weather,SportingGoodPark.weather,DignityHealthPark.weather,DRVPNKStadium.weather,
                   PayPal.weather,ESPN.weather,Exploria.weather,FedExField.weather,GEODIS.weather,Gillete.weather,Crew.weather,Levi.weather,
                   LowerField.weather,MerceedesBenz.weather, Nippert.weather, Nissan.weather, Pratt.weather, ProvidencePark.weather, q2stadium.weather,
                   redBull.weather,rfk.weather,rioTinto.weather,seatGeek.weather,soldier.weather,stadeolympique.weather,stadesaputo.weather,
                   stanford.weather,subarupark.weather,tcfbank.weather,toyota.weather,tql.weather,yankee.weather)

trial <- left_join(mls.fixtures, Stadium44, by=c('Venue'='Stadium', 'time'='time','Date'='date'))

#cityPark Stadium:45
citypark.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/CityPark.xlsx')

citypark.weather$Stadium <- rep("CityPark",14640)

citypark.weather$date <- substr(citypark.weather$time, 1, 10)

citypark.weather$time <- substr(citypark.weather$time, 12, 16)

citypark.weather <- subset(citypark.weather, date %in% mls.fixtures.dates)

Stadium45 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather,BMO.weather,BbD.weather,Buck.weather,BMOStad.weather,Camp.weather,
                   Lumen.weather,ChildrenMercyStad.weather,CitiField.weather,SportingGoodPark.weather,DignityHealthPark.weather,DRVPNKStadium.weather,
                   PayPal.weather,ESPN.weather,Exploria.weather,FedExField.weather,GEODIS.weather,Gillete.weather,Crew.weather,Levi.weather,
                   LowerField.weather,MerceedesBenz.weather, Nippert.weather, Nissan.weather, Pratt.weather, ProvidencePark.weather, q2stadium.weather,
                   redBull.weather,rfk.weather,rioTinto.weather,seatGeek.weather,soldier.weather,stadeolympique.weather,stadesaputo.weather,
                   stanford.weather,subarupark.weather,tcfbank.weather,toyota.weather,tql.weather,yankee.weather,citypark.weather)

trial <- left_join(mls.fixtures, Stadium45, by=c('Venue'='Stadium', 'time'='time','Date'='date'))


sum(is.na(trial$`is_day ()`))

#Other Stadium:46
other.weather = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/OtherStadiums.xlsx')

other.weather$date <- substr(other.weather$time, 1, 10)

other.weather$time <- substr(other.weather$time, 12, 16)

other.weather <- subset(other.weather, date %in% mls.fixtures.dates)

Stadium46 <- rbind(BBVA.weather,Allianz.weather,Audi.weather,BCPlace.weather,BOA.weather,BMO.weather,BbD.weather,Buck.weather,BMOStad.weather,Camp.weather,
                   Lumen.weather,ChildrenMercyStad.weather,CitiField.weather,SportingGoodPark.weather,DignityHealthPark.weather,DRVPNKStadium.weather,
                   PayPal.weather,ESPN.weather,Exploria.weather,FedExField.weather,GEODIS.weather,Gillete.weather,Crew.weather,Levi.weather,
                   LowerField.weather,MerceedesBenz.weather, Nippert.weather, Nissan.weather, Pratt.weather, ProvidencePark.weather, q2stadium.weather,
                   redBull.weather,rfk.weather,rioTinto.weather,seatGeek.weather,soldier.weather,stadeolympique.weather,stadesaputo.weather,
                   stanford.weather,subarupark.weather,tcfbank.weather,toyota.weather,tql.weather,yankee.weather,citypark.weather,other.weather)

fixture.final <- left_join(mls.fixtures, Stadium46, by=c('Venue'='Stadium', 'time'='time','Date'='date'))


sum(is.na(Stadium46$`is_day ()`))

clipr::write_clip(fixture.final)



#Joining Stadiums into Fixtures Data
stadiums = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Data/Stadiums.xlsx')

fixtures = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/FixtureComplete.xlsx')

#assign match IDs to fixtures
fixtures$MatchID <- substr(fixtures$MatchURL, 30, 37) 

fixtures$Time <- substr(fixtures$Time, 12, 16)

sort(unique(fixtures$Venue))


#Assign Stadium IDs to fixtures data
fixtures <- fixtures %>%
  mutate(StadiumId = case_when(Venue == "Allianz Field" ~ 1, 
                                    Venue == "Audi Field" ~ 2,
                                    Venue == "Bank of America Stadium" ~ 4, 
                                    Venue == "BBVA Stadium" ~ 5,
                                    Venue == "BC Place" ~ 6,
                                    Venue == "BMO Field" ~ 7,
                                    Venue == "BMO Stadium" ~ 3,
                                    Venue ==  "Bobby Dodd Stadium" ~ 8,
                                    Venue == "Buck Shaw Stadium" ~ 9,
                                    Venue == "Camping World Stadium" ~ 10,
                                    Venue == "Children's Mercy Park" ~ 12,
                                    Venue == "Citi Field Stadium" ~ 13,
                                    Venue == "CityPark" ~ 47,
                                    Venue == "Dick's Sporting Goods Park" ~ 14,
                                    Venue == "Dignity Health Sports Park" ~ 15,
                                    Venue == "DRV PNK Stadium" ~ 16,
                                    Venue == "ESPN Wide World of Sports Complex" ~ 18,
                                    Venue == "Exploria Stadium" ~ 19, 
                                    Venue == "FedEx Field" ~ 20,
                                    Venue == "GEODIS Park" ~ 21,
                                    Venue == "Gillette Stadium" ~ 22,
                                    Venue == "Historic Crew Stadium" ~ 23,
                                    Venue ==  "Levi's Stadium" ~ 24,
                                    Venue == "Lower.com Field" ~ 25,
                                    Venue == "Lumen Field" ~ 11,
                                    Venue == "Maryland SoccerPlex" ~ 26,
                                    Venue == "Mercedes-Benz Stadium" ~ 27,
                                    Venue == "Navy-Marine Corps Memorial Stadium" ~ 28,
                                    Venue == "Nippert Stadium" ~ 29,
                                    Venue == "Nissan Stadium" ~ 30,
                                    Venue == "PayPal Park" ~ 17,
                                    Venue == "Pratt & Whitney Stadium at Rentschler Field" ~ 31,
                                    Venue == "Providence Park" ~ 32, 
                                    Venue == "Q2 Stadium" ~ 33,
                                    Venue == "Red Bull Arena" ~ 34,
                                    Venue == "RFK Stadium" ~ 35,
                                    Venue == "Rio Tinto Stadium" ~ 36,
                                    Venue == "SeatGeek Stadium" ~ 37,
                                    Venue == "Soldier Field" ~ 38,
                                    Venue == "Stade Olympique" ~ 39,
                                    Venue == "Stade Saputo" ~ 40,
                                    Venue == "Stanford Stadium" ~ 41,
                                    Venue == "Subaru Park" ~ 42,
                                    Venue == "TCF Bank Stadium" ~ 43,
                                    Venue == "Toyota Stadium" ~ 44,
                                    Venue == "TQL Stadium" ~ 45,
                                    Venue == "Yankee Stadium" ~ 46,
                                   ))

#Left join Stadiums
fixtures <- fixtures %>% left_join(stadiums, by=c('StadiumId' = 'StadiumId'))

#Selecting columns
fixtures <- fixtures %>% select(1:5,7,8,9,34,10:16,18,36,35,37,38,42,22:33,20)

clipr::write_clip(fixtures)

agg_tbl <- fixtures %>% group_by(Season_End_Year,Surface) %>% 
  summarise(total_count=n(),
            .groups = 'drop')
agg_tbl

agg_tbl <- fixtures %>% group_by(Season_End_Year,Surface) %>% 
  summarise(total_count=n(),.groups = 'drop') %>%
  as.data.frame()
agg_tbl


agg_df <- aggregate(fixtures$Surface, by=list(fixtures$Season_End_Year,fixtures$Surface), FUN=length)

#Line Chart of Graph
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)

don <- agg_df %>% 
  filter(Group.2 %in% c("Field Turf", "Grass"))

