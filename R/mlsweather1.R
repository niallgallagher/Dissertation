library(dplyr)
library(lubridate)


#Load in dataset
mls.fixtures = read.csv('C:/Users/niall/OneDrive/Documents/Dissertation/Data/matches.csv')

#Filter Fixtures after 2012
mls.fixtures.new <- filter(mls.fixtures, year >= 2012)

mls.fixtures.new <- mls.fixtures.new[ c(1:6,8) ]

#Changing 
mls.fixtures.new$month <- sub('.*,', '', mls.fixtures.new$date)

mls.fixtures.new$month <- strsplit(paste(mls.fixtures.new$month, collapse=', '), ' ')[[1]]     

mls.fixtures.new$date2 <- paste(mls.fixtures.new$month,mls.fixtures.new$year)

mls.fixtures.new <- mls.fixtures.new %>% 
  mutate(converted_date = parse_date_time(date2, 
                                          orders = c("mdy", "dmy", "ymd")))

mls.fixtures.final <- mls.fixtures.new[ c(1:3,6,7,10) ]

colnames(mls.fixtures.final)[4] ="time"

mls.fixtures.final$converted_date <- as.character(mls.fixtures.final$converted_date)

trial <- left_join(mls.fixtures.final, Allianz.weather, by=c('venue'='Stadium', 'time'='time','converted_date'='date'))


#Allianz Field Weather
Allianz.weather = read.csv('C:/Users/niall/OneDrive/Documents/Dissertation/Weather/Allianz_Field.csv')

Allianz.weather$date <- substr(Allianz.weather$time, 1, 10)

Allianz.weather$time <- substr(Allianz.weather$time, 12, 16)



