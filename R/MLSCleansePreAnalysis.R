####Pre-Cleanse Analysis####
####Niall Gallagher#######

#Libraries
library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)
library(tidyverse)
library(data.table)

###Read the .xlsx files######
Player.game.data <- read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/CleansedDatasets/MLSPlayerGames.xlsx')

Player.fixtures.data <- read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/CleansedDatasets/MLSFixtures.xlsx')

Player.info.data <- read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/CleansedDatasets/PlayerInfo.xlsx')

Player.key.data <- read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/CleansedDatasets/PlayerKey.xlsx')

#stadiums.data <- read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/CleansedDatasets/stadiums.xlsx')

#modify injury date to merge transfermarkt and fbref
Player.injury.data$injured_since <- as.Date(Player.injury.data$injured_since, format = '%Y-%m-%d') 

Player.injury.data$injured_sincenew <- Player.injury.data$injured_since - days(1)

Player.injury.data$injured_sincenew2 <- Player.injury.data$injured_since + days(1)

Player.game.data <- Player.game.data %>% select(1:13,31,32)

#Joining Data 
Player.info.data.mod <- Player.info.data %>% left_join( Player.key.data, 
                                              by=c('URL'='URL',
                                                   'PlayerId' = 'PlayerId'))

Player.game.modified <- Player.game.data %>% left_join(Player.info.data.mod, 
                                                       by=c('Player'='PlayerFBref'))

#Modifying Date
Player.game.modified$Date <- as.Date(Player.game.modified$Date, format = '%Y-%m-%d') 

#Join Injury date with games
Player.game.modified.inj <- Player.game.modified %>% left_join(Player.injury.data, 
                                                       by=c('URL'='URL',
                                                            'Date'='injured_since'))

test <- Player.game.modified.inj[!is.na(Player.game.modified.inj$DaysInjured),]

#Join New Injury date with games
Player.game.modified.inj2 <- Player.game.modified %>% left_join(Player.injury.data, 
                                                               by=c('URL'='URL',
                                                                    'Date'='injured_sincenew'))

Player.game.modified.inj3 <- Player.game.modified %>% left_join(Player.injury.data, 
                                                                by=c('URL'='URL',
                                                                     'Date'='injured_sincenew2'))

test2 <- Player.game.modified.inj2[!is.na(Player.game.modified.inj2$DaysInjured),]

test3 <- Player.game.modified.inj3[!is.na(Player.game.modified.inj3$DaysInjured),]

#Modidy Injuries for MLS Games
mls.player.games <- subset(Player.game.modified, Comp %in% c("MLS")) 

mls.player.games <- mls.player.games %>% select(1,3,8,9,13:16,20:24,26,27)

unique_strings2 <- unique(data.frame(mls.player.games$Squad))

unique_strings <- unique(data.frame(Player.fixtures.data$Home))
#Changing Squad Names 
#1
mls.player.games$Squad[mls.player.games$Squad == "A Galaxy"] <- "LA Galaxy"
#2
mls.player.games$Squad[mls.player.games$Squad == "Chivas USA"] <- "Chivas USA"
#3
mls.player.games$Squad[mls.player.games$Squad == "Y Red Bulls"] <- "NY Red Bulls"
#4
mls.player.games$Squad[mls.player.games$Squad == "Sporting KC"] <- "Sporting KC"
#5
mls.player.games$Squad[mls.player.games$Squad == "D.C. United"] <- "D.C. United"
#5
mls.player.games$Squad[mls.player.games$Squad == "Philadelphia"] <- "Philadelphia"
#5
mls.player.games$Squad[mls.player.games$Squad == "Vancouver"] <- "Vancouver"
#5
mls.player.games$Squad[mls.player.games$Squad == "Portland Timbers"] <- "Portland Timbers"
#5
mls.player.games$Squad[mls.player.games$Squad == "San Jose"] <- "San Jose"
#5
mls.player.games$Squad[mls.player.games$Squad == "Chicago Fire"] <- "Chicago Fire"
#5
mls.player.games$Squad[mls.player.games$Squad == "Seattle"] <- "Seattle"
#5
mls.player.games$Squad[mls.player.games$Squad == "Colorado Rapids"] <- "Colorado Rapids"
#5
mls.player.games$Squad[mls.player.games$Squad == "Dynamo"] <- "Houston Dynamo"
#5
mls.player.games$Squad[mls.player.games$Squad == "Columbus Crew"] <- "Columbus Crew"
#5
mls.player.games$Squad[mls.player.games$Squad == "Montreal Impact"] <- "CF Montréal"
#5
mls.player.games$Squad[mls.player.games$Squad == "New England"] <- "New England Revolution"
#5
mls.player.games$Squad[mls.player.games$Squad == "Toronto FC"] <- "Toronto FC"
#5
mls.player.games$Squad[mls.player.games$Squad == "C Dallas"] <- "FC Dallas"
#5
mls.player.games$Squad[mls.player.games$Squad == "YCFC"] <- "NYCFC"
#5
mls.player.games$Squad[mls.player.games$Squad == "Orlando City"] <- "Orlando City"
#5
mls.player.games$Squad[mls.player.games$Squad == "Atlanta Utd"] <- "Atlanta Utd"
#5
mls.player.games$Squad[mls.player.games$Squad == "Minnesota Utd"] <- "Minnesota Utd"
#5
mls.player.games$Squad[mls.player.games$Squad == "Los Angeles FC"] <- "Los Angeles FC"
#5
mls.player.games$Squad[mls.player.games$Squad == "C Cincinnati"] <- "FC Cincinnati"
#5
mls.player.games$Squad[mls.player.games$Squad == "Nashville"] <- "Nashville SC"
#5
mls.player.games$Squad[mls.player.games$Squad == "Nashville SC"] <- "Nashville SC"
#5
mls.player.games$Squad[mls.player.games$Squad == "New England Revolution"] <- "New England Revolution"
#5
mls.player.games$Squad[mls.player.games$Squad == "Inter Miami"] <- "Inter Miami"
#5
mls.player.games$Squad[mls.player.games$Squad == "F Montréal"] <- "CF Montréal"
#5
mls.player.games$Squad[mls.player.games$Squad == "Dynamo FC"] <- "Houston Dynamo"
#5
mls.player.games$Squad[mls.player.games$Squad == "Austin"] <- "Austin"
#5
mls.player.games$Squad[mls.player.games$Squad == "Charlotte"] <- "Charlotte"
#5
mls.player.games$Squad[mls.player.games$Squad == "St. Louis"] <- "St. Louis"

#1
Player.fixtures.data$Home[Player.fixtures.data$Home == "Dynamo FC"] <- "Houston Dynamo"

Player.fixtures.data$Away[Player.fixtures.data$Away == "Dynamo FC"] <- "Houston Dynamo"

Player.fixtures.data$Home[Player.fixtures.data$Home == "Montreal Impact"] <- "CF Montréal"

Player.fixtures.data$Away[Player.fixtures.data$Away == "Montreal Impact"] <- "CF Montréal"

Player.fixtures.data$Home[Player.fixtures.data$Home == "Nashville"] <- "Nashville SC"

Player.fixtures.data$Away[Player.fixtures.data$Away == "Nashville"] <- "Nashville SC"

Player.fixtures.data$Home[Player.fixtures.data$Home == "New England"] <- "New England Revolution"

Player.fixtures.data$Away[Player.fixtures.data$Away == "New England"] <- "New England Revolution"


Player.fixtures.data$Date <- as.Date(Player.fixtures.data$Date, format = '%Y-%m-%d') 

mls.player.games$Date <- as.Date(mls.player.games$Date, format = '%Y-%m-%d') 

#mls.player.games$Squad2 <- mls.player.games$Squad
#Join Fixtures from injuries to Regular Fixtures
Player.game.modified.fixtures <- mls.player.games %>% left_join(Player.fixtures.data, 
                                                               by=c('Squad'='Home',
                                                                    'Date'='Date')) %>%
                                                      left_join(Player.fixtures.data, 
                                                                  by = c('Squad' = 'Away', 'Date' = 'Date'
                                                                         ))
#Removing Columns 
Player.game.modified.fixtures <- select(Player.game.modified.fixtures, -26,-29,-60,-62)

#Filing Values from Two Sources
Player.game.modified.fixtures$Season_End_Year <- coalesce(Player.game.modified.fixtures$Season_End_Year.x, Player.game.modified.fixtures$Season_End_Year.y)

Player.game.modified.fixtures$Round <- coalesce(Player.game.modified.fixtures$Round.x, Player.game.modified.fixtures$Round.y)

Player.game.modified.fixtures$Day <- coalesce(Player.game.modified.fixtures$Day.x, Player.game.modified.fixtures$Day.y)

Player.game.modified.fixtures$Time <- coalesce(Player.game.modified.fixtures$Time.x, Player.game.modified.fixtures$Time.y)

Player.game.modified.fixtures$MatchID <- coalesce(Player.game.modified.fixtures$MatchID.x, Player.game.modified.fixtures$MatchID.y)

Player.game.modified.fixtures$HomeGoals <- coalesce(Player.game.modified.fixtures$HomeGoals.x, Player.game.modified.fixtures$HomeGoals.y)

Player.game.modified.fixtures$AwayGoals <- coalesce(Player.game.modified.fixtures$AwayGoals.x, Player.game.modified.fixtures$AwayGoals.y)

Player.game.modified.fixtures$`Stadium Name` <- coalesce(Player.game.modified.fixtures$`Stadium Name.x`, Player.game.modified.fixtures$`Stadium Name.y`)

Player.game.modified.fixtures$StadiumId <- coalesce(Player.game.modified.fixtures$StadiumId.x, Player.game.modified.fixtures$StadiumId.y)

Player.game.modified.fixtures$Surface <- coalesce(Player.game.modified.fixtures$Surface.x, Player.game.modified.fixtures$Surface.y)

Player.game.modified.fixtures$Roof <- coalesce(Player.game.modified.fixtures$Roof.x, Player.game.modified.fixtures$Roof.y)

Player.game.modified.fixtures$`temperature_2m (°C)` <- coalesce(Player.game.modified.fixtures$`temperature_2m (°C).x`, Player.game.modified.fixtures$`temperature_2m (°C).y`)

Player.game.modified.fixtures$`relative_humidity_2m (%)` <- coalesce(Player.game.modified.fixtures$`relative_humidity_2m (%).x`, Player.game.modified.fixtures$`relative_humidity_2m (%).y`)

Player.game.modified.fixtures$`dew_point_2m (°C)` <- coalesce(Player.game.modified.fixtures$`dew_point_2m (°C).x`, Player.game.modified.fixtures$`dew_point_2m (°C).y`)

Player.game.modified.fixtures$`apparent_temperature (°C)` <- coalesce(Player.game.modified.fixtures$`apparent_temperature (°C).x`, Player.game.modified.fixtures$`apparent_temperature (°C).y`)

Player.game.modified.fixtures$`precipitation (mm)` <- coalesce(Player.game.modified.fixtures$`precipitation (mm).x`, Player.game.modified.fixtures$`precipitation (mm).y`)

Player.game.modified.fixtures$`rain (mm)` <- coalesce(Player.game.modified.fixtures$`rain (mm).x`, Player.game.modified.fixtures$`rain (mm).y`)

Player.game.modified.fixtures$`snowfall (cm)` <- coalesce(Player.game.modified.fixtures$`snowfall (cm).x`, Player.game.modified.fixtures$`snowfall (cm).y`)

Player.game.modified.fixtures$`cloud_cover (%)` <- coalesce(Player.game.modified.fixtures$`cloud_cover (%).x`, Player.game.modified.fixtures$`cloud_cover (%).y`)

Player.game.modified.fixtures$`wind_speed_10m (km/h)` <- coalesce(Player.game.modified.fixtures$`wind_speed_10m (km/h).x`, Player.game.modified.fixtures$`wind_speed_10m (km/h).y`)

Player.game.modified.fixtures$`soil_temperature_0_to_7cm (°C)` <- coalesce(Player.game.modified.fixtures$`soil_temperature_0_to_7cm (°C).x`, Player.game.modified.fixtures$`soil_temperature_0_to_7cm (°C).y`)

Player.game.modified.fixtures$`soil_moisture_0_to_7cm (m³/m³)` <- coalesce(Player.game.modified.fixtures$`soil_moisture_0_to_7cm (m³/m³).x`, Player.game.modified.fixtures$`soil_moisture_0_to_7cm (m³/m³).y`)

Player.game.modified.fixtures$`is_day ()` <- coalesce(Player.game.modified.fixtures$`is_day ().x`, Player.game.modified.fixtures$`is_day ().y`)

Player.game.modified.fixtures$MatchURL <- coalesce(Player.game.modified.fixtures$MatchURL.x, Player.game.modified.fixtures$MatchURL.y)

#Select Columns
Player.game.modified.fixtures2 <- select(Player.game.modified.fixtures, 1:16,26,79:102)

#Keep All rows 
df <- Player.game.modified.fixtures2[complete.cases(Player.game.modified.fixtures2$`soil_temperature_0_to_7cm (°C)`), ]

df <- select(df, -16,-21)

clipr::write_clip(df)

#Injuries with Matches
Player.injury.data <- read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/CleansedDatasets/MLSInjuries.xlsx')

Player.injury.data$injured_since <- as.Date(Player.injury.data$injured_since, format = '%Y-%m-%d') 

Player.injury.data <- Player.injury.data %>% arrange(FBrefPlayerId, injured_since)

Player.injury.data <- Player.injury.data %>%
  group_by(FBrefPlayerId) %>%
  mutate(InjuryCount = row_number()) %>%
  ungroup()

#Player Data Date
Player.mls.data <- read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/CleansedDatasets/UpdatePlayerFix.xlsx')

#MLS Date Format
Player.mls.data$Date <- as.Date(Player.mls.data$Date, format = '%Y-%m-%d') 

#unique_strings <- unique(data.frame(Player.injury.data$club))
##test 1
# specific_clubs <- c("Houston Dynamo FC", "Houston Dynamo", "Charlotte FC", "D.C. United",
#                     "Atlanta United FC", "Minnesota United FC", "Vancouver Whitecaps FC",
#                     "Columbus Crew SC", "Columbus Crew", "Chicago Fire", "Chicago Fire FC",
#                     "FC Cincinnati","Colorado Rapids", "FC Dallas", "Los Angeles FC",
#                     "Los Angeles Galaxy", "Inter Miami CF", "Montreal Impact", "Nashville SC",
#                     "New England Revolution","New York City FC", "Orlando City SC",
#                     "New York Red Bulls", "Philadelphia Union","Portland Timbers",
#                     "Real Salt Lake City", "San Jose Earthquakes", "Seattle Sounders FC",
#                     "Sporting Kansas City", "Toronto FC", "Austin FC", "Saint Louis FC", "St. Louis CITY SC"
#                     )
# 
# Player.injury.data <- Player.injury.data %>%
#   filter(club %in% specific_clubs)
# 
# year_threshold <- 2011
# 
# Player.injury.data <- Player.injury.data %>%
#   filter(as.integer(substring(injured_since, 1, 4)) > year_threshold)

setDT(Player.mls.data)
setDT(Player.injury.data)

Player.injury.data$Date <- Player.injury.data$injured_since

#Rolling Dates of Injury data and Player Data
test <- Player.mls.data[Player.injury.data, c("Date.y") := .(i.Date), on = .(URL, Date), roll = Inf]
test2 <- Player.mls.data[Player.injury.data, c("Date.y") := .(i.Date), on = .(URL, Date), roll = -Inf]

test$date_diff <- abs(test$Date - test$Date.y)

test2$date_diff <- abs(test2$Date - test2$Date.y)

# test$date_diff <- as.character(test$date_diff)
# specific_values <- c("0 days", "1 days")
# 
# df_filtered <- test %>%
#   filter(date_diff %in% specific_values)

clipr::write_clip(test)
clipr::write_clip(test2)

# test <- select(Player.mls.data, 2,13)
# 
# test2 <- select(Player.injury.data, 7,10 )
# 
# 
# test <- test %>%
#   semi_join(test2, by = "URL")
# 
# # Now df1_filtered contains only the rows from df1 that have the same common column value in df2
# 
# # Print the filtered dataframe
# #print(df1_filtered)
# test$Date <- as.Date(test$Date)
# test2$Date <- as.Date(test2$injured_since)
# 
# # For each row in Test1, find the nearest date in Test2 for that ID
# test <- test %>%
#   group_by(URL) %>%
#   mutate(nearest_date = sapply(Date, function(x) {
#     min(abs(x - test2$injured_since[test2$URL == URL]))
#   })) %>%
#   ungroup()
# 
# # Join based on the nearest date and ID
# joined_data <- left_join(test, test2, by = c("ID", "nearest_date"))
# 
#  set.seed(42)
#  df1 <- data.frame(ID = sample(1:3, 10, rep=T), dateTarget=(strptime((paste
#                                                                       (sprintf("%02d", sample(1:30,10, rep=T)), sprintf("%02d", sample(1:12,10, rep=T)), 
#                                                                         (sprintf("%02d", sample(2013:2015,10, rep=T))), sep="")),"%d%m%Y")), Value=sample(15:100, 10, rep=T))
#  
#  df2 <- data.frame(ID = sample(1:3, 10, rep=T), dateTarget=(strptime((paste
#                                                                       (sprintf("%02d", sample(1:30,20, rep=T)), sprintf("%02d", sample(1:12,20, rep=T)), 
#                                                                         (sprintf("%02d", sample(2013:2015,20, rep=T))), sep="")),"%d%m%Y")), ValueMatch=sample(15:100, 20, rep=T))
#  

#Reading Dataset Set to join 
cleansed.dataset <- read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/CleansedDatasets/test.xlsx')

stage1 <- Player.injury.data %>% left_join(cleansed.dataset, 
                                           by=c('URL'='URL',
                                                'injured_since'='Date.y'))
#Filter based on URL
stage1 <- stage1 %>%
  filter(!is.na(MatchURL))

#Join Based on URL
final <- Player.mls.data %>% left_join(stage1, 
                                       by=c('URL'='URL',
                                            'MatchURL'='MatchURL'))

#Select Columns for Final Dataset that encompasses all the dataset
final.dataset <- select(final, 1,2:39,44,45,47:60 )

final.dataset <- final.dataset %>%
  rename_with(~gsub("\\.x$", "", .), contains(".x"))

clipr::write_clip(final.dataset)

final.dataset.injury <- final.dataset %>%
  filter(!is.na(InjuryCount))

clipr::write_clip(final.dataset.injury)

#More Cleansing
cleansingAnalysis <- read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Analysis/GameInjuries.xlsx')

specific_clubs <- c("Houston Dynamo FC", "Houston Dynamo", "Charlotte FC", "D.C. United",
                                        "Atlanta United FC", "Minnesota United FC", "Vancouver Whitecaps FC",
                                        "Columbus Crew SC", "Columbus Crew", "Chicago Fire", "Chicago Fire FC",
                                        "FC Cincinnati","Colorado Rapids", "FC Dallas", "Los Angeles FC",
                                        "Los Angeles Galaxy", "Inter Miami CF", "Montreal Impact", "Nashville SC",
                                        "New England Revolution","New York City FC", "Orlando City SC",
                                        "New York Red Bulls", "Philadelphia Union","Portland Timbers",
                                        "Real Salt Lake City", "San Jose Earthquakes", "Seattle Sounders FC",
                                        "Sporting Kansas City", "Toronto FC", "Austin FC", "Saint Louis FC", "St. Louis CITY SC"
                                        )

cleansingAnalysis <- cleansingAnalysis %>%
                      filter(club %in% specific_clubs)

cleansingAnalysis <- distinct(cleansingAnalysis)
  
clipr::write_clip(cleansingAnalysis)

#final.dataset <- select(final, 1,2:39,44,45,47:60 )

final.dataset <- final.dataset %>%
  filter(club %in% specific_clubs)

final.dataset <- distinct(final.dataset)

clipr::write_clip(final.dataset)
