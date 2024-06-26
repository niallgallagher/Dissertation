####Transfermarkt Injury and Biography####
####Niall Gallagher#######
install.packages("devtools")
devtools::install_github("JaseZiv/worldfootballR")
library(devtools)
library(worldfootballR)
library(rvest)
library(dplyr)

##Getting Fixture Results from Last 12 Seasons###
usa_match_results <- load_match_results(country = "USA", gender = c("M"), season_end_year = c(2012:2023), tier = "1st")
dplyr::glimpse(usa_match_results)

#Copy DataFrame for Excel
clipr::write_clip(usa_match_results)
 
#Source Website Has changed since first execution. 
#Code worked in November 2023
#Please refer to data files in folders to see results of code

#2012
team_urls2011 <- tm_league_team_urls(country_name = "United States", start_year = 2011)
usa_2011 <- tm_squad_stats(team_url = team_urls2011)
usa_2011.test <- usa_2011[!(usa_2012$appearances==0),]

player_url2011 <- usa_2011.test[['player_url']]

#Player Bio Data
league2011.bio <- tm_player_bio(player_urls = player_url2011)
league2011.bio$PlayerId <- sub('.*spieler/', '', league2011.bio$URL)
clipr::write_clip(league2011.bio)

#Player Injury Data
league2011.injury <- tm_player_injury_history(player_urls = player_url2011)
league2011.injury$PlayerId <- sub('.*spieler/', '', league2011.injury$player_url)
clipr::write_clip(league2011.injury)

#2013
team_urls2012 <- tm_league_team_urls(country_name = "United States", start_year = 2012)
usa_2012 <- tm_squad_stats(team_url = team_urls2012)
usa_2012.test <- usa_2012[!(usa_2012$appearances==0),]
player_url2012 <- usa_2012.test[['player_url']]
 
#Player Bio Data
league2012.bio <- tm_player_bio(player_urls = player_url2012)
league2012.bio$PlayerId <- sub('.*ler/', '', league2012.bio$URL)
clipr::write_clip(league2012.bio)
 
#Player Injury Data
league2012.injury <- tm_player_injury_history(player_urls = player_url2012)
league2012.injury$PlayerId <- sub('.*ler/', '', league2012.injury$player_url)
clipr::write_clip(league2012.injury)
 
#2014
team_urls2013 <- tm_league_team_urls(country_name = "United States", start_year = 2013)
usa_2013 <- tm_squad_stats(team_url = team_urls2013)
usa_2013.test <- usa_2013[!(usa_2012$appearances==0),]
player_url2013 <- usa_2013.test[['player_url']]
 
#Player Bio Data
league2013.bio <- tm_player_bio(player_urls = player_url2013)
league2013.bio$PlayerId <- sub('.*spieler/', '', league2013.bio$URL)
clipr::write_clip(league2013.bio)
 
#Player Injury Data
league2013.injury <- tm_player_injury_history(player_urls = player_url2013)
league2013.injury$PlayerId <- sub('.*spieler/', '', league2013.injury$player_url)
clipr::write_clip(league2013.injury)
 
#2015
team_urls2014 <- tm_league_team_urls(country_name = "United States", start_year = 2014)
usa_2014 <- tm_squad_stats(team_url = team_urls2014)
usa_2014.test <- usa_2014[!(usa_2014$appearances==0),]
player_url2014 <- usa_2014.test[['player_url']]

#Player Bio Data
league2014.bio <- tm_player_bio(player_urls = player_url2014)
league2014.bio$PlayerId <- sub('.*spieler/', '', league2014.bio$URL)
clipr::write_clip(league2014.bio)
 
#Player Injury Data
league2014.injury <- tm_player_injury_history(player_urls = player_url2014)
league2014.injury$PlayerId <- sub('.*spieler/', '', league2014.injury$player_url)
clipr::write_clip(league2014.injury)
 
#2016
team_urls2015 <- tm_league_team_urls(country_name = "United States", start_year = 2015)
usa_2015 <- tm_squad_stats(team_url = team_urls2015)
usa_2015.test <- usa_2015[!(usa_2015$appearances==0),]
player_url2015 <- usa_2015.test[['player_url']]

#Player Bio Data
league2015.bio <- tm_player_bio(player_urls = player_url2015)
league2015.bio$PlayerId <- sub('.*spieler/', '', league2015.bio$URL)
clipr::write_clip(league2015.bio)
 
#Player Injury Data
league2015.injury <- tm_player_injury_history(player_urls = player_url2015)
league2015.injury$PlayerId <- sub('.*spieler/', '', league2015.injury$player_url)
clipr::write_clip(league2015.injury)
 
#2017
team_urls2016 <- tm_league_team_urls(country_name = "United States", start_year = 2016)
usa_2016 <- tm_squad_stats(team_url = team_urls2016)
usa_2016.test <- usa_2016[!(usa_2016$appearances==0),]
player_url2016 <- usa_2016.test[['player_url']]

#Player Bio Data
league2016.bio <- tm_player_bio(player_urls = player_url2016)
league2016.bio$PlayerId <- sub('.*spieler/', '', league2016.bio$URL)
clipr::write_clip(league2016.bio)
 
#Player Injury Data
league2016.injury <- tm_player_injury_history(player_urls = player_url2016)
league2016.injury$PlayerId <- sub('.*spieler/', '', league2016.injury$player_url)
clipr::write_clip(league2016.injury)
 
#2018
team_urls2017 <- tm_league_team_urls(country_name = "United States", start_year = 2017)
usa_2017 <- tm_squad_stats(team_url = team_urls2017)
usa_2017.test <- usa_2017[!(usa_2017$appearances==0),]
player_url2017 <- usa_2017.test[['player_url']]

#Player Bio Data
league2017.bio <- tm_player_bio(player_urls = player_url2017)
league2017.bio$PlayerId <- sub('.*spieler/', '', league2017.bio$URL)
clipr::write_clip(league2017.bio)
 
#Player Injury Data
league2017.injury <- tm_player_injury_history(player_urls = player_url2017)
league2017.injury$PlayerId <- sub('.*spieler/', '', league2017.injury$player_url)
clipr::write_clip(league2017.injury)
 
#2019
team_urls2018 <- tm_league_team_urls(country_name = "United States", start_year = 2018)
usa_2018 <- tm_squad_stats(team_url = team_urls2018)
usa_2018.test <- usa_2018[!(usa_2018$appearances==0),]
player_url2018 <- usa_2018.test[['player_url']]

#Player Bio Data
league2018.bio <- tm_player_bio(player_urls = player_url2018)
league2018.bio$PlayerId <- sub('.*spieler/', '', league2018.bio$URL)
clipr::write_clip(league2018.bio)
 
#Player Injury Data
league2018.injury <- tm_player_injury_history(player_urls = player_url2018)
league2018.injury$PlayerId <- sub('.*spieler/', '', league2018.injury$player_url)
clipr::write_clip(league2018.injury)
 
#2020
team_urls2019 <- tm_league_team_urls(country_name = "United States", start_year = 2019)
usa_2019 <- tm_squad_stats(team_url = team_urls2019)
usa_2019.test <- usa_2019[!(usa_2019$appearances==0),]
player_url2019 <- usa_2019.test[['player_url']]

#Player Bio Data
league2019.bio <- tm_player_bio(player_urls = player_url2019)
league2019.bio$PlayerId <- sub('.*spieler/', '', league2019.bio$URL)
clipr::write_clip(league2019.bio)
 
#Player Injury Data
league2019.injury <- tm_player_injury_history(player_urls = player_url2019)
league2019.injury$PlayerId <- sub('.*spieler/', '', league2019.injury$player_url)
clipr::write_clip(league2019.injury)
 
#2021
team_urls2020 <- tm_league_team_urls(country_name = "United States", start_year = 2020)
usa_2020 <- tm_squad_stats(team_url = team_urls2020)
usa_2020.test <- usa_2020[!(usa_2020$appearances==0),]
player_url2020 <- usa_2020.test[['player_url']]

#Player Bio Data
league2020.bio <- tm_player_bio(player_urls = player_url2020)
league2020.bio$PlayerId <- sub('.*spieler/', '', league2020.bio$URL)
clipr::write_clip(league2020.bio)
 
#Player Injury Data
league2020.injury <- tm_player_injury_history(player_urls = player_url2020)
league2020.injury$PlayerId <- sub('.*spieler/', '', league2020.injury$player_url)
clipr::write_clip(league2020.injury)
 
#2022
team_urls2021 <- tm_league_team_urls(country_name = "United States", start_year = 2021)
usa_2021 <- tm_squad_stats(team_url = team_urls2021)
usa_2021.test <- usa_2021[!(usa_2021$appearances==0),]
player_url2021 <- usa_2021.test[['player_url']]

#Player Bio Data
league2021.bio <- tm_player_bio(player_urls = player_url2021)
league2021.bio$PlayerId <- sub('.*spieler/', '', league2021.bio$URL)
clipr::write_clip(league2021.bio)
 
#Player Injury Data
league2021.injury <- tm_player_injury_history(player_urls = player_url2021)
league2021.injury$PlayerId <- sub('.*spieler/', '', league2021.injury$player_url)
clipr::write_clip(league2021.injury)
 
#2023
team_urls2022 <- tm_league_team_urls(country_name = "United States", start_year = 2022)
usa_2022 <- tm_squad_stats(team_url = team_urls2022)
usa_2022.test <- usa_2022[!(usa_2022$appearances==0),]
player_url2022 <- usa_2022.test[['player_url']]

#Player Bio Data
league2022.bio <- tm_player_bio(player_urls = player_url2022)
league2022.bio$PlayerId <- sub('.*spieler/', '', league2022.bio$URL)
clipr::write_clip(league2022.bio)
 
#Player Injury Data
league2022.injury <- tm_player_injury_history(player_urls = player_url2022)
league2022.injury$PlayerId <- sub('.*spieler/', '', league2022.injury$player_url)
clipr::write_clip(league2022.injury)