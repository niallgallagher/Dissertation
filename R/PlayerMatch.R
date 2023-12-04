#Scrapping Player match data 
library(dplyr)
library(tidyverse)
library(worldfootballR)
library(eatTools)

big5_player_possession <- fb_big5_advanced_season_stats(season_end_year= 2021, stat_type= "possession", team_or_player= "player")
dplyr::glimpse(big5_player_possession)

fleetwood_standard_stats <- fb_team_player_stats(team_urls= "https://fbref.com/en/squads/d6a369a2/Fleetwood-Town-Stats", stat_type= 'standard')
dplyr::glimpse(fleetwood_standard_stats)

#Store Players Urls that played in a given season
#2012
mls_2012_team_urls <- fb_teams_urls("https://fbref.com/en/comps/22/2012/2012-Major-League-Soccer-Stats")

mls_2012.Players <- fb_team_player_stats(team_urls = mls_2012_team_urls, stat_type= 'standard')

avector2012 <- as.vector(mls_2012.Players['PlayerURL'])

avector2012$PlayerURL[1]

# ederson_summary <- fb_player_match_logs("https://fbref.com/en/players/3bb7b8b4/Ederson", season_end_year = 2019, stat_type = 'summary')
# dplyr::glimpse(ederson_summary)


mls2012_summary <- fb_player_match_logs(avector2012$PlayerURL[1], season_end_year = 2012, stat_type = 'summary')

# Create a sample vector
my_vector <- c(1, 2, 3, 4, 5)

# Initialize an empty data frame to store results
result_df <- data.frame(index = integer(), value = numeric())

# Using a for loop to iterate through the vector and store results in the data frame
for (i in 1:length(my_vector)) {
  result_df <- rbind(result_df, data.frame(index = i, value = my_vector[i]))
}

# Print the resulting data frame
print(result_df)



for (i in 1:length(avector2012$PlayerURL)) {
  result_df <- fb_player_match_logs(avector2012$PlayerURL[i],season_end_year = 2012, stat_type = 'summary')
}

df = data.frame()

for (i in 1:length(avector2012$PlayerURL)) {
  output = fb_player_match_logs(avector2012$PlayerURL[i],season_end_year = 2012, stat_type = 'summary')
  
  df = rbind(df,output)
}

colnames(df) <- c("Player","Season","Date","Day","Comp","Round","Venue","Result","Squad",
                  "Opponent","Start","Pos","Min","Gls_Performance","Ast_Performance","Pk_Performance","Pkatt_Performance",
                  "Sh_Performance","SoT_Performance","CrdY_Performance","CrdR_Performance","Fls_Performance","Fld_Performance",
                  "Off_Performance","Crs_Performance","TkIW_Performance","TkIW_Performance","Int_Performance",
                  "OG_Performance","PKwon_Performance","PKcon_Performance")




####
mls2012.season.stats <- fb_player_season_stats(avector2012, stat_type =  "playing_time")




df2 = data.frame()

for (i in 1:length(avector2012$PlayerURL)) {
  output = fb_player_season_stats(avector2012$PlayerURL[i],stat_type =  "playing_time")
  
  df2 = rbind(df2,output)
}


 multiple_playing_time <- fb_player_season_stats(player_url = c("https://fbref.com/en/players/d70ce98e/Lionel-Messi",
                                                 "https://fbref.com/en/players/dea698d9/Cristiano-Ronaldo"),
                                  stat_type = "playing_time")
 
 
vela_primary <- fb_player_scouting_report(player_url = "https://fbref.com/en/players/e0cd04e0/Carlos-Vela", pos_versus = "primary")
 

##Official Script of Code Above
#Scrapping Player match data 
library(dplyr)
library(tidyverse)
library(worldfootballR)

#2012 storing Players that Plat MLS That Year
mls_2012_team_urls <- fb_teams_urls("https://fbref.com/en/comps/22/2012/2012-Major-League-Soccer-Stats")

mls_2012.Players <- fb_team_player_stats(team_urls = mls_2012_team_urls, stat_type= 'standard')

avector2012 <- as.vector(mls_2012.Players['PlayerURL'])

avector2012$PlayerURL[1]

df = data.frame()

#Game-By-Game Player Data 
for (i in 1:length(avector2012$PlayerURL)) {
  output = fb_player_match_logs(avector2012$PlayerURL[i],season_end_year = 2012, stat_type = 'summary')
  
  df = rbind(df,output)
}

MLS.Player.Game.2012 <- df

#2012 Players Season Data
df.season = data.frame()

for (i in 1:length(avector2012$PlayerURL)) {
  output = fb_player_season_stats(player_url = avector2012$PlayerURL[i], stat_type = "playing_time")
  
  df.season = bind_rows(df.season,output)
}

MLS.Player.Season.2012 <- df.season

clipr::write_clip(MLS.Player.Season.2012)


#2012 Players Scout Data
# df.possession = data.frame()
# 
# for (i in 1:length(avector2012$PlayerURL)) {
#   output = fb_player_season_stats(player_url = avector2012$PlayerURL[i], stat_type = "possession")
#   
#   df.possession = bind_rows(df.possession,output)
# }
# 
# MLS.Player.Season.2012 <- df
# 
# MLS.Player.Scout.2012 <- df


#mo_shooting <- fb_player_season_stats("https://fbref.com/en/players/e342ad68/Mohamed-Salah", stat_type = 'passing')

#2013 MLS Players
mls_2013_team_urls <- fb_teams_urls("https://fbref.com/en/comps/22/2013/2013-Major-League-Soccer-Stats")

mls_2013.Players <- fb_team_player_stats(team_urls = mls_2013_team_urls, stat_type= 'standard')

avector2013 <- as.vector(mls_2013.Players['PlayerURL'])

avector2013$PlayerURL[1]

df.game2013 = data.frame()

#Game-By-Game Player Data 
for (i in 1:length(avector2013$PlayerURL)) {
  output = fb_player_match_logs(avector2013$PlayerURL[i],season_end_year = 2013, stat_type = 'summary')
  
  df.game2013 = rbind(df.game2013,output)
}

MLS.Player.Game.2013 <- df.game2013

clipr::write_clip(MLS.Player.Game.2013)

#2013 Players Season Data
df.season2013 = data.frame()

for (i in 1:length(avector2013$PlayerURL)) {
  output = fb_player_season_stats(player_url = avector2013$PlayerURL[i], stat_type = "playing_time")
  
  df.season2013 = bind_rows(df.season2013,output)
}

MLS.Player.Season.2013 <- df.season2013

clipr::write_clip(MLS.Player.Season.2013)


#2014 MLS Players
mls_2014_team_urls <- fb_teams_urls("https://fbref.com/en/comps/22/2014/2014-Major-League-Soccer-Stats")

mls_2014.Players <- fb_team_player_stats(team_urls = mls_2014_team_urls, stat_type= 'standard')

test <- mls_2014.Players %>% drop_na(Min_Playing_Time)

avector2014 <- as.vector(test['PlayerURL'])

avector2014$PlayerURL[1]


#Create test Set
Player <- c('Test')
Season <- c(2021)
Date <- c('01-01-2022')
Comp <- c('MLS')
Round <- c('Playoff')
Venue <- c('TEst')
Result <- c('L 1–3')
Squad <- c('Test')
Opponent <- c('Test')
Start <- c('Y')
Pos <- c('Test')
Min <- c(0)


df = data.frame(Player,Season,Date,Comp,Round,Venue,Result,Squad,Opponent,Start,Pos,Min)
#names(df.game2014) <- c('Player', 'Season', 'Date', 'Day','Comp','Round','Venue','Result','Squad','Opponent','Start','Pos','Min')

df = data.frame()
#Game-By-Game Player Data 
for (i in 1:length(avector2014$PlayerURL)) {
  output = fb_player_match_logs(avector2014$PlayerURL[i],season_end_year = 2014, stat_type = 'summary')
  
  
  df = bind_rows(df,output)
}

df2 <- df[!duplicated(df), ]


MLS.Player.Game.2014 <- df

clipr::write_clip(MLS.Player.Game.2014)

#2012 Players Season Data
df.season2014 = data.frame()

for (i in 1:length(avector2014$PlayerURL)) {
  output = fb_player_season_stats(player_url = avector2014$PlayerURL[i], stat_type = "playing_time")
  
  df.season2014 = bind_rows(df.season2014,output)
}

MLS.Player.Season.2014 <- df.season2014

clipr::write_clip(MLS.Player.Season.2014)

#2015 MLS Players
mls_2015_team_urls <- fb_teams_urls("https://fbref.com/en/comps/22/2015/2015-Major-League-Soccer-Stats")

mls_2015.Players <- fb_team_player_stats(team_urls = mls_2015_team_urls, stat_type= 'standard')

avector2015 <- as.vector(mls_2015.Players['PlayerURL'])

avector2015$PlayerURL[1]

df.game2015 = data.frame()

#Game-By-Game Player Data 
for (i in 1:length(avector2015$PlayerURL)) {
  output = fb_player_match_logs(avector2015$PlayerURL[i],season_end_year = 2015, stat_type = 'summary')
  
  df.game2015 = rbind(df.game2015,output)
}

MLS.Player.Game.2015 <- df.game2015

clipr::write_clip(MLS.Player.Game.2015)

#2015 Players Season Data
df.season2015 = data.frame()

for (i in 1:length(avector2015$PlayerURL)) {
  output = fb_player_season_stats(player_url = avector2015$PlayerURL[i], stat_type = "playing_time")
  
  df.season2015 = bind_rows(df.season2015,output)
}

MLS.Player.Season.2015 <- df.season2015

clipr::write_clip(MLS.Player.Season.2015)