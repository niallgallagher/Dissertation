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

df.game2014 = data.frame()
#Game-By-Game Player Data 
for (i in 1:length(avector2014$PlayerURL)) {
  tryCatch({
    output = fb_player_match_logs(avector2014$PlayerURL[i], season_end_year = 2014, stat_type = 'summary')
    df.game2014 = bind_rows(df.game2014, output)
  }, error = function(e) {
    print(paste("Error for URL:", avector2014$PlayerURL[i]))
    print(e)
  })
  Sys.sleep(1)
}

length(unique(df.game2014$Player))

length(unique(test$Player))

#df2 <- df[!duplicated(df), ]


MLS.Player.Game.2014 <-df.game2014

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

mls_2015.Players <- mls_2015.Players %>% drop_na(Min_Playing_Time)

avector2015 <- as.vector(mls_2015.Players['PlayerURL'])

avector2015$PlayerURL[1]

df.game2015 = data.frame()

#Game-By-Game Player Data 
for (i in 1:length(avector2015$PlayerURL)) {
  output = fb_player_match_logs(avector2015$PlayerURL[i],season_end_year = 2015, stat_type = 'summary')
  
  df.game2015 = bind_rows(df.game2015,output)
  Sys.sleep(1)
}

for (i in 1:length(avector2015$PlayerURL)) {
  tryCatch({
    output = fb_player_match_logs(avector2015$PlayerURL[i], season_end_year = 2015, stat_type = 'summary')
    df.game2015 = bind_rows(df.game2015, output)
  }, error = function(e) {
    print(paste("Error for URL:", avector2015$PlayerURL[i]))
    print(e)
  })
  Sys.sleep(1)
}

mls_2015.Players

df.game2015

mls_2015.Players
length(unique(mls_2015.Players$Player))

length(unique(df.game2015$Player))


test <- c(unique(df.game2015$Player))

test2 <- c(mls_2015.Players$Player)

identical(test2, test) 

test3 <- test2 %in% test

which(cumsum(test3) & rev(cumsum(rev(test3))) & !test3)

match(test,test2)

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

ederson_summary <- fb_player_match_logs("https://fbref.com/en/players/87f1f9b9/Alvaro-Rey", season_end_year = 2014, stat_type = 'summary')

clipr::write_clip(ederson_summary)

#2016 MLS Players Game Breakdown. 
mls_2016_team_urls <- fb_teams_urls("https://fbref.com/en/comps/22/2016/2016-Major-League-Soccer-Stats")

mls_2016.Players <- fb_team_player_stats(team_urls = mls_2016_team_urls, stat_type= 'standard')

mls_2016.Players <- mls_2016.Players %>% drop_na(Min_Playing_Time)

avector2016 <- as.vector(mls_2016.Players['PlayerURL'])

avector2016$PlayerURL[1]

df.game2016 = data.frame()

for (i in 1:length(avector2016$PlayerURL)) {
  tryCatch({
    output = fb_player_match_logs(avector2016$PlayerURL[i], season_end_year = 2016, stat_type = 'summary')
    df.game2016 = bind_rows(df.game2016, output)
  }, error = function(e) {
    print(paste("Error for URL:", avector2016$PlayerURL[i]))
    print(e)
  })
  Sys.sleep(1)
}


length(unique(mls_2016.Players$Player))

length(unique(df.season2016$player_name))


MLS.Player.Game.2016 <- df.game2016

clipr::write_clip(MLS.Player.Game.2016)

#2016 MLS Season Breakdown 
df.season2016 = data.frame()

# for (i in 1:length(avector2016$PlayerURL)) {
#   output = fb_player_season_stats(player_url = avector2016$PlayerURL[i], stat_type = "playing_time")
#   
#   df.season2016 = bind_rows(df.season2016,output)
# }

for (i in 1:length(avector2016$PlayerURL)) {
  tryCatch({
    output = fb_player_season_stats(player_url = avector2016$PlayerURL[i], stat_type = "playing_time")
    df.season2016 = bind_rows(df.season2016, output)
  }, error = function(e) {
    print(paste("Error for URL:", avector2016$PlayerURL[i]))
    print(e)
  })
  Sys.sleep(1)
}


MLS.Player.Season.2016 <- df.season2016

clipr::write_clip(MLS.Player.Season.2016)


#clipr::write_clip(MLS.Player.Season.2015)

#2017 MLS Players Game Breakdown. 
mls_2017_team_urls <- fb_teams_urls("https://fbref.com/en/comps/22/2017/2017-Major-League-Soccer-Stats")

mls_2017.Players <- fb_team_player_stats(team_urls = mls_2017_team_urls, stat_type= 'standard')

mls_2017.Players <- mls_2017.Players %>% drop_na(Min_Playing_Time)

avector2017 <- as.vector(unique(mls_2017.Players['PlayerURL']))

avector2017$PlayerURL[1]

avector2017part2 <- avector2017$PlayerURL[575:588]

df.game2017 = data.frame()

for (i in 1:length(avector2017part2)) {
  tryCatch({
    output = fb_player_match_logs(avector2017part2[i], season_end_year = 2017, stat_type = 'summary')
    df.game2017 = bind_rows(df.game2017, output)
  }, error = function(e) {
    print(paste("Error for URL:", avector2017part2[i]))
    print(e)
  })
  Sys.sleep(1)
}



length(unique(mls_2017.Players))

length(unique(df.game2017$Player))

df.game2017 <- df.game2017[!duplicated(df.game2017), ]


MLS.Player.Game.2017 <- df.game2017

clipr::write_clip(MLS.Player.Game.2017)

#2016 MLS Season Breakdown 
df.season2017 = data.frame()

# for (i in 1:length(avector2016$PlayerURL)) {
#   output = fb_player_season_stats(player_url = avector2016$PlayerURL[i], stat_type = "playing_time")
#   
#   df.season2016 = bind_rows(df.season2016,output)
# }

for (i in 1:length(avector2017$PlayerURL)) {
  tryCatch({
    output = fb_player_season_stats(player_url = avector2017$PlayerURL[i], stat_type = "playing_time")
    df.season2017 = bind_rows(df.season2017, output)
  }, error = function(e) {
    print(paste("Error for URL:", avector2017$PlayerURL[i]))
    print(e)
  })
  Sys.sleep(1)
}


MLS.Player.Season.2017 <- df.season2017

clipr::write_clip(MLS.Player.Season.2017)

length(unique(mls_2017.Players))

length(unique(df.season2017$player_name))


#2018 MLS Players Game Breakdown. 
mls_2018_team_urls <- fb_teams_urls("https://fbref.com/en/comps/22/2018/2018-Major-League-Soccer-Stats")

mls_2018.Players <- fb_team_player_stats(team_urls = mls_2018_team_urls, stat_type= 'standard')

mls_2018.Players <- mls_2018.Players %>% drop_na(Min_Playing_Time)

avector2018 <- as.vector(unique(mls_2018.Players['PlayerURL']))

avector2018$PlayerURL[1]

df.game2018 = data.frame()

for (i in 1:length(avector2018$PlayerURL)) {
  tryCatch({
    output = fb_player_match_logs(avector2018$PlayerURL[i], season_end_year = 2018, stat_type = 'summary')
    df.game2018 = bind_rows(df.game2018, output)
  }, error = function(e) {
    print(paste("Error for URL:", avector2018$PlayerURL[i]))
    print(e)
  })
  Sys.sleep(1)
}


length(unique(mls_2018.Players$Player))

length(unique(df.game2018$Player))


MLS.Player.Game.2018 <- df.game2018

clipr::write_clip(MLS.Player.Game.2018)

df.season2018 = data.frame()


for (i in 1:length(avector2018$PlayerURL)) {
  tryCatch({
    output = fb_player_season_stats(player_url = avector2018$PlayerURL[i], stat_type = "playing_time")
    df.season2018 = bind_rows(df.season2018, output)
  }, error = function(e) {
    print(paste("Error for URL:", avector2018$PlayerURL[i]))
    print(e)
  })
  Sys.sleep(1)
}

length(unique(mls_2018.Players$Player))

length(unique(df.season2018$player_name))

clipr::write_clip(df.season2018)


#2019 MLS Players Game Breakdown. 
mls_2019_team_urls <- fb_teams_urls("https://fbref.com/en/comps/22/2019/2019-Major-League-Soccer-Stats")

mls_2019.Players <- fb_team_player_stats(team_urls = mls_2019_team_urls, stat_type= 'standard')

mls_2019.Players <- mls_2019.Players %>% drop_na(Min_Playing_Time)

avector2019 <- as.vector(unique(mls_2019.Players['PlayerURL']))

avector2019$PlayerURL[1]

avector2019part1 <- avector2019$PlayerURL[1:311]

avector2019part2 <- avector2019$PlayerURL[312:623]

df.game2019 = data.frame()

for (i in 1:length(avector2019part1)) {
  tryCatch({
    output = fb_player_match_logs(avector2019part1[i], season_end_year = 2019, stat_type = 'summary')
    df.game2019 = bind_rows(df.game2019, output)
  }, error = function(e) {
    print(paste("Error for URL:", avector2019part1[i]))
    print(e)
  })
  Sys.sleep(1)
}


for (i in 1:length(avector2019part2)) {
  tryCatch({
    output = fb_player_match_logs(avector2019part2[i], season_end_year = 2019, stat_type = 'summary')
    df.game2019 = bind_rows(df.game2019, output)
  }, error = function(e) {
    print(paste("Error for URL:", avector2019part2[i]))
    print(e)
  })
  Sys.sleep(1)
}

df

length(unique(mls_2019.Players$Player))

length(unique(df.game2019$Player))


MLS.Player.Game.2019 <- df.game2019

clipr::write_clip(MLS.Player.Game.2019)

df.season2019 = data.frame()

for (i in 1:length(avector2019$PlayerURL)) {
  tryCatch({
    output = fb_player_season_stats(player_url = avector2019$PlayerURL[i], stat_type = "playing_time")
    df.season2019 = bind_rows(df.season2019, output)
  }, error = function(e) {
    print(paste("Error for URL:", avector2019$PlayerURL[i]))
    print(e)
  })
  Sys.sleep(1)
}

length(unique(mls_2019.Players$Player))

length(unique(df.season2019$player_name))

clipr::write_clip(df.season2019)

#2020 MLS Players Game Breakdown. 
mls_2020_team_urls <- fb_teams_urls("https://fbref.com/en/comps/22/2020/2020-Major-League-Soccer-Stats")

mls_2020.Players <- fb_team_player_stats(team_urls = mls_2020_team_urls, stat_type= 'standard')

mls_2020.Players <- mls_2020.Players %>% drop_na(Min_Playing_Time)

avector2020 <- as.vector(unique(mls_2020.Players['PlayerURL']))

avector2020$PlayerURL[1]

avector2020part1 <- avector2020$PlayerURL[1:334]

avector2020part2 <- avector2020$PlayerURL[335:668]

df.game2020 = data.frame()

for (i in 1:length(avector2020part1)) {
  tryCatch({
    output = fb_player_match_logs(avector2020part1[i], season_end_year = 2020, stat_type = 'summary')
    df.game2020 = bind_rows(df.game2020, output)
  }, error = function(e) {
    print(paste("Error for URL:", avector2020part1[i]))
    print(e)
  })
  Sys.sleep(1)
}


for (i in 1:length(avector2020part2)) {
  tryCatch({
    output = fb_player_match_logs(avector2020part2[i], season_end_year = 2020, stat_type = 'summary')
    df.game2020 = bind_rows(df.game2020, output)
  }, error = function(e) {
    print(paste("Error for URL:", avector2020part2[i]))
    print(e)
  })
  Sys.sleep(1)
}

df

length(unique(mls_2020.Players$Player))

length(unique(df.game2020$Player))


MLS.Player.Game.2020 <- df.game2020

clipr::write_clip(MLS.Player.Game.2020)

df.season2020 = data.frame()

for (i in 1:length(avector2020$PlayerURL)) {
  tryCatch({
    output = fb_player_season_stats(player_url = avector2020$PlayerURL[i], stat_type = "playing_time")
    df.season2020 = bind_rows(df.season2020, output)
  }, error = function(e) {
    print(paste("Error for URL:", avector2020$PlayerURL[i]))
    print(e)
  })
  Sys.sleep(1)
}

length(unique(mls_2020.Players$Player))

length(unique(df.season2020$player_name))

clipr::write_clip(df.season2020)

ederson_summary <- fb_player_match_logs("https://fbref.com/en/players/b76e1394/Pedro-Gallese", season_end_year = 2020, stat_type = 'summary')

ederson_summary2 <- fb_player_match_logs("https://fbref.com/en/players/7e1dbecf/Andres-Perea", season_end_year = 2020, stat_type = 'summary')

clipr::write_clip(ederson_summary)
clipr::write_clip(ederson_summary2)

#2021 MLS Players Game Breakdown. 
mls_2021_team_urls <- fb_teams_urls("https://fbref.com/en/comps/22/2021/2021-Major-League-Soccer-Stats")

mls_2021.Players <- fb_team_player_stats(team_urls = mls_2021_team_urls, stat_type= 'standard')

mls_2021.Players <- mls_2021.Players %>% drop_na(Min_Playing_Time)

avector2021 <- as.vector(unique(mls_2021.Players['PlayerURL']))

avector2021$PlayerURL[1]

avector2021part1 <- avector2021$PlayerURL[1:365]

avector2021part2 <- avector2021$PlayerURL[366:730]

df.game2021 = data.frame()

for (i in 1:length(avector2021part1)) {
  tryCatch({
    output = fb_player_match_logs(avector2021part1[i], season_end_year = 2021, stat_type = 'summary')
    df.game2021 = bind_rows(df.game2021, output)
  }, error = function(e) {
    print(paste("Error for URL:", avector2021part1[i]))
    print(e)
  })
  Sys.sleep(1)
}


for (i in 1:length(avector2021part2)) {
  tryCatch({
    output = fb_player_match_logs(avector2021part2[i], season_end_year = 2021, stat_type = 'summary')
    df.game2021 = bind_rows(df.game2021, output)
  }, error = function(e) {
    print(paste("Error for URL:", avector2021part2[i]))
    print(e)
  })
  Sys.sleep(1)
}

ederson_summary <- fb_player_match_logs("https://fbref.com/en/players/ec730d9a/Russell-Canouse", season_end_year = 2021, stat_type = 'summary')
ederson_summary2 <- fb_player_match_logs("https://fbref.com/en/players/1bb2acce/Paul-Arriola", season_end_year = 2021, stat_type = 'summary')

length(unique(mls_2021.Players$Player))

length(unique(df.game2021$Player))


MLS.Player.Game.2021 <- df.game2021

clipr::write_clip(MLS.Player.Game.2021)

df.season2021 = data.frame()

for (i in 1:length(avector2021$PlayerURL)) {
  tryCatch({
    output = fb_player_season_stats(player_url = avector2021$PlayerURL[i], stat_type = "playing_time")
    df.season2021 = bind_rows(df.season2021, output)
  }, error = function(e) {
    print(paste("Error for URL:", avector2021$PlayerURL[i]))
    print(e)
  })
  Sys.sleep(1)
}

length(unique(mls_2021.Players$Player))

length(unique(df.season2021$player_name))

clipr::write_clip(df.season2021)

#2022 MLS Players Game Breakdown. 
mls_2022_team_urls <- fb_teams_urls("https://fbref.com/en/comps/22/2022/2022-Major-League-Soccer-Stats")

mls_2022.Players <- fb_team_player_stats(team_urls = mls_2022_team_urls, stat_type= 'standard')

mls_2022.Players <- mls_2022.Players %>% drop_na(Min_Playing_Time)

avector2022 <- as.vector(unique(mls_2022.Players['PlayerURL']))

avector2022$PlayerURL[1]

avector2022part1 <- avector2022$PlayerURL[1:385]

avector2022part2 <- avector2022$PlayerURL[386:770]

df.game2022 = data.frame()

for (i in 1:length(avector2022part1)) {
  tryCatch({
    output = fb_player_match_logs(avector2022part1[i], season_end_year = 2022, stat_type = 'summary')
    df.game2022 = bind_rows(df.game2022, output)
  }, error = function(e) {
    print(paste("Error for URL:", avector2022part1[i]))
    print(e)
  })
  Sys.sleep(1)
}


for (i in 1:length(avector2022part2)) {
  tryCatch({
    output = fb_player_match_logs(avector2022part2[i], season_end_year = 2022, stat_type = 'summary')
    df.game2022 = bind_rows(df.game2022, output)
  }, error = function(e) {
    print(paste("Error for URL:", avector2022part2[i]))
    print(e)
  })
  Sys.sleep(1)
}

ederson_summary <- fb_player_match_logs("https://fbref.com/en/players/ec730d9a/Russell-Canouse", season_end_year = 2022, stat_type = 'summary')
ederson_summary2 <- fb_player_match_logs("https://fbref.com/en/players/1bb2acce/Paul-Arriola", season_end_year = 2022, stat_type = 'summary')

length(unique(mls_2022.Players$Player))

length(unique(df.game2022$Player))


MLS.Player.Game.2022 <- df.game2022

clipr::write_clip(MLS.Player.Game.2022)

df.season2022 = data.frame()

for (i in 1:length(avector2022$PlayerURL)) {
  tryCatch({
    output = fb_player_season_stats(player_url = avector2022$PlayerURL[i], stat_type = "playing_time")
    df.season2022 = bind_rows(df.season2022, output)
  }, error = function(e) {
    print(paste("Error for URL:", avector2022$PlayerURL[i]))
    print(e)
  })
  Sys.sleep(1)
}

length(unique(mls_2022.Players$Player))

length(unique(df.season2022$player_name))

clipr::write_clip(df.season2022)

#2023 MLS Players Game Breakdown. 
mls_2023_team_urls <- fb_teams_urls("https://fbref.com/en/comps/22/2023/2023-Major-League-Soccer-Stats")

mls_2023.Players <- fb_team_player_stats(team_urls = mls_2023_team_urls, stat_type= 'standard')

mls_2023.Players <- mls_2023.Players %>% drop_na(Min_Playing_Time)

avector2023 <- as.vector(unique(mls_2023.Players['PlayerURL']))

avector2023$PlayerURL[1]

avector2023part1 <- avector2023$PlayerURL[1:413]

avector2023part2 <- avector2023$PlayerURL[414:826]

df.game2023 = data.frame()

for (i in 1:length(avector2023part1)) {
  tryCatch({
    output = fb_player_match_logs(avector2023part1[i], season_end_year = 2023, stat_type = 'summary')
    df.game2023 = bind_rows(df.game2023, output)
  }, error = function(e) {
    print(paste("Error for URL:", avector2023part1[i]))
    print(e)
  })
  Sys.sleep(1)
}


for (i in 1:length(avector2023part2)) {
  tryCatch({
    output = fb_player_match_logs(avector2023part2[i], season_end_year = 2023, stat_type = 'summary')
    df.game2023 = bind_rows(df.game2023, output)
  }, error = function(e) {
    print(paste("Error for URL:", avector2023part2[i]))
    print(e)
  })
  Sys.sleep(1)
}

#ederson_summary <- fb_player_match_logs("https://fbref.com/en/players/73fdc4c2/Ian-Harkes", season_end_year = 2023, stat_type = 'passing')
#ederson_summary2 <- fb_player_match_logs("https://fbref.com/en/players/1bb2acce/Paul-Arriola", season_end_year = 2023, stat_type = 'summary')
#ederson_summary2 <- fb_player_season_stats("https://fbref.com/en/players/73fdc4c2/Ian-Harkes", stat_type = "playing_time")
clipr::write_clip(ederson_summary2)


length(unique(mls_2023.Players$Player))

length(unique(df.game2023$Player))


MLS.Player.Game.2023 <- df.game2023

clipr::write_clip(MLS.Player.Game.2023)

df.season2023 = data.frame()

for (i in 1:length(avector2023$PlayerURL)) {
  tryCatch({
    output = fb_player_season_stats(player_url = avector2023$PlayerURL[i], stat_type = "playing_time")
    df.season2023 = bind_rows(df.season2023, output)
  }, error = function(e) {
    print(paste("Error for URL:", avector2023$PlayerURL[i]))
    print(e)
  })
  Sys.sleep(1)
}

length(unique(mls_2023.Players$Player))

length(unique(df.season2023$player_name))

clipr::write_clip(df.season2023)

#################################
#Binding My Data Together Player's Seasons
library(readxl)
#2012
#Load 2012 MLS Players
mls.players.season.2012 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/PlayerSeasons/MLS2012PlayerSeason.xlsx')

#Load 2013 MLS Players
mls.players.season.2013 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/PlayerSeasons/MLS2013PlayerSeason.xlsx')

mergeplayerseason2012.2013 <- rbind(mls.players.season.2012, mls.players.season.2013)

mls.seasonsplayers2012_13 <- mergeplayerseason2012.2013[!duplicated(mergeplayerseason2012.2013), ]

#Load 2014 MLS Players
mls.players.season.2014 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/PlayerSeasons/MLS2014PlayerSeason.xlsx')

mergeplayerseason2012.2014 <- rbind(mls.seasonsplayers2012_13, mls.players.season.2014)

mls.seasonsplayers2012_14 <- mergeplayerseason2012.2014[!duplicated(mergeplayerseason2012.2014), ]

#Load 2015 MLS Players
mls.players.season.2015 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/PlayerSeasons/MLS2015PlayerSeason.xlsx')

mergeplayerseason2012.2015 <- rbind(mls.seasonsplayers2012_14, mls.players.season.2015)

mls.seasonsplayers2012_15 <- mergeplayerseason2012.2015[!duplicated(mergeplayerseason2012.2015), ]

#Load 2015 MLS Players
mls.players.season.2015 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/PlayerSeasons/MLS2015PlayerSeason.xlsx')

mergeplayerseason2012.2015 <- rbind(mls.seasonsplayers2012_14, mls.players.season.2015)

mls.seasonsplayers2012_15 <- mergeplayerseason2012.2015[!duplicated(mergeplayerseason2012.2015), ]

#Load 2016 MLS Players
mls.players.season.2016 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/PlayerSeasons/MLS2016PlayerSeason.xlsx')

mergeplayerseason2012.2016 <- rbind(mls.seasonsplayers2012_15, mls.players.season.2016)

mls.seasonsplayers2012_16 <- mergeplayerseason2012.2016[!duplicated(mergeplayerseason2012.2016), ]

#Load 2017 MLS Players
mls.players.season.2017 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/PlayerSeasons/MLS2017PlayerSeason.xlsx')

mergeplayerseason2012.2017 <- rbind(mls.seasonsplayers2012_16, mls.players.season.2017)

mls.seasonsplayers2012_17 <- mergeplayerseason2012.2017[!duplicated(mergeplayerseason2012.2017), ]


#Load 2018 MLS Players
mls.players.season.2018 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/PlayerSeasons/MLS2018PlayerSeason.xlsx')

mergeplayerseason2012.2018 <- rbind(mls.seasonsplayers2012_17, mls.players.season.2018)

mls.seasonsplayers2012_18 <- mergeplayerseason2012.2018[!duplicated(mergeplayerseason2012.2018), ]

#Load 2019 MLS Players
mls.players.season.2019 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/PlayerSeasons/MLS2019PlayerSeason.xlsx')

mergeplayerseason2012.2019 <- rbind(mls.seasonsplayers2012_18, mls.players.season.2019)

mls.seasonsplayers2012_19 <- mergeplayerseason2012.2019[!duplicated(mergeplayerseason2012.2019), ]


#Load 2020 MLS Players
mls.players.season.2020 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/PlayerSeasons/MLS2020PlayerSeason.xlsx')

mergeplayerseason2012.2020 <- rbind(mls.seasonsplayers2012_19, mls.players.season.2020)

mls.seasonsplayers2012_20 <- mergeplayerseason2012.2020[!duplicated(mergeplayerseason2012.2020), ]


#Load 2021 MLS Players
mls.players.season.2021 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/PlayerSeasons/MLS2021PlayerSeason.xlsx')

mergeplayerseason2012.2021 <- rbind(mls.seasonsplayers2012_20, mls.players.season.2021)

mls.seasonsplayers2012_21 <- mergeplayerseason2012.2021[!duplicated(mergeplayerseason2012.2021), ]

#Load 2022 MLS Players
mls.players.season.2022 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/PlayerSeasons/MLS2022PlayerSeason.xlsx')

mergeplayerseason2012.2022 <- rbind(mergeplayerseason2012.2021, mls.players.season.2022)

mls.seasonsplayers2012_22 <- mergeplayerseason2012.2022[!duplicated(mergeplayerseason2012.2022), ]


#Load 2020 MLS Players
mls.players.season.2023 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/PlayerSeasons/MLS2023PlayerSeason.xlsx')

mergeplayerseason2012.2023 <- rbind(mls.seasonsplayers2012_22, mls.players.season.2023)

mls.seasonsplayers2012_23 <- mergeplayerseason2012.2023[!duplicated(mergeplayerseason2012.2023), ]

mlscareer <- mls.seasonsplayers2012_23

# mlscareer2 <- mlscareer %>%  
#   group_by(player_url)  %>%  
# distinct(Season, Comp, .keep_all = TRUE)


mlscareer2 <- mlscareer[-which(is.na(mlscareer$Comp)),]

mlscareer2 <- mlscareer2[grepl("/", mlscareer2$player_url),]

mlscareer2$PlayerId <- substr(mlscareer2$player_url, 30, 37)


#*Note performing Cleanse of Data set
library(dplyr)
#library(plyr) 

sort(unique(mlscareer$Season))


mlscareer2 <- mlscareer2 %>%
  group_by(player_url) %>%
  dplyr::mutate(SeasonNumber = dense_rank(Age))


mlscareerfinal <- mlscareer2 %>%
  group_by(player_url) %>%
  arrange(SeasonNumber) %>%
  dplyr::mutate(CountComp = row_number())

# Specify the string you want to find and keep values before
target_string <- "1. MLS"

# Use dplyr to filter rows based on the condition within each group
result_df <- mlscareerfinal %>%
  group_by(player_url) %>%
  filter(CountComp <= max(which(Comp == target_string)))

# Print the result
print(result_df)

# result_df <- result_df[!duplicated(result_df), ]
# 
# result_df <- result_df %>%  
#    group_by(player_url)  %>%  
#   distinct(Season, Comp,Squad, .keep_all = TRUE)

clipr::write_clip(result_df)


#Loading PlayerMatches 2012
mls.players.games.2012 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Data/PlayerGame/PlayerSeasonMLS2012.xlsx')

mls.players.games.2012 <- mls.players.games.2012 %>%
  group_by(Player)  %>%
distinct(Date, Comp, .keep_all = TRUE)

mls.players.games.2012 <- mls.players.games.2012 %>%
  group_by(Player) %>%
  dplyr::mutate(Games = row_number(),Season_Mins = cumsum(Min))

#Load 2013 MLS Players
mls.players.games.2013 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Data/PlayerGame/PlayerSeasonMLS2013.xlsx')

mls.players.games.2013 <- mls.players.games.2013 %>%
  group_by(Player)  %>%
  distinct(Date, Comp, .keep_all = TRUE)

mls.players.games.2013 <- mls.players.games.2013 %>%
  group_by(Player) %>%
  dplyr::mutate(Games = row_number(),Season_Mins = cumsum(Min))


mergeplayergames2012.2013 <- rbind(mls.players.games.2012, mls.players.games.2013)

#Load 2014 MLS Players
mls.players.games.2014 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Data/PlayerGame/PlayerSeasonMLS2014.xlsx')

mls.players.games.2014 <- mls.players.games.2014 %>%
  group_by(Player)  %>%
  distinct(Date, Comp, .keep_all = TRUE)

mls.players.games.2014 <- mls.players.games.2014 %>%
  group_by(Player) %>%
  dplyr::mutate(Games = row_number(),Season_Mins = cumsum(Min))


mergeplayergames2012.2014 <- rbind(mergeplayergames2012.2013, mls.players.games.2014)


#Load 2015 MLS Players
mls.players.games.2015 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Data/PlayerGame/PlayerSeasonMLS2015.xlsx')

mls.players.games.2015 <- mls.players.games.2015 %>%
  group_by(Player)  %>%
  distinct(Date, Comp, .keep_all = TRUE)

mls.players.games.2015 <- mls.players.games.2015 %>%
  group_by(Player) %>%
  dplyr::mutate(Games = row_number(),Season_Mins = cumsum(Min))


mergeplayergames2012.2015 <- rbind(mergeplayergames2012.2014, mls.players.games.2015)

#Load 2016 MLS Players
mls.players.games.2016 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Data/PlayerGame/PlayerSeasonMLS2016.xlsx')

mls.players.games.2016 <- mls.players.games.2016 %>%
  group_by(Player)  %>%
  distinct(Date, Comp, .keep_all = TRUE)

mls.players.games.2016 <- mls.players.games.2016 %>%
  group_by(Player) %>%
  dplyr::mutate(Games = row_number(),Season_Mins = cumsum(Min))

mls.players.games.2016$Season <- as.character(mls.players.games.2016$Season)

mergeplayergames2012.2016 <- rbind(mergeplayergames2012.2015, mls.players.games.2016)

#Load 2017 MLS Players
mls.players.games.2017 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Data/PlayerGame/PlayerSeasonMLS2017.xlsx')

mls.players.games.2017 <- mls.players.games.2017 %>%
  group_by(Player)  %>%
  distinct(Date, Comp, .keep_all = TRUE)

mls.players.games.2017 <- mls.players.games.2017 %>%
  group_by(Player) %>%
  dplyr::mutate(Games = row_number(),Season_Mins = cumsum(Min))


mergeplayergames2012.2017 <- rbind(mergeplayergames2012.2016, mls.players.games.2017)

#Load 2018 MLS Players
mls.players.games.2018 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Data/PlayerGame/PlayerSeasonMLS2018.xlsx')

mls.players.games.2018 <- mls.players.games.2018 %>%
  group_by(Player)  %>%
  distinct(Date, Comp, .keep_all = TRUE)

mls.players.games.2018 <- mls.players.games.2018 %>%
  group_by(Player) %>%
  dplyr::mutate(Games = row_number(),Season_Mins = cumsum(Min))


mergeplayergames2012.2018 <- rbind(mergeplayergames2012.2017, mls.players.games.2018)

#Load 2019 MLS Players
mls.players.games.2019 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Data/PlayerGame/PlayerSeasonMLS2019.xlsx')

mls.players.games.2019 <- mls.players.games.2019 %>%
  group_by(Player)  %>%
  distinct(Date, Comp, .keep_all = TRUE)

mls.players.games.2019 <- mls.players.games.2019 %>%
  group_by(Player) %>%
  dplyr::mutate(Games = row_number(),Season_Mins = cumsum(Min))


mergeplayergames2012.2019 <- rbind(mergeplayergames2012.2018, mls.players.games.2019)

#Load 2013 MLS Players
mls.players.games.2020 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Data/PlayerGame/PlayerSeasonMLS2020.xlsx')

mls.players.games.2020 <- mls.players.games.2020 %>%
  group_by(Player)  %>%
  distinct(Date, Comp, .keep_all = TRUE)

mls.players.games.2020 <- mls.players.games.2020 %>%
  group_by(Player) %>%
  dplyr::mutate(Games = row_number(),Season_Mins = cumsum(Min))


mergeplayergames2012.2020 <- rbind(mergeplayergames2012.2019, mls.players.games.2020)

#Load 2021 MLS Players
mls.players.games.2021 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Data/PlayerGame/PlayerSeasonMLS2021.xlsx')

mls.players.games.2021 <- mls.players.games.2021 %>%
  group_by(Player)  %>%
  distinct(Date, Comp, .keep_all = TRUE)

mls.players.games.2021 <- mls.players.games.2021 %>%
  group_by(Player) %>%
  dplyr::mutate(Games = row_number(),Season_Mins = cumsum(Min))


mergeplayergames2012.2021 <- rbind(mergeplayergames2012.2020, mls.players.games.2021)

#Load 2022 MLS Players
mls.players.games.2022 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Data/PlayerGame/PlayerSeasonMLS2022.xlsx')

mls.players.games.2022 <- mls.players.games.2022 %>%
  group_by(Player)  %>%
  distinct(Date, Comp, .keep_all = TRUE)

mls.players.games.2022 <- mls.players.games.2022 %>%
  group_by(Player) %>%
  dplyr::mutate(Games = row_number(),Season_Mins = cumsum(Min))


mergeplayergames2012.2022 <- rbind(mergeplayergames2012.2021, mls.players.games.2022)


#Load 2023 MLS Players
mls.players.games.2023 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Data/PlayerGame/PlayerSeasonMLS2023.xlsx')

mls.players.games.2023 <- mls.players.games.2023 %>%
  group_by(Player)  %>%
  distinct(Date, Comp, .keep_all = TRUE)

mls.players.games.2023 <- mls.players.games.2023 %>%
  group_by(Player) %>%
  dplyr::mutate(Games = row_number(),Season_Mins = cumsum(Min))


mergeplayergames2012.2023 <- rbind(mergeplayergames2012.2022, mls.players.games.2023)


clipr::write_clip(mergeplayergames2012.2023)

#Work On Player Key 
playerkey = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/PlayerKey.xlsx')

playerkey <- playerkey[order(playerkey$player_name),]


playerfbref <- mls.seasonsplayers2012_23 %>% select(1,2)


playerfbref <- trial[!duplicated(trial), ]


trial <- dplyr::filter(playerfbref, grepl("/",player_url))

playerfbref <- playerfbref[order(playerfbref$player_name),]


clipr::write_clip(playerfbref)

df2 <- playerkey %>% left_join( playerfbref, 
                             by=c('player_name'='player_name'))


sapply(df2, function(x) sum(is.na(x)))

sapply(playerkey, function(x) sum(is.na(x)))


playerbio = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/PlayerWeight.xlsx')

playerbio <- playerbio %>% 
  rename(PlayerTm = 1, DOBTm = 2)

#playertest <- playerbio %>% select(1,2)


test <- left_join(playerkey, playerbio, by='PlayerId','date_of_birth')

test <- test[order(test$PlayerNumber),]

clipr::write_clip(test)

nrow(playerkey[duplicated(playerkey), ])

sum(duplicated(playerkey$PlayerFBref))

duplicates <- playerkey[duplicated(playerkey$URL), ]

duplicates2 <- playerkey[duplicated(playerkey$PlayerFBref), ]

