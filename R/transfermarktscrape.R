install.packages("worldfootballR")
install.packages("rtools")
devtools::install_github("JaseZiv/worldfootballR")

library(worldfootballR)


test <- tm_team_player_urls(team_url = "https://www.transfermarkt.com/fc-burnley/startseite/verein/1132/saison_id/2020")

