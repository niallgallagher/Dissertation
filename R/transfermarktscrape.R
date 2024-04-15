install.packages("worldfootballR")


#install.packages("rtools")
#library(rtools)

library(devtools)

devtools::install_github("JaseZiv/worldfootballR")

library(worldfootballR)
library(rvest)
library(dplyr)


test2 <- worldfootballR::tm_team_player_urls(team_url = "https://www.transfermarkt.co.uk/real-madrid/startseite/verein/418")


burnley_player_urls <- tm_team_player_urls(team_url = "https://www.transfermarkt.com/fc-burnley/startseite/verein/1132/saison_id/2020")


burnley_bios <- tm_player_bio(player_urls = burnley_player_urls)


rm <- tm_team_player_urls("https://www.transfermarkt.co.uk/real-madrid/startseite/verein/418")


usa_match_results <- load_match_results(country = "USA", gender = c("M"), season_end_year = c(2012:2023), tier = "1st")
dplyr::glimpse(eng_match_results)


clipr::write_clip(usa_match_results)




tm_team_player_urls(team_url = "https://www.transfermarkt.com/fc-burnley/startseite/verein/1132/saison_id/2020")


all_leeds_united_links <- tm_team_player_urls(team_url = "https://www.transfermarkt.com/leeds-united/startseite/verein/399")







burnley_player_urls3 <- tm_team_player_urls(team_url = "https://www.transfermarkt.com/fc-burnley/startseite/verein/1132/saison_id/2020")

burnley_player_urls <- tm_team_player_urls(team_url = "https://www.transfermarkt.com/fc-burnley/startseite/verein/1132/saison_id/2020")






test <- tm_team_player_urls(team_url = "https://www.transfermarkt.com/charlotte-fc/spielplan/verein/78435/saison_id/2022")


epl_teams <- tm_league_team_urls(country_name = "England", start_year = 2021)
# get all EPL managers for the 2021/22 season
epl_managers <- tm_team_player_urls(team_urls = epl_teams)


jfc <- tm_team_player_urls(epl_teams[1])


epl_managers <- tm_team_staff_urls(team_urls = epl_teams, staff_role = "Manager")

library(tidyr)



burnley_bios <- tm_player_bio(player_urls = trial23)


st


mapped_players <- player_dictionary_mapping()
dplyr::glimpse(mapped_players)

tm_team_player_urls(team_urls = "https://www.transfermarkt.com/fc-burnley/startseite/verein/1132/saison_id/2020")

burnley_player_urls <- mapped_players$UrlTmarkt


library(rvest)

page <- "https://www.transfermarkt.co.uk/transfers/transferrekorde/statistik/top/plus/0/galerie/0?saison_id=2000"

scraped_page <- read_html(page)

PlayerNames  % html_nodes("#yw2 .spielprofil_tooltip") %>% html_text() #%>% as.character()
TransferValue % html_nodes(".rechts.hauptlink a") %>% html_text() %>% as.character()


laliga_debutants <- tm_league_debutants(country_name = "United States", debut_type = "league", debut_start_year = 2021, debut_end_year = 2021)


league_one_PRO_debutants <- tm_league_debutants(country_name = "", league_url = "https://www.transfermarkt.com/major-league-soccer/startseite/wettbewerb/MLS1", debut_type = "league", debut_start_year = 2021, debut_end_year = 2021)
dplyr::glimpse(league_one_PRO_debutants)


trial23 <- league_one_PRO_debutants$player_url

test <- laliga_debutants[!duplicated(laliga_debutants), ]

messi_primary <- fb_player_scouting_report(player_url = "https://fbref.com/en/players/d70ce98e/Lionel-Messi", pos_versus = "primary")
dplyr::glimpse(messi_primary)



ederson_summary <- fb_player_match_logs("https://fbref.com/en/players/3bb7b8b4/Ederson", season_end_year = 2021, stat_type = 'keepers')
dplyr::glimpse(ederson_summary)

man_city_url <- "https://fbref.com/en/squads/b8fd03ef/Manchester-City-Stats"
man_city_wages <- fb_squad_wages(team_urls = man_city_url)


tm_team_player_urls(team_url = "https://www.transfermarkt.de/fc-burnley/startseite/verein/1132/saison_id/2020")


hazard_bio <- tm_player_bio(player_url = "https://www.transfermarkt.de/fc-burnley/startseite/verein/1132/saison_id/2020")


#----- for a single player: -----#
hazard_injuries <- tm_player_injury_history(player_urls = "https://www.transfermarkt.com/eden-hazard/profil/spieler/50202")
dplyr::glimpse(hazard_injuries)

#----- for multiple players: -----#
# # can make use of a tm helper function:
 burnley_player_urls <- tm_team_player_urls(team_url = "https://www.transfermarkt.com/fc-burnley/startseite/verein/1132/saison_id/2021")
# # then pass all those URLs to the tm_player_injury_history
 burnley_player_injuries <- tm_player_injury_history(player_urls = burnley_player_urls)
 