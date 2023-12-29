#This R Script will condense player columns by Merging Players Names

#Date 24/11/2023
library(dplyr)
library(tidyverse)
library(worldfootballR)
library(readxl)
#Load 2012 MLS Players
mls.players2012 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Data/Bio/league2012PlayerBio.xlsx')

#Load 2013 MLS Players
mls.players2013 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Data/Bio/league2013PlayerBio.xlsx')

#Sorting Out common column names
sort(colnames(mls.players2013))

sort(colnames(mls.players2012))

#Removing Column names
mls.players2013 <- select(mls.players2013, -contract_there_expires)

mls.players2013 <- select(mls.players2013, -on_loan_from)

merge2012.2013 <- rbind(mls.players2012, mls.players2013)

#Removing Duplicate Player Id's
mls.players2012_13 <- merge2012.2013[!duplicated(merge2012.2013$PlayerId), ]

#Load 2014 MLS Players Do the same Process above for 2014
mls.players2014 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Data/Bio/league2014PlayerBio.xlsx')

mls.players2014 <- select(mls.players2014, -contract_there_expires)

mls.players2014 <- select(mls.players2014, -on_loan_from)

merge2012.2014 <- rbind(mls.players2014, mls.players2012_13)

mls.players2012_14 <- merge2012.2014[!duplicated(merge2012.2014$PlayerId), ]

#Load 2015 MLS Players Do the same Process above for 2014
mls.players2015 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Data/Bio/league2015PlayerBio.xlsx')

mls.players2015 <- select(mls.players2015, -contract_there_expires)

mls.players2015 <- select(mls.players2015, -on_loan_from)

mls.players2015 <- select(mls.players2015, -x2nd_club)

mls.players2012_14 <- select(mls.players2012_14, -date_of_death)

merge2012.2015 <- rbind(mls.players2015, mls.players2012_14)

mls.players2012_15 <- merge2012.2015[!duplicated(merge2012.2015$PlayerId), ]

#Load 2016 MLS Players Do the same Process above for 2014
mls.players2016 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Data/Bio/league2016PlayerBio.xlsx')

mls.players2016 <- select(mls.players2016, -contract_there_expires)

mls.players2016 <- select(mls.players2016, -on_loan_from)

mls.players2016 <- select(mls.players2016, -x2nd_club)

mls.players2016 <- select(mls.players2016, -date_of_death)

mls.players2016 <- select(mls.players2016, -tik_tok)

merge2012.2016 <- rbind(mls.players2016, mls.players2012_15)

mls.players2012_16 <- merge2012.2016[!duplicated(merge2012.2016$PlayerId), ]

#Load 2017 MLS Players Do the same Process above for 2014
mls.players2017 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Data/Bio/league2017PlayerBio.xlsx')

mls.players2017 <- select(mls.players2017, -contract_there_expires)

mls.players2017 <- select(mls.players2017, -on_loan_from)

mls.players2017 <- select(mls.players2017, -x2nd_club)

mls.players2017 <- select(mls.players2017, -date_of_death)

mls.players2017 <- select(mls.players2017, -tik_tok)

merge2012.2017 <- rbind(mls.players2017, mls.players2012_16)

mls.players2012_17 <- merge2012.2017[!duplicated(merge2012.2017$PlayerId), ]

#Load 2018 MLS Players Do the same Process above for 2014
mls.players2018 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Data/Bio/league2018PlayerBio.xlsx')

mls.players2018 <- select(mls.players2018, -contract_there_expires)

mls.players2018 <- select(mls.players2018, -on_loan_from)

#mls.players2018 <- select(mls.players2018, -x2nd_club)

#mls.players2018 <- select(mls.players2018, -date_of_death)

mls.players2018 <- select(mls.players2018, -tik_tok)

merge2012.2018 <- rbind(mls.players2018, mls.players2012_17)

mls.players2012_18 <- merge2012.2018[!duplicated(merge2012.2018$PlayerId), ]

#Load 2019 MLS Players Do the same Process above for 2014
mls.players2019 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Data/Bio/league2019PlayerBio.xlsx')

mls.players2019 <- select(mls.players2019, -contract_there_expires)

mls.players2019 <- select(mls.players2019, -on_loan_from)

mls.players2019 <- select(mls.players2019, -x2nd_club)

#mls.players2018 <- select(mls.players2018, -date_of_death)

#mls.players2019 <- select(mls.players2019, -tik_tok)

merge2012.2019 <- rbind(mls.players2019, mls.players2012_18)

mls.players2012_19 <- merge2012.2019[!duplicated(merge2012.2019$PlayerId), ]

#Load 2020 MLS Players Do the same Process above for 2014
mls.players2020 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Data/Bio/league2020PlayerBio.xlsx')

mls.players2020 <- select(mls.players2020, -contract_there_expires)

mls.players2020 <- select(mls.players2020, -on_loan_from)

mls.players2020 <- select(mls.players2020, -x2nd_club)

mls.players2020 <- select(mls.players2020, -date_of_death)

#mls.players2019 <- select(mls.players2019, -tik_tok)

merge2012.2020 <- rbind(mls.players2020, mls.players2012_19)

mls.players2012_20 <- merge2012.2020[!duplicated(merge2012.2020$PlayerId), ]

#Load 2021 MLS Players Do the same Process above for 2014
mls.players2021 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Data/Bio/league2021PlayerBio.xlsx')

mls.players2021 <- select(mls.players2021, -contract_there_expires)

mls.players2021 <- select(mls.players2021, -on_loan_from)

mls.players2021 <- select(mls.players2021, -x2nd_club)

mls.players2021 <- select(mls.players2021, -date_of_death)

mls.players2021 <- select(mls.players2021, -tik_tok)

merge2012.2021 <- rbind(mls.players2021, mls.players2012_20)

mls.players2012_21 <- merge2012.2021[!duplicated(merge2012.2021$PlayerId), ]


#Load 2022 MLS Players Do the same Process above for 2014
mls.players2022 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Data/Bio/league2022PlayerBio.xlsx')

mls.players2022 <- select(mls.players2022, -contract_there_expires)

mls.players2022 <- select(mls.players2022, -on_loan_from)

mls.players2022 <- select(mls.players2022, -x2nd_club)

mls.players2022 <- select(mls.players2022, -date_of_death)

mls.players2022 <- select(mls.players2022, -tik_tok)

mls.players2022 <- select(mls.players2022, -twitter_hashtag)

merge2012.2022 <- rbind(mls.players2022, mls.players2012_21)

mls.players2012_22 <- merge2012.2022[!duplicated(merge2012.2022$PlayerId), ]

#Load 2023 MLS Players Do the same Process above for 2014
mls.players2023 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Data/Bio/league2023PlayerBio.xlsx')

mls.players2023 <- select(mls.players2023, -contract_there_expires)

mls.players2023 <- select(mls.players2023, -on_loan_from)

mls.players2023 <- select(mls.players2023, -x2nd_club)

#mls.players2023 <- select(mls.players2023, -date_of_death)

#mls.players2023 <- select(mls.players2023, -tik_tok)

#mls.players2023 <- select(mls.players2023, -twitter_hashtag)

merge2012.2023 <- rbind(mls.players2023, mls.players2012_22)

mls.players2012_23 <- merge2012.2023[!duplicated(merge2012.2023$PlayerId), ]

mls.key.player <- mls.players2012_23[,c(1,3,20,27)]

mapped_players <- player_dictionary_mapping()


trialKey <- mls.key.player %>% left_join(mapped_players, 
                                         by=c('URL'='UrlTmarkt'))

sum(is.na(trialKey$PlayerFBref))
clipr::write_clip(trialKey)


clipr::write_clip(mls.players2012_23)

#Performing data cleanse of the dataset.
mls.players2012_2023 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Player2012_2023.xlsx')

#Trimming down data.
library(stringr)

mls.test <- mls.players2012_2023 %>% select(27,1,3,5,6,8,21,20)


#Create a position group. 
sort(unique(mls.test$position))

#Create a Case When

mls.test <- mls.test %>%
  mutate(Position_Group = case_when(position == "Attack" ~ 'Striker', 
                                    position == "Attack - Centre-Forward" ~ 'Striker',
                                    position == "Attack - Second Striker" ~ 'Striker',
                                    position == "Attack - Left Winger" ~ 'Winger',
                                    position == "Attack - Right Winger" ~ 'Winger',
                                    position ==  "Goalkeeper" ~ 'Goalkeeper',
                                    position == "Defender - Centre-Back" ~ 'Central Defender',
                                    position == "Defender" ~ 'Central Defender',
                                    position == "Defender - Left-Back" ~ 'Fullback',
                                    position == "Defender - Right-Back" ~ 'Fullback',
                                    position == "midfield" ~ 'Central Midfielder',
                                    position == "midfield - Attacking Midfield" ~ 'Central Midfielder',
                                    position == "midfield - Defensive Midfield" ~ 'Central Midfielder',
                                    position == "midfield - Central Midfield" ~ 'Central Midfielder',
                                    position == "midfield - Right Midfield" ~ 'Side Midfielder',
                                    position == "midfield - Left Midfield" ~ 'Side Midfielder'))

mls.test <- mls.test %>%
  mutate(Main_Position = case_when(position =="Attack" ~ 'Centre-Forward', 
                                    position == "Attack - Centre-Forward" ~ 'Centre-Forward',
                                    position == "Attack - Second Striker" ~ 'Second Striker',
                                    position == "Attack - Left Winger" ~ 'Left Winger',
                                    position == "Attack - Right Winger" ~ 'Right Winger',
                                    position == "Goalkeeper" ~ 'Goalkeeper',
                                    position == "Defender - Centre-Back" ~ 'Central Defender',
                                    position == "Defender" ~ 'Central Defender',
                                    position == "Defender - Left-Back" ~ 'Left-Back',
                                    position == "Defender - Right-Back" ~ 'Right-Back',
                                    position == "midfield" ~ 'Central Midfielder',
                                    position == "midfield - Attacking Midfield" ~ 'Attacking Midfielder',
                                    position == "midfield - Defensive Midfield" ~ 'Defensive Midfielder',
                                    position == "midfield - Central Midfield" ~ 'Central Midfielder',
                                    position == "midfield - Right Midfield" ~ 'Right Midfielder',
                                    position == "midfield - Left Midfield" ~ 'Left Midfielder'))

# To DO. Reassign players with distribution of feet  
mls.test <- mls.test %>% select(1,2,3,4,5,6,7,10,9,8)

sum(!complete.cases(mls.test$foot))

count(mls.test, foot)


mls.playersweights = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/PlayerWeightsUpdated.xlsx')

mls.players <- mls.playersweights %>% left_join(mls.test, by=c('PlayerId' = 'PlayerId',
                                                               'URL' = 'URL',
                                                               'PlayerTm' = 'player_name'))

count(mls.players, foot)

sum(!complete.cases(mls.players$height))

mls.players
# Creating a sample data frame
#df <- data.frame(column_name = c('A', 'B', 'A', 'B', 'A', 'C', NA, 'B', 'C', NA))

# Calculate the distribution of strings in the column
string_distribution <- table(mls.players$foot) / length(mls.players$foot)

# Function to fill NAs with random strings based on distribution
fill_na_random <- function(x) {
  if (is.na(x)) {
    return(sample(names(string_distribution), 1, prob = string_distribution))
  } else {
    return(x)
  }
}

# Apply the function to fill NAs in the column
mls.players$foot <- sapply(mls.players$foot, fill_na_random)

#Removing Unnecessary columns r
mls.players <- mls.players %>% select(2,3,4,9,6,10,13,14,1)


clipr::write_clip(mls.players)

#Chart of Player Positions in the dataset.
test <- count(mls.players, Position_Group)



test %>% 
     ggplot(aes(x = Position_Group, y = n)) +
     geom_col(mapping = aes(x=Position_Group, y=n), fill = c("black","purple","bisque3","red","pink","green","lightblue")) +
     scale_y_continuous(breaks = seq(0, 650, by=50)) +
     labs(x = "Player Positions", y = "Number of Players", title = "Bar Chart of Player Positions", subtitle = "MLS Players") +
     geom_text(aes(label= n), vjust=1.2, size=3, col = "white") +
     theme_bw()  +
     guides(x = guide_axis(angle = 90))

