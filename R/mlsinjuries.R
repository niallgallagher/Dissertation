#Date 24/11/2023
library(dplyr)
library(tidyverse)
library(readxl)

#Load 2012 MLS Players
mls.players.injury.2012 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Data/injury/league2012injuryplayers.xlsx')

#Load 2013 MLS Players
mls.players.injury.2013 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Data/injury/league2013injuryplayers.xlsx')

mergeinjury2012.2013 <- rbind(mls.players.injury.2012, mls.players.injury.2013)

#Removing Duplicate Player Id's
mergeinjury2012_2013 <- mergeinjury2012.2013[!duplicated(mergeinjury2012.2013), ]

#Load 2014 MLS Players Do the same Process above for 2014
mls.players.injury.2014 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Data/injury/league2014injuryplayers.xlsx')

mergeinjury2012.2014 <- rbind(mls.players.injury.2014, mergeinjury2012_2013)

mls.injuryplayers2012_14 <- mergeinjury2012.2014[!duplicated(mergeinjury2012.2014), ]

#Load 2015 MLS Players Do the same Process above for 2014
mls.players.injury.2015 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Data/injury/league2015injuryplayers.xlsx')

mergeinjury2012.2015 <- rbind(mls.players.injury.2015, mls.injuryplayers2012_14)

mls.injury.players2012_15 <- mergeinjury2012.2015[!duplicated(mergeinjury2012.2015), ]

#Load 2016 MLS Players Do the same Process above for 2014
mls.players.injury.2016 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Data/injury/league2016injuryplayers.xlsx')

mergeinjury2012.2016 <- rbind(mls.players.injury.2016, mls.injury.players2012_15)

mls.injury.players2012_16 <- mergeinjury2012.2016[!duplicated(mergeinjury2012.2016), ]

#Load 2017 MLS Players Do the same Process above for 2014
mls.players.injury.2017 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Data/injury/league2017injuryplayers.xlsx')

mergeinjury2012.2017 <- rbind(mls.players.injury.2017, mls.injury.players2012_16)

mls.injury.players2012_17 <- mergeinjury2012.2017[!duplicated(mergeinjury2012.2017), ]

#Load 2018 MLS Players Do the same Process above for 2014
mls.players.injury.2018 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Data/injury/league2018injuryplayers.xlsx')

mergeinjury2012.2018 <- rbind(mls.players.injury.2018, mls.injury.players2012_17)

mls.injury.players2012_18 <- mergeinjury2012.2018[!duplicated(mergeinjury2012.2018), ]

#Load 2019 MLS Players Do the same Process above for 2014
mls.players.injury.2019 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Data/injury/league2019injuryplayers.xlsx')

mergeinjury2012.2019 <- rbind(mls.players.injury.2019, mls.injury.players2012_18)

mls.injury.players2012_19 <- mergeinjury2012.2019[!duplicated(mergeinjury2012.2019), ]

#Load 2020 MLS Players Do the same Process above for 2014
mls.players.injury.2020 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Data/injury/league2020injuryplayers.xlsx')

mergeinjury2012.2020 <- rbind(mls.players.injury.2020, mls.injury.players2012_19)

mls.injury.players2012_20 <- mergeinjury2012.2020[!duplicated(mergeinjury2012.2020), ]

#Load 2021 MLS Players Do the same Process above for 2014
mls.players.injury.2021 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Data/injury/league2021injuryplayers.xlsx')

merge.injury2012.2021 <- rbind(mls.players.injury.2021, mls.injury.players2012_20)

mls.injury.players2012_21 <- merge.injury2012.2021[!duplicated(merge.injury2012.2021), ]

#Load 2022 MLS Players Do the same Process above for 2014
mls.players.injury.2022 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Data/injury/league2022injuryplayers.xlsx')

merge.injury2012.2022 <- rbind(mls.players.injury.2022, mls.injury.players2012_21)

mls.injury.players2012_22 <- merge.injury2012.2022[!duplicated(merge.injury2012.2022), ]

#Load 2023 MLS Players Do the same Process above for 2014
mls.players.injury.2023 = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Data/injury/league2023injuryplayers.xlsx')

mergeinjury2012.2023 <- rbind(mls.players.injury.2023, mls.injury.players2012_22)

mls.injury.players2012_23 <- mergeinjury2012.2023[!duplicated(mergeinjury2012.2023), ]

mls.injury.players2012_23.final <- filter(mls.injury.players2012_23,!is.na(injury))

mls.injury.players2012_23.final <- mls.injury.players2012_23.final %>% mutate(club = ifelse(is.na(club), 'Unknown', club))

mls.injury.players2012_23.final2 <- mls.injury.players2012_23.final

mls.injury.players2012_23.final2$injured_until <- as.Date('2023-10-30')


test <- filter(mls.injury.players2012_23.final,is.na(injured_until))

test$injured_until <- as.Date('2023-10-30')

test <- test %>% mutate(club = ifelse(is.na(club), 'Unknown', club))

test2 <- rbind(test,mls.injury.players2012_23.final)

test2 <- test2 %>%
     group_by(PlayerId) %>%
     distinct(injured_since, .keep_all = TRUE) 


sum(is.na(test2$club))

sum(is.na(mls.injury.players2012_23.final2))

clipr::write_clip(mls.injury.players2012_23.final)

#Load All Injuries
mls.players.injury = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/Data/InjuriesPlayers2012_2023.xlsx')

#Need to refence player Careers and Player Key
playerkey = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/CleansedDatasets/PlayerKey.xlsx')

playerCareer = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/CleansedDatasets/PlayerCareer.xlsx')

#Need to reduce dataset
PlayerSeasons <- playerCareer[,c(1:3,32:34)]

PlayerSeasons$SeasonYear <- substr(PlayerSeasons$Season, 1, 4)

mls.players.injury$SeasonYear <- substr(mls.players.injury$injured_since, 1, 4)


test <- PlayerSeasons %>% left_join( playerkey, 
                             by=c('player_url' = 'UrlFBref'))

#test <- left_join(PlayerSeasons, playerkey)

#test <- merge(PlayerSeasons,playerkey, all.x=TRUE)

test2 <- test %>% left_join(mls.players.injury, 
                                     by=c('PlayerId.y' = 'PlayerId',
                                          'SeasonYear' = 'SeasonYear'))

test3 <- test2[!is.na(test2$injured_since),]

test4 <- test3 %>% distinct(PlayerId.x, injured_since, .keep_all = TRUE)


mlsinjuries.update <- test4[,c(1,2,4,5,7,9,10,11,16:21)]

injury.type <- sort(unique(mlsinjuries.update$injury))


clipr::write_clip(injury.type)


sum(!complete.cases(mlsinjuries.update$duration))

mlsinjuries.update$DaysInjured <-sub(" days*", "", mlsinjuries.update$duration) 

mlsinjuries.update$DaysInjured <- as.numeric(mlsinjuries.update$DaysInjured)

mlsinjuries.update2 <- mlsinjuries.update %>%
  add_column(`No-Time-Loss` = NA,
             `Slight (1-3 Days)` = NA,
             `Minor (3-7 Days)` = NA,
             `Moderate (8-28 Days)` = NA,
             `Major (28+ Days)` = NA)

mlsinjuries.update2$`No-Time-Loss` <- ifelse(mlsinjuries.update2$DaysInjured == 0, 1,0)

mlsinjuries.update2$`Slight (1-3 Days)` <- ifelse(mlsinjuries.update2$DaysInjured > 0 & mlsinjuries.update2$DaysInjured <= 3, 1,0)

mlsinjuries.update2$`Minor (3-7 Days)` <- ifelse(mlsinjuries.update2$DaysInjured > 3 & mlsinjuries.update2$DaysInjured <= 7, 1,0)

mlsinjuries.update2$`Moderate (8-28 Days)` <- ifelse(mlsinjuries.update2$DaysInjured > 7 & mlsinjuries.update2$DaysInjured <= 28, 1,0)

mlsinjuries.update2$`Major (28+ Days)` <- ifelse(mlsinjuries.update2$DaysInjured > 28, 1,0)


clipr::write_clip(mlsinjuries.update2)


#Load Injuries Data 
mls.players.injury = read_xlsx('C:/Users/niall/OneDrive/Documents/Dissertation/CleansedDatasets/MLsInjuries.xlsx')

#define vector of strings
remove <- c('Appendectomy',
            'Appendicitis',
            'bronchitis',
            'Cancer',
            'Circulation problems',
            'cold',
            'Corona virus',
            'Dental surgery',
            'depression',
            'Fever',
            'flu',
            'Food poisoning',
            'Ill',
            'infection',
            'influenza',
            'Insect bite',
            'Intestinal virus',
            'Lymphatic cancer',
            'malaria',
            'Nerval disease',
            'pneumonia',
            'Quarantine',
            'Stomach flu',
            'stomach problems',
           'Tonsillitis',
            'Tooth infection',
            'Toothache',
            'Virus'
)

#remove rows that contain any string in the vector in the team column
mls.players.injury.reduce <- mls.players.injury[!grepl(paste(remove, collapse='|'), mls.players.injury$injury),]


rep_str = c('ankle sprain'= 'Ankle sprain',
'bruise' = 'Bruise', 
'collapsed lung'= 'Collapsed lung', 
'combustion'= 'Combustion', 
'concussion'= 'Concussion', 
'fatigue fracture'= 'Fatigue fracture', 
'flesh wound'= 'Flesh wound', 
'fracture'= 'Fracture', 
'heart problems'= 'Heart problems', 
'horse kiss'= 'Horse kiss', 
'inflamed wound'= 'Bruise', 
'inflammation'= 'Bruise', 
'laceration wound'= 'Laceration wound', 
'minor knock'= 'Minor knock', 
'muscular problems'= 'Muscular problems', 
'sprain'= 'Sprain', 
'strain'= 'Strain', 
'surgery'= 'Surgery', 
'unknown injury'= 'Unknown injury')

mls.players.injury.reduce$injury <- str_replace_all(mls.players.injury.reduce$injury, rep_str)

