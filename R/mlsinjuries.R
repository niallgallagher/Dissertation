#Date 24/11/2023
library(dplyr)
library(tidyverse)

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


# test <- test %>% mutate(injured_until = ifelse(is.na(injured_until), '2023-11-01', injured_until))
# 
# test <- test %>% mutate(games_missed = ifelse(is.na(games_missed), 'Unknown', games_missed))
# 
#
# 
# test2 <- rbind(test,mls.injury.players2012_23.final)
# 
# 
# #mls.injury.players2012_23 <- test2[!duplicated(test2$), ]
# 
# 
# test2 <- test2 %>%
#   group_by(PlayerId) %>%
#   distinct(injured_since, .keep_all = TRUE) 
