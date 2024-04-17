####Analysis MLS####
####Niall Gallagher####
####Sheet *7*########

#Libraries 
library(readxl)
library(tidyverse)
library(lubridate)
library(scales)
library(ggridges)
library(ggplot2)
library(dplyr)
library(corrplot)
library(survival)
library(survminer)
#library(data.table)

#Load Excel Files
mls.fixtures <- read_excel('C:/Users/niall/OneDrive/Documents/Dissertation/Analysis/MLSFixtures.xlsx')
mls.injury.fixtures <-read_excel('C:/Users/niall/OneDrive/Documents/Dissertation/Analysis/GameInjuries.xlsx')
mls.player.fixtures <-read_excel('C:/Users/niall/OneDrive/Documents/Dissertation/Analysis/MLSFull.xlsx')

#Removing ".x" in column names of Injuries and Fixture Data
names(mls.injury.fixtures) <- gsub("\\.x", "", names(mls.injury.fixtures))

names(mls.player.fixtures) <- gsub("\\.x", "", names(mls.player.fixtures))

#Removing Illnesses from Injuries
mls.injury.fixtures <- mls.injury.fixtures[mls.injury.fixtures$injury != "Ill" & mls.injury.fixtures$injury != "Corona virus", ]


# set up plotting theme
theme_niall <- function(legend_pos="top", base_size=12, font=NA){
  
  # come up with some default text details
  txt <- element_text(size = base_size+3, colour = "black", face = "plain")
  bold_txt <- element_text(size = base_size+3, colour = "black", face = "bold")
  
  # use the theme_minimal() theme as a baseline
  theme_minimal(base_size = base_size, base_family = font)+
    theme(text = txt,
          # axis title and text
          axis.title.x = element_text(size = 15, hjust = 1),
          axis.title.y = element_text(size = 15, hjust = 1),
          # gridlines on plot
          panel.grid.major = element_line(linetype = 2),
          panel.grid.minor = element_line(linetype = 2),
          # title and subtitle text
          plot.title = element_text(size = 18, colour = "grey25", face = "bold"),
          plot.subtitle = element_text(size = 16, colour = "grey44"),
          
          ###### clean up!
          legend.key = element_blank(),
          # the strip.* arguments are for faceted plots
          strip.background = element_blank(),
          strip.text = element_text(face = "bold", size = 13, colour = "grey35")) +
    
    #----- AXIS -----#
    theme(
      #### remove Tick marks
      axis.ticks=element_blank(),
      
      ### legend depends on argument in function and no title
      legend.position = legend_pos,
      legend.title = element_blank(),
      legend.background = element_rect(fill = NULL, size = 0.5,linetype = 2)
      
    )
}


plot_cols <- c("#498972", "#3E8193", "#BC6E2E", "#A09D3C", "#E06E77", "#7589BC", "#A57BAF", "#4D4D4D")


#Counting games injuries, stadiums, etc
dplyr::count(mls.injury.fixtures, injury)            # Applying count function
dplyr::count(mls.injury.fixtures, Games)
dplyr::count(mls.fixtures, `Stadium Name`)
dplyr::count(mls.injury.fixtures, Position_Group)
dplyr::count(mls.injury.fixtures, Surface)
dplyr::count(mls.fixtures, Surface)

#Counting Max minutes per game. This is counting minutes played on each surface.
games_played_per_player_surface.t <- mls.player.fixtures %>%
  group_by(Surface,MatchID) %>%
  summarise(max = max(Min, na.rm=TRUE))

games_played_per_player_surface.t %>% 
  group_by(Surface) %>% 
  count(max) 

#Calculating Games played 
trial <- mls.player.fixtures

trial <- trial[complete.cases(trial[ , c('Season_Mins')]), ]

games_played_per_player_surface <- trial %>%
  group_by(Surface) %>%
  summarize(GamesPlayed = n())

# Creating Tables comparing surfaces
df1 <- data.frame(
  Injury = c(rep("Injured", 227), rep("Not-Injured", 13453)),
  Surface = c(rep("Field Turf", 13680))
)

df2 <- data.frame(
  Injury = c(rep("Injured", 630), rep("Not-Injured", 40297)),
  Surface = c(rep("Grass", 40927))
)

df3 <- data.frame(
  Injury = c(rep("Injured", 82), rep("Not-Injured", 4174)),
  Surface = c(rep("Hybrid", 4256))
)

#Chi-Square Tests being performed
chi.test <- rbind(df1,df2)
chisq.test(chi.test$Surface, chi.test$Injury, correct=FALSE)
table(chi.test)

chi.test2 <- rbind(df1,df3)
chisq.test(chi.test2$Surface, chi.test2$Injury, correct=FALSE)

chi.test3 <- rbind(df2,df3)
chisq.test(chi.test3$Surface, chi.test3$Injury, correct=FALSE)

#Counting Injury Severity Based on Player Position on Surface.
mls.player.fixtures %>% 
  distinct(PlayerId) %>%
  n_distinct()

mls.injury.fixtures %>% 
  group_by(Surface) %>% 
  count(Position_Group) 

mls.injury.fixtures %>% 
  group_by(Position_Group) %>% 
  count(`Slight (1-3 Days)`) 

mls.injury.fixtures %>% 
  group_by(Position_Group) %>% 
  count(`Minor (3-7 Days)`) 

mls.injury.fixtures %>% 
  group_by(Position_Group) %>% 
  count(`Moderate (8-28 Days)`) 

mls.injury.fixtures %>% 
  group_by(Position_Group) %>% 
  count(`Major (28+ Days)`) 

dplyr::count(mls.player.fixtures, Position_Group)

#Counting Per Player and Surface
mls.player.fixtures %>% 
  group_by(PlayerId) %>% 
  count(Position_Group) 

#Injury by Surface Type
mls.injury.fixtures %>% 
  group_by(Surface) %>% 
  count(`Slight (1-3 Days)`) 

#Injury by Surface Type
mls.injury.fixtures %>% 
  group_by(Surface) %>% 
  count(`Minor (3-7 Days)`) 

mls.injury.fixtures %>% 
  group_by(Surface) %>% 
  count(`Moderate (8-28 Days)`) 

mls.injury.fixtures %>% 
  group_by(Surface) %>% 
  count(`Major (28+ Days)`) 


#Tables of Injury Severity
df1.sev <- data.frame(
  Injury = c(rep("Major", 106), rep("Not-Major", 13574)),
  Surface = c(rep("Field Turf", 13680))
)

df2.sev <- data.frame(
  Injury = c(rep("Major", 266), rep("Not-Major", 40031)),
  Surface = c(rep("Grass", 40297))
)

df.sev <- rbind(df1.sev,df2.sev)

#Chi-Square Injury Severity
chisq.test(df.sev$Surface, df.sev$Injury, correct=FALSE)

#Moderate Injuries
df1.mod <- data.frame(
  Injury = c(rep("Moderate", 104), rep("Not-Moderate", 13576)),
  Surface = c(rep("Field Turf", 13680))
)

df2.mod <- data.frame(
  Injury = c(rep("Moderate", 318), rep("Not-Moderate", 39979)),
  Surface = c(rep("Grass", 40297))
)

df.mod <- rbind(df1.mod,df2.mod)

chisq.test(df.mod$Surface, df.mod$Injury, correct=FALSE)

#Minor Injuries
df1.light <- data.frame(
  Injury = c(rep("Minor", 14), rep("Not-Minor", 13666)),
  Surface = c(rep("Field Turf", 13680))
)

df2.light<- data.frame(
  Injury = c(rep("Minor", 40), rep("Not-Minor", 40257)),
  Surface = c(rep("Grass", 40297))
)

df.light <- rbind(df1.light,df2.light)

chisq.test(df.light$Surface, df.light$Injury, correct=FALSE)

#Data Frame of Injury Severity
surface <- c("Artifical Turf", "Natural Grass", "Hybrid Surface")
injs.sev <- c("Slight (1-3 Days)", "Minor (4-7 Days)", "Moderate (8-28 Days)","Major (28+ Days)")
InjSev <-  data.frame(
  "Injury_Severity" = c(rep(injs.sev, 3)),
  "Surface" = c(rep(surface, 4)),
  "Injury_Count" = c(3,40,36,106,6,12,104,266,0,14,318,34)
)

#Joining Injury Count to Severity 
total_injuries <- InjSev %>%
  group_by(Surface) %>%
  summarise(total_injury_count = sum(Injury_Count)) %>%
  arrange(desc(total_injury_count))  # Arrange by total injury count in descending order

# Arrange the dataframe based on the total injury count
InjSev <- InjSev %>%
  left_join(total_injuries, by = "Surface")

# Calculate percentage of injury count for each surface type and injury severity level
InjSev <- InjSev %>%
  mutate(Percentage = (Injury_Count / total_injury_count) * 100)

# Reorder the levels of Injury_Severity
InjSev$Injury_Severity <- factor(InjSev$Injury_Severity, levels = c("Minor (4-7 Days)","Moderate (8-28 Days)","Major (28+ Days)","Slight (1-3 Days)"))
InjSev_filtered <- filter(InjSev, Injury_Severity != "Slight (1-3 Days)")

#Plotting Injury Severity
ggplot(InjSev_filtered, aes(x = reorder(Surface, -total_injury_count), y = Percentage, fill = Injury_Severity)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage Breakdown of Injury Severity by Surface Type",
       x = "Surface Type", y = "Percentage") +
  geom_text(aes(label=paste0(sprintf("%1.2f", Percentage),"%")),
            position=position_stack(vjust=0.5), color = "white", size = 5) +
  scale_fill_manual(values = c("Slight (1-3 Days)" = "#00b300", 
                               "Minor (4-7 Days)" = "#00b300", 
                               "Moderate (8-28 Days)" = "#ff8000", 
                               "Major (28+ Days)" = "#ff0000"),
                    name = "Injury Severity") +
  theme_niall() +
  scale_y_continuous(labels = scales::percent_format(scale = 1))

#Injuries over Season **Line Plot**
line.plot <- mls.injury.fixtures %>% 
  filter(`Games` < 35) %>% 
  group_by(Games) %>% summarise(num_players = n_distinct(PlayerId)) %>% 
  ggplot(aes(x=Games, y=num_players)) +
  geom_line(colour = "#cc0000", size = 1) +
  geom_point(colour = "#cc0000", size = 3) +
  labs(x= "Game Number", y= "Number Injuries") +
  ggtitle("NUMBER OF INJURIES DECREASING THROUGH GAMES", subtitle = "Week 10 has the most injuries (n=46)") +
  theme_niall() +
  scale_x_continuous(breaks = seq(0, 34, by = 4)) +
  scale_y_continuous(breaks = seq(0, 50, by = 5))

line.plot
# Define custom colors for each category
custom_colors <- c("Grass" = "#4daf4a", "Field Turf" = "#e41a1c", "Hybrid" = "blue")

#Injuries over Season per Surface **Line Plot**
line.plot.surface <- mls.injury.fixtures %>% 
  filter(Games < 35) %>% 
  group_by(Games, Surface) %>% 
  summarise(num_players = n_distinct(PlayerId)) %>% 
  ggplot(aes(x = Games, y = num_players, color = Surface, group = Surface)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(x = "Game Number", y = "Number Injuries") +
  ggtitle("NUMBER OF INJURIES DECREASING THROUGH GAMES", subtitle = "Week 10 has the most injuries (n=46)") +
  theme_niall() +
  scale_x_continuous(breaks = seq(0, 34, by = 4)) +
  scale_y_continuous(breaks = seq(0, 50, by = 5)) +
  scale_color_manual(values = custom_colors)  # Set custom colors

line.plot.surface

#Histogram plotting minutes and Injuries
minute.breakdown.game <- select(mls.injury.fixtures,`Min`)

filtered_data_minute <- minute.breakdown.game %>%
  filter(Min <= 90 | !is.na(Min))

filtered_data_minute <- filtered_data_minute %>%
  mutate(Min_Bin = cut(Min, breaks = seq(0, 90, by = 5), labels = FALSE))

filtered_data_minute$bin_group <- ifelse(filtered_data_minute$Min_Bin <= 9, "First Half", "Second Half")

# Define colors for the two groups
group_colors <- c("First Half" = "#4daf4a", "Second Half" = "#e41a1c")

# Histogram of Minute Breakdown
ggplot(filtered_data_minute, aes(x = Min_Bin, fill = bin_group)) +
  geom_histogram(binwidth = 1, color = "black") +
  labs(x = "Binned Time (every 5 minutes)", y = "Injury Count", 
       title = "Histogram of Injuries in 5-Minute Intervals",
       subtitle = "Each bar represents a 5-minute interval of a game",
       caption = "MLS Data 2012 - 2023") +
  scale_fill_manual(values = group_colors) +  # Assign colors to the groups
  theme_niall() +
  scale_x_continuous(breaks = seq(0, 18, by = 3))

#DataFrame of Injuries and Surface
games <-  data.frame(
  "Games" = c(1126, 3233, 370),
  "Surface" = c("Field Turf", "Grass", "Hybrid"),
  "Injury_Count" = c(227,630,82)
)

#Plotting games and Injuries on Surface Type
plot.games <- games %>% 
  ggplot(aes(x= Surface, y= Games, fill = Surface)) +
  geom_col(alpha = 0.6) +
  labs(x= "Surface Type", y= "Number of Games") +
  ggtitle("Breakdown of MLS Games Based on Surface Type", subtitle = "68% of the Games in the MLS were played on Natural Grass") +
  coord_flip() + theme_niall() +
  scale_color_manual(values = custom_colors)

plot.injury <- games %>% 
  #count(injury) %>% 
  ggplot(aes(x= Surface, y= Injury_Count,fill = Surface)) +
  geom_col(alpha = 0.6) +
  labs(x= "Surface Type", y= "Number of Injuries") +
  ggtitle("Breakdown of MLS Injuries Based on Surface Type") +
  coord_flip() + theme_niall() +
  scale_color_manual(values = custom_colors)

#Merging Two Charts Together
gridExtra::grid.arrange(plot.games, plot.injury)

#Plotting Percentages
plot.injury.pct <-  games %>% 
  group_by(Surface) %>% 
  mutate(perc_injured = Injury_Count / Games) %>% 
  ggplot(aes(x= Surface, y= perc_injured, fill = Surface)) +
  geom_col(alpha = 0.6) +
  geom_text(aes(label = percent(perc_injured)), vjust=1, size = 7, colour = "black") +
  geom_hline(yintercept = 0.1986, linetype = "dashed", colour = "purple", size = 0.1,) +
  annotate(geom = "text", x=1, y= 0.1286, label = "Injuries occur on\n19.86% of all Games", colour = "purple", size = 5) +
  ggtitle("MORE LIKELY TO BE INJURED ON SYNTHETIC OR HYBRID SURFACES", subtitle = "Of over 4729 Games analysed, injuries occured more frequently on \nSynthetic and Hybrid Surfaces") +
  theme_niall() +
  scale_color_manual(values = custom_colors) +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank())

plot.injury.pct

####Positon Breakdown and Injury Severity####
position.games <-  data.frame(
  "Injury_Severity" = c("Slight(1-3)","Minor(4-7)","Moderate(8-28)","Major(28+)",
                        "Slight(1-3)","Minor(4-7)","Moderate(8-28)","Major(28+)",
                        "Slight(1-3)","Minor(4-7)","Moderate(8-28)","Major(28+)",
                        "Slight(1-3)","Minor(4-7)","Moderate(8-28)","Major(28+)",
                        "Slight(1-3)","Minor(4-7)","Moderate(8-28)","Major(28+)",
                        "Slight(1-3)","Minor(4-7)","Moderate(8-28)","Major(28+)",
                        "Slight(1-3)","Minor(4-7)","Moderate(8-28)","Major(28+)"),
  "Position" = c("Central Defender", "Central Midfielder", "Fullback", "Goalkeeper","Side Midfielder","Striker","Winger",
                 "Central Defender", "Central Midfielder", "Fullback", "Goalkeeper","Side Midfielder","Striker","Winger",
                 "Central Defender", "Central Midfielder", "Fullback", "Goalkeeper","Side Midfielder","Striker","Winger",
                 "Central Defender", "Central Midfielder", "Fullback", "Goalkeeper","Side Midfielder","Striker","Winger"),
  "Injury_Count" = c(1,16,57,25,0,14,52,73,5,7,23,13,1,15,94,102,1,6,12,58,0,8,141,75,1,0,79,60)
)

#Positional Injury Breakdown
position.chart.1 <- mls.injury.fixtures %>% 
  #group_by(Surface) %>% 
  count(Position_Group) %>%
  ggplot(aes(x= Position_Group, y= n)) +
  geom_col(fill = "#4daf4a", colour = "#4daf4a", alpha = 0.6) +
  labs(x= "Position", y= "Injury Count") +
  ggtitle("Breakdown of MLS Injuries Based on Position") +
  coord_flip() + theme_niall()


position.chart.2 <- position.games %>%
  group_by(Position) %>%
  mutate(perc_injury = Injury_Count / sum(Injury_Count) * 100) %>%
  ggplot(aes(x = Position, y = perc_injury, fill = Injury_Severity)) +
  geom_bar(stat = "identity", position = "dodge") +
  ylab("Percentage of Injuries") +
  ggtitle("Percentage of Injuries by Position and Severity") +
  theme_niall() +
  scale_fill_manual(values = c("Slight(1-3)" ="green" , 
                               "Minor(4-7)" = "yellow", 
                               "Moderate(8-28)" = "orange", 
                               "Major(28+)" = "red")) +
  labs(fill = "Injury Severity") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  coord_flip()

gridExtra::grid.arrange(position.chart.1, position.chart.2)


####Weather Variables####
#Separating Matches Based On Injury
mls.fixture.no.injury <- mls.fixtures[!(mls.fixtures$MatchID %in% mls.injury.fixtures$MatchID), , drop = FALSE]

#Selecting Weather Variables
mls.fixture.no.injury <- select(mls.fixture.no.injury, 9,20, 22:28,30:34)
mls.fixture.no.injury$MatchInjury <- rep("No",3952)
mls.injury.fixtures.yes<- select(mls.injury.fixtures,20,25:32,34:38)

mls.injury.fixtures.yes$MatchInjury <- rep("Yes",939)

#MLS Weather Dataset
mls.weatherdataset <- rbind(mls.injury.fixtures.yes,mls.fixture.no.injury)

#MLS Weather Dataset Candle Plots
#Precipitation
w1 <- mls.weatherdataset %>% 
  filter(Roof %in% c("Open"),`precipitation (mm)` > 0) %>% 
  distinct(MatchID, `precipitation (mm)`, MatchInjury) %>% 
  ggplot(aes(y= `precipitation (mm)`, x = MatchInjury, fill = MatchInjury, colour = MatchInjury)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(values = c("#00b300", "#cc0000"), guide = "none") +
  scale_colour_manual(values = c("#00b300", "#cc0000"), guide = "none") +
  labs(x= "Player Injured?") +
  ggtitle("Precipitation") +
  coord_flip() +
  theme_niall()

#Wind Speed
w2 <- mls.weatherdataset %>% 
  filter(Roof %in% c("Open")) %>% 
  distinct(MatchID, `wind_speed_10m (km/h)`, MatchInjury) %>% 
  ggplot(aes(y= `wind_speed_10m (km/h)`, x = MatchInjury, fill = MatchInjury, colour = MatchInjury)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(values = c("#00b300", "#cc0000"), guide = "none") +
  scale_colour_manual(values = c("#00b300", "#cc0000"), guide = "none") +
  labs(x= "Player Injured?") +
  ggtitle("Wind Speed") +
  coord_flip() +
  theme_niall()

#Humidity
w3 <- mls.weatherdataset %>% 
  filter(Roof %in% c("Open")) %>% 
  distinct(MatchID, `relative_humidity_2m (%)`, MatchInjury) %>% 
  ggplot(aes(y= `relative_humidity_2m (%)`, x = MatchInjury, fill = MatchInjury, colour = MatchInjury)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(values = c("#00b300", "#cc0000"), guide = "none") +
  scale_colour_manual(values = c("#00b300", "#cc0000"), guide = "none") +
  labs(x= "Player Injured?") +
  ggtitle("Relative Humidity") +
  coord_flip() +
  theme_niall()

#Cloud Cover
w4 <- mls.weatherdataset %>% 
  filter(Roof %in% c("Open")) %>% 
  distinct(MatchID, `cloud_cover (%)`, MatchInjury) %>% 
  ggplot(aes(y= `cloud_cover (%)`, x = MatchInjury, fill = MatchInjury, colour = MatchInjury)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(values = c("#00b300", "#cc0000"), guide = "none") +
  scale_colour_manual(values = c("#00b300", "#cc0000"), guide = "none") +
  labs(x= "Player Injured?") +
  ggtitle("Cloud Cover") +
  coord_flip() +
  theme_niall()

#Soil Temperature
w5 <- mls.weatherdataset %>% 
  filter(Roof %in% c("Open")) %>% 
  distinct(MatchID, `soil_temperature_0_to_7cm (°C)`, MatchInjury) %>% 
  ggplot(aes(y= `soil_temperature_0_to_7cm (°C)`, x = MatchInjury, fill = MatchInjury, colour = MatchInjury)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(values = c("#00b300", "#cc0000"), guide = "none") +
  scale_colour_manual(values = c("#00b300", "#cc0000"), guide = "none") +
  labs(x= "Player Injured?") +
  ggtitle("Soil Temperature") +
  coord_flip() +
  theme_niall()

#Soil Moisture
w6 <- mls.weatherdataset %>%
  filter(Roof %in% c("Open")) %>%
  distinct(MatchID, `soil_moisture_0_to_7cm (m³/m³)`, MatchInjury) %>%
  ggplot(aes(y = `soil_moisture_0_to_7cm (m³/m³)`, x = MatchInjury, fill = MatchInjury, colour = MatchInjury)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(values = c("#00b300", "#cc0000"), guide = "none") +
  scale_colour_manual(values = c("#00b300", "#cc0000"), guide = "none") +
  labs(x = "Player Injured?") +
  ggtitle("Soil Moisture") +
  coord_flip() +
  theme_niall()

#Temperature
w7 <- mls.weatherdataset %>%
  filter(Roof %in% c("Open")) %>%
  distinct(MatchID, `temperature_2m (°C)`, MatchInjury) %>%
  ggplot(aes(y = `temperature_2m (°C)`, x = MatchInjury, fill = MatchInjury, colour = MatchInjury)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(values = c("#00b300", "#cc0000"), guide = "none") +
  scale_colour_manual(values = c("#00b300", "#cc0000"), guide = "none") +
  labs(x = "Player Injured?") +
  ggtitle("Temperature") +
  coord_flip() +
  theme_niall()

#Apparent Temperature 
w8 <- mls.weatherdataset %>%
  filter(Roof %in% c("Open")) %>%
  distinct(MatchID, `apparent_temperature (°C)`, MatchInjury) %>%
  ggplot(aes(y = `apparent_temperature (°C)`, x = MatchInjury, fill = MatchInjury, colour = MatchInjury)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(values = c("#00b300", "#cc0000"), guide = "none") +
  scale_colour_manual(values = c("#00b300", "#cc0000"), guide = "none") +
  labs(x = "Player Injured?") +
  ggtitle("Apparent Temperature") +
  coord_flip() +
  theme_niall()

#Plotting All 8 Charts Together
gridExtra::grid.arrange(w1, w2, w3, w4, w5,w6,w7,w8, nrow = 4)


#Absolute Goal Difference of Fixtures
mls.fixtures$absolute_difference <- abs(mls.fixtures$HomeGoals - mls.fixtures$AwayGoals)
mls.injury.fixtures$absolute_difference <- abs(mls.injury.fixtures$HomeGoals - mls.injury.fixtures$AwayGoals)

#Count Absolute Goal Difference
dplyr::count(mls.fixtures, absolute_difference)
dplyr::count(mls.injury.fixtures, absolute_difference)
 
#Absolute goal Difference Injury Fixtures
 abs1 <- mls.injury.fixtures %>% 
   group_by(absolute_difference) %>% 
   count(absolute_difference)
 
 new_row <- data.frame(
   absolute_difference = 6,
   n = 0
 )
 
 insert_index <- 7
 
#Split the dataframe into two parts
df_above <- abs1[1:(insert_index - 1), ]
df_below <- abs1[insert_index:nrow(abs1), ]
 
# Combine the two parts with the new row inserted in between
abs1 <- rbind(df_above, new_row, df_below)
 
#Absolute Goal Differecne of Fixtures
abs2 <- mls.fixtures %>% 
   group_by(absolute_difference) %>% 
   count(absolute_difference)

abs2 <- na.omit(abs2)
 
mls.scores <- cbind(abs1,abs2) 

mls.scores <- select(mls.scores,1,2,4)

colnames(mls.scores) <- c("Absolute_Goal_Difference", "Injury_Count","Total_Games")

mls.scores$Injury_Percentage <- (mls.scores$Injury_Count / mls.scores$Total_Games) * 100

mls.scores$Absolute_Goal_Difference <- as.character(mls.scores$Absolute_Goal_Difference)

#Bar plot of Absolute Goal Difference
mls.scores %>% 
  ggplot(aes(x= Absolute_Goal_Difference, y= Injury_Percentage)) +
  geom_col(fill = "#e41a1c", colour = "#e41a1c", alpha = 0.6) +
  geom_text(aes(label = paste0(round(Injury_Percentage, 1), "%")), vjust = -0.5, size = 3, color = "black") +  # Add percentages on bars
  labs(x= "Absolute Goal Difference", y= "Percentage of Injuries") +
  ggtitle("Breakdown of MLS Injuries Based on Game Situation", subtitle = "The relationship Between Goal Difference and Injury\nDoesn't look to be significant") +
  theme_niall() 
  
#Comparing Home and Away Fixtures Based on Surface Type
mls.home.away <- mls.injury.fixtures$Away
mls.home.surface <- mls.injury.fixtures$Surface

#Data Frame of Surfaces and Home Fixtures
mls.home.away <- data.frame(mls.home.away,
                            mls.home.surface)

#Not Home Fixtures
mls.not.home <-  mls.home.away %>%
  mutate(HomeVsAway = case_when(mls.home.away  != "NA"  ~ "Home",
                                             TRUE ~ "Away"))

dplyr::count(mls.not.home, HomeVsAway)

dplyr::count(mls.not.home, mls.home.surface)

#Groupping Togther Based On surface
mls.home.surface.injury <- mls.not.home %>% 
  group_by(mls.home.surface) %>% 
  count(HomeVsAway)

mls.home.surface.injury$SurfaceGames <- c(227,227,630,630,82,82)

colnames(mls.home.surface.injury) <- c("SurfaceType", "HomeVsAway","InjuryCount","SurfaceGames")

mls.home.surface.injury$Injury_Percentage <- (mls.home.surface.injury$InjuryCount / mls.home.surface.injury$SurfaceGames) * 100

#Stacked Bar Plot Of Injury Based on Team Surface
mls.home.surface.injury %>%
  ggplot(aes(x = SurfaceType, y = Injury_Percentage, fill = HomeVsAway)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(Injury_Percentage, 1), "%")),
            position = position_stack(vjust = 0.5),  # Position text at the center of each segment
            size = 6, color = "black") +  # Add percentages on bars
  labs(x = "Surface Type", y = "Percentage of Injuries") +
  ggtitle("Breakdown of MLS Injuries Based on Team's Surface Type",
          subtitle = "The Stacked Bar chart shows a signifcant difference in Injury Percentage \nwith teams who play most games on Field Turf") +
  theme_niall() 

#Stadium Count of Injuries
stad <- dplyr::count(mls.injury.fixtures, `Stadium Name`)

stad <- dplyr::count(mls.injury.fixtures, `Stadium Name`)

stad.fix <- dplyr::count(mls.fixtures, `Stadium Name`)

stad.surf <- select(mls.injury.fixtures,23,25 )

stadium <- left_join(stad, stad.fix, by = "Stadium Name")

stadium <- left_join(stadium, stad.surf, by = "Stadium Name")

stadium <- unique(stadium)

colnames(stadium) <- c("Stadium Name", "Injury_Count","Games","Surface")

stadium$Injury_Percentage <- (stadium$Injury_Count / stadium$Games) * 100

#Filter Based on a minimu of 15 Games on A stadium
stadium <- stadium %>% 
  filter(Games > 15)

clipr::write_clip(stadium)

#Plot Top 8 stadiums
top_stadiums <- head(stadium[order(-stadium$Injury_Percentage),], 8)

# Bar Plot of highest Stadium Injury Rates
stad1 <- ggplot(top_stadiums, aes(x = reorder(`Stadium Name`, Injury_Percentage), y = Injury_Percentage, fill = Surface)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Injury_Percentage, 1), "%")), vjust = -0.5, size = 6, color = "black") +  # Add percentages on bars
  scale_fill_manual(values = c("Grass" = "#4daf4a", "Field Turf" = "#e41a1c", "Hybrid" = "blue")) +
  labs(x = "Stadium Name", y = "Injury Percentage (%)", fill = "Surface") +  # Add "%" to the y-axis label
  ggtitle("Top 8 Stadiums by Injury Percentage (2012-2023)") +
  theme_niall() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

stad1
#Remove Unactive Stadiums 
stadium2 <- subset(stadium, !(grepl("Camping World Stadium", `Stadium Name`, ignore.case = TRUE) | grepl("TCF Bank Stadium", `Stadium Name`, ignore.case = TRUE)))

stadium2 <- head(stadium2[order(-stadium2$Injury_Percentage),], 8)

stad2 <- ggplot(stadium2, aes(x = reorder(`Stadium Name`, Injury_Percentage), y = Injury_Percentage, fill = Surface)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Injury_Percentage, 1), "%")), vjust = -0.5, size = 6, color = "black") +  # Add percentages on bars
  scale_fill_manual(values = c("Grass" = "#4daf4a", "Field Turf" = "#e41a1c", "Hybrid" = "blue")) +
  labs(x = "Stadium Name", y = "Injury Percentage (%)", fill = "Surface") +  # Add "%" to the y-axis label
  ggtitle("Top 8 Stadiums by Injury Percentage (Active)") +
  theme_niall() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

stad2

### Manipulating Data for Kaplan Meier###
#Grouping Data by Surface and Match ID
games_played_per_player_surface.t <- mls.player.fixtures %>%
  group_by(Surface,MatchID) %>%
  summarise(max = max(Min, na.rm=TRUE))

# Count the number of games played per player ID
games_played_per_player_surface <- mls.player.fixtures %>%
  group_by(URL,Surface) %>%
  summarize(GamesPlayed = n())

# Filter data for grass surface
grass_surface_data <- subset(mls.player.fixtures, Surface == "Grass")

# Filter data for field turf surface
turf_surface_data <- subset(mls.player.fixtures, Surface == "Field Turf")

# Aggregate data to get count of games played by each player on grass surface
grass_player_counts <- aggregate(Surface ~ URL, data = grass_surface_data, FUN = length)

# Aggregate data to get count of games played by each player on field turf surface
turf_player_counts <- aggregate(Surface ~ URL, data = turf_surface_data, FUN = length)

# Calculate total games played by each player
total_games_per_player <- aggregate(Surface ~ URL, data = mls.player.fixtures, FUN = length)

# Define the majority criteria for grass and turf
majority_threshold_grass <- total_games_per_player$Surface * 0.5  # Considering majority as more than half
majority_threshold_turf <- total_games_per_player$Surface * 0.5  # Considering majority as more than half

# Find players who played the majority of games on grass
players_majority_grass <- grass_player_counts$URL[grass_player_counts$Surface > majority_threshold_grass]

# Find players who played the majority of games on field turf
players_majority_turf <- turf_player_counts$URL[turf_player_counts$Surface > majority_threshold_turf]

# Subset data based on whether they played the majority of games on grass or field turf
players_majority_grass_data <- subset(mls.player.fixtures, URL %in% players_majority_grass)
players_majority_turf_data <- subset(mls.player.fixtures, URL %in% players_majority_turf)

# Output the subsets
print("Players who played the majority of games on grass:")
print(players_majority_grass_data)

print("Players who played the majority of games on field turf:")
print(players_majority_turf_data)

#Filter based on Surface
majority_surface <- games_played_per_player_surface %>%
  group_by(URL) %>%
  summarize(MajoritySurface = Surface[which.max(GamesPlayed)])

# Filter URLs where the majority is Field Turf
urls_majority_field_turf <- majority_surface %>%
  filter(MajoritySurface == "Field Turf") %>%
  pull(URL)

# Filter original data frame to keep only URLs with majority Field Turf
filtered_data <- games_played_per_player_surface %>%
  filter(URL %in% urls_majority_field_turf)

# Print the filtered data
print(filtered_data)

#Hybrid Majority
urls_majority_hybrid <- majority_surface %>%
  filter(MajoritySurface == "Hybrid") %>%
  pull(URL)

filtered_data2 <- games_played_per_player_surface %>%
  filter(URL %in% urls_majority_hybrid)

#Grass majority
urls_majority_grass <- majority_surface %>%
  filter(MajoritySurface == "Grass") %>%
  pull(URL)

#Filter Based on Grass by URL
filtered_data3 <- games_played_per_player_surface %>%
  filter(URL %in% urls_majority_grass)

#Minimum of 10 games Played for Each Surface
filtered_data <- filtered_data %>%
  filter(GamesPlayed > 10)

filtered_data2 <- filtered_data2 %>%
  filter(GamesPlayed > 10)

filtered_data3 <- filtered_data3 %>%
  filter(GamesPlayed > 10)

#Complete Rows of Grass
filtered_data3 <- filtered_data3[complete.cases(filtered_data3),]

#URL of Turf,Grass, Hybrid
unique_values <- unique(filtered_data$URL)
unique_values2 <- unique(filtered_data2$URL)
unique_values3 <- unique(filtered_data3$URL)


#######**Filtering Based on Grass**########
grass.matches <- mls.player.fixtures %>%
  filter(URL %in% unique_values3)

grass.matches$game_injury <- ifelse(is.na(grass.matches$injury), 0, 1)

grass.matches$Date <- as.Date(grass.matches$Date)

grass.matches <- grass.matches %>%
  arrange(Date) %>%
  group_by(Date, MatchID) %>%
  mutate(Game_Number = cur_group_id())

grass.matches$game_injury <- ifelse(is.na(grass.matches$injury), 0, 1)

grass.matches$Date <- as.Date(grass.matches$Date)

#Arranging Matches  by date and Game Number
grass.matches <- grass.matches %>%
  arrange(Date) %>%
  group_by(Date, MatchID) %>%
  mutate(Game_Number = cur_group_id())

#Start at 100 Percent 
grass.matches$Percent <- 100

# Function to update Percent based on Injury
update_percent <- function(grass.matches) {
  # Initialize percent_remaining as 100
  percent_remaining <- 100
  
  # Loop through each row
  for (i in 1:nrow(grass.matches)) {
    # If Injury is 1, decrease percent_remaining by 20 (assuming 20% decrement)
    if (grass.matches$game_injury[i] == 1) {
      percent_remaining <- percent_remaining - ((1/n_distinct(grass.matches$URL))*100)
    }
    # Update Percent column with percent_remaining value
    grass.matches$Percent[i] <- percent_remaining
  }
  
  return(grass.matches)
}

# Update Percent column based on Injury
grass.matches <- update_percent(grass.matches)

#Percentages are kept as result
highest_game_number_per_percentage <- function(grass.matches) {
  result <- aggregate(Game_Number ~ Percent, grass.matches, max)
  return(result)
}

highest_game_numbers <- highest_game_number_per_percentage(grass.matches)
print(highest_game_numbers)

# Sort the data by Game_Number in ascending order and then by Percent in ascending order
df <- highest_game_numbers[order(highest_game_numbers$Game_Number, highest_game_numbers$Percent), ]

# Keep the first occurrence of each Game_Number
df.grass <- df[!duplicated(df$Game_Number), ]

print(df.grass)

# Print the updated dataframe
head(highest_game_numbers)
#######**Filtering Based on Field Turf**#######
#Same Process to Grass is Applied to Field Turf
turf.matches <- mls.player.fixtures %>%
  filter(URL %in% unique_values)

turf.matches$game_injury <- ifelse(is.na(turf.matches$injury), 0, 1)

turf.matches$Date <- as.Date(turf.matches$Date)

turf.matches <- turf.matches %>%
  arrange(Date) %>%
  group_by(Date, MatchID) %>%
  mutate(Game_Number = cur_group_id())

turf.matches <- mls.player.fixtures %>%
  filter(URL %in% unique_values)

turf.matches$game_injury <- ifelse(is.na(turf.matches$injury), 0, 1)

turf.matches$Date <- as.Date(turf.matches$Date)

turf.matches <- turf.matches %>%
  arrange(Date) %>%
  group_by(Date, MatchID) %>%
  mutate(Game_Number = cur_group_id())

turf.matches$Percent <- 100

# Function to update Percent based on Injury
update_percent <- function(turf.matches) {
  # Initialize percent_remaining as 100
  percent_remaining <- 100
  
  # Loop through each row
  for (i in 1:nrow(turf.matches)) {
    # If Injury is 1, decrease percent_remaining by 20 (assuming 20% decrement)
    if (turf.matches$game_injury[i] == 1) {
      percent_remaining <- percent_remaining - ((1/n_distinct(turf.matches$URL))*100)
    }
    # Update Percent column with percent_remaining value
    turf.matches$Percent[i] <- percent_remaining
  }
  
  return(turf.matches)
}

# Update Percent column based on Injury
turf.matches <- update_percent(turf.matches)

highest_game_number_per_percentage <- function(turf.matches) {
  result <- aggregate(Game_Number ~ Percent, turf.matches, max)
  return(result)
}

highest_game_numbers <- highest_game_number_per_percentage(turf.matches)
print(highest_game_numbers)

# Sort the data by Game_Number in ascending order and then by Percent in ascending order
df <- highest_game_numbers[order(highest_game_numbers$Game_Number, highest_game_numbers$Percent), ]

# Keep the first occurrence of each Game_Number
df.turf <- df[!duplicated(df$Game_Number), ]

print(df.turf)

# Print the updated dataframe
head(highest_game_numbers)

#Grass and Field Turf being assigned Kaplan datasets
grass.Kaplan <- df.grass

turf.Kaplan <- df.turf

#Reducing Grass Games to 3000 to compare
grass.Kaplan <- grass.Kaplan %>% filter(Game_Number < 3000)

string_to_replicate <- "Grass"

#Replicate the string for each row and assign it to a new column
grass.Kaplan$Surface <- rep(string_to_replicate, nrow(grass.Kaplan))

#Replicating Field Turf
string_to_replicate <- "Field Turf"
turf.Kaplan$Surface <- rep(string_to_replicate, nrow(turf.Kaplan))

# Assuming you have grass.Kaplan and turf.Kaplan data frames
datadata = rbind(grass.Kaplan, turf.Kaplan)

datadata$Injury <- 1

datadata <- datadata[,-1]

game_numbers_grass <- 1:4730
game_numbers_field_turf <- 1:2826

# Create a data frame with all combinations of game numbers and surfaces
all_combinations <- data.frame(
  Game_Number = c(rep(game_numbers_grass, each = length(unique(datadata$Surface))),
                 rep(game_numbers_field_turf, each = length(unique(datadata$Surface)))),
  Surface = c(rep("Grass", length(game_numbers_grass) * length(unique(datadata$Surface))),
              rep("Field Turf", length(game_numbers_field_turf) * length(unique(datadata$Surface))))
)

# Merge with existing data and fill missing values with 0
complete_data <- merge(all_combinations, datadata, by = c("Game_Number", "Surface"), all.x = TRUE) %>%
  mutate(Injury = ifelse(is.na(Injury), 0, 1))

# Reorder columns 
complete_data <- complete_data[, c("Game_Number", "Surface", "Injury")]

# Print the complete data frame
print(complete_data)

complete_data <- unique(complete_data)

complete_data2 <- subset(complete_data, Game_Number < 2827)

#Applying Kaplan-Meier
km_fit <- survfit(Surv(Game_Number, Injury) ~ Surface, data = complete_data2)

# Plot Kaplan-Meier curves
ggsurvplot(km_fit, data = complete_data2, risk.table = TRUE,
           pval = TRUE, conf.int = TRUE,
           ggtheme = theme_niall(),
           palette = c( "#e41a1c","#4daf4a"), # Custom colors for curves
           linetype = c("solid", "dashed"))   # Custom line types for curves

table(datadata$Surface)

#######Day VS Night Fixtures#######
#Grouping Based on Day VS Night
mls.fixtures %>% 
    group_by(Surface) %>% 
    count(`is_day ()`) 

#Getting Data frame together of Day vs Night Fixtures
games.day.night <-  data.frame(
  "Games" = c(448, 678, 1652,1581,137,233),
  "Surface" = c("Field Turf","Field Turf", "Grass","Grass", "Hybrid","Hybrid"),
  "Injury_Count" = c(68,159,284,346,27,55),
  "Time" = c("Night","Day", "Night","Day", "Night","Day")
)

games.day.night$Percentage <- ((games.day.night$Injury_Count / games.day.night$Games)*100)

# Grouped Bar chart
# Define custom colors based on surface type and day/night time
my_colors <- c("Field Turf_Day" = "#e41a1c", 
               "Field Turf_Night" = "#ff9999", 
               "Grass_Day" = "#4daf4a", 
               "Grass_Night" = "#98df8a", 
               "Hybrid_Day" = "#0066ff", 
               "Hybrid_Night" = "#99c2ff")

# Create the grouped bar chart with percentage on the y-axis and custom colors
ggplot(games.day.night, aes(x = Surface, y = Percentage, fill = paste(Surface, Time, sep = "_"))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Injury Percentages by Surface Type and Time",
       x = "Surface Type",
       y = "Percentage (%)",
       fill = "Surface & Time") +
  scale_fill_manual(values = my_colors) +  # Use custom colors
  theme_niall() +
  theme(legend.position = "bottom")  # Adjust legend position if necessary

#Chi-sqaure Test Day vs Night in Injury Rate 
grass.chi <- mls.weatherdataset %>%
  filter(Surface == "Grass")

field.turf.chi <- mls.weatherdataset %>%
  filter(Surface == "Field Turf")

hybrid.chi <- mls.weatherdataset %>%
  filter(Surface == "Hybrid")

chisq.test(grass.chi$MatchInjury, grass.chi$`is_day ()`, correct=FALSE)

chisq.test(field.turf.chi$MatchInjury, field.turf.chi$`is_day ()`, correct=FALSE)

chisq.test(hybrid.chi$MatchInjury, hybrid.chi$`is_day ()`, correct=FALSE)

names(mls.weatherdataset)[names(mls.weatherdataset) == "temperature_2m (°C)"] <- "temperature_2m_C"

# List of columns (variables) for which you want to perform t-tests
columns_of_interest <- c("temperature_2m_C", "relative_humidity_2m (%)", "dew_point_2m (°C)", "apparent_temperature (°C)","precipitation (mm)","rain (mm)","soil_moisture_0_to_7cm (m³/m³)","soil_temperature_0_to_7cm (°C)","wind_speed_10m (km/h)","cloud_cover (%)" )

#T-test of weather variables
result1.01 <- t.test(`temperature_2m_C` ~ MatchInjury, data = mls.weatherdataset)
result1.02 <- t.test(`relative_humidity_2m (%)` ~ MatchInjury, data = mls.weatherdataset)
result1.03 <- t.test(`soil_temperature_0_to_7cm (°C)` ~ MatchInjury, data = mls.weatherdataset)
result1.04 <- t.test(`apparent_temperature (°C)` ~ MatchInjury, data = mls.weatherdataset)
result1.05 <- t.test(`precipitation (mm)` ~ MatchInjury, data = mls.weatherdataset)
result1.06 <- t.test(`soil_moisture_0_to_7cm (m³/m³)` ~ MatchInjury, data = mls.weatherdataset)
result1.07 <- t.test(`wind_speed_10m (km/h)` ~ MatchInjury, data = mls.weatherdataset)
result1.08 <- t.test(`cloud_cover (%)` ~ MatchInjury, data = mls.weatherdataset)

#Correlation Matrix of Weather Variables
corr.data <- mls.weatherdataset %>% select(10:14,15)
corr.data <- corr.data %>%
  mutate(MatchInjury = case_when(
    MatchInjury == "Yes" ~ 1,
    TRUE ~ 0
  ))

correlation_matrix <- cor(corr.data)

corrplot(correlation_matrix, method = "number")

#######Correlation Matrix of Player Biometric Variables###########
corr.data <- mls.player.fixtures

corr.data$MatchInjury <- ifelse(is.na(corr.data$injury), 0, 1)

corr.data <- corr.data %>% select(6,7,9,10,12,25,56)

corr.data <- na.omit(corr.data)

encoded <- model.matrix(~ Surface - 1, data = corr.data)

# Convert the encoded matrix to a dataframe
encoded_df <- as.data.frame(encoded)

# Print the encoded dataframe
encoded2 <- model.matrix(~ Position_Group - 1, data = corr.data)

# Convert the encoded matrix to a dataframe
encoded_df2 <- as.data.frame(encoded2)

#######Correlation Matrix of Player Biometric Positions###########

test <- cbind(corr.data,encoded_df,encoded_df2)

corr.data <- test %>% select(1:4,7:17)

corr.data1 <- corr.data %>% select(1:8)

correlation_matrix <- cor(corr.data1)

corrplot(correlation_matrix, method = "number")

corr.data2 <- corr.data %>% select(5,9:15)

correlation_matrix <- cor(corr.data2)

corrplot(correlation_matrix, method = "number")

######Player Injuries#########
mls.injury.fixtures %>%
  group_by(Surface)  %>%
  count(Position_Group)

t.tt <- mls.player.fixtures %>%
  group_by(Surface)  %>%
  count(Position_Group)

clipr::write_clip(t.tt)

inj.type <- mls.injury.fixtures %>% count(injury)

#Grouping Injuries Together
mls.injury.fixtures <- mls.injury.fixtures %>%  
  mutate(Injury_Type = case_when( injury %in% c("Achilles tendon problems", "Achilles tendon rupture", "Adductor injury", "Adductor pain", "Cruciate ligament injury",
                                             "Cruciate ligament strain", "Cruciate ligament tear","Inner ligament injury","Inner ligament stretch of the knee",
                                             "Internal ligament strain",
                                             "Internal ligament tear",
                                             "Left hip flexor problems",
                                             "Ligament stretching",
                                             "Meniscus damage",
                                             "Meniscus injury",
                                             "Meniscus tear",
                                             "Muscle fiber tear",
                                             "Torn knee ligaments",
                                             "Torn lateral knee ligament",
                                             "Ankle surgery",
                                             "Knee surgery",
                                             "Groin surgery") ~ "Joint, Tendon, and Ligament Injuries",
                               injury %in% c("Ankle injury",
                                             "Broken fibula",
                                             "Broken foot",
                                             "Broken leg",
                                             "Broken tibia",
                                             "Broken toe",
                                             "Foot bruise",
                                             "Foot injury",
                                             "Foot surgery",
                                             "Hairline crack in foot",
                                             "Hairline crack in the lumbar region",
                                             "Heel injury",
                                             "Heel problems",
                                             "Hip injury",
                                             "Hip problems",
                                             "Knee bruise",
                                             "Knee injury",
                                             "Knee problems",
                                             "Leg injury",
                                             "Metatarsal bruise",
                                             "Shin bruise",
                                             "Shin injury",
                                             "Thigh problems",
                                             "Toe injury",
                                             "ankle sprain"
                               ) ~ "Lower Limb Injuries",
                               injury %in% c("Calf injury",
                                             "Calf problems",
                                             "Calf strain",
                                             "Groin injury",
                                             "Groin problems",
                                             "Groin strain",
                                             "Hamstring injury",
                                             "Hamstring strain",
                                             "Injury to abdominal muscles",
                                             "Muscle injury",
                                             "Muscle strain",
                                             "Muscle tear",
                                             "Strain in the thigh and gluteal muscles",
                                             "Torn muscle fiber",
                                             "Torn thigh muscle",
                                             "muscular problems"
                               ) ~ "Muscle Injuries",
                               injury %in% c("Fitness",
                                             "Rest",
                                             "bruise",
                                             "fracture",
                                             "sprain",
                                             "strain",
                                             "unknown injury"
                               ) ~ "Recovery and Rest/Other",
                               injury %in% c("Back injury",
                                             "Back problems",
                                             "Broken arm",
                                             "Broken collarbone",
                                             "Broken jaw",
                                             "Broken nose bone",
                                             "Bruised ribs",
                                             "Chest injury",
                                             "Elbow injury",
                                             "Eye injury",
                                             "Eyebow fracture",
                                             "Facial fracture",
                                             "Facial injury",
                                             "Finger injury",
                                             "Head injury",
                                             "Herniated disc",
                                             "Knock",
                                             "Lumbar vertebra fracture",
                                             "Lung contusion",
                                             "Neck injury",
                                             "Pelvic injury",
                                             "Pneumothorax",
                                             "Pubalgia",
                                             "Rib fracture",
                                             "Shoulder injury",
                                             "Wrist fracture",
                                             "Wrist injury",
                                             "concussion",
                                             "horse kiss"
                               ) ~ "Upper Body Injuries/Concussions/Spine",
                               
                               TRUE ~ "Other"
  ))

# Injury Type Data Frame 
data.inj.type <- data.frame(
  Surface = c("Field Turf", "Field Turf", "Field Turf", "Field Turf", "Field Turf",
              "Grass", "Grass", "Grass", "Grass", "Grass",
              "Hybrid", "Hybrid", "Hybrid", "Hybrid", "Hybrid"),
  Injury_Type = c("Joint, Tendon, and Ligament Injuries", "Lower Limb Injuries", "Muscle Injuries",
                  "Recovery and Rest/Other", "Upper Body Injuries/Concussions/Spine",
                  "Joint, Tendon, and Ligament Injuries", "Lower Limb Injuries", "Muscle Injuries",
                  "Recovery and Rest/Other", "Upper Body Injuries/Concussions/Spine",
                  "Joint, Tendon, and Ligament Injuries", "Lower Limb Injuries", "Muscle Injuries",
                  "Recovery and Rest/Other", "Upper Body Injuries/Concussions/Spine"),
  n = c(29, 56, 108, 10, 24, 78, 190, 253, 28, 81, 9, 35, 24, 5, 9)
)

# Plot
# Calculate percentages by Surface
data.inj.type <- transform(data.inj.type,
                  Percent = n / ave(n, Surface, FUN = sum) * 100)

# Define custom colors
my_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd")

# Plot of Injury Types Based on Surface Type
ggplot(data.inj.type, aes(x = Surface, y = Percent, fill = Injury_Type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", Percent)), 
            position = position_stack(vjust = 0.5), 
            size = 7, 
            color = "white") +
  labs(title = "Distribution of Injury Types Across Surfaces",
       x = "Surface",
       y = "Percentage of Injuries",
       fill = "Injury Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(angle = 0, hjust = 1)) +  # Change orientation of y-axis label
  scale_fill_manual(name = "Injury Type", values = my_colors) + # Set custom colors
  theme_niall() +
  theme(legend.position = "right") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()

#Count of Injury Types and Surface
mls.injury.fixtures %>%
       group_by(Surface) %>%
       count(Injury_Type)

#Injury Type and Surface Chi-square Tests
df1.joint <- data.frame(
  JointInjury = c(rep("Injured", 29), rep("Not-Injured", 13651)),
  Surface = c(rep("Field Turf", 13680))
)

df2.joint <- data.frame(
  JointInjury = c(rep("Injured", 78), rep("Not-Injured", 40849)),
  Surface = c(rep("Grass", 40927))
)

chi.test.joint <- rbind(df1.joint,df2.joint)
chisq.test(chi.test.joint$Surface, chi.test.joint$JointInjury, correct=FALSE)

df1.lowerlimb <- data.frame(
  LowerLimbInjury = c(rep("Injured", 56), rep("Not-Injured", 13624)),
  Surface = c(rep("Field Turf", 13680))
)

df2.lowerlimb <- data.frame(
  LowerLimbInjury = c(rep("Injured", 190), rep("Not-Injured", 40737)),
  Surface = c(rep("Grass", 40927))
)

chi.test.lowerlimb <- rbind(df1.lowerlimb,df2.lowerlimb)
chisq.test(chi.test.lowerlimb$Surface, chi.test.lowerlimb$LowerLimbInjury, correct=FALSE)

df1.muscle <- data.frame(
  MuscleInjury = c(rep("Injured", 108), rep("Not-Injured", 13572)),
  Surface = c(rep("Field Turf", 13680))
)

df2.muscle <- data.frame(
  MuscleInjury = c(rep("Injured", 253), rep("Not-Injured", 40674)),
  Surface = c(rep("Grass", 40927))
)

chi.test.muscle <- rbind(df1.muscle,df2.muscle)
chisq.test(chi.test.muscle$Surface, chi.test.muscle$MuscleInjury, correct=FALSE)

df1.up <- data.frame(
  UpInjury = c(rep("Injured", 24), rep("Not-Injured", 13656)),
  Surface = c(rep("Field Turf", 13680))
)

df2.up <- data.frame(
  UpInjury = c(rep("Injured", 81), rep("Not-Injured", 40846)),
  Surface = c(rep("Grass", 40927))
)

chi.test.up <- rbind(df1.up,df2.up)
chisq.test(chi.test.up$Surface, chi.test.up$UpInjury, correct=FALSE)

###Hybrid VS Others###
df1.joint <- data.frame(
  JointInjury = c(rep("Injured", 9), rep("Not-Injured", 4247)),
  Surface = c(rep("Hybrid", 4256))
)

df2.joint <- data.frame(
  JointInjury = c(rep("Injured", 107), rep("Not-Injured", 54500)),
  Surface = c(rep("Other", 54607))
)

chi.test.joint <- rbind(df1.joint,df2.joint)
chisq.test(chi.test.joint$Surface, chi.test.joint$JointInjury, correct=FALSE)

df1.lowerlimb <- data.frame(
  LowerLimbInjury = c(rep("Injured", 35), rep("Not-Injured", 4221)),
  Surface = c(rep("Hybrid", 4256))
)

df2.lowerlimb <- data.frame(
  LowerLimbInjury = c(rep("Injured", 246), rep("Not-Injured", 54361)),
  Surface = c(rep("Other", 54607))
)

chi.test.lowerlimb <- rbind(df1.lowerlimb,df2.lowerlimb)
chisq.test(chi.test.lowerlimb$Surface, chi.test.lowerlimb$LowerLimbInjury, correct=FALSE)

df1.muscle <- data.frame(
  MuscleInjury = c(rep("Injured", 24), rep("Not-Injured", 4232)),
  Surface = c(rep("Hybrid", 4256))
)

df2.muscle <- data.frame(
  MuscleInjury = c(rep("Injured", 361), rep("Not-Injured", 54246)),
  Surface = c(rep("Other", 54607))
)

chi.test.muscle <- rbind(df1.muscle,df2.muscle)
chisq.test(chi.test.muscle$Surface, chi.test.muscle$MuscleInjury, correct=FALSE)

df1.up <- data.frame(
  UpInjury = c(rep("Injured", 9), rep("Not-Injured", 4247)),
  Surface = c(rep("Hybrid", 4256))
)

df2.up <- data.frame(
  UpInjury = c(rep("Injured", 95), rep("Not-Injured", 54512)),
  Surface = c(rep("Other", 54607))
)

chi.test.up <- rbind(df1.up,df2.up)
chisq.test(chi.test.up$Surface, chi.test.up$UpInjury, correct=FALSE)
