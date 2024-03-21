####Analysis and Machine Learning MLS####
####Niall Gallagher#######

#Libaries 
library(readxl)
library(tidyverse)
library(lubridate)
library(scales)
library(ggridges)

mls.fixtures <- read_excel('C:/Users/niall/OneDrive/Documents/Dissertation/Analysis/MLSFixtures.xlsx')
mls.injury.fixtures <-read_excel('C:/Users/niall/OneDrive/Documents/Dissertation/Analysis/GameInjuries.xlsx')
mls.player.fixtures <-read_excel('C:/Users/niall/OneDrive/Documents/Dissertation/Analysis/MLSFull.xlsx')

names(mls.injury.fixtures) <- gsub("\\.x", "", names(mls.injury.fixtures))

names(mls.player.fixtures) <- gsub("\\.x", "", names(mls.player.fixtures))


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


Test <- dplyr::count(mls.injury.fixtures, injury)            # Applying count function
Test2 <- dplyr::count(mls.injury.fixtures, Games)

stad <- dplyr::count(mls.fixtures, `Stadium Name`)

dplyr::count(mls.injury.fixtures, Position_Group)


dplyr::count(mls.injury.fixtures, Surface)
dplyr::count(mls.fixtures, Surface)

mls.player.fixtures %>% 
  #group_by(Surface.x) %>% 
  distinct(PlayerId.x) %>%
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

#mls.injury.fixtures$
dplyr::count(mls.player.fixtures, Position_Group.x)

mls.player.fixtures %>% 
  group_by(PlayerId.x) %>% 
  count(Position_Group.x) 


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



surface <- c("Artifical Turf", "Natural Grass", "Hybrid Surface")
injs.sev <- c("Slight (1-3 Days)", "Minor (4-7 Days)", "Moderate (8-28 Days)","Major (28+ Days)")
InjSev <-  data.frame(
  "Injury_Severity" = c(rep(injs.sev, 3)),
  "Surface" = c(rep(surface, 4)),
  "Injury_Count" = c(3,40,36,106,6,12,104,266,0,14,318,34)
)


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

# Plot the percentage breakdown
ggplot(InjSev, aes(x = reorder(Surface, -total_injury_count), y = Percentage, fill = Injury_Severity)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage Breakdown of Injury Severity by Surface Type",
       x = "Surface Type", y = "Percentage") +
  geom_text(aes(label=paste0(sprintf("%1.2f", Percentage),"%")),
            position=position_stack(vjust=0.5)) +
  scale_fill_manual(values = c("Slight (1-3 Days)" = "lightgreen", 
                               "Minor (4-7 Days)" = "green", 
                               "Moderate (8-28 Days)" = "yellow", 
                               "Major (28+ Days)" = "red"),
                    name = "Injury Severity") +
  theme_niall() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))





pt2 <- mls.injury.fixtures %>% 
  filter(`Games` < 35) %>% 
  group_by(Games) %>% summarise(num_players = n_distinct(PlayerId)) %>% 
  ggplot(aes(x=Games, y=num_players)) +
  geom_line(colour = plot_cols[2], size = 1) +
  geom_point(colour = plot_cols[2], size = 3) +
  labs(x= "Game Number", y= "Number Injuries") +
  ggtitle("NUMBER OF INJURIES DECREASING THROUGH GAMES", subtitle = "Week 10 has the most injuries (n=46)") 
  theme_niall() +
  scale_x_continuous(breaks = seq(0, 35, by = 5)) +
  scale_y_continuous(breaks = seq(0, 50, by = 5))


pt3 <- mls.injury.fixtures %>% 
  filter(Games < 35) %>% 
  group_by(Games, Surface) %>% 
  summarise(num_players = n_distinct(PlayerId)) %>% 
  ggplot(aes(x = Games, y = num_players, color = Surface, group = Surface)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(x = "Game Number", y = "Number Injuries") +
  ggtitle("NUMBER OF INJURIES DECREASING THROUGH GAMES", subtitle = "Week 10 has the most injuries (n=46)") +
  theme_niall() +
  scale_x_continuous(breaks = seq(0, 35, by = 5)) +
  scale_y_continuous(breaks = seq(0, 50, by = 5))




test <- select(mls.injury.fixtures,`Min`)

filtered_data <- test %>%
  filter(Min <= 90 | !is.na(Min))

filtered_data <- filtered_data %>%
  mutate(Min_Bin = cut(Min, breaks = seq(0, 90, by = 5), labels = FALSE))


# Plot the histogram
x_intercept <- 9  # Example value

# Create the plot
ggplot(filtered_data, aes(x = Min_Bin)) +
  geom_histogram(binwidth = 1, fill = plot_cols[2], color = plot_cols[2]) +
  geom_vline(xintercept = x_intercept, linetype = "dashed", color = "red") +  # Add dashed line
  geom_text(aes(x = x_intercept, y = 9, label = "Half Time"), color = "lightgreen", vjust = 1) +  # Add text
  labs(x = "Binned Time (every 5 minutes)", y = "Injury Count", 
       title = "Histogram of Injuries in 5-Minute Intervals",
       subtitle = "Each bar represents a 5-minute interval of a game",
       caption = "MLS Data 2012 - 2023") +
  theme_niall() +
  scale_x_continuous(breaks = seq(0, 18, by = 3))

games <-  data.frame(
  "Games" = c(1126, 3233, 370),
  "Surface" = c("Artifical Turf", "Natural Grass", "Hybrid Surface"),
  "Injury_Count" = c(227,630,82)
)

#games$Injury_Rate <- (games$Injury_Count / games$Games)


p1 <- games %>% 
  #count(injury) %>% 
  ggplot(aes(x= Surface, y= Games)) +
  geom_col(fill = plot_cols[7], colour = plot_cols[7], alpha = 0.6) +
  labs(x= "Surface Type", y= "Number of Games") +
  ggtitle("Breakdown of MLS Games Based on Surface Type", subtitle = "68% of the Games in the MLS were played on Natural Grass") +
  coord_flip() + theme_niall()


p2 <- games %>% 
  #count(injury) %>% 
  ggplot(aes(x= Surface, y= Injury_Count)) +
  geom_col(fill = plot_cols[1], colour = plot_cols[1], alpha = 0.6) +
  labs(x= "Surface Type", y= "Number of Injuries") +
  ggtitle("Breakdown of MLS Injuries Based on Surface Type") +
  coord_flip() + theme_niall()



gridExtra::grid.arrange(p1, p2)






games %>% 
  #distinct(GameID, FieldType, IsInjured) %>% 
  group_by(Surface) %>% 
  #summarise(n = n()) %>% 
  mutate(perc_injured = Injury_Count / Games) %>% 
  #filter(IsInjured == TRUE) %>% 
  ggplot(aes(x= Surface, y= perc_injured)) +
  geom_col(alpha = 0.6, fill = plot_cols[5], colour = plot_cols[5]) +
  geom_text(aes(label = percent(perc_injured)), vjust=1, size = 7, colour = plot_cols[8]) +
  geom_hline(yintercept = 0.1986, linetype = 2, colour = plot_cols[1]) +
  annotate(geom = "text", x=0.95, y= 0.1486, label = "Injuries occur on\n19.86% of all Games", colour = plot_cols[1], size = 5) +
  ggtitle("MORE LIKELY TO BE INJURED ON SYNTHETIC/Hybrid SURFACES", subtitle = "Of over 4729 Games analysed, injuries\noccured more frequently on Synthetic and Hybrid Surfaces") +
  theme_niall() +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank())



mls.injury.fixtures %>% 
  #group_by(Surface) %>% 
  count(Position_Group) %>%
  ggplot(aes(x= Position_Group, y= n)) +
  geom_col(fill = plot_cols[2], colour = plot_cols[2], alpha = 0.6) +
  labs(x= "Surface Type", y= "Number of Games") +
  ggtitle("Breakdown of MLS Games Based on Surface Type", subtitle = "68% of the Games in the MLS were played on Natural Grass") +
  coord_flip() + theme_niall()

####Positon Breakdown####
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

pos1 <- mls.injury.fixtures %>% 
  #group_by(Surface) %>% 
  count(Position_Group) %>%
  ggplot(aes(x= Position_Group, y= n)) +
  geom_col(fill = plot_cols[2], colour = plot_cols[2], alpha = 0.6) +
  labs(x= "Position", y= "Injury Count") +
  ggtitle("Breakdown of MLS Injuries Based on Position") +
  coord_flip() + theme_niall()


pos2 <- position.games %>%
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

gridExtra::grid.arrange(pos1, pos2)






###Weather 

mls.injury.fixtures %>% 
  filter(Roof %in% c("Open")) %>% 
  distinct(MatchID) %>% 
  ggplot(aes(x= `temperature_2m (°C)`)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(values = plot_cols, guide = "none") +
  scale_colour_manual(values = plot_cols, guide = "none") +
  ggtitle("INJURIES OCCUR EARLIER ON NATURAL SURFACES", subtitle = "28 records not included as exact play\nwhere injury occurred is unknown") +
  labs(x= "Field Type", y= "Game Play") +
  theme_niall()



temp1 <- mls.injury.fixtures %>%
  filter(Roof %in% c("Open")) %>% 
  distinct(MatchID) %>% 
  inner_join(mls.injury.fixtures, by = "MatchID") %>%
  ggplot(aes(x = `temperature_2m (°C)`)) +
  geom_boxplot(alpha = 0.5, fill = "pink", color = "red") +
  ggtitle("Tempeartures With Injury", subtitle = "No Significant Difference can be seen Between Tempearture and Injury") +
  labs(x = "Temperature (°C)") +
  theme_bw() +
  theme(axis.text.y = element_blank()) 


temp2 <- df3 %>%
  filter(Roof %in% c("Open")) %>% 
  distinct(MatchID) %>% 
  inner_join(df3, by = "MatchID") %>%
  ggplot(aes(x = `temperature_2m (°C)`)) +
  geom_boxplot(alpha = 0.5, fill = "lightgreen", color = "green") +
  ggtitle("Tempeartures With No Injury") +
  labs(x = "Temperature (°C)") +
  theme_bw() +
  theme(axis.text.y = element_blank()) 

gridExtra::grid.arrange(temp1, temp2)


df3 <- mls.fixtures[!(mls.fixtures$MatchID %in% mls.injury.fixtures$MatchID), , drop = FALSE]


df3 <- select(df3, 9,20, 22:28,30:34)
df3$MatchInjury <- rep("No",3952)
mls.injury.fixtures.test <- select(mls.injury.fixtures,20,25:32,34:38)

mls.injury.fixtures.test$MatchInjury <- rep("Yes",939)

mls.weatherdataset <- rbind(mls.injury.fixtures.test,df3)



w1 <- mls.weatherdataset %>% 
  filter(Roof %in% c("Open"),`precipitation (mm)` > 0) %>% 
  distinct(MatchID, `precipitation (mm)`, MatchInjury) %>% 
  ggplot(aes(y= `precipitation (mm)`, x = MatchInjury, fill = MatchInjury, colour = MatchInjury)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(values = c("#00b300", "#cc0000"), guide = "none") +
  scale_colour_manual(values = c("#00b300", "#cc0000"), guide = "none") +
  labs(x= "Player Injured?") +
  ggtitle("Precipitation", subtitle = "While injuries tend to occur in slightly lower\nprecipitation levels, doesn't look significant") +
  coord_flip() +
  theme_niall()

w2 <- mls.weatherdataset %>% 
  filter(Roof %in% c("Open")) %>% 
  distinct(MatchID, `wind_speed_10m (km/h)`, MatchInjury) %>% 
  ggplot(aes(y= `wind_speed_10m (km/h)`, x = MatchInjury, fill = MatchInjury, colour = MatchInjury)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(values = c("#00b300", "#cc0000"), guide = "none") +
  scale_colour_manual(values = c("#00b300", "#cc0000"), guide = "none") +
  labs(x= "Player Injured?") +
  ggtitle("Wind Speed", subtitle = "While injuries tend to occur in slightly higher\nwind speed, doesn't look significant") +
  coord_flip() +
  theme_niall()

w3 <- mls.weatherdataset %>% 
  filter(Roof %in% c("Open")) %>% 
  distinct(MatchID, `relative_humidity_2m (%)`, MatchInjury) %>% 
  ggplot(aes(y= `relative_humidity_2m (%)`, x = MatchInjury, fill = MatchInjury, colour = MatchInjury)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(values = c("#00b300", "#cc0000"), guide = "none") +
  scale_colour_manual(values = c("#00b300", "#cc0000"), guide = "none") +
  labs(x= "Player Injured?") +
  ggtitle("Relative Humidity", subtitle = "While injuries tend to occur in slightly lower\nhumidity, doesn't look significant") +
  coord_flip() +
  theme_niall()


w4 <- mls.weatherdataset %>% 
  filter(Roof %in% c("Open")) %>% 
  distinct(MatchID, `cloud_cover (%)`, MatchInjury) %>% 
  ggplot(aes(y= `cloud_cover (%)`, x = MatchInjury, fill = MatchInjury, colour = MatchInjury)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(values = c("#00b300", "#cc0000"), guide = "none") +
  scale_colour_manual(values = c("#00b300", "#cc0000"), guide = "none") +
  labs(x= "Player Injured?") +
  ggtitle("Cloud Cover", subtitle = "While injuries tend to occur in slightly lower\ncloud cover %, doesn't look significant") +
  coord_flip() +
  theme_niall()


w5 <- mls.weatherdataset %>% 
  filter(Roof %in% c("Open")) %>% 
  distinct(MatchID, `soil_temperature_0_to_7cm (°C)`, MatchInjury) %>% 
  ggplot(aes(y= `soil_temperature_0_to_7cm (°C)`, x = MatchInjury, fill = MatchInjury, colour = MatchInjury)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(values = c("#00b300", "#cc0000"), guide = "none") +
  scale_colour_manual(values = c("#00b300", "#cc0000"), guide = "none") +
  labs(x= "Player Injured?") +
  ggtitle("Soil Temperature", subtitle = "While injuries tend to occur in slightly lower\nsoil temperatures, doesn't look significant") +
  coord_flip() +
  theme_niall()

w6 <- mls.weatherdataset %>%
  filter(Roof %in% c("Open")) %>%
  distinct(MatchID, `soil_moisture_0_to_7cm (m³/m³)`, MatchInjury) %>%
  ggplot(aes(y = `soil_moisture_0_to_7cm (m³/m³)`, x = MatchInjury, fill = MatchInjury, colour = MatchInjury)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(values = c("#00b300", "#cc0000"), guide = "none") +
  scale_colour_manual(values = c("#00b300", "#cc0000"), guide = "none") +
  labs(x = "Player Injured?") +
  ggtitle("Soil Moisture", subtitle = "While injuries tend to occur in slightly lower\nsoil moisture, doesn't look significant") +
  coord_flip() +
  theme_niall()


w7 <- mls.weatherdataset %>%
  filter(Roof %in% c("Open")) %>%
  distinct(MatchID, `temperature_2m (°C)`, MatchInjury) %>%
  ggplot(aes(y = `temperature_2m (°C)`, x = MatchInjury, fill = MatchInjury, colour = MatchInjury)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(values = c("#00b300", "#cc0000"), guide = "none") +
  scale_colour_manual(values = c("#00b300", "#cc0000"), guide = "none") +
  labs(x = "Player Injured?") +
  ggtitle("Temperature", subtitle = "While injuries tend to occur in slightly lower\ntemperature, doesn't look significant") +
  coord_flip() +
  theme_niall()


w8 <- mls.weatherdataset %>%
  filter(Roof %in% c("Open")) %>%
  distinct(MatchID, `apparent_temperature (°C)`, MatchInjury) %>%
  ggplot(aes(y = `apparent_temperature (°C)`, x = MatchInjury, fill = MatchInjury, colour = MatchInjury)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(values = c("#00b300", "#cc0000"), guide = "none") +
  scale_colour_manual(values = c("#00b300", "#cc0000"), guide = "none") +
  labs(x = "Player Injured?") +
  ggtitle("Apparent Temperature", subtitle = "While injuries tend to occur in slightly lower\napparent temperature, doesn't look significant") +
  coord_flip() +
  theme_niall()

gridExtra::grid.arrange(w1, w2, w3, w4, w5,w6,w7,w8, nrow = 4)

gridExtra::grid.arrange(w1, w2, w3, w4, w5,w6, nrow = 3)





 mls.weatherdataset %>%
  filter(Roof %in% c("Open")) %>% 
  distinct(MatchID) %>% 
  #inner_join(df3, by = "MatchID") %>%
  ggplot(aes(x = `precipitation (mm)`)) +
  geom_boxplot(alpha = 0.5, fill = "lightgreen", color = "green") +
  ggtitle("Tempeartures With No Injury") +
  labs(x = "Temperature (°C)") +
  theme_bw() +
  theme(axis.text.y = element_blank()) 

 
 
 library(ggplot2)
 
 mls.weatherdataset %>%
   filter(Roof %in% c("Open")) %>%
   distinct(MatchID, `precipitation (mm)`, MatchInjury) %>%
   ggplot(aes(y = `precipitation (mm)`, fill = MatchInjury)) +
   geom_boxplot(alpha = 0.5) +
   scale_fill_manual(values = c("no" = "blue", "yes" = "red")) +  # Specify colors for the fill
   labs(x = "Player Injured?", y = "Precipitation (mm)") +
   ggtitle("Precipitation Levels in Matches with and without Injuries") +
   facet_wrap(~ MatchInjury) +  # Facet based on MatchInjury
   theme_bw()
 
 #ABS
 mls.fixtures$absolute_difference <- abs(mls.fixtures$HomeGoals - mls.fixtures$AwayGoals)
 mls.injury.fixtures$absolute_difference <- abs(mls.injury.fixtures$HomeGoals - mls.injury.fixtures$AwayGoals)
 
 dplyr::count(mls.fixtures, absolute_difference)
 dplyr::count(mls.injury.fixtures, absolute_difference)
 
 
 abs1 <- mls.injury.fixtures %>% 
   group_by(absolute_difference) %>% 
   count(absolute_difference)
 
 new_row <- data.frame(
   absolute_difference = 6,
   n = 0
 )
 
 insert_index <- 7
 
 # Split the dataframe into two parts
 df_above <- abs1[1:(insert_index - 1), ]
 df_below <- abs1[insert_index:nrow(abs1), ]
 
 # Combine the two parts with the new row inserted in between
 abs1 <- rbind(df_above, new_row, df_below)
 
 # Reset row names
 #rownames(abs1) <- NULL
 
 #abs1 <- rbind(abs1,new_row) 
 
 
 abs2 <- mls.fixtures %>% 
   group_by(absolute_difference) %>% 
   count(absolute_difference)

 abs2 <- na.omit(abs2)
 
mls.scores <- cbind(abs1,abs2) 

mls.scores <- select(mls.scores,1,2,4)

colnames(mls.scores) <- c("Absolute_Goal_Difference", "Injury_Count","Total_Games")

mls.scores$Injury_Percentage <- (mls.scores$Injury_Count / mls.scores$Total_Games) * 100

mls.scores$Absolute_Goal_Difference <- as.character(mls.scores$Absolute_Goal_Difference)

mls.scores %>% 
  ggplot(aes(x= Absolute_Goal_Difference, y= Injury_Percentage)) +
  geom_col(fill = plot_cols[6], colour = plot_cols[6], alpha = 0.6) +
  geom_text(aes(label = paste0(round(Injury_Percentage, 1), "%")), vjust = -0.5, size = 3, color = "black") +  # Add percentages on bars
  labs(x= "Absolute Goal Difference", y= "Percentage of Injuries") +
  ggtitle("Breakdown of MLS Injuries Based on Game Situation", subtitle = "The relationship Between Goal Difference and Injury\nDoesn't look to be significant") +
  theme_niall() 
  

mls.home.away <- mls.injury.fixtures$Away
mls.home.surface <- mls.injury.fixtures$Surface


mls.home.away <- data.frame(mls.home.away,
                            mls.home.surface)

test <-  mls.home.away %>%
  mutate(HomeVsAway = case_when(mls.home.away  != "NA"  ~ "Home",
                                             TRUE ~ "Away"))

dplyr::count(test, HomeVsAway)

dplyr::count(test, mls.home.surface)

trial <- test %>% 
  group_by(mls.home.surface) %>% 
  count(HomeVsAway)


trial$SurfaceGames <- c(227,227,630,630,82,82)

colnames(trial) <- c("SurfaceType", "HomeVsAway","InjuryCount","SurfaceGames")

trial$Injury_Percentage <- (trial$InjuryCount / trial$SurfaceGames) * 100


trial %>%
  ggplot(aes(x = SurfaceType, y = Injury_Percentage, fill = HomeVsAway)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(Injury_Percentage, 1), "%")),
            position = position_stack(vjust = 0.5),  # Position text at the center of each segment
            size = 6, color = "black") +  # Add percentages on bars
  labs(x = "Surface Type", y = "Percentage of Injuries") +
  ggtitle("Breakdown of MLS Injuries Based on Team's Surface Type",
          subtitle = "The Stacked Bar chart shows a signifcant difference in Injury Percentage \nwith teams who play most games on Field Turf") +
  theme_niall() 

stad <- dplyr::count(mls.injury.fixtures, `Stadium Name`)


stad <- dplyr::count(mls.injury.fixtures, `Stadium Name`)

stad.fix <- dplyr::count(mls.fixtures, `Stadium Name`)

stad.surf <- select(mls.injury.fixtures,23,25 )

stadium <- left_join(stad, stad.fix, by = "Stadium Name")

stadium <- left_join(stadium, stad.surf, by = "Stadium Name")

stadium <- unique(stadium)

colnames(stadium) <- c("Stadium Name", "Injury_Count","Games","Surface")



stadium$Injury_Percentage <- (stadium$Injury_Count / stadium$Games) * 100

#mls.scores$Absolute_Goal_Difference <- as.character(mls.scores$Absolute_Goal_Difference)

stadium %>% 
  ggplot(aes(x= stadium, y= Injury_Percentage, fill = surface)) +
  geom_col(fill = plot_cols[6], colour = plot_cols[6], alpha = 0.6) +
  geom_text(aes(label = paste0(round(Injury_Percentage, 1), "%")), vjust = -0.5, size = 3, color = "black") +  # Add percentages on bars
  labs(x= "Absolute Goal Difference", y= "Percentage of Injuries") +
  ggtitle("Breakdown of MLS Injuries Based on Game Situation", subtitle = "The relationship Between Goal Difference and Injury\nDoesn't look to be significant") +
  theme_niall() 

stadium <- stadium %>% 
  filter(Games > 15)

clipr::write_clip(stadium)


top_stadiums <- head(stadium[order(-stadium$Injury_Percentage),], 8)

# Create the bar plot
stad1 <- ggplot(top_stadiums, aes(x = reorder(`Stadium Name`, Injury_Percentage), y = Injury_Percentage, fill = Surface)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Injury_Percentage, 1), "%")), vjust = -0.5, size = 6, color = "black") +  # Add percentages on bars
  scale_fill_manual(values = c("Grass" = "green", "Field Turf" = "darkgreen", "Hybrid" = "#AFE1AF")) +
  labs(x = "Stadium Name", y = "Injury Percentage (%)", fill = "Surface") +  # Add "%" to the y-axis label
  ggtitle("Top 8 Stadiums by Injury Percentage (2012-2023)") +
  theme_niall() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#stadium2 <- subset(stadium, `Stadium Name` %in% c("Camping World Stadium", "TCF Bank Stadium"))

stadium2 <- subset(stadium, !(grepl("Camping World Stadium", `Stadium Name`, ignore.case = TRUE) | grepl("TCF Bank Stadium", `Stadium Name`, ignore.case = TRUE)))

stadium2 <- head(stadium2[order(-stadium2$Injury_Percentage),], 8)


stad2 <- ggplot(stadium2, aes(x = reorder(`Stadium Name`, Injury_Percentage), y = Injury_Percentage, fill = Surface)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Injury_Percentage, 1), "%")), vjust = -0.5, size = 6, color = "black") +  # Add percentages on bars
  scale_fill_manual(values = c("Grass" = "green", "Field Turf" = "darkgreen", "Hybrid" = "#AFE1AF")) +
  labs(x = "Stadium Name", y = "Injury Percentage (%)", fill = "Surface") +  # Add "%" to the y-axis label
  ggtitle("Top 8 Stadiums by Injury Percentage (Active)") +
  theme_niall() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###############################Trial#############################################
# Assuming you have a data frame called player_data with columns: PlayerID, Surface, GamesPlayed
# Assuming you have a data frame called player_data with columns: PlayerID, Surface

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



urls_majority_hybrid <- majority_surface %>%
  filter(MajoritySurface == "Hybrid") %>%
  pull(URL)

filtered_data2 <- games_played_per_player_surface %>%
  filter(URL %in% urls_majority_hybrid)



urls_majority_grass <- majority_surface %>%
  filter(MajoritySurface == "Grass") %>%
  pull(URL)



filtered_data3 <- games_played_per_player_surface %>%
  filter(URL %in% urls_majority_grass)



filtered_data <- filtered_data %>%
  filter(GamesPlayed > 10)


filtered_data2 <- filtered_data2 %>%
  filter(GamesPlayed > 10)


filtered_data3 <- filtered_data3 %>%
  filter(GamesPlayed > 10)


filtered_data3 <- filtered_data3[complete.cases(filtered_data3),]

unique_values <- unique(filtered_data$URL)
unique_values2 <- unique(filtered_data2$URL)
unique_values3 <- unique(filtered_data3$URL)


grass.matches <- mls.player.fixtures %>%
  filter(URL %in% unique_values3)

grass.matches$game_injury <- ifelse(is.na(grass.matches$injury), 0, 1)

grass.matches$Date <- as.Date(grass.matches$Date)

grass.matches <- grass.matches %>%
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


hybrid.matches <- mls.player.fixtures %>%
  filter(URL %in% unique_values2)

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
df <- df[!duplicated(df$Game_Number), ]

print(df)

# Print the updated dataframe
head(highest_game_numbers)

#grass.Kaplan <- df

#turf.Kaplan <- df

grass.Kaplan <- grass.Kaplan %>% filter(Game_Number < 3000)

string_to_replicate <- "Grass"

# Replicate the string for each row and assign it to a new column
grass.Kaplan$Surface <- rep(string_to_replicate, nrow(grass.Kaplan))


string_to_replicate <- "Field Turf"

new_row <- data.frame(
  Percent = 51.59420, 
  Game_Number = 3000,
  Surface = "Field Turf"
)
  
turf.Kaplan <- rbind(turf.Kaplan, new_row)

# Replicate the string for each row and assign it to a new column
turf.Kaplan$Surface <- rep(string_to_replicate, nrow(turf.Kaplan))


# Stacked area chart
ggplot() +
  geom_area(data = rbind(grass.Kaplan, turf.Kaplan), aes(x = Game_Number, y = Percent, fill = Surface), 
            position = "stack") +
  scale_fill_manual(values = c("Grass" = "red", "Field Turf" = "blue")) +  # Custom colors
  theme_niall() +
  labs(x = "X-axis Label", y = "Y-axis Label", title = "Stacked Area Chart")



# Assuming you have grass.Kaplan and turf.Kaplan data frames
datadata = rbind(grass.Kaplan, turf.Kaplan)
ggplot() +
  geom_area(data = rbind(grass.Kaplan, turf.Kaplan), aes(x = Game_Number, y = Percent, fill = Surface), 
            position = "identity", alpha = 0.8) +  # Alpha controls transparency
  geom_line(data = rbind(grass.Kaplan, turf.Kaplan), aes(x = Game_Number, y = Percent, color = Surface)) +
  scale_fill_manual(values = c("Grass" = "red", "Field Turf" = "black")) +  # Custom colors for area
  scale_color_manual(values = c("Grass" = "red", "Field Turf" = "black")) +  # Custom colors for line
  theme_niall() +
  labs(x = "X-axis Label", y = "Y-axis Label", title = "Line Area Chart")

ggplot(data = rbind(grass.Kaplan, turf.Kaplan), aes(x = Game_Number, y = Percent, fill = Surface)) +
  geom_area()




ggplot() +
  geom_area(data = rbind(grass.Kaplan, turf.Kaplan), aes(x = Game_Number, y = Percent, fill = Surface), 
            position = "identity", alpha = 0.5) +  # Alpha controls transparency
  geom_line(data = rbind(grass.Kaplan, turf.Kaplan), aes(x = Game_Number, y = Percent, color = Surface)) +
  scale_fill_manual(values = c("Grass" = "#4daf4a", "Field Turf" = "#e41a1c")) +  # Custom colors for area
  scale_color_manual(values = c("Grass" = "#4daf4a", "Field Turf" = "#e41a1c")) +  # Custom colors for line
  #scale_linetype_manual(values = c("Grass" = "solid", "Field Turf" = "dashed")) +
  theme_niall() +
  labs(x = "Matches", y = "Percentage (%)", title = "Kaplan–Meier Estimator Based On Surface Type",
       subtitle = "Compared Player Injury Rates with players who played more than\n50% of Games on one surface.\nP-Value = 0.4793")

#chisq.test()

chisq.test(datadata$Percent, datadata$Surface)


library(plotly)
fig <- plot_ly(x = ~grass.Kaplan$Game_Number, y = ~grass.Kaplan$Percent, type = 'scatter', mode = 'lines', name = 'Fair cut', fill = 'tozeroy')
fig <- fig %>% add_trace(x = ~turf.Kaplan$Game_Number, y = ~turf.Kaplan$Percent, name = 'Ideal cut', fill = 'tozeroy')
fig <- fig %>% layout(xaxis = list(title = 'Carat'),
                      yaxis = list(title = 'Density'))

fig  


