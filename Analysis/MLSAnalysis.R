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

mls.fixtures$absolute_difference <- abs(mls.fixtures$HomeGoals - mls.fixtures$AwayGoals)
mls.injury.fixtures$absolute_difference <- abs(mls.injury.fixtures$HomeGoals - mls.injury.fixtures$AwayGoals)

dplyr::count(mls.fixtures, absolute_difference)
dplyr::count(mls.injury.fixtures, absolute_difference)

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
 