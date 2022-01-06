#Attaching packages we'll be using

library(tidyverse)

#Let's take a look at the touchback rate on punts <= 60 yards from the opponents endzone since 2000.
#2021 data through Week 13. Data courtesy of nflfastR.

nflfastR_2000_2021_punt_pbp <- read.csv('../input/2000-2020-nflfastr-punt-play-by-play/punt_pbp_2000_2021.csv')

punts <- nflfastR_2000_2021_punt_pbp %>% group_by(season) %>% filter(yardline_100 <= 60, play_type == 'punt') %>% summarise(punts = n())

# I picked the distance of <= 60 yards from the opponents end zone because it seems like a distance that most,
#if not all, punters should be capable of getting a touchback from.

punts_touchback <- nflfastR_2000_2021_punt_pbp %>% group_by(season) %>% filter(yardline_100 <= 60, touchback == '1', play_type == 'punt') %>% summarise(touchbacks = n())

total_punts <- left_join(punts_touchback, punts)

total_punts <- total_punts %>% mutate(touchback_rate = touchbacks/punts)

total_punts$touchback_rate <- round(total_punts$touchback_rate, 2)

total_punts %>% ggplot(aes(x = season, y = touchback_rate*100)) +
  geom_point() +
  geom_smooth(method=lm) +
  labs(x = 'Season', y = 'Touchback Rate (%)',
       title = 'Touchback Rate on Punts 2000-2021',
       subtitle = 'On punts <= 60 yards from opponent end zone',
       caption = 'Graph by @Mike_Lounsberry, data from nflfastR')

#Touchback Rate

#Filtering the plays dataset to just include punt plays.

plays <- read.csv('../input/nfl-big-data-bowl-2022/plays.csv')

punt_plays <- plays %>% filter(specialTeamsPlayType == 'Punt')

#Joining the punt_plays data set and PFFScoutingData data set together.

PFFScoutingData <- read.table('../input/nfl-big-data-bowl-2022/PFFScoutingData.csv',
                              header = TRUE,
                              sep = ",",
                              colClasses = c("numeric", "numeric", "NULL",
                                             "numeric", "NULL", "NULL",
                                             "NULL", "NULL", "NULL",
                                             "NULL", "NULL", "NULL",
                                             "NULL", "NULL", "NULL",
                                             "NULL", "NULL", "NULL",
                                             "NULL", "character"))

pff_punt_plays <- left_join(punt_plays, PFFScoutingData)

#Combining all the tracking data into one data set.

tracking2018 <- read.table('../input/nfl-big-data-bowl-2022/tracking2018.csv',
                           header = TRUE,
                           sep = ",",
                           colClasses = c("NULL", "numeric", "numeric",
                                          "NULL", "NULL", "NULL", "NULL",
                                          "NULL", "character", "NULL", "character",
                                          "NULL", "NULL", "NULL", "NULL",
                                          "numeric", "numeric", "NULL"))

tracking2019 <- read.table('../input/nfl-big-data-bowl-2022/tracking2019.csv',
                           header = TRUE,
                           sep = ",",
                           colClasses = c("NULL", "numeric", "numeric",
                                          "NULL", "NULL", "NULL", "NULL",
                                          "NULL", "character", "NULL", "character",
                                          "NULL", "NULL", "NULL", "NULL",
                                          "numeric", "numeric", "NULL"))

tracking2020 <- read.table('../input/nfl-big-data-bowl-2022/tracking2020.csv',
                           header = TRUE,
                           sep = ",",
                           colClasses = c("NULL", "numeric", "numeric",
                                          "NULL", "NULL", "NULL", "NULL",
                                          "NULL", "character", "NULL", "character",
                                          "NULL", "NULL", "NULL", "NULL",
                                          "numeric", "numeric", "NULL"))

complete_tracking <- rbind(tracking2018, tracking2019, tracking2020)

#Joining both of our new data sets together.

pff_punt_plays_tracking <- left_join(pff_punt_plays, complete_tracking)

#Getting all the punts that landed inside of the 10 yard line.

#Don't need to be more specific on X values because punts don't get assigned a kickContactType if they
#land in the endzone. Fair catch is filtered out because occasionally a returner was interfered with
#and this resulted in a kickContactType being assigned.

inside_10_all <- pff_punt_plays_tracking %>% filter(kickContactType == 'BB' | kickContactType == 'BF' |
                                                      kickContactType == 'KTB' | kickContactType == 'KTC',
                                                    event == 'punt_land',
                                                    x < 20 | x > 100,
                                                    y > 0 | y < 53.3,
                                                    specialTeamsResult != 'Fair Catch',
                                                    specialTeamsResult != 'Return',
                                                    displayName == 'football')

#Getting the punts that landed inside the 10 yard line that didn't result in a touchback.

inside_10_touchback <- pff_punt_plays_tracking %>% filter(kickContactType == 'BB' | kickContactType == 'BF' |
                                                            kickContactType == 'KTB' | kickContactType == 'KTC',
                                                          event == 'punt_land',
                                                          x < 20 | x > 100,
                                                          y > 0 | y < 53.3,
                                                          specialTeamsResult == 'Touchback',
                                                          specialTeamsResult != 'Fair Catch',
                                                          displayName == 'football')

nrow(inside_10_touchback)/nrow(inside_10_all)

#Field Surface

#Bringing in play by play data for just 2018-2020 from nflfastR

pbp_2018 <- read.csv('../input/nfl-play-data-2010-2020/play_by_play_2018.csv')
pbp_2019 <- read.csv('../input/nfl-play-data-2010-2020/play_by_play_2019.csv')
pbp_2020 <- read.csv('../input/nfl-play-data-2010-2020/play_by_play_2020.csv')

pbp2018_2020 <- rbind(pbp_2018, pbp_2019, pbp_2020)

#Renaming columns to help with joining.

pbp2018_2020 <- pbp2018_2020 %>% rename(playId = play_id)
pbp2018_2020 <- pbp2018_2020 %>% rename(gameId = old_game_id)

#Removing game_id from nflfastR to avoid confusion.

pbp2018_2020$game_id <- NULL

#Changing the playId and gameId to numeric so we can complete a left join later on.

pbp2018_2020$playId <- as.numeric(pbp2018_2020$playId)
pbp2018_2020$gameId <- as.numeric(pbp2018_2020$gameId)

#Bringing in field surfaces from nflfastR

pbp_with_surface <- pbp2018_2020 %>% select(gameId, playId, surface, stadium, season, temp, roof)

#Correcting stadium names to be more uniform

pbp_with_surface$stadium <- str_replace(pbp_with_surface$stadium, "New Era Field", "Bills Stadium")
pbp_with_surface$stadium <- str_replace(pbp_with_surface$stadium, "CenturyField", "CenturyLink Field")
pbp_with_surface$stadium <- str_replace(pbp_with_surface$stadium, "Lumen Field", "CenturyLink Field")
pbp_with_surface$stadium <- str_replace(pbp_with_surface$stadium, "Broncos Stadium at Mile High", "Empower Field at Mile High")
pbp_with_surface$stadium <- str_replace(pbp_with_surface$stadium, "NRG Stadium", "NRG")
pbp_with_surface$stadium <- str_replace(pbp_with_surface$stadium, "NRG", "NRG Stadium")
pbp_with_surface$stadium <- str_replace(pbp_with_surface$stadium, "MetLife Stadium", "MetLife")
pbp_with_surface$stadium <- str_replace(pbp_with_surface$stadium, "MetLife", "MetLife Stadium")
pbp_with_surface$stadium <- str_replace(pbp_with_surface$stadium, "Los Angeles Memorial Coliesum", "Los Angeles Memorial Coliseum")
pbp_with_surface$stadium <- str_replace(pbp_with_surface$stadium, "Lambeau field", "Lambeau Field")
pbp_with_surface$stadium <- str_replace(pbp_with_surface$stadium, "FedexField", "FedExField")
pbp_with_surface$stadium <- str_replace(pbp_with_surface$stadium, "FirstEnergyStadium", "FirstEnergy Stadium")
pbp_with_surface$stadium <- str_replace(pbp_with_surface$stadium, "First Energy Stadium", "FirstEnergy Stadium")
pbp_with_surface$stadium <- str_replace(pbp_with_surface$stadium, "Dignity Health Sports Park", "StubHub Center")

#Correcting multiple spellings of the same type of field surface

pbp_with_surface$surface <- str_replace(pbp_with_surface$surface, "fieldturf ", "fieldturf")
pbp_with_surface$surface <- str_replace(pbp_with_surface$surface, "matrixturf", "matrix")

#Correcting field surfaces

pbp_with_surface <- within(pbp_with_surface, surface[stadium == 'Bills Stadium'] <- 'a_turf')
pbp_with_surface <- within(pbp_with_surface, surface[stadium == 'New Era Field'] <- 'a_turf')

pbp_with_surface <- within(pbp_with_surface, surface[surface == 'fieldturf' & stadium == 'MetLife Stadium' & season == '2018'] <- 'UBU')
pbp_with_surface <- within(pbp_with_surface, surface[surface == 'fieldturf' & stadium == 'MetLife Stadium' & season == '2019'] <- 'UBU')

pbp_with_surface <- within(pbp_with_surface, surface[surface == 'astroturf' & stadium == 'Arrowhead Stadium'] <- 'grass')

pbp_with_surface <- within(pbp_with_surface, surface[stadium == 'AT&T Stadium'] <- 'matrix')

pbp_with_surface <- within(pbp_with_surface, surface[stadium == 'Mercedes-Benz Superdome' & season == '2018'] <- 'UBU')
pbp_with_surface <- within(pbp_with_surface, surface[stadium == 'Mercedes-Benz Superdome' & season == '2019'] <- 'turf_nation')
pbp_with_surface <- within(pbp_with_surface, surface[stadium == 'Mercedes-Benz Superdome' & season == '2020'] <- 'turf_nation')

pbp_with_surface <- within(pbp_with_surface, surface[stadium == 'Lucas Oil Stadium'] <- 'shaw')

pbp_with_surface <- within(pbp_with_surface, surface[stadium == 'NRG Stadium'] <- 'matrix')

pbp_with_surface <- within(pbp_with_surface, surface[stadium == 'Paul Brown Stadium'] <- 'shaw')

pbp_with_surface <- within(pbp_with_surface, surface[stadium == 'U.S. Bank Stadium'] <- 'UBU')

pbp_with_surface <- within(pbp_with_surface, surface[stadium == 'Gillette Stadium'] <- 'fieldturf')

#Adjusting temperature, setting dome temperatures to 68. For open,
#assuming it's a mild climate and also went with 68.

pbp_with_surface <- within(pbp_with_surface, temp[roof == 'dome'] <- '68')
pbp_with_surface <- within(pbp_with_surface, temp[roof == 'closed'] <- '68')
pbp_with_surface <- within(pbp_with_surface, temp[roof == 'open'] <- '68')

pbp_with_surface$temp[is.na(pbp_with_surface$temp)]<- '68'

#Joining together with existing data

all_punts_inside_10_with_surface <- left_join(inside_10_all, pbp_with_surface)
punts_inside_10_with_surface_TB <- left_join(inside_10_touchback, pbp_with_surface)

#Creating tables

all_punts_by_surface <- all_punts_inside_10_with_surface %>%
  group_by(surface) %>%
  summarise(total_punts_inside_10 = n())

touchback_punts_by_surface <- punts_inside_10_with_surface_TB %>% 
  group_by(surface) %>%
  summarise(touchback_punts = n())

Touchback_By_Surface <- left_join(all_punts_by_surface, touchback_punts_by_surface)

Touchback_By_Surface <- Touchback_By_Surface %>%
  mutate(touchback_percentage = round(touchback_punts/total_punts_inside_10, 2))

Touchback_By_Surface %>% ggplot(aes(x = surface, y = touchback_percentage*100)) +
  geom_col(fill = "#85C285", color = "black") +
  geom_text(aes(label = paste0(total_punts_inside_10, " punts"), vjust = -0.5)) +
  labs(title = "Touchback Rate On Punts By Field Surface",
       subtitle = "When punts land inside of the 10 yard line, 2018-2020",
       x = "Field Surface",
       y = "Touchback Percentage (%)") +
  scale_y_continuous(breaks = seq(0,80,10)) +
  geom_hline(yintercept = sum(Touchback_By_Surface$touchback_punts)/sum(Touchback_By_Surface$total_punts_inside_10)*100,
             linetype = 'dashed')

#Temperature

#Creating temperature df

#90-97

punts_97_90 <- all_punts_inside_10_with_surface %>%
  filter(between(temp, 90, 97))

touchback_punts_97_90 <- punts_inside_10_with_surface_TB %>%
  filter(between(temp, 90, 97))

touchback_percentage_97_90 <- nrow(touchback_punts_97_90)/nrow(punts_97_90)*100

touchback_percentage_97_90 <- round(touchback_percentage_97_90, 2)

#89-80

punts_89_80 <- all_punts_inside_10_with_surface %>%
  filter(between(temp, 80, 89))

touchback_punts_89_80 <- punts_inside_10_with_surface_TB %>%
  filter(between(temp, 80, 89))

touchback_percentage_89_80 <- nrow(touchback_punts_89_80)/nrow(punts_89_80)*100

touchback_percentage_89_80 <- round(touchback_percentage_89_80, 2)

#79-70

punts_79_70 <- all_punts_inside_10_with_surface %>%
  filter(between(temp, 70, 79))

touchback_punts_79_70 <- punts_inside_10_with_surface_TB %>%
  filter(between(temp, 70, 79))

touchback_percentage_79_70 <- nrow(touchback_punts_79_70)/nrow(punts_79_70)*100

touchback_percentage_79_70 <- round(touchback_percentage_79_70, 2)

#69-60

punts_69_60 <- all_punts_inside_10_with_surface %>%
  filter(between(temp, 60, 69))

touchback_punts_69_60 <- punts_inside_10_with_surface_TB %>%
  filter(between(temp, 60, 69))

touchback_percentage_69_60 <- nrow(touchback_punts_69_60)/nrow(punts_69_60)*100

touchback_percentage_69_60 <- round(touchback_percentage_69_60, 2)

#59-50

punts_59_50 <- all_punts_inside_10_with_surface %>%
  filter(between(temp, 50, 59))

touchback_punts_59_50 <- punts_inside_10_with_surface_TB %>%
  filter(between(temp, 50, 59))

touchback_percentage_59_50 <- nrow(touchback_punts_59_50)/nrow(punts_59_50)*100

touchback_percentage_59_50 <- round(touchback_percentage_59_50, 2)

#49-40

punts_49_40 <- all_punts_inside_10_with_surface %>%
  filter(between(temp, 40, 49))

touchback_punts_49_40 <- punts_inside_10_with_surface_TB %>%
  filter(between(temp, 40, 49))

touchback_percentage_49_40 <- nrow(touchback_punts_49_40)/nrow(punts_49_40)*100

touchback_percentage_49_40 <- round(touchback_percentage_49_40, 2)

#39 >

punts_under_39 <- all_punts_inside_10_with_surface %>%
  filter(temp <= 39)

touchback_punts_under_39 <- punts_inside_10_with_surface_TB %>%
  filter(temp <= 39)

touchback_percentage_under_39 <- nrow(touchback_punts_under_39)/nrow(punts_under_39)*100

touchback_percentage_under_39 <- round(touchback_percentage_under_39, 2)

temp_df <- data.frame(
  Temperature = c("<= 39", "40-49", "50-59", "60-69", "70-79", "80-89", "90-97"),   
  Touchbacks = c(nrow(touchback_punts_under_39), nrow(touchback_punts_49_40), nrow(touchback_punts_59_50), nrow(touchback_punts_69_60), nrow(touchback_punts_79_70), nrow(touchback_punts_89_80), nrow(touchback_punts_97_90)),
  Total_Punts = c(nrow(punts_under_39), nrow(punts_49_40), nrow(punts_59_50), nrow(punts_69_60), nrow(punts_79_70), nrow(punts_89_80), nrow(punts_97_90)),
  Touchback_Percentage = c(touchback_percentage_under_39, touchback_percentage_49_40, touchback_percentage_59_50, touchback_percentage_69_60, touchback_percentage_79_70, touchback_percentage_89_80, touchback_percentage_97_90))

temp_df %>% ggplot(aes(x = Temperature, y = Touchback_Percentage)) +
  geom_col(fill = "#4FC3F7", color = "black") +
  geom_text(aes(label = paste0(Total_Punts, " punts"), vjust = -0.25)) +
  labs(title = "Touchback Rate On Punts By Temperature",
       subtitle = "When punts land inside of the 10 yard line, 2018-2020",
       x = "Temperature (Fahrenheit)",
       y = "Touchback Percentage (%)") +
  scale_y_continuous(breaks = seq(0,80,10)) +
  geom_hline(yintercept = sum(Touchback_By_Surface$touchback_punts)/sum(Touchback_By_Surface$total_punts_inside_10)*100,
             linetype = 'dashed')

#Code for ShinyApp

#Creating dataset to work off

players <- read.csv('../input/nfl-big-data-bowl-2022/players.csv')

inside_10_all_with_pbp <- left_join(inside_10_all, pbp2018_2020)

inside_10_all_with_pbp$displayName <- NULL

inside_10_all_with_pbp <- inside_10_all_with_pbp %>% rename(nflId = kickerId)

data <- left_join(inside_10_all_with_pbp, players)

data$stadium <- str_replace(data$stadium, "New Era Field", "Bills Stadium")
data$stadium <- str_replace(data$stadium, "CenturyField", "CenturyLink Field")
data$stadium <- str_replace(data$stadium, "Lumen Field", "CenturyLink Field")
data$stadium <- str_replace(data$stadium, "Broncos Stadium at Mile High", "Empower Field at Mile High")
data$stadium <- str_replace(data$stadium, "NRG Stadium", "NRG")
data$stadium <- str_replace(data$stadium, "NRG", "NRG Stadium")
data$stadium <- str_replace(data$stadium, "MetLife Stadium", "MetLife")
data$stadium <- str_replace(data$stadium, "MetLife", "MetLife Stadium")
data$stadium <- str_replace(data$stadium, "Los Angeles Memorial Coliesum", "Los Angeles Memorial Coliseum")
data$stadium <- str_replace(data$stadium, "Lambeau field", "Lambeau Field")
data$stadium <- str_replace(data$stadium, "FedexField", "FedExField")
data$stadium <- str_replace(data$stadium, "FirstEnergyStadium", "FirstEnergy Stadium")
data$stadium <- str_replace(data$stadium, "First Energy Stadium", "FirstEnergy Stadium")
data$stadium <- str_replace(data$stadium, "Dignity Health Sports Park", "StubHub Center")

data$surface <- str_replace(data$surface, "fieldturf ", "fieldturf")
data$surface <- str_replace(data$surface, "matrixturf", "matrix")

Correcting Field Surfaces

data <- within(data, surface[surface == 'astroturf' & stadium == 'Bills Stadium'] <- 'a_turf')
data <- within(data, surface[surface != 'a_turf' & stadium == 'Bills Stadium' | stadium == 'New Era Field'] <- 'a_turf')

data <- within(data, surface[surface == 'fieldturf' & stadium == 'MetLife Stadium' & season == '2018'] <- 'UBU')
data <- within(data, surface[surface == 'fieldturf' & stadium == 'MetLife Stadium' & season == '2019'] <- 'UBU')

data <- within(data, surface[surface == 'astroturf' & stadium == 'Arrowhead Stadium'] <- 'grass')

data <- within(data, surface[stadium == 'AT&T Stadium'] <- 'matrix')

data <- within(data, surface[stadium == 'Mercedes-Benz Superdome' & season == '2018'] <- 'UBU')
data <- within(data, surface[stadium == 'Mercedes-Benz Superdome' & season == '2019'] <- 'turf_nation')
data <- within(data, surface[stadium == 'Mercedes-Benz Superdome' & season == '2020'] <- 'turf_nation')

data <- within(data, surface[stadium == 'Lucas Oil Stadium'] <- 'shaw')

data <- within(data, surface[stadium == 'NRG Stadium'] <- 'matrix')

data <- within(data, surface[stadium == 'Paul Brown Stadium'] <- 'shaw')

data <- within(data, surface[stadium == 'U.S. Bank Stadium'] <- 'UBU')

data <- within(data, surface[stadium == 'Gillette Stadium'] <- 'fieldturf')

Adjusting temperature, setting dome temperatures to 68. For open,
assuming it's a mild climate and also went with 68.

data <- within(data, temp[roof == 'dome'] <- '68')
data <- within(data, temp[roof == 'closed'] <- '68')
data <- within(data, temp[roof == 'open'] <- '68')

UI

ui <- fluidPage(
  titlePanel("Punter Scouting Tool"),
  h3("By Mike Lounsberry"),
  
  mainPanel(
    tabsetPanel(type = "tabs",
      tabPanel("Punters",
        br(),
        sidebarPanel(
            sliderInput("punt_amount", "Punts Inside 10 Yard Line", round = TRUE, min = 1, max = 30, value = 1),
            sliderInput("temp_slider", "Temperature", round = TRUE, min = 14, max = 97, value = c(14, 97)),
              radioButtons("surface_type", "Surface Type",
                           c("A-Turf" = "a_turf",
                             "FieldTurf" = "fieldturf",
                             "Grass" = "grass",
                             "Matrix Turf" = "matrix",
                             "Shaw Turf" = "shaw",
                             "Turf Nation" = "turf_nation",
                             "UBU Turf" = "UBU",
                             "All" = "All"),
                           selected = "All"),
            selectInput("punter_name", "Punter Name",
                        c("All" = "All",
                          "A.J. Cole" = "A.J. Cole",
                          "Andy Lee" = "Andy Lee",
                          "Austin Seibert" = "Austin Seibert",
                          "Braden Mann" = "Braden Mann",
                          "Bradley Pinion" = "Bradley Pinion",
                          "Brett Kern" = "Brett Kern",
                          "Britton Colquitt" = "Britton Colquitt",
                          "Bryan Anger" = "Bryan Anger",
                          "Cameron Johnston" = "Cameron Johnston",
                          "Chris Jones" = "Chris Jones",
                          "Colby Wadman" = "Colby Wadman",
                          "Colton Schmidt" = "Colton Schmidt",
                          "Corey Bojorquez" = "Corey Bojorquez",
                          "Donnie Jones" = "Donnie Jones",
                          "Drew Kaser" = "Drew Kaser",
                          "Dustin Colquitt" = "Dustin Colquitt",
                          "Hunter Niswander" = "Hunter Niswander",
                          "J.K. Scott" = "J.K. Scott",
                          "Jack Fox" = "Jack Fox",
                          "Jake Bailey" = "Jake Bailey",
                          "Jamie Gillan" = "Jamie Gillan",
                          "Johnny Hekker" = "Johnny Hekker",
                          "Johnny Townsend" = "Johnny Townsend",
                          "Jordan Berry" = "Jordan Berry",
                          "Joseph Charlton" = "Joseph Charlton",
                          "Kevin Huber" = "Kevin Huber",
                          "Lachlan Edwards" = "Lachlan Edwards",
                          "Logan Cooke" = "Logan Cooke",
                          "Marquette King" = "Marquette King",
                          "Matt Bosher" = "Matt Bosher",
                          "Matt Haack" = "Matt Haack",
                          "Matt Wile" = "Matt Wile",
                          "Michael Dickson" = "Michael Dickson",
                          "Michael Palardy" = "Michael Palardy",
                          "Mitch Wishnowsky" = "Mitch Wishnowsky",
                          "Pat O'Donnell" = "Pat O'Donnell",
                          "Rigoberto Sanchez" = "Rigoberto Sanchez",
                          "Riley Dixon" = "Riley Dixon",
                          "Ryan Allen" = "Ryan Allen",
                          "Ryan Winslow" = "Ryan Winslow",
                          "Sam Koch" = "Sam Koch",
                          "Sam Martin" = "Sam Martin",
                          "Sterling Hofrichter" = "Sterling Hofrichter",
                          "Thomas Morstead" = "Thomas Morstead",
                          "Tommy Townsend" = "Tommy Townsend",
                          "Tress Way" = "Tress Way",
                          "Trevor Daniel" = "Trevor Daniel",
                          "Ty Long" = "Ty Long")),
            selectInput("stadium_select", "Stadium",
                        c("All" = "All",
                          "Allegiant Stadium" = "Allegiant Stadium",
                          "Arrowhead Stadium" = "Arrowhead Stadium",
                          "AT&T Stadium" = "AT&T Stadium",
                          "Bank of America Stadium" = "Bank of America Stadium",
                          "Bills Stadium" = "Bills Stadium",
                          "CenturyLink Field" = "CenturyLink Field",
                          "Empower Field at Mile High" = "Empower Field at Mile High",
                          "FedExField" = "FedExField",
                          "FirstEnergy Stadium" = "FirstEnergy Stadium",
                          "Ford Field" = "Ford Field",
                          "Gillette Stadium" = "Gillette Stadium",
                          "Hard Rock Stadium" = "Hard Rock Stadium",
                          "Heinz Field" = "Heinz Field",
                          "Lambeau Field" = "Lambeau Field",
                          "Levi's Stadium" = "Levi's Stadium",
                          "Lincoln Financial Field" = "Lincoln Financial Field",
                          "Los Angeles Memorial Coliseum" = "Los Angeles Memorial Coliseum",
                          "Lucas Oil Stadium" = "Lucas Oil Stadium",
                          "M&T Bank Stadium" = "M&T Bank Stadium",
                          "Mercedes-Benz Stadium" = "Mercedes-Benz Stadium",
                          "Mercedes-Benz Superdome" = "Mercedes-Benz Superdome",
                          "MetLife Stadium" = "MetLife Stadium",
                          "Nissan Stadium" = "Nissan Stadium",
                          "NRG Stadium" = "NRG Stadium",
                          "Oakland-Alameda County Coliseum" = "Oakland-Alameda County Coliseum",
                          "Paul Brown Stadium" = "Paul Brown Stadium",
                          "Raymond James Stadium" = "Raymond James Stadium",
                          "SoFi Stadium" = "SoFi Stadium",
                          "Soldier Field" = "Soldier Field",
                          "State Farm Stadium" = "State Farm Stadium",
                          "StubHub Center" = "StubHub Center",
                          "TIAA Bank Field" = "TIAA Bank Field",
                          "Tottenham Hotspur Stadium" = "Tottenham Hotspur Stadium",
                          "U.S. Bank Stadium" = "U.S. Bank Stadium",
                          "Wembley Stadium"))),
      tableOutput("performance")),
    
      tabPanel("Stadium",
        br(),
        sidebarPanel(
          sliderInput("temp_slider_stadium", "Temperature", round = TRUE, min = 14, max = 97, value = c(14, 97)),
          radioButtons("stadium_surface", "Surface Type:",
                       c("A-Turf" = "a_turf",
                         "FieldTurf" = "fieldturf",
                         "Grass" = "grass",
                         "Matrix Turf" = "matrix",
                         "Shaw Turf" = "shaw",
                         "Turf Nation" = "turf_nation",
                         "UBU Turf" = "UBU",
                         "All" = "All"),
                       selected = "All"),
          ),

        tableOutput("stadiums")),
      
      tabPanel("Coverage Team",
               br(),
               sidebarPanel(
                 sliderInput("punt_amount_cov", "Punts Inside 10 Yard Line", round = TRUE, min = 1, max = 30, value = 1)),
               tableOutput("coverage")),
)
)
)


Server

library(gert)
library(dplyr)
library(tidyverse)
library(gt)

server <- function(input, output) {

output$performance <- render_gt({

     if (input$surface_type == 'All') {
     data <- data
   } else {
     data <- data %>% filter(surface == input$surface_type)
   }

   if (input$punter_name == 'All') {
     data <- data
   } else{
     data <- data %>% filter(displayName == input$punter_name)
   }

   if (input$stadium_select == 'All') {
     data <- data
   } else{
     data <- data %>% filter(stadium == input$stadium_select)
   }

data <- data %>% filter(between(temp, input$temp_slider[1], input$temp_slider[2]))

  all <- data %>% group_by(displayName) %>% summarise(all_punts_inside_10 = n())
  touchbacks <- data %>% filter(touchback == '1') %>% group_by(displayName) %>% summarise(punts_inside_10_touchback = n())

  punts_inside_10 <- left_join(all, touchbacks)

  punts_inside_10[is.na(punts_inside_10)] <- 0

  punts_inside_10 <- punts_inside_10 %>% mutate(touchback_percentage = (punts_inside_10_touchback/all_punts_inside_10)*100)

  punts_inside_10$touchback_percentage <- round(punts_inside_10$touchback_percentage, 2)

  punts_inside_10 <- punts_inside_10 %>% arrange(touchback_percentage)

  punts_inside_10 <- punts_inside_10 %>% filter(all_punts_inside_10 >= input$punt_amount)

  punts_inside_10 %>%
    ungroup() %>%
    gt() %>%
    tab_header(
      title = "Touchback Rates By Punter, 2018-2020",
      subtitle = "On Punts That Land Inside Of The 10 Yard Line") %>%
    cols_move(
      columns = all_punts_inside_10,
      after = punts_inside_10_touchback) %>%
    cols_label(
      displayName = "Punter",
      all_punts_inside_10 = "Punts Inside 10",
      punts_inside_10_touchback = "Touchbacks",
      touchback_percentage = "Touchback Percentage") %>%
    cols_align("center") %>%
    data_color(columns = c(touchback_percentage),
               colors = scales::col_numeric(
                 palette = c("085D29", "FD8C24", "D70915"),
                 domain = NULL))
})

output$stadiums <- render_gt({

if (input$stadium_surface == 'All') {
  data <- data
} else {
  data <- data %>% filter(surface == input$stadium_surface)
}

all_punts_stadium <- data %>% group_by(stadium, surface) %>% summarise(all_punts_inside_10 = n())

touchback_punts_stadium <- data %>% filter(touchback == '1') %>%
  group_by(stadium, surface) %>%
  summarise(touchback_punts_inside_10 = n())

combined_stadium <- left_join(touchback_punts_stadium, all_punts_stadium)

combined_stadium[is.na(combined_stadium)] <- 0

combined_stadium <- combined_stadium %>% mutate(touchback_percentage = touchback_punts_inside_10/all_punts_inside_10*100)

combined_stadium$touchback_percentage <- round(combined_stadium$touchback_percentage, 2)

combined_stadium <- combined_stadium %>% arrange(touchback_percentage)

combined_stadium %>%
  ungroup() %>%
  gt() %>%
  tab_header(
    title = "Touchback Rates By Stadium, 2018-2020",
    subtitle = "On Punts That Land Inside Of The 10 Yard Line") %>%
  cols_move(
    columns = all_punts_inside_10,
    after = touchback_punts_inside_10) %>%
  cols_label(
    stadium = "Stadium",
    all_punts_inside_10 = "Punts Inside 10",
    touchback_punts_inside_10 = "Touchbacks",
    touchback_percentage = "Touchback Percentage") %>%
  cols_align("center") %>%
  data_color(columns = c(touchback_percentage),
             colors = scales::col_numeric(
               palette = c("085D29", "FD8C24", "D70915"),
               domain = NULL))

})

output$coverage <- render_gt({

data_all <- data %>% group_by(displayName) %>% summarise(total_punts = n())

data_downed <- data %>% filter(specialTeamsResult == 'Downed') %>%
  group_by(displayName) %>%
  summarise(downed_punts = n())

data_combined <- left_join(data_downed, data_all)

data_combined <- data_combined %>% mutate(down_rate = downed_punts/total_punts*100)
data_combined$down_rate <- round(test$down_rate, 2)

data_combined <- data_combined %>% mutate(down_rate_above_average = down_rate - (sum(data_combined$downed_punts)/sum(data_combined$total_punts)*100))
data_combined$down_rate_above_average <- round(data_combined$down_rate_above_average, 2)

data_combined <- data_combined %>% filter(total_punts >= input$punt_amount_cov)

data_combined %>% ungroup() %>%
    arrange(desc(down_rate)) %>%
    gt() %>%
    tab_header(
      title = "Downed Punt Rate On Punts Inside 10",
      subtitle = "Average punt down rate is 46.44%") %>%
    cols_label(
      displayName = "Punter",
      downed_punts = "Downed Punts",
      down_rate = "Down Rate",
      total_punts = "Total Punts Inside 10",
      down_rate_above_average = "Down Rate Above Average") %>%
    cols_align("center") %>%
    data_color(columns = c(down_rate_above_average),
               colors = scales::col_numeric(
                 palette = c("D70915", "FD8C24", "085D29"),
                 domain = NULL))
})

}
