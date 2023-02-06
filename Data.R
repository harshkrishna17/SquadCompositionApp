# Libraries 

library(tidyverse)
library(worldfootballR)

# Data 

data1 <- tm_player_market_values(country_name = "England", start_year = 2022)
data2 <- tm_player_market_values(country_name = "Spain", start_year = 2022)
data3 <- tm_player_market_values(country_name = "Germany", start_year = 2022)
data4 <- tm_player_market_values(country_name = "Italy", start_year = 2022)
data5 <- tm_player_market_values(country_name = "France", start_year = 2022)
data6 <- tm_player_market_values(country_name = "Portugal", start_year = 2022)
data7 <- tm_player_market_values(country_name = "Austria", start_year = 2022)
data8 <- tm_player_market_values(country_name = "Netherlands", start_year = 2022)
data9 <- tm_player_market_values(country_name = "Scotland", start_year = 2022)
data10 <- tm_player_market_values(league_url = "https://www.transfermarkt.co.in/championship/startseite/wettbewerb/GB2",
                                  start_year = 2022)

# Combine and Wrangle

data <- rbind(data1, data2, data3, data4, data5, data6, data7, data8, data9, data10) 

data <- data %>%
  filter(!player_name == "Benjamin Mendy") %>%
  filter(!player_name == "Mason Greenwood") %>%
  filter(!player_name == "Thomas Partey")

# Save

write.csv(data, "CompositionSquadDataJanuary23.csv", row.names = FALSE)
