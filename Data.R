# Libraries 

library(tidyverse)
library(worldfootballR)

# Data 

data1 <- tm_player_market_values(country_name = "England", start_year = 2022)
data2 <- tm_player_market_values(country_name = "Spain", start_year = 2022)
data3 <- tm_player_market_values(country_name = "Germany", start_year = 2022)
data4 <- tm_player_market_values(country_name = "Italy", start_year = 2022)
data5 <- tm_player_market_values(country_name = "France", start_year = 2022)

# Combine and Wrangle

data <- rbind(data1, data2, data3, data4, data5) 

data <- data %>%
  filter(!player_name == "Benjamin Mendy") %>%
  filter(!player_name == "Mason Greenwood")
  
# Save

write.csv(data, "CompositionSquadData.csv", row.names = FALSE)
