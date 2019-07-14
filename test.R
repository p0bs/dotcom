library(premPredictor)
suppressMessages(library(tidyverse))

iterations <- 10000

player_info <- "https://www.dropbox.com/s/uin6zk4w5cyk2m1/PremPredict-18-19.csv"
player_data <- get_player_data(url_value = player_info)

odds_info <- "https://www.dropbox.com/s/cb5qyxyexksvez1/odds.csv"
odds_data <- get_odds_data(url_value = odds_info)

get_latest_standings(data_input = player_data, use_saved_data = T, data_file = 'content/portfolio/data/201819_penultimate.rds')
