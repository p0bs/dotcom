library(premPredictor)
suppressMessages(library(tidyverse))

iterations <- 10000
seed <- 26

dataInput <- read_rds('_posts/2019-08-10-twenty-one-teams/data_input_2019-20.rds')

odds_info <- tribble(
  ~game, ~homeValue, ~drawValue, ~awayValue,
  "West Ham-Aston Villa", 3.1, 3.6, 2.25,
  "Man City-Norwich", 1.07, 15, 21,
  "Burnley-Brighton", 2.25, 3.5, 3.1,
  "Everton-AFC Bournemouth", 2.2, 3.9, 3,
  "Chelsea-Wolves", 1.83, 3.7, 4.33,
  "Leicester-Man Utd", 3.2, 3.4, 2.25,
  "Arsenal-Watford", 1.91, 3.9, 3.7,
  "C Palace-Spurs", 6.5, 4.2, 1.52,
  "Newcastle-Liverpool", 8, 5, 1.38,
  "Southampton-Sheff Utd", 2.15, 3.5, 3.3
)

gameOddsBuilder <- odds_info %>%
  separate(
    col = game,
    into = c("homeTeam", "awayTeam"),
    sep = "-",
    remove = FALSE) %>%
  rowwise() %>%
  mutate(
    overround = (1/homeValue) + (1/drawValue) + (1/awayValue),
    homeLikelihood = 1/(homeValue * overround),
    drawLikelihood = 1/(drawValue * overround),
    awayLikelihood = 1/(awayValue * overround),
    winSlice = homeLikelihood,
    drawSlice = homeLikelihood + drawLikelihood
  )


# Alter data_file logic!
clubOrder <- get_latest_EPL_table(use_saved_data = FALSE, data_file = 'dummy.rds') %>%
  dplyr::mutate(fullPts = (1000*Pts) + GD)

clubsABC <- clubOrder %>%
  dplyr::select(Team, fullPts) %>%
  dplyr::arrange(Team)

clubStandings <- match(dataInput$Club, clubOrder$Team, 0)

predictions <- dataInput[,-1]
nPlayers <- ncol(predictions)

ssq <- function(x){sum((x-clubStandings)^2)}
score <- tibble::as_tibble(apply(predictions,2,ssq))
names <- tibble::as_tibble(colnames(predictions))
names1 <- tibble::as_tibble(stringr::str_replace_all(t(names), "_", " "))


vectorRandom <- purrr::as_vector(
  ceiling(
    runif(
      10*iterations,
      min = 0,
      max = 7)
  )
)

netGameGoals <- matrix(data = vectorRandom, ncol = 10)
colnames(netGameGoals) <- paste0("n", 1:10)

unitVectorRandom <- as.vector(runif(10*iterations, min = 0, max = 1))
randomGameValues <- matrix(data = unitVectorRandom, ncol = 10)
colnames(randomGameValues) <- paste0("r", 1:10)

simulatedCalcs <- tibble::tibble(
  iteration = seq.int(iterations)) %>%
  cbind(randomGameValues, netGameGoals)

pointsAdjuster <- function(team) {
  
  teamName <- dplyr::enquo(team)
  
  matchNo <- teamData$matchNo[teamData$team == lazyeval::uq(teamName)]
  isHomeTeam <- teamData$isHomeTeam[teamData$team == lazyeval::uq(teamName)]
  existingPoints <- clubsABC$fullPts[clubsABC == lazyeval::uq(teamName)]
  winBreakpoint <- teamData$winSlice[teamData$team == lazyeval::uq(teamName)]
  drawBreakpoint <- teamData$drawSlice[teamData$team == lazyeval::uq(teamName)]
  workings <- tibble::tibble(
    firstColumn = simulatedCalcs[, (matchNo + 1)],
    secondColumn = simulatedCalcs[, (matchNo + 11)])
  
  existingPoints + if (isHomeTeam == TRUE) {
    workings %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        newPoints = ifelse(
          firstColumn < winBreakpoint,
          3000 + secondColumn,
          ifelse(
            firstColumn < drawBreakpoint,
            1000,
            -secondColumn)
        )
      ) %>%
      dplyr::pull(newPoints)
    
  } else {
    
    workings %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        newPoints = ifelse(
          firstColumn < winBreakpoint,
          -secondColumn,
          ifelse(
            firstColumn < drawBreakpoint,
            1000,
            3000 + secondColumn)
        )
      ) %>%
      dplyr::pull(newPoints)
  }
}

homeTeamData <- gameOddsBuilder %>%
  tibble::rownames_to_column(var = "matchNo") %>%
  dplyr::rename(
    team = homeTeam,
    otherTeam = awayTeam) %>%
  dplyr::select(team, otherTeam, game, matchNo, homeValue:drawSlice) %>%
  dplyr::mutate(
    isHomeTeam = TRUE,
    matchNo = as.integer(matchNo))

awayTeamData <- gameOddsBuilder %>%
  tibble::rownames_to_column(var = "matchNo") %>%
  dplyr::rename(
    team = awayTeam,
    otherTeam = homeTeam) %>%
  dplyr::select(team, otherTeam, game, matchNo, homeValue:drawSlice) %>%
  dplyr::mutate(
    isHomeTeam = FALSE,
    matchNo = as.integer(matchNo))

teamData <- dplyr::bind_rows(homeTeamData, awayTeamData) %>%
  dplyr::arrange(matchNo, desc(isHomeTeam))

adjustedPoints <- clubsABC$Team %>%
  purrr::map(pointsAdjuster) %>%
  as.data.frame()

colnames(adjustedPoints) <- clubsABC$Team

simClubOrderString <- as.integer(apply(-adjustedPoints, 1, rank, ties.method="average"))
tsimClubOrder <- matrix(simClubOrderString, nrow = 20)

simClubOrder <- as.data.frame(t(tsimClubOrder))
colnames(simClubOrder) <- clubsABC$Team

simPlayerScores <- matrix(rep(0L, nPlayers*iterations), nrow = iterations)
tPredictions <- t(predictions)
  
for (i in 1:nPlayers){
  
  mPredictions <- matrix(
    rep(tPredictions[i,], iterations),
    nrow = iterations,
    byrow = TRUE)
  
  workingMisses <- (simClubOrder - mPredictions)^2
  
  bonusPossibility <- simClubOrder == 1
  
  bonusNumber <- apply(
    X = bonusPossibility & (mPredictions == 1),
    MARGIN = 1,
    FUN = max
  )
  
  bonusScore <- -50 * t(bonusNumber)
  
  simPlayerScores[,i] <- rowSums(workingMisses) + bonusScore
}

colnames(simPlayerScores) <- t(names1)

simPlayerRanks <- data.frame(
  t(
    apply(simPlayerScores, 1, rank, ties.method='min')
  )
)









# results <- get_latest_standings(player_data)
# 
# results <- get_latest_standings(
#   data_input = player_data, 
#   use_saved_data = T, 
#   data_file = '201819_penultimate.rds'
# )
# 
# project_players <- get_projection(
#   url_odds = odds_info,
#   url_predictions = player_info,
#   project_players = TRUE,
#   use_saved_data = T,
#   data_file = "201819_penultimate.rds",
#   runs = iterations)
# 
# winPlayer <- as.data.frame(project_players==1L)
# 
# winLikelihood <- as.data.frame(
#   apply(winPlayer, 2, sum)
# )
# 
# winSummary <- tibble(
#   Name = colnames(project_players), 
#   Frequency = pull(winLikelihood, 1)) %>% 
#   filter(Frequency > 0) %>% 
#   arrange(-Frequency) %>%
#   mutate(Likelihood = Frequency/iterations) %>%
#   select(Name, Likelihood)



# project_team <- get_projection(
#   url_odds = odds_info, 
#   url_predictions = player_info, 
#   project_players = FALSE,
#   use_saved_data = T,
#   data_file = "201819_penultimate.rds",
#   runs = iterations)

# project_team %>% 
#   select(Liverpool, `Man City`) %>% 
#   count(Liverpool, `Man City`) %>% 
#   mutate(chance = n / sum(n)) %>% 
#   arrange(desc(chance)) %>% 
#   knitr::kable()


# bind_cols(winPlayer, project_team) %>% 
#   select(
#     Roger = Roger.Gathercole, 
#     Luke = Luke.Finnis, 
#     `Man City`, Liverpool) %>% 
#   count(Roger, Luke, `Man City`, Liverpool) %>% 
#   mutate(chance = n / sum(n)) %>% 
#   arrange(`Man City`, desc(chance)) %>% 
#   knitr::kable()
