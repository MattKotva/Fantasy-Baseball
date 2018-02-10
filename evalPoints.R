library(dplyr)

all2016 <- read.csv("C:/Users/mattk/OneDrive/Documents/draftkings/data/unzipped/all2016.csv")

roster <- read.csv("C:/Users/mattk/OneDrive/Documents/draftkings/data/unzipped/roster2016.csv" )
roster <- cbind(roster, year = 2016)
colnames(roster) <- c("Player_ID", "Last_Name", "First_Name", "Bats", "Pitches", "Team", "Position", "Year")

names(all2016) <- c("GAME_ID", "AWAY_TEAM_ID", "INN_CT", "BAT_HOME_ID", "OUTS_CT", "BALLS_CT", "STRIKES_CT", 
                    "PITCH_SEQ_TX", "AWAY_SCORE_CT", "HOME_SCORE_CT", "BAT_ID", "BAT_HAND_CD",
                    "RESP_BAT_ID", "RESP_BAT_HAND_CD", "PIT_ID","PIT_HAND_CD",
                    "RESP_PIT_ID", "RESP_PIT_HAND_CD", "CATCHER", "First_Baseman", "Second_Baseman",
                    "Third_Baseman", "Shortstop", "Left_Fielder", "Center_Fielder", "Right_Fielder", "Runner_On_First",
                    "Runner_On_Second", "Runner_On_Third", "Event_Text", "Leadoff_Flag", "Pitch_Hit_Flag",
                    "Defensive_Position", "Lineup_Position", "Event_Type", "Batter_Event_Flag", "Official_Time_At_Bat_Flag",
                    "Hit_Value", "Sacrifice_Hit", "Sacrifice_Fly", "Outs_on_Play", "Double_Play", "Triple_Play",
                    "RBI_On_Play", "Wild_Pitch", "Passed_Ball", "Fielded_By", "Batted_Ball_Type", "Bunt", "Foul",
                    "Hit_Location", "Number_Of_Errors", "First_Error_Player", "First_Error_Type", "Second_Error_Player",
                    "Second_Error_Type", "Third_Error_Player", "Third_Error_Type", "Batter_Destination", 
                    "Runner_On_First_Destination","Runner_On_Second_Destination", "Runner_On_Third_Destination",
                    "Play_On_Batter", "Play_On_Runner_On_First", "Play_On_Runner_Second", "Play_On_Runner_Third", "Stolen_Base_Runner_On_First",
                    "Stolen_Base_Runner_On_Second", "Stolen_Base_Runner_On_Third", "Caught_Stealing_Runner_On_First", 
                    "Caught_Stealing_Runner_On_Second", "Caught_Stealing_Runner_On_Third", "Pickoff_Runner_On_First",
                    "Pickoff_Runner_On_Second", "Pickoff_Runner_On_Third", "Pitcher_Charged_With_Runner_On_First", 
                    "Pitcher_Charged_With_Runner_On_Second", "Pitcher_Charged_With_Runner_On_Third", "New_Game_Flag",
                    "End_Game_Flag", "Pinch_Runner_On_First", "Pinch_Runner_On_Second", "Pinch_Runner_On_Third",
                    "Runner_Removed_For_Pinch_Runner_On_First", "Runner_Removed_For_Pinch_Runner_On_Second", "Runner_Removed_For_Pinch_Runner_On_Third",
                    "Batter_Removed_For_Pinch_Hitter", "Position_Of_Batter_Removed_For_Pinch_Hitter", 
                    "Fielder_With_First_Putout", "Fielder_With_Second_Putout", "Fielder_With_Third_Putout",
                    "Fielder_With_First_Assist", "Fielder_With_Second_Assist", "Fielder_With_Third_Assist",
                    "Fielder_With_Fourth_Assist", "Fielder_With_Fifth_Assist", "Event_Number")


# Evaluate hitters
getPoints <- function(data_row){

  # Relevant players
  hitter <- data_row['BAT_ID']
  first <- data_row['Runner_On_First']
  second <- data_row['Runner_On_Second']
  third <- data_row['Runner_On_Third']
  
  # Score hitter
  hit <- data_row['Hit_Value']
  rbi <- data_row['RBI_On_Play']
  walk <- data_row['Event_Type'] == 14
  hbp <- data_row['Event_Type'] == 16
  
  hitter_points <- scoreHit(hit, rbi,walk, hbp)
  
  # Score players on base
  first_points <- 0
  second_points <- 0 
  third_points <- 0
  
  if(data_row['Runner_On_First_Destination'] == 4) {
    first_points <- first_points + 2
  }
  
  if(data_row['Runner_On_Second_Destination'] == 4) {
    second_points <- second_points + 2
  }
  
  if(data_row['Runner_On_Third_Destination'] == 4) {
    third_points <- third_points + 2
  }
  
  if(as.logical(trimws(data_row['Stolen_Base_Runner_On_First'])) == TRUE) {
    first_points <- first_points + 5
  }
  
  if(as.logical(trimws(data_row['Stolen_Base_Runner_On_Second'])) == TRUE) {
    second_points <- second_points + 5
  }

  if(as.logical(trimws(data_row['Stolen_Base_Runner_On_Third'])) == TRUE) {
    third_points <- third_points + 5
  }

  all_points <- list(hitter_points, first_points, second_points, third_points)
  
  return(all_points)
}


scoreHit <- function(hit, rbi, walk, hbp){
  points <- 0
  
  if (hit == 0) {
    points <- points + 0
  } else if (hit == 1) {
    points <- points + 3
  } else if (hit == 2) {
    points <- points + 5
  } else if (hit == 3) {
    points <- points + 8
  } else if (hit == 4) {
    points <- points + 12
  }
  
    rbi_points <- 2*as.integer(rbi)
    points <- points + rbi_points
  
  if (walk || hbp) {
    points <- points + 2
  }
  
  return(points)
}

score <- apply(X = all2016,MARGIN = 1, FUN = getPoints)

pointsdf <- data.frame(do.call(rbind, score))
names(pointsdf) <- c("Batter_Points", "Runner_On_First_Points", 
                    "Runner_On_Second_Points", "Runner_On_Third_Points")

pointsdf$Batter_Points <- unlist(pointsdf$Batter_Points)
pointsdf$Runner_On_First_Points <- unlist(pointsdf$Runner_On_First_Points)
pointsdf$Runner_On_Second_Points <- unlist(pointsdf$Runner_On_Second_Points)
pointsdf$Runner_On_Third_Points <- unlist(pointsdf$Runner_On_Third_Points)

all2016_scored <- cbind(all2016, pointsdf)

# Group Scoring by Players
players <- all2016_scored %>% 
  select(BAT_ID, GAME_ID, Batter_Points) %>% 
  group_by(BAT_ID) %>% 
  summarise(total_points_hitting= sum(Batter_Points), total_games= length(unique(GAME_ID)))

runner_scoring <- lapply(players$BAT_ID, function(x) {
  runnerFirst <- which(as.character(all2016_scored$Runner_On_First) == as.character(x))
  runnerFirstPoints <- sum(all2016_scored[runnerFirst,]$Runner_On_First_Points)
  runnerSecond <- which(as.character(all2016_scored$Runner_On_Second) == as.character(x))
  runnerSecondPoints <- sum(all2016_scored[runnerSecond,]$Runner_On_Second_Points)
  runnerThird <- which(as.character(all2016_scored$Runner_On_Third) == as.character(x))
  runnerThirdPoints <- sum(all2016_scored[runnerThird,]$Runner_On_Third_Points)
  
  runnerPointsTotal <- runnerFirstPoints + runnerSecondPoints + runnerThirdPoints
  return (runnerPointsTotal)
})

runner_scoring <- do.call(rbind, runner_scoring)

players <- cbind(players, runner_scoring)
totalPoints <- players$total_points_hitting + players$runner_scoring
players <- cbind(players, totalPoints)

p <- players[with(players, order(-totalPoints)),]

player_score <- inner_join(p, roster, by = c("BAT_ID" = "Player_ID"))


average_points <- player_score$totalPoints/player_score$total_games
player_score_average <- cbind(player_score, average_points)

p <- filter(player_score_average, total_games > 30)

p <- p[with(p, order(-average_points)),]























