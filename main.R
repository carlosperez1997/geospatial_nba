library(jsonlite)
library(data.table)
library(dplyr)
library(progress)
source("helper_functions.R")

DATA_DIR <- "./data"
file <- "0021500622.json"

# Read file
json_data <- fromJSON(file.path(DATA_DIR, file))

# PLAYERS
players <- get_players_info(json_data)

# PLAYS
plays <- json_data$events$moments

# BALL + PLAYERS
  ball_positions <- data.frame(play_id=NULL, moment_id=NULL, time=NULL, clock=NULL, x=NULL, y=NULL)
  players_positions <- data.frame(play_id=NULL, moment_id=NULL, time=NULL, clock=NULL, player_id=NULL, x=NULL, y=NULL)

  # progress_bar
  total_iterations <- length(plays)
  
  # Initialize progress bar
  pb <- progress_bar$new(
    format = "[:bar] :percent ETA: :eta",
    total = total_iterations
  )
  for (id_play in seq(length(plays))) {
    moments <- plays[[id_play]]
    #print(id_play)
    if (length(moments) > 0) {
      for (id_moment in seq(length(moments))) {
        
        moment <- plays[[id_play]][[id_moment]]
        
        res <- get_moment_info(id_play, moment, ball_positions, players_positions)
        ball_positions <- res[[1]]
        players_positions <- res[[2]]
      }
    }
    pb$tick()
  }

  # Close progress bar
  pb$close()

# POINTS - Get last possesion
  ball_last_possession <- data.table(ball_positions)[, .SD[.N], by = play_id]
  
  players_last_possesion <- merge(ball_last_possession, 
                                  data.table(players_positions)[moment_id %in% last_possesion$moment_id], 
                                  by=c("play_id", "moment_id"), 
                                  all.x=T, allow.cartesian = T, suffixes = c("_ball", ""))

  # Calculate distance between each player and the ball
  players_last_possesion[, distance_ball := sqrt((x - x_ball)^2 + (y - y_ball)^2)]

  # Find the player closest to the ball (the shooter)
  shooters <- merge(
    players_last_possesion[, .SD[which.min(distance_ball)], by = play_id],
    data.table(players), by.x='player_id', by.y='playerid', all.x=T, all.y=F
  ) %>% select(play_id, time, clock, x, y, distance_ball, lastname, firstname, jersey, team) %>% arrange(time)
  
  # Avoid free throws (and short plays)
  shooters[, diff_last_time := time - shift(time)]
  shooters <- shooters[diff_last_time > 5.0]
  shooters

# LEBRON JAMES
  player_id_ <- players[players$lastname=="James", ]$playerid
  player_positions <- players_positions[players_positions$player_id == player_id_,]
  player_positions
  
  # Total distance covered by the player
  sum( c(0, sqrt(diff(player_positions$x)^2 + diff(player_positions$y)^2)) / 1000)

# Number of 

