# BALL
get_ball <- function(data) {
  return(data.frame(x = data[3], y=data[4]))
}

# PLAYERS
get_players <- function(data) {
  return(data.frame(player_id=data[2:nrow(data),2], x = data[2:nrow(data),3], y=data[2:nrow(data),4]))
}

# PLAYERS
get_players_info <- function(json_data) {
  home_players <- json_data$events$home$players[[1]]
  visitor_players <- json_data$events$visitor$players[[1]]
  home_players$team <- "home"
  visitor_players$team <- "visitor"

  return(rbind(home_players, visitor_players))
}

# MOMENT INFO
get_moment_info <- function(id_play, moment, ball_positions, players_positions) {
  clock <- if (is.null(moment[[4]])) { 0 } else { moment[[4]] }
  metadata <- data.frame(play_id = id_play,  moment_id = as.character(moment[[2]]), time=moment[[3]], clock=clock)
  
  ball_positions <- rbind(ball_positions, 
                          cbind( metadata, get_ball(moment[[6]][1,]))
  )
  players_positions <- rbind(players_positions, 
                             cbind(metadata, get_players(moment[[6]]))
  )
  
  return(list(ball_positions, players_positions))
}