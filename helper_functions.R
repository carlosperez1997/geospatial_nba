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
get_moment_info <- function(quarter, id_play, moment, ball_positions, players_positions) {
  clock <- if (is.null(moment[[4]])) { 0 } else { moment[[4]] }
  metadata <- data.frame(quarter = quarter, play_id = id_play,  moment_id = as.character(moment[[2]]), time=moment[[3]], clock=clock)
  
  ball_positions <- rbind(ball_positions, 
                          cbind( metadata, get_ball(moment[[6]][1,]))
  )
  players_positions <- rbind(players_positions, 
                             cbind(metadata, get_players(moment[[6]]))
  )
  
  return(list(ball_positions, players_positions))
}

load_court <- function(rotate=F) {
  DATA_DIR <- "./data"
  court_lines_in <- st_read(file.path(DATA_DIR, "nba-court-lines-05feb2024.gpkg"),  layer = "nba-court-lines-05feb2024")
  court_lines_in <- court_lines_in[-1, ]
  
  # Make left side of court
  # rotate 90 degrees
  if (rotate) {
    st_geometry(court_lines_in) * matrix(c(0, 1, -1, 0), ncol = 2) -> court_lines_rotated
  } else {
    court_lines_rotated <- st_geometry(court_lines_in)
  }
  
  # Convert back to sf dataframe
  court_left <- court_lines_in
  for(i in 1:11) {
    court_left$geom[i] <- court_lines_rotated[[i]]
    if (rotate) {
      court_left$geom[i] <- st_geometry(court_left$geom[i]) + c(0, 25) # shift up   
    } else {
      court_left$geom[i] <- st_geometry(court_left$geom[i])
    }
    court_left$Feature[i] <- paste0(court_left$Feature[i], " L")
  }
  
  court_lines_rotated<-NULL
  
  # Make right side of court
  # rotate 90 degrees
  if (rotate) {
    st_geometry(court_lines_in) * matrix(c(0, -1, 1, 0), ncol = 2) -> court_lines_right_rotated
  } else {
    court_lines_right_rotated <- st_geometry(court_lines_in)
  }
  
  # Convert back to sf dataframe
  court_right <- court_lines_in
  for(i in 1:11) {
    court_right$geom[i] <- court_lines_right_rotated[[i]] 
    if (rotate) {
      court_left$geom[i] <- st_geometry(court_left$geom[i]) + c(94, 25) # shift up   
    } else {
      court_left$geom[i] <- st_geometry(court_left$geom[i])
    }
    #court_right$geom[i] <- st_geometry(court_right$geom[i]) + c(94, 25) # shift up and right
    court_right$Feature[i] <- paste0(court_right$Feature[i], " R")
  }
  
  court_lines_right_rotated<-NULL
  
  # Full court
  full_court <- bind_rows(court_left, court_right)
  return(full_court)
}