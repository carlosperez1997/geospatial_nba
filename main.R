library(jsonlite)

DATA_DIR <- "./data"
file <- "0021500622.json"

# Read file
json_data <- fromJSON(file.path(DATA_DIR, file))
# Keys of json file
#names(json_data)

home_players <- json_data$events$home$players[[1]]
visitor_players <- json_data$events$visitor$players[[1]]

home_players
visitor_players

moments <- json_data$events$moments
moments


