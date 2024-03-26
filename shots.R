library(data.table)
library(dplyr)
library(progress)
library(sf)
library(ggplot2)
source("helper_functions.R")

DATA_DIR <- "./data"
file <- "NBA_2016_Shots.csv"

# Read file
data <- read.csv(file.path(DATA_DIR, file)) %>% as.data.table()
data

# Player shots
stephen_curry_sf <- data %>% 
  .[PLAYER_NAME == "Stephen Curry",] %>% select(LOC_X, LOC_Y, SHOT_DISTANCE, SHOT_MADE, SHOT_TYPE) %>% 
  st_as_sf(coords = c("LOC_X", "LOC_Y"), crs = 4326)
stephen_curry_sf

# Create the basketball court plot
court_plot <- ggplot() +
  geom_sf(data = load_court(), color = "gray", fill = "transparent") +
  theme_void() 

# Combine the scatter plot and court plot
combined_plot <- court_plot +
  geom_sf(data = stephen_curry_sf, aes(color = SHOT_MADE), alpha=0.3, size = 1) + 
  labs(title = "Stephen Curry's Shots") + 
  coord_sf(xlim = c(-25, 25), ylim = c(0, 47)) + 
  theme_minimal() + # minimal theme for better visualization
  theme(panel.grid = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), # Remove elements
        plot.title = element_text(hjust = 0.5, size = 20), # title
        legend.position = "bottom", legend.direction = "horizontal", legend.box = "horizontal") # legend

# SHOTS PLOT
print(combined_plot)

# HEATMAP

cellsize <- c(3, 3)

# Create a bounding box representing the court
court_bbox <- st_bbox(load_court()) %>% 
  st_make_grid(cellsize = cellsize) %>%
  st_as_sf() %>%
  st_set_crs(st_crs(load_court()))

# Perform a spatial join to count shot attempts in each region
intersection <- st_intersection(court_bbox, stephen_curry_sf)

# Join the intersected points with the polygons to get the polygon ID
joined <- st_join(court_bbox, intersection)

counts <- joined %>%
  group_by(geometry) %>%
  summarize(total_attempts = n(), made_attempts = sum(SHOT_MADE)) %>%
  mutate(percentage_made = (made_attempts / total_attempts) * 100)

court_plot + 
  geom_sf(data = counts, aes(fill = percentage_made), alpha=0.3, color = NA) +
  scale_fill_gradient(low = "red", high = "green", name = "% Made") +
  coord_sf(xlim = c(-25, 25), ylim = c(0, 50)) + 
  labs(title = "Shot Acceptance by Region") +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 20), 
        legend.position = "bottom", legend.direction = "horizontal", legend.box = "horizontal")

# SCORES

unique(data$SHOT_TYPE)

scores <- data[SHOT_MADE == T] %>% 
  mutate(points = if_else(SHOT_TYPE == "2PT Field Goal", 2, 
                          if_else(SHOT_TYPE == "3PT Field Goal", 3, 0)))
scores





