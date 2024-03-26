library(data.table)
library(dplyr)
library(progress)
library(sf)
library(ggplot2)
source("helper_functions.R")

DATA_DIR <- "./data"
year <- 2016
file <- paste0("NBA_", year,"_Shots.csv")

# Read file
data <- read.csv(file.path(DATA_DIR, file)) %>% as.data.table()

data$zone <- paste0(data$BASIC_ZONE, ' - ', data$ZONE_NAME, ' - ', data$ZONE_RANGE)

shots_made_sf <- data[SHOT_MADE == T] %>% select(LOC_X, LOC_Y, SHOT_DISTANCE, SHOT_MADE, SHOT_TYPE, zone) %>% 
  st_as_sf(coords = c("LOC_X", "LOC_Y"), crs = 4326)

shots_multipolygon <- shots_made_sf %>%
  group_by(zone) %>%
  summarize(total_shots = n()) %>%
  st_convex_hull() # Convert to multipolygon

# Perform spatial intersection between points and court polygon
court_bbox <- st_bbox(court_polygon)
points_in_court <- st_intersection(court_bbox, shots_multipolygon)

# Define custom colors for the gradient
custom_colors <- c("green", "yellow", "red")

# Define breaks for the gradient
breaks <- c(0, 1000, 2000, 3000, 4000)

points_in_court_ <- points_in_court %>% 
  filter(total_shots > 100) %>% 
  filter(zone != "Restricted Area - Center - Less Than 8 ft.") %>%
  filter(zone != "In The Paint (Non-RA) - Center - Less Than 8 ft.")

# Plot the map
combined_plot <- court_plot +
  geom_sf(data = points_in_court_, aes(fill = total_shots), size = 2, shape = 21, alpha=0.8) +
  scale_fill_gradientn(colors = custom_colors, breaks = breaks, name = "Total Shots", labels = scales::comma) +
  labs(title = paste0("Total Shots Made in Each Zone - ", year)) +
  theme_minimal()

combined_plot

# ALL SHOTS HEATMAP
cellsize <- c(3, 3)

# Create a bounding box representing the court
court_bbox <- st_bbox(load_court()) %>% 
  st_make_grid(cellsize = cellsize) %>%
  st_as_sf() %>%
  st_set_crs(st_crs(load_court()))

# Perform a spatial join to count shot attempts in each region
intersection <- st_intersection(court_bbox, shots_made_sf)

# Join the intersected points with the polygons to get the polygon ID
joined <- st_join(court_bbox, intersection)

counts <- joined %>%
  group_by(geometry) %>%
  summarize(total = n())

court_plot + 
  geom_sf(data = counts, aes(fill = total), alpha=0.3, color = NA) +
  scale_fill_gradient(low = "red", high = "green", name = "% Made") +
  coord_sf(xlim = c(-25, 25), ylim = c(0, 50)) + 
  labs(title = "Shot Acceptance by Region") +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 20), 
        legend.position = "bottom", legend.direction = "horizontal", legend.box = "horizontal")