# Packages
rm(list=ls())
library(data.table)
library(jsonlite)
library(dplyr)
library(progress)
library(sf)
library(ggplot2)
library(paletteer)
library(stringr)
library(lubridate)
library(gridExtra)
source("helper_functions.R")

# Directory
DATA_DIR <- "./data"

# LOAD DATA
################################################################################

# Load Courts data
full_court <- st_read(file.path(DATA_DIR, "nba-court-lines-05feb2024.gpkg"),  
                      layer = "nba-court-lines-05feb2024")
half_court <- full_court[-1, ]

# Create the basketball court plot
court_plot <- ggplot() +
  geom_sf(data = half_court, color = "black", fill = "transparent", 
          linewidth= 1)  # Increase line thickness here
court_plot 

# Load shots data
shots_data <- data.table()
for (year in 2004:2005) {
  
  file_name <- sprintf("NBA_%d_Shots.csv", year)
  year_data <- fread(file.path(DATA_DIR, file_name))
  shots_data <- rbindlist(list(shots_data, year_data), use.names = TRUE, fill = TRUE)
  
}

# Make SF point object
shots_data_sf <- st_as_sf(shots_data, coords = c("LOC_X", "LOC_Y"), crs = st_crs(half_court))

## Identify shots in the restricted area
################################################################################

# The restricted area is currently just a semi circle, so we need to connect the
# end points
r_a <- half_court %>%
  filter(Feature=="Restricted area")

# Create back line
back_line <- st_sfc(st_linestring(matrix(c(-4, 4, 4, 4), ncol = 2, byrow = TRUE)), crs = st_crs(r_a))
r_a <- st_union(r_a, back_line) # Combine them

# Create polygon from coordinates
r_a_coords <- st_coordinates(r_a) # Extract coordinates
r_a_coords <- rbind(r_a_coords, r_a_coords[1,]) # Make first and last the same


r_a_polygon <- st_polygon(list(r_a_coords)) # Make polygon
r_a_polygon <- st_zm(r_a_polygon) # Only keep x and y coords
r_a_polygon <- st_make_valid(r_a_polygon)
r_a_polygon_sf <- st_sf(geometry = st_sfc(r_a_polygon, crs = st_crs(r_a))) # make sf object

# Identify shots inside restricted area using st_within
inside_r_a <- st_within(shots_data_sf, r_a_polygon_sf)

# Create_indicator
shots_data_sf$inside_r_a <- as.integer(lengths(inside_r_a) > 0)
shots_data$inside_r_a <- as.integer(lengths(inside_r_a) > 0)


## Identify shots just behind the 3 point line
################################################################################


# HEAT MAP: per players
################################################################################
# Define a function to create the plot for a player
create_player_plot <- function(shots_data, year, player_name) {
  
  data <- shots_data %>%
      filter(SEASON_1 == year) %>%
      filter(SHOT_MADE == TRUE) %>%
      filter(str_detect(PLAYER_NAME, player_name))
  
  title <- paste0(player_name, ' - ', year)
  
  ggplot() +
    geom_sf(data = half_court, color = "black", fill = "transparent", linewidth = 1) +
    geom_point(data = data, aes(x = LOC_X, y = LOC_Y),
               size = 1, alpha = 0.1) +
    geom_density_2d_filled(data, mapping = aes(x = LOC_X, y = LOC_Y, fill = ..level..), 
                           contour_var = "ndensity", breaks = seq(0.03, 1.0, length.out = 80), alpha = .7) +
    scale_x_continuous(limits = c(-27.5, 27.5)) + 
    scale_y_continuous(limits = c(0, 45)) +
    labs(title = title) + 
    theme(legend.position = "none", 
          plot.title = element_text(hjust = 0.5, size = 20), 
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank())
}

# Create plots for each player
player1 <- create_player_plot(shots_data, 2013, "LeBron James")
player2 <- create_player_plot(shots_data, 2016, "Stephen Curry")
player3 <- create_player_plot(shots_data, 2019, "Giannis Antetokounmpo")

# Arrange plots side by side
side_by_side_plots <- grid.arrange(player1, player2, player3, ncol = 3)
print(side_by_side_plots)

# HEAT MAP:  overall
################################################################################
outside_before <- shots_data %>% 
  filter(SEASON_1 == 2004) %>%
  filter(BASIC_ZONE != "Restricted Area")

outside_now <- shots_data %>% 
  filter(SEASON_1 == 2019) %>%
  filter(BASIC_ZONE != "Restricted Area")
  
before <- ggplot() +
  geom_sf(data = half_court, color = "black", fill = "transparent", linewidth = 1) +
  geom_density_2d_filled(outside_before, mapping = aes(x = LOC_X, y = LOC_Y, fill = ..level..), 
                         contour_var = "ndensity", breaks = seq(0.1, 1.0, length.out = 50), alpha = .8) +
  scale_x_continuous(limits = c(-27.5, 27.5)) + 
  scale_y_continuous(limits = c(0, 45)) +
  labs(title = 'Shots attempts 2004') + 
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, size = 20), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

now <- ggplot() +
  geom_sf(data = half_court, color = "black", fill = "transparent", linewidth = 1) +
  geom_density_2d_filled(outside_now, mapping = aes(x = LOC_X, y = LOC_Y, fill = ..level..), 
                         contour_var = "ndensity", breaks = seq(0.1, 1.0, length.out = 50), alpha = .8) +
  scale_x_continuous(limits = c(-27.5, 27.5)) + 
  scale_y_continuous(limits = c(0, 45)) +
  labs(title = 'Shots attempts 2019') + 
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, size = 20), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

side_by_side_plots <- grid.arrange(before, now, ncol = 2)
print(side_by_side_plots)


# ACCEPTANCE RATE:  Curry
################################################################################
shots_curry <- shots_data %>%
  filter(SEASON_1 == 2016) %>%
  filter(str_detect(PLAYER_NAME, "Stephen Curry")) %>% 
  select(PLAYER_NAME, GAME_DATE, EVENT_TYPE, SHOT_MADE, SHOT_TYPE, LOC_X, LOC_Y)

shots_curry 

# Create scatter plot
scatter_plot <- ggplot() +
  geom_sf(data = half_court, color = "black", fill = "transparent", linewidth = 1) +
  geom_point(data = shots_curry, aes(x = LOC_X, y = LOC_Y, color = SHOT_MADE), alpha = 0.3) +  # Add points with transparency
  labs(title = "Shots Scatterplot", x = "LOC_X", y = "LOC_Y") +  # Add title and axis labels
  theme_minimal() +
  scale_x_continuous(limits = c(-27.5, 27.5)) + 
  scale_y_continuous(limits = c(0, 45)) +
  labs(title = 'Stephen Curry 2016 shots') + 
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 20), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

# Print the scatter plot
print(scatter_plot)

shots_data$LOC_X_simple <- as.integer(shots_data$LOC_X/4)*4
shots_data$LOC_Y_simple <- as.integer(shots_data$LOC_Y/4)*4

shot_summary <- shots_data %>%
  group_by(LOC_X_simple, LOC_Y_simple) %>%
  summarize(total_shots = n(), made_shots = sum(SHOT_MADE))

# Calculate the acceptance rate
shot_summary$acceptance_rate <- shot_summary$made_shots / shot_summary$total_shots * 100

# Create the heatmap plot
heatmap_plot <- ggplot() +
  geom_sf(data = half_court, color = "grey", fill = "transparent", linewidth = 1, alpha=0.8) +  # Plot the basketball court
  geom_raster(data = shot_summary, aes(x = LOC_X_simple, y = LOC_Y_simple, fill = acceptance_rate), alpha=0.5) +
  scale_fill_gradient(low = "red", high = "darkblue", name = "Acceptance Rate") +  # Customize fill color scale
  scale_x_continuous(limits = c(-27.5, 27.5)) +  # Set x-axis limits
  scale_y_continuous(limits = c(0, 45)) +  # Set y-axis limits
  labs(title = "Shooting Percentage Heatmap") +  # Add title
  theme_minimal() +
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5, size = 20), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

# Print the heatmap plot
print(heatmap_plot)

side_by_side_plots <- grid.arrange(scatter_plot, heatmap_plot, ncol = 2)
print(side_by_side_plots)
