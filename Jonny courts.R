# Packages
rm(list=ls())
library(data.table)
library(dplyr)
library(progress)
library(sf)
library(ggplot2)
library(paletteer)
library(stringr)

# Directory
DATA_DIR <- "./data"

# LOAD DATA
################################################################################

# Load Courts data
full_court <- st_read(file.path(DATA_DIR, "nba-court-lines-05feb2024.gpkg"),  layer = "nba-court-lines-05feb2024")
half_court <- full_court[-1, ]

# Create the basketball court plot
court_plot <- ggplot() +
  geom_sf(data = half_court, color = "black", fill = "transparent", linewidth= 1)  # Increase line thickness here
court_plot 


# Load shots data
shots_data <- data.table()
for (year in 2004:2023) {
  
  file_name <- sprintf("NBA_%d_Shots.csv", year)
  year_data <- fread(file.path(DATA_DIR, file_name))
  shots_data <- rbindlist(list(shots_data, year_data), use.names = TRUE, fill = TRUE)
  
}

# Make SF point object
shots_data_sf <- st_as_sf(shots_data, coords = c("LOC_X", "LOC_Y"), crs = 4326)



# SHOT SUCESS RATE BY DISTANCE
################################################################################
# Extract the hoop location
hoop_location <- half_court %>% 
  filter(Feature == "Hoop") %>%
  st_geometry()

# Calculate distance to hoop
shot_distances <- st_distance(shots_data_sf, hoop_location)
shots_data_sf$shot_distances <- as.numeric(shot_distances/100000)

# Plot shooting % versus shot distance (feet)
ggplot(data = shots_data_sf, aes(x = shot_distances, y = as.numeric(SHOT_MADE))) +
  geom_smooth(color = "red") +
  scale_y_continuous(
    breaks = seq(from = 0, to = 1, by = 0.1),
    labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c("red")) +  
  theme_classic() +
  labs(
    x = "Shot Distance (feet)",
    y = "Shooting Percentage "
  )

# HEAT MAP
################################################################################
shots_2010_curry <- shots_data %>%
  filter(SEASON_1 == 2009) %>%
  filter(str_detect(PLAYER_NAME, "Stephen Curry"))

steph_curry_2010 <- ggplot() +
  geom_sf(data = half_court, color = "black", fill = "transparent", linewidth= 1) +
  geom_density_2d_filled(shots_2010_curry, mapping = aes(x = LOC_X, y = LOC_Y,fill = ..level..,),  adjust = 1,
                         contour_var = "ndensity", breaks = seq(0.1, 1.0, length.out = 10), alpha = .8)  +
  scale_x_continuous(limits = c(-27.5, 27.5)) + 
  scale_y_continuous(limits = c(0, 45)) +
  theme(legend.position = "none", 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
steph_curry_2010




