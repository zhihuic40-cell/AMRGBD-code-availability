# ==============================================================================
# Figure 2D: Global Distribution of ATT Net Drift
# ==============================================================================
# Description: This script generates a world map showing the net drift in 
# age-standardized mortality rate for antimicrobial-treated staphylococcal 
# infections (ATT).
#
# Input files:
#   - data/att-ndrift.csv: GBD data for ATT net drift
#   - data/Location.csv: Location metadata
#   - data/世界国家.shp: World countries shapefile
#
# Output: Combined world map with regional insets
# ==============================================================================

# Load required packages
library(sf)
library(patchwork)
library(ggplot2)
library(tidyverse)
library(here)
library(export)

# Setup paths for Code Ocean compatibility
is_code_ocean <- dir.exists("/code") && dir.exists("/data") && dir.exists("/results")

if (is_code_ocean) {
  # Running on Code Ocean
  data_dir <- "/data/Figure2"
  code_dir <- "/code/Figure2"
  results_dir <- "/results/Figure2"
} else {
  # Running locally - auto-detect directory
  if (dir.exists("data/Figure2")) {
    # Running from root directory
    data_dir <- "data/Figure2"
    code_dir <- "code/Figure2"
    results_dir <- "results/Figure2"
  } else if (dir.exists("../../data/Figure2")) {
    # Running from code/Figure2/
    data_dir <- file.path("..", "..", "data", "Figure2")
    code_dir <- "."
    results_dir <- file.path("..", "..", "results", "Figure2")
  } else {
    # Fallback: use here package
    data_dir <- here("data", "Figure2")
    code_dir <- here("code", "Figure2")
    results_dir <- here("results", "Figure2")
  }
}
if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)

# Source utility functions
source(file.path(code_dir, "utils.R"))

# ==============================================================================
# 1. DATA LOADING AND PREPARATION
# ==============================================================================

# Load GBD data and location information
GBD <- read.csv(file.path(data_dir, "att-ndrift.csv"), header = TRUE)
location <- read.csv(file.path(data_dir, "Location.csv"))
GBD <- left_join(GBD, location, by = "location_name")

# Filter for death rate data
GBDASR <- filter_death_rate_data(GBD, year = 2021)

# Load world shapefile
map <- st_read(file.path(data_dir, "world_countries.shp"))
map <- st_set_crs(map, 4326)

# ==============================================================================
# 2. MAIN MAP DATA PREPARATION
# ==============================================================================

# Join map data with GBD data
main_map_data <- left_join(map, GBD, by = c("NAME" = "location3")) %>% 
  filter_death_rate_data(year = 2021)

# Calculate quantile breaks for color scale
breaks <- quantile(main_map_data$val, probs = seq(0, 1, length.out = 6), na.rm = TRUE)
labels <- sprintf("%.2f to %.2f", breaks[-length(breaks)], breaks[-1])

# Convert values to categorical variable
main_map_data$val_cat <- cut(main_map_data$val, 
                             breaks = breaks, 
                             include.lowest = TRUE, 
                             labels = labels)

# Define color scheme
new_colors <- c("#2f4f4f", "#4682b4", "#b0c4de", "#ff6347", "#8b0000")

# ==============================================================================
# 3. CREATE MAIN WORLD MAP
# ==============================================================================

p <- main_map_data %>%
  ggplot() +
  geom_sf(aes(group = NAME, fill = val_cat), color = 'black', size = 0.5) +
  theme_void() +
  scale_fill_manual(values = new_colors, 
                    name = "ASR",
                    labels = labels) +
  labs(x = "", y = "", title = "") +
  theme(legend.position = c(0.1, 0.2),
        legend.title = element_text(color = "black", size = 10),
        plot.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 10),
        panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

print(p)

# ==============================================================================
# 4. PREPARE DATA FOR REGIONAL INSETS
# ==============================================================================

# Get world map data for polygon plotting
worldData <- map_data('world')
small_map_data <- filter_death_rate_data(GBD, year = 2021)

# Standardize country names for joining
small_map_data <- standardize_country_names(small_map_data)

# Join with world map data
small_map_data <- full_join(worldData, small_map_data, by = c('region' = 'location_name')) %>%   
  dplyr::filter(val != "NA")

# Categorize values using same breaks as main map
small_map_data$val_cat <- cut(small_map_data$val, 
                              breaks = breaks, 
                              include.lowest = TRUE, 
                              labels = labels)

# ==============================================================================
# 5. CREATE BASE FIGURE FOR REGIONAL INSETS
# ==============================================================================

fig <- small_map_data %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = val_cat),
               colour = "black", size = 0.5) +
  theme_bw() +
  scale_fill_manual(values = new_colors, name = "ASR") + 
  create_map_theme()

# ==============================================================================
# 6. CREATE REGIONAL INSETS
# ==============================================================================

# Caribbean and Central America
p2 <- fig + 
  labs(x = " ", y = "", title = "Caribbean and Central America") +
  coord_cartesian(xlim = c(-92, -60), ylim = c(5, 27))

# Persian Gulf
p3 <- fig + 
  labs(x = " ", y = "", title = "Persian Gulf") +
  coord_cartesian(xlim = c(45, 55), ylim = c(19, 31))

# Balkan Peninsula
p4 <- fig + 
  labs(x = " ", y = "", title = "Balkan Peninsula") +
  coord_cartesian(xlim =  c(12, 32), ylim = c(35, 53))

# Southeast Asia
p5 <- fig + 
  labs(x = " ", y = "", title = "Southeast Asia") +
  coord_cartesian(xlim =  c(98, 123), ylim = c(-10, 8))

# West Africa
p6 <- fig + 
  labs(x = " ", y = "", title = "West Africa") +
  coord_cartesian(xlim =  c(-17, -7), ylim = c(7, 20))

# Eastern Mediterranean
p7 <- fig + 
  labs(x = " ", y = "", title = "Eastern \nMediterranean") +
  coord_cartesian(xlim =  c(32, 37), ylim = c(29, 35))

# Northern Europe
p8 <- fig + 
  labs(x = " ", y = "", title = "Northern Europe") +
  coord_cartesian(xlim =  c(5, 25), ylim = c(48, 60))

# ==============================================================================
# 7. COMBINE ALL PLOTS
# ==============================================================================

# Arrange regional insets
A = (p6 | p7) / p8 

# Create final combined plot
final_plot <- p +
  (p2 + p3 + p4 + p5 + A + plot_layout(ncol = 5, widths = c(1.5, 1, 1.1, 1.2, 1))) + 
  plot_layout(ncol = 1, heights = c(9, 3))  

print(final_plot)

# Save to results directory
output_file <- file.path(results_dir, "Figure2D.pdf")
ggsave(output_file, final_plot, width = 16, height = 10, units = "in")
cat(sprintf("✓ Figure saved to: %s\n", output_file))
