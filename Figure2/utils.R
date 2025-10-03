# ==============================================================================
# Utility Functions for Figure 2 Generation
# ==============================================================================
# Description: This file contains shared functions used across all Figure 2 
# scripts to avoid code duplication and improve maintainability.
# ==============================================================================

#' Standardize Country Names for Map Data
#' 
#' @description
#' Converts GBD country names to match the naming convention used in 
#' the world map data. This is necessary for proper joining of datasets.
#' 
#' @param data A data frame containing location_name column
#' @return Data frame with standardized location names
#' @export
standardize_country_names <- function(data) {
  # Main country name mappings
  data$location[data$location_name == 'United States of America'] <- 'USA'
  data$location_name[data$location_name == 'Russian Federation'] <- 'Russia'
  data$location_name[data$location_name == 'United Kingdom'] <- 'UK'
  data$location_name[data$location_name == 'Congo'] <- 'Republic of Congo'
  data$location_name[data$location_name == "Iran (Islamic Republic of)"] <- 'Iran'
  data$location_name[data$location_name == "Democratic People's Republic of Korea"] <- 'North Korea'
  data$location_name[data$location_name == "Taiwan (Province of China)"] <- 'Taiwan'
  data$location_name[data$location_name == "Republic of Korea"] <- 'South Korea'
  data$location_name[data$location_name == "United Republic of Tanzania"] <- 'Tanzania'
  data$location_name[data$location_name == "Bolivia (Plurinational State of)"] <- 'Bolivia'
  data$location_name[data$location_name == "Venezuela (Bolivarian Republic of)"] <- 'Venezuela'
  data$location_name[data$location_name == "Czechia"] <- 'Czech Republic'
  data$location_name[data$location_name == "Republic of Moldova"] <- 'Moldova'
  data$location_name[data$location_name == "Viet Nam"] <- 'Vietnam'
  data$location_name[data$location_name == "Lao People's Democratic Republic"] <- 'Laos'
  data$location_name[data$location_name == "Syrian Arab Republic"] <- 'Syria'
  data$location_name[data$location_name == "North Macedonia"] <- 'Macedonia'
  data$location_name[data$location_name == "Micronesia (Federated States of)"] <- 'Micronesia'
  data$location_name[data$location_name == "Macedonia"] <- 'North Macedonia'
  
  # Handle countries/territories with multiple islands
  # Trinidad and Tobago
  data$location_name[data$location_name == "Trinidad and Tobago"] <- 'Trinidad'
  trinidad_data <- data[data$location_name == "Trinidad", ]
  trinidad_data$location_name <- 'Tobago'
  data <- rbind(data, trinidad_data)
  
  # Antigua and Barbuda
  data$location_name[data$location_name == "Antigua and Barbuda"] <- 'Antigu'
  antigua_data <- data[data$location_name == "Antigu", ]
  antigua_data$location_name <- 'Barbuda'
  data <- rbind(data, antigua_data)
  
  # Saint Kitts and Nevis
  data$location_name[data$location_name == "Saint Kitts and Nevis"] <- 'Saint Kitts'
  kitts_data <- data[data$location_name == "Saint Kitts", ]
  kitts_data$location_name <- 'Nevis'
  data <- rbind(data, kitts_data)
  
  # Saint Vincent and the Grenadines
  data$location_name[data$location_name == "Saint Vincent and the Grenadines"] <- 'Saint Vincent'
  vincent_data <- data[data$location_name == "Saint Vincent", ]
  vincent_data$location_name <- 'Grenadines'
  data <- rbind(data, vincent_data)
  
  # Other name standardizations
  data$location_name[data$location_name == "Cabo Verde"] <- 'Cape Verde'
  data$location_name[data$location_name == "United States Virgin Islands"] <- 'Virgin Islands'
  data$location_name[data$location_name == "Côte d'Ivoire"] <- 'Ivory Coast'
  data$location_name[data$location_name == "Eswatini"] <- 'Swaziland'
  data$location_name[data$location_name == "Brunei Darussalam"] <- 'Brunei'
  
  return(data)
}


#' Create Base Map Theme
#' 
#' @description
#' Returns a standardized ggplot2 theme for small regional maps
#' 
#' @return A ggplot2 theme object
#' @export
create_map_theme <- function() {
  theme(
    legend.position = 'none',
    legend.title = element_blank(),
    plot.title = element_text(color = "black", size = 10),
    legend.text = element_text(color = "black", size = 12),
    panel.grid = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, size = 0.5),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )
}


#' Filter GBD Data for Death Rates
#' 
#' @description
#' Filters GBD data for age-standardized death rates in a specific year
#' 
#' @param data GBD data frame
#' @param year Year to filter for (default: 2021)
#' @return Filtered data frame
#' @export
filter_death_rate_data <- function(data, year = 2021) {
  data %>%
    dplyr::filter(Measure == "Deaths") %>%
    dplyr::filter(Metric == "Rate (per 100k)") %>%
    dplyr::filter(Age == "Age-standardized") %>%
    dplyr::filter(Year == year)
}

