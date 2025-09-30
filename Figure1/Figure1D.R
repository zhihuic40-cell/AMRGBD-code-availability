# Load required libraries
library(ggplot2)
library(dplyr)
library(readr)
library(here)
library(forcats)
library(scales)
library(stringr)
library(ggsci)

# Dynamically construct the file path
file_path <- here("data", "attnumberper.csv")

# Read and process the data
df <- read_csv(file_path) %>%
  mutate(percentage = CR/ALL * 100) %>%
  dplyr::filter(year_id %in% c(1990, 2021)) %>%
  # Reorder factor levels to change bar order
  mutate(year_id = factor(year_id, levels = c("2021", "1990"))) %>%
  # Keep original names but add line breaks for long names
  mutate(location_id = case_when(
    location_id == "Southeast Asia, East Asia, and Oceania" ~ "Southeast Asia, East Asia,\nand Oceania",
    location_id == "Central Europe, Eastern Europe, and Central Asia" ~ "Central Europe, Eastern Europe,\nand Central Asia",
    TRUE ~ location_id
  )) %>%
  # Custom ordering: Global at bottom, others alphabetically reversed
  mutate(location_id = factor(location_id, 
                              levels = rev(c("Global",
                                             sort(unique(location_id[location_id != "Global"]))))
  ))

# Define Lancet-style colors
lancet_blue_light <- "#86B0DA"  # Lighter blue for 1990
lancet_blue_dark <- "#2E5A87"   # Darker blue for 2021

# Create the plot
p4 <- ggplot(df, aes(x = percentage, 
                    y = location_id,
                    fill = year_id)) +
  # Add bars
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.8), 
           width = 0.7) +
  # Custom Lancet-style theme
  theme_minimal() +
  theme(
    # Text elements
    text = element_text(family = "Arial", color = "black", face = "bold"),
    axis.text = element_text(size = 9, color = "black", face = "bold"),
    axis.title = element_text(size = 10, color = "black", face = "bold"),
    legend.text = element_text(size = 9, face = "bold"),
    legend.title = element_text(size = 10, face = "bold"),
    
    # Remove all grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Legend
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    legend.margin = margin(t = 0, r = 0, b = 10, l = 0),
    legend.key.size = unit(0.8, "lines"),
    
    # Add axis ticks
    axis.ticks = element_line(color = "black", size = 0.3),
    axis.ticks.length = unit(2, "mm"),
    
    # Axis lines
    axis.line = element_line(color = "black", size = 0.3),
    
    # Plot margins
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  ) +
  # Customize scales
  scale_fill_manual(values = c(lancet_blue_dark, lancet_blue_light),
                    name = "Year") +
  scale_x_continuous(limits = c(0, 45),
                     breaks = seq(0, 45, by = 10),
                     expand = c(0, 0),
                     labels = function(x) x) +
  scale_y_discrete(expand = c(0, 0.7)) +
  # Labels
  labs(x = "Percentage(%)",
       y = "",
       caption = "") +
  # Add percentage labels
  geom_text(aes(label = sprintf("%.1f", percentage)),
            position = position_dodge(width = 0.8),
            hjust = -0.2,
            size = 3,
            family = "Arial",
            fontface = "bold")
ggsave("Figure1D.pdf", p4, width = 10.68, height = 7.1, units = "in",
       device = cairo_pdf)