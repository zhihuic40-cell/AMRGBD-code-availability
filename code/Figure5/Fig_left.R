# Load required packages
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)
library(RColorBrewer)

# Setup paths for Code Ocean compatibility
is_code_ocean <- dir.exists("/code") && dir.exists("/data") && dir.exists("/results")

if (is_code_ocean) {
  data_dir <- "/data/Figure5"
  results_dir <- "/results/Figure5"
} else {
  if (dir.exists("data/Figure5")) {
    data_dir <- "data/Figure5"
    results_dir <- "results/Figure5"
  } else if (dir.exists("../../data/Figure5")) {
    data_dir <- file.path("..", "..", "data", "Figure5")
    results_dir <- file.path("..", "..", "results", "Figure5")
  } else {
    data_dir <- here("data", "Figure5")
    results_dir <- here("results", "Figure5")
  }
}
if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)

# Read Excel file
file_path <- file.path(data_dir, "ASSproportion.xlsx")
data <- read_excel(file_path)

# Define new age groups
new_age_group <- function(age) {
  case_when(
    age %in% c('Neonatal') ~ 'Neonatal',
    age %in% c('Post neonatal to 5') ~ 'Post Neonatal to 5 years',
    age %in% c('5 to 49') ~ '5-49 years',
    age %in% c('50 to 69') ~ '50-69 years',
    age %in% c('70+ years') ~ '≥70 years' 
  )
}
unique(data$age_name)
age_order <- c('≥70 years',
               '50-69 years',
               '5-49 years',
               'Post Neonatal to 5 years',
               'Neonatal')

path<- c("Acinetobacter baumannii", "Citrobacter spp.", "Enterobacter spp.", 
              "Escherichia coli", "Klebsiella pneumoniae", "Pseudomonas aeruginosa", 
              "Serratia spp.")

data_filtered <- data %>%
  dplyr::filter(age_name != 'All ages',
                Pathogen %in% path) %>%
  mutate(new_age_group = new_age_group(age_name)) %>%
  dplyr::filter(new_age_group != 'Other')

data_filtered <- data_filtered %>%
  group_by(Pathogen, year, new_age_group) %>%
  summarise(val = sum(val), .groups = 'drop')

total_data <- data_filtered %>%
  group_by(Pathogen, year) %>%
  summarise(total_val = sum(val), .groups = 'drop')

merged_data <- data_filtered %>%
  left_join(total_data, by = c("Pathogen", "year")) %>%
  mutate(proportion = val / total_val * 100,
         new_age_group = factor(new_age_group, levels = age_order, ordered = TRUE))

lancet_colors <- brewer.pal(6, "Blues")
years_to_show <- c(1990, 2000, 2010, 2020)

# Common theme for all pathogen plots
common_bar_theme <- theme_minimal() +
  theme(
    legend.position = "right",
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
    axis.text.x = element_text(margin = margin(t = 5), face = "bold", size = 12),
    axis.text.y = element_text(margin = margin(r = 5), face = "bold", size = 12),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(3, "pt"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(face = "bold", size = 12)
  )

create_bar_plot <- function(data, pathogen_name) {
  data %>%
    dplyr::filter(Pathogen == pathogen_name) %>%
    ggplot(aes(x = factor(year), y = proportion, fill = new_age_group)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = lancet_colors) +
    scale_x_discrete(breaks = years_to_show) +
    labs(x = "Year", y = "Proportion of deaths (%)",
         fill = "Age groups", title = pathogen_name) +
    common_bar_theme +
    coord_cartesian(clip = "off") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
}

PF1aba <- create_bar_plot(merged_data, "Acinetobacter baumannii")
PF1cit <- create_bar_plot(merged_data, "Citrobacter spp.")
PF1ent <- create_bar_plot(merged_data, "Enterobacter spp.")
PF1eco <- create_bar_plot(merged_data, "Escherichia coli")
PF1kpn <- create_bar_plot(merged_data, "Klebsiella pneumoniae")
PF1pae <- create_bar_plot(merged_data, "Pseudomonas aeruginosa")
PF1ser <- create_bar_plot(merged_data, "Serratia spp.")

PF1aba; PF1cit; PF1ent; PF1eco; PF1kpn; PF1pae; PF1ser

ggsave(file.path(results_dir, "Figure5_PF1aba.pdf"), PF1aba, width = 10.22, height = 6.81)
ggsave(file.path(results_dir, "Figure5_PF1cit.pdf"), PF1cit, width = 10.22, height = 6.81)
ggsave(file.path(results_dir, "Figure5_PF1ent.pdf"), PF1ent, width = 10.22, height = 6.81)
ggsave(file.path(results_dir, "Figure5_PF1eco.pdf"), PF1eco, width = 10.22, height = 6.81)
ggsave(file.path(results_dir, "Figure5_PF1kpn.pdf"), PF1kpn, width = 10.22, height = 6.81)
ggsave(file.path(results_dir, "Figure5_PF1pae.pdf"), PF1pae, width = 10.22, height = 6.81)
ggsave(file.path(results_dir, "Figure5_PF1ser.pdf"), PF1ser, width = 10.22, height = 6.81)
cat("✓ All Figure5 left panel plots saved to results directory\n")
