# Load required libraries
library(ggplot2)
library(dplyr)
library(readr)
library(here)
library(forcats)
library(scales)
library(stringr)

# Setup paths for Code Ocean compatibility
is_code_ocean <- dir.exists("/code") && dir.exists("/data") && dir.exists("/results")

if (is_code_ocean) {
  # Running on Code Ocean
  data_dir <- "/data/Figure1"
  results_dir <- "/results/Figure1"
} else {
  # Running locally - auto-detect directory
  if (dir.exists("data/Figure1")) {
    # Running from root directory
    data_dir <- "data/Figure1"
    results_dir <- "results/Figure1"
  } else if (dir.exists("../../data/Figure1")) {
    # Running from code/Figure1/
    data_dir <- file.path("..", "..", "data", "Figure1")
    results_dir <- file.path("..", "..", "results", "Figure1")
  } else {
    # Fallback: use here package
    data_dir <- here("data", "Figure1")
    results_dir <- here("results", "Figure1")
  }
}

# Ensure results directory exists
if (!dir.exists(results_dir)) {
  dir.create(results_dir, recursive = TRUE)
}

# Read and prepare data
file_path <- file.path(data_dir, "attnumber19902021.csv")
df <- read_csv(file_path)

# Filter and prepare the data
plot_data <- df %>%
  dplyr::filter(age_group_id=="22") %>%
  dplyr::filter(sex_id=="3") %>%
  dplyr::filter(year_id=="2021") %>%
  dplyr::filter(infectious_syndrome=="0") %>%
  dplyr::filter(resistance_class=="carbapenem") %>%
  dplyr::filter(counterfactual=="1") %>%
  select(location_id, pathogen, deathcounts_mean) %>%
  mutate(deathcounts_mean = deathcounts_mean / 1e3)

# Improved function to wrap long labels
wrap_labels_two_lines <- function(x, width = 25) {
  sapply(x, function(y) {
    if (nchar(y) > width) {
      wrapped <- str_wrap(y, width = width)
      lines <- strsplit(wrapped, "\n")[[1]]
      if (length(lines) > 2) {
        paste(lines[1], lines[2], sep = "\n")
      } else {
        wrapped
      }
    } else {
      y
    }
  }, USE.NAMES = FALSE)
}

# Sort data and wrap labels
plot_data <- plot_data %>%
  mutate(location_id = fct_relevel(location_id, "Global")) %>%
  mutate(location_id = fct_reorder(location_id, -deathcounts_mean)) %>%
  mutate(location_id = fct_relabel(location_id, wrap_labels_two_lines))

# Define color palette
lancet_colors <- c(
  "#00468B", "#ED0000", "#42B540", "#0099B4", "#925E9F", 
  "#FDAF91", "#AD002A", "#ADB6B6", "#1B1919", "#66A61E", 
  "#E6AB02", "#6A3D9A", "#F15854", "#5DA5DA", "#60BD68"
)

n_syndromes <- length(unique(plot_data$pathogen))
if(n_syndromes > length(lancet_colors)) {
  lancet_colors <- colorRampPalette(lancet_colors)(n_syndromes)
}

# Create the plot with improved label visibility and more y-axis breaks
p3 <- ggplot(plot_data, aes(x = location_id, y = deathcounts_mean, fill = pathogen)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_y_continuous(
    labels = label_number(suffix = " K", scale = 1),
    expand = expansion(mult = c(0, 0.2)),
    breaks = seq(0, 250, by = 50)  
  ) +
  labs(
    x = "Location",
    y = "Number of deaths (thousands)",
    title = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1,
      size = 9,
      face = "bold",
      margin = margin(t = 5)
    ),
    axis.text.y = element_text(
      size = 10,
      face = "bold"
    ),
    axis.title.x = element_text(
      size = 12,
      margin = margin(r = 10),
      face = "bold"
    ),
    axis.title.y = element_text(
      size = 12,
      margin = margin(r = 10),
      face = "bold"
    ),
    # Legend position and settings
    legend.position = c(0.85, 0.8),
    legend.title = element_blank(),
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(
      size = 8,
      face = "bold"
    ),
    legend.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    # Increased bottom margin for complete labels
    plot.margin = margin(t = 10, r = 10, b = 60, l = 10, unit = "pt")
  ) +
  scale_fill_manual(values = lancet_colors) +
  coord_cartesian(clip = "off")

# Display the plot
print(p3)

# Save to results directory
output_file <- file.path(results_dir, "Figure1C.pdf")
ggsave(output_file, p3, width = 10.68, height = 7.1, units = "in",
       device = cairo_pdf)
cat(sprintf("✓ Figure saved to: %s\n", output_file))
