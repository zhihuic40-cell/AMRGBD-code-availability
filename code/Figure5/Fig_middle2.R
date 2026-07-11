# Load required packages
library(ggplot2)
library(dplyr)

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

# Load RDS data files for each pathogen (Associated)
apc_web_data_ass_aba_output <- readRDS(file.path(data_dir, "apc_web_data_ass_aba_output.rds"))
apc_web_data_ass_cit_output <- readRDS(file.path(data_dir, "apc_web_data_ass_cit_output.rds"))
apc_web_data_ass_ent_output <- readRDS(file.path(data_dir, "apc_web_data_ass_ent_output.rds"))
apc_web_data_ass_eco_output <- readRDS(file.path(data_dir, "apc_web_data_ass_eco_output.rds"))
apc_web_data_ass_kpn_output <- readRDS(file.path(data_dir, "apc_web_data_ass_kpn_output.rds"))
apc_web_data_ass_pae_output <- readRDS(file.path(data_dir, "apc_web_data_ass_pae_output.rds"))
apc_web_data_ass_ser_output <- readRDS(file.path(data_dir, "apc_web_data_ass_ser_output.rds"))

# Extract LongAge data for each pathogen
long_age_aba_ass <- apc_web_data_ass_aba_output[["LongAge"]] %>% as.data.frame()
long_age_cit_ass <- apc_web_data_ass_cit_output[["LongAge"]] %>% as.data.frame()
long_age_ent_ass <- apc_web_data_ass_ent_output[["LongAge"]] %>% as.data.frame()
long_age_eco_ass <- apc_web_data_ass_eco_output[["LongAge"]] %>% as.data.frame()
long_age_kpn_ass <- apc_web_data_ass_kpn_output[["LongAge"]] %>% as.data.frame()
long_age_pae_ass <- apc_web_data_ass_pae_output[["LongAge"]] %>% as.data.frame()
long_age_ser_ass <- apc_web_data_ass_ser_output[["LongAge"]] %>% as.data.frame()

get_data_range <- function(data) {
  min_val <- min(data$CILo, na.rm = TRUE)
  max_val <- max(data$CIHi, na.rm = TRUE)
  return(c(min_val, max_val))
}

all_data_ranges <- list(
  get_data_range(long_age_aba_ass), get_data_range(long_age_cit_ass),
  get_data_range(long_age_ent_ass), get_data_range(long_age_eco_ass),
  get_data_range(long_age_kpn_ass), get_data_range(long_age_pae_ass),
  get_data_range(long_age_ser_ass)
)

overall_min <- floor(min(sapply(all_data_ranges, function(x) x[1])) / 25) * 25
overall_max <- ceiling(max(sapply(all_data_ranges, function(x) x[2])) / 25) * 25

range_size <- overall_max - overall_min
n_breaks <- 12
interval <- ceiling(range_size / n_breaks / 25) * 25
y_breaks <- seq(overall_min, overall_max, by = interval)

if(length(y_breaks) < 10) {
  interval <- ceiling(range_size / 15 / 10) * 10
  y_breaks <- seq(overall_min, overall_max, by = interval)
}

pathogens <- list(
  list(name = "Acinetobacter baumannii", data = long_age_aba_ass),
  list(name = "Citrobacter spp.", data = long_age_cit_ass),
  list(name = "Enterobacter spp.", data = long_age_ent_ass),
  list(name = "Escherichia coli", data = long_age_eco_ass),
  list(name = "Klebsiella pneumoniae", data = long_age_kpn_ass),
  list(name = "Pseudomonas aeruginosa", data = long_age_pae_ass),
  list(name = "Serratia spp.", data = long_age_ser_ass)
)

lancet_colors <- "#ED0000"

common_theme <- theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold", color = "black"),
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
    axis.ticks = element_line(color = "black", size = 0.5),
    axis.ticks.length = unit(3, "pt"),
    legend.position = "none"
  )

create_plot <- function(data, title) {
  ggplot(data=data, aes(x=Age, y=Rate)) +
    geom_ribbon(aes(ymin=CILo, ymax=CIHi), alpha=0.05, fill=lancet_colors) +
    geom_line(size=0.8, color=lancet_colors) + 
    geom_point(size=2, color=lancet_colors) +
    scale_x_continuous(breaks = seq(0, 100, by = 20)) +
    scale_y_continuous(limits = c(overall_min, overall_max), breaks = y_breaks) +
    labs(title = title, x = "Age (years)",
         y = "Mortality rate (per 100,000 population)") +
    common_theme
}

for (i in 1:length(pathogens)) {
  p <- create_plot(pathogens[[i]]$data, pathogens[[i]]$name)
  clean_name <- gsub(" ", "", pathogens[[i]]$name)
  clean_name <- gsub("\\.", "", clean_name)
  filename <- file.path(results_dir, paste0("Figure5_PMTWO", i, clean_name, ".pdf"))
  ggsave(filename, p, width = 10.22, height = 6.81)
  assign(paste0("PMTWO", i, clean_name), p)
}

PMTWO1Acinetobacterbaumannii
PMTWO2Citrobacterspp
PMTWO3Enterobacterspp
PMTWO4Escherichiacoli
PMTWO5Klebsiellapneumoniae
PMTWO6Pseudomonasaeruginosa
PMTWO7Serratiaspp
