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

# Extract CohortRR data for each pathogen
cohort_rr_aba_ass <- apc_web_data_ass_aba_output[["CohortRR"]] %>% as.data.frame()
cohort_rr_cit_ass <- apc_web_data_ass_cit_output[["CohortRR"]] %>% as.data.frame()
cohort_rr_ent_ass <- apc_web_data_ass_ent_output[["CohortRR"]] %>% as.data.frame()
cohort_rr_eco_ass <- apc_web_data_ass_eco_output[["CohortRR"]] %>% as.data.frame()
cohort_rr_kpn_ass <- apc_web_data_ass_kpn_output[["CohortRR"]] %>% as.data.frame()
cohort_rr_pae_ass <- apc_web_data_ass_pae_output[["CohortRR"]] %>% as.data.frame()
cohort_rr_ser_ass <- apc_web_data_ass_ser_output[["CohortRR"]] %>% as.data.frame()

get_data_range <- function(data) {
  min_val <- min(data$CILo, na.rm = TRUE)
  max_val <- max(data$CIHi, na.rm = TRUE)
  return(c(min_val, max_val))
}

all_data_ranges <- list(
  get_data_range(cohort_rr_aba_ass), get_data_range(cohort_rr_cit_ass),
  get_data_range(cohort_rr_ent_ass), get_data_range(cohort_rr_eco_ass),
  get_data_range(cohort_rr_kpn_ass), get_data_range(cohort_rr_pae_ass),
  get_data_range(cohort_rr_ser_ass)
)

y_min <- floor(min(sapply(all_data_ranges, function(x) x[1])))
y_max <- ceiling(max(sapply(all_data_ranges, function(x) x[2])))

pathogens <- list(
  list(name = "Acinetobacter baumannii", data = cohort_rr_aba_ass),
  list(name = "Citrobacter spp.", data = cohort_rr_cit_ass),
  list(name = "Enterobacter spp.", data = cohort_rr_ent_ass),
  list(name = "Escherichia coli", data = cohort_rr_eco_ass),
  list(name = "Klebsiella pneumoniae", data = cohort_rr_kpn_ass),
  list(name = "Pseudomonas aeruginosa", data = cohort_rr_pae_ass),
  list(name = "Serratia spp.", data = cohort_rr_ser_ass)
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
  reference_point <- data[which.min(abs(data$`Rate Ratio` - 1)), "Cohort"]
  ggplot(data=data, aes(x=Cohort, y=`Rate Ratio`)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 0.5) +
    geom_vline(xintercept = reference_point, linetype = "dashed", color = "black", size = 0.5) +
    geom_ribbon(aes(ymin=CILo, ymax=CIHi), alpha=0.05, fill=lancet_colors) +
    geom_line(size=0.8, color=lancet_colors) + 
    geom_point(size=2, color=lancet_colors) +
    scale_x_continuous(limits = c(1900, 2020), breaks = seq(1900, 2020, by = 20)) +
    scale_y_continuous(limits = c(y_min, y_max), breaks = seq(y_min, y_max, by = 1)) +
    labs(title = title, x = "Birth Cohort", y = "Rate Ratio") +
    common_theme
}

for (i in 1:length(pathogens)) {
  p <- create_plot(pathogens[[i]]$data, pathogens[[i]]$name)
  clean_name <- gsub(" ", "", pathogens[[i]]$name)
  clean_name <- gsub("\\.", "", clean_name)
  filename <- file.path(results_dir, paste0("Figure5_PMFOUR", i, clean_name, ".pdf"))
  ggsave(filename, p, width = 10.22, height = 6.81)
  assign(paste0("PMFOUR", i, clean_name), p)
}

PMFOUR1Acinetobacterbaumannii
PMFOUR2Citrobacterspp
PMFOUR3Enterobacterspp
PMFOUR4Escherichiacoli
PMFOUR5Klebsiellapneumoniae
PMFOUR6Pseudomonasaeruginosa
PMFOUR7Serratiaspp
