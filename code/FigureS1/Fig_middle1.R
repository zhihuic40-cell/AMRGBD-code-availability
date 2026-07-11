# Load required packages
library(ggplot2)
library(dplyr)

# Setup paths for Code Ocean compatibility
is_code_ocean <- dir.exists("/code") && dir.exists("/data") && dir.exists("/results")

if (is_code_ocean) {
  data_dir <- "/data/FigureS1"
  results_dir <- "/results/FigureS1"
} else {
  if (dir.exists("data/FigureS1")) {
    data_dir <- "data/FigureS1"
    results_dir <- "results/FigureS1"
  } else if (dir.exists("../../data/FigureS1")) {
    data_dir <- file.path("..", "..", "data", "FigureS1")
    results_dir <- file.path("..", "..", "results", "FigureS1")
  } else {
    data_dir <- here("data", "FigureS1")
    results_dir <- here("results", "FigureS1")
  }
}
if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)

# Load RDS data files for each pathogen
apc_web_data_att_aba_output <- readRDS(file.path(data_dir, "apc_web_data_att_aba_output.rds"))
apc_web_data_att_cit_output <- readRDS(file.path(data_dir, "apc_web_data_att_cit_output.rds"))
apc_web_data_att_ent_output <- readRDS(file.path(data_dir, "apc_web_data_att_ent_output.rds"))
apc_web_data_att_eco_output <- readRDS(file.path(data_dir, "apc_web_data_att_eco_output.rds"))
apc_web_data_att_kpn_output <- readRDS(file.path(data_dir, "apc_web_data_att_kpn_output.rds"))
apc_web_data_att_pae_output <- readRDS(file.path(data_dir, "apc_web_data_att_pae_output.rds"))
apc_web_data_att_ser_output <- readRDS(file.path(data_dir, "apc_web_data_att_ser_output.rds"))

# Extract LocalDrifts data for each pathogen
Local_abaATT <- apc_web_data_att_aba_output[["LocalDrifts"]] %>% as.data.frame()
Net_abaATT <- apc_web_data_att_aba_output[["NetDrift"]] %>% as.data.frame()
Local_citATT <- apc_web_data_att_cit_output[["LocalDrifts"]] %>% as.data.frame()
Net_citATT <- apc_web_data_att_cit_output[["NetDrift"]] %>% as.data.frame()
Local_entATT <- apc_web_data_att_ent_output[["LocalDrifts"]] %>% as.data.frame()
Net_entATT <- apc_web_data_att_ent_output[["NetDrift"]] %>% as.data.frame()
Local_ecoATT <- apc_web_data_att_eco_output[["LocalDrifts"]] %>% as.data.frame()
Net_ecoATT <- apc_web_data_att_eco_output[["NetDrift"]] %>% as.data.frame()
Local_kpnATT <- apc_web_data_att_kpn_output[["LocalDrifts"]] %>% as.data.frame()
Net_kpnATT <- apc_web_data_att_kpn_output[["NetDrift"]] %>% as.data.frame()
Local_paeATT <- apc_web_data_att_pae_output[["LocalDrifts"]] %>% as.data.frame()
Net_paeATT <- apc_web_data_att_pae_output[["NetDrift"]] %>% as.data.frame()
Local_serATT <- apc_web_data_att_ser_output[["LocalDrifts"]] %>% as.data.frame()
Net_serATT <- apc_web_data_att_ser_output[["NetDrift"]] %>% as.data.frame()

pathogens <- list(
  list(name = "Acinetobacter baumannii", data = Local_abaATT),
  list(name = "Citrobacter spp.", data = Local_citATT), 
  list(name = "Enterobacter spp.", data = Local_entATT),
  list(name = "Escherichia coli", data = Local_ecoATT),
  list(name = "Klebsiella pneumoniae", data = Local_kpnATT),
  list(name = "Pseudomonas aeruginosa", data = Local_paeATT),
  list(name = "Serratia spp.", data = Local_serATT)
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
  ggplot(data=data, aes(x=Age, y=`Percent per Year`)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = 20, linetype = "dashed", color = "grey50") +
    geom_ribbon(aes(ymin=CILo, ymax=CIHi), alpha=0.05, fill=lancet_colors) +
    geom_line(size=1, color=lancet_colors) + 
    geom_point(size=2, color=lancet_colors) +
    scale_y_continuous(breaks = seq(-6, 6, by = 2)) +
    coord_cartesian(ylim = c(-6, 6)) +
    ylab("Annual change of mortality (% per year)") +
    xlab("Age (years)") +
    ggtitle(title) +
    common_theme
}

for (i in 1:length(pathogens)) {
  p <- create_plot(pathogens[[i]]$data, pathogens[[i]]$name)
  clean_name <- gsub(" ", "", pathogens[[i]]$name)
  clean_name <- gsub("\\.", "", clean_name)
  filename <- file.path(results_dir, paste0("FigureS1_PMONE", i, clean_name, ".pdf"))
  ggsave(filename, p, width = 10.22, height = 6.81)
  assign(paste0("PMONE", i, clean_name), p)
}

PMONE1Acinetobacterbaumannii
PMONE2Citrobacterspp
PMONE3Enterobacterspp
PMONE4Escherichiacoli
PMONE5Klebsiellapneumoniae
PMONE6Pseudomonasaeruginosa
PMONE7Serratiaspp
