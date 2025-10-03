# Load required packages
library(ggplot2)
library(dplyr)

# ==== Load RDS data files ====
# Load RDS data files for each pathogen
apc_web_data_att_aba_output <- readRDS(file.path("data", "apc_web_data_att_aba_output.rds"))
apc_web_data_att_cit_output <- readRDS(file.path("data", "apc_web_data_att_cit_output.rds"))
apc_web_data_att_ent_output <- readRDS(file.path("data", "apc_web_data_att_ent_output.rds"))
apc_web_data_att_eco_output <- readRDS(file.path("data", "apc_web_data_att_eco_output.rds"))
apc_web_data_att_kpn_output <- readRDS(file.path("data", "apc_web_data_att_kpn_output.rds"))
apc_web_data_att_pae_output <- readRDS(file.path("data", "apc_web_data_att_pae_output.rds"))
apc_web_data_att_ser_output <- readRDS(file.path("data", "apc_web_data_att_ser_output.rds"))
apc_web_data_att_spn_output <- readRDS(file.path("data", "apc_web_data_att_spn_output.rds"))

# ==== Extract LocalDrifts data for each pathogen ====
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

Local_spnATT <- apc_web_data_att_spn_output[["LocalDrifts"]] %>% as.data.frame()
Net_spnATT <- apc_web_data_att_spn_output[["NetDrift"]] %>% as.data.frame()

# ==== Define pathogen list ====
pathogens <- list(
  list(name = "Acinetobacter baumannii", data = Local_abaATT),
  list(name = "Citrobacter spp.", data = Local_citATT), 
  list(name = "Enterobacter spp.", data = Local_entATT),
  list(name = "Escherichia coli", data = Local_ecoATT),
  list(name = "Klebsiella pneumoniae", data = Local_kpnATT),
  list(name = "Pseudomonas aeruginosa", data = Local_paeATT),
  list(name = "Serratia spp.", data = Local_serATT),
  list(name = "Streptococcus pneumoniae", data = Local_spnATT)
)

# ==== Set visualization parameters ====
# Define color scheme
lancet_colors <- "#ED0000"

# Define common theme settings
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

# ==== Create plotting function ====
create_plot <- function(data, title) {
  ggplot(data=data, aes(x=Age, y=`Percent per Year`)) +
    # Add reference lines
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = 20, linetype = "dashed", color = "grey50") +
    # Add confidence interval ribbon
    geom_ribbon(aes(ymin=CILo, ymax=CIHi), alpha=0.05, fill=lancet_colors) +
    # Add lines and points
    geom_line(size=1, color=lancet_colors) + 
    geom_point(size=2, color=lancet_colors) +
    # Set axis scales
    scale_y_continuous(breaks = seq(-6, 6, by = 2)) +
    coord_cartesian(ylim = c(-6, 6)) +
    # Add labels
    ylab("Annual change of mortality (% per year)") +
    xlab("Age (years)") +
    ggtitle(title) +
    common_theme
}

# ==== Create and save plots ====
for (i in 1:length(pathogens)) {
  # Create plot
  p <- create_plot(pathogens[[i]]$data, pathogens[[i]]$name)
  
  # Create clean filename (remove special characters)
  clean_name <- gsub(" ", "", pathogens[[i]]$name)
  clean_name <- gsub("\\.", "", clean_name)
  filename <- paste0("PMONE", i, clean_name, ".pdf")
  
  # Save plot
  ggsave(filename, p, width = 10.22, height = 6.81)
  
  # Save plot object in environment
  assign(paste0("PMONE", i, clean_name), p)
}

# ==== Display all plots ====
PMONE1Acinetobacterbaumannii
PMONE2Citrobacterspp
PMONE3Enterobacterspp
PMONE4Escherichiacoli
PMONE5Klebsiellapneumoniae
PMONE6Pseudomonasaeruginosa
PMONE7Serratiaspp
PMONE8Streptococcuspneumoniae

