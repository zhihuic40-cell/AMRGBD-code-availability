# Load required packages
library(ggplot2)  # 用于创建图形
library(dplyr)    # 用于数据处理

# ==== Data loading and preparation ====
# Load LocalDrifts and NetDrift for each pathogen

# Acinetobacter baumannii
Local_abaATT <- apc_web_data_ass_aba_output[["LocalDrifts"]] %>% as.data.frame()
Net_abaATT <- apc_web_data_ass_aba_output[["NetDrift"]] %>% as.data.frame()

# Citrobacter spp.
Local_citATT <- apc_web_data_ass_cit_output[["LocalDrifts"]] %>% as.data.frame()
Net_citATT <- apc_web_data_ass_cit_output[["NetDrift"]] %>% as.data.frame()

# Enterobacter spp.
Local_entATT <- apc_web_data_ass_ent_output[["LocalDrifts"]] %>% as.data.frame()
Net_entATT <- apc_web_data_ass_ent_output[["NetDrift"]] %>% as.data.frame()

# Escherichia coli
Local_ecoATT <- apc_web_data_ass_eco_output[["LocalDrifts"]] %>% as.data.frame()
Net_ecoATT <- apc_web_data_ass_eco_output[["NetDrift"]] %>% as.data.frame()

# Klebsiella pneumoniae
Local_kpnATT <- apc_web_data_ass_kpn_output[["LocalDrifts"]] %>% as.data.frame()
Net_kpnATT <- apc_web_data_ass_kpn_output[["NetDrift"]] %>% as.data.frame()

# Pseudomonas aeruginosa
Local_paeATT <- apc_web_data_ass_pae_output[["LocalDrifts"]] %>% as.data.frame()
Net_paeATT <- apc_web_data_ass_pae_output[["NetDrift"]] %>% as.data.frame()

# Serratia spp.
Local_serATT <- apc_web_data_ass_ser_output[["LocalDrifts"]] %>% as.data.frame()
Net_serATT <- apc_web_data_ass_ser_output[["NetDrift"]] %>% as.data.frame()


# ==== Define pathogen list ====
# Use full scientific names with data mapping
pathogens <- list(
  list(name = "Acinetobacter baumannii", data = Local_abaATT),
  list(name = "Citrobacter spp.", data = Local_citATT), 
  list(name = "Enterobacter spp.", data = Local_entATT),
  list(name = "Escherichia coli", data = Local_ecoATT),
  list(name = "Klebsiella pneumoniae", data = Local_kpnATT),
  list(name = "Pseudomonas aeruginosa", data = Local_paeATT),
  list(name = "Serratia spp.", data = Local_serATT)
)

# ==== Visual parameters ====
# Lancet-style palette
lancet_colors <- "#ED0000"  # red for attributable data

# Define common theme
# Define common theme
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
# ==== Plotting function ====
create_plot <- function(data, title) {
  ggplot(data=data, aes(x=Age, y=`Percent per Year`)) +
    # Reference lines
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = 20, linetype = "dashed", color = "grey50") +
    # Confidence ribbon
    geom_ribbon(aes(ymin=CILo, ymax=CIHi), alpha=0.05, fill=lancet_colors) +
    # Lines and points
    geom_line(size=1, color=lancet_colors) + 
    geom_point(size=2, color=lancet_colors) +
    # Axes
    scale_y_continuous(breaks = seq(-6, 6, by = 2)) +
    coord_cartesian(ylim = c(-6, 6)) +
    # Labels
    ylab("Annual change of mortality (% per year)") +
    xlab("Age (years)") +
    ggtitle(title) +
    common_theme
}

# ==== Create and save plots ====
# Build plot per pathogen
for (i in 1:length(pathogens)) {
  # Build plot
  p <- create_plot(pathogens[[i]]$data, pathogens[[i]]$name)
  
  # Clean file name (remove special characters)
  clean_name <- gsub(" ", "", pathogens[[i]]$name)
  clean_name <- gsub("\\.", "", clean_name)
  filename <- paste0("PMONE", i, clean_name, ".pdf")
  
  # Save plot (standard size)
  ggsave(filename, p, width = 10.22, height = 6.81)
  
  # Keep plot object in environment for display
  assign(paste0("PMONE", i, clean_name), p)
}

# ==== Display all plots ====
# Show plots in order
PMONE1Acinetobacterbaumannii
PMONE2Citrobacterspp
PMONE3Enterobacterspp
PMONE4Escherichiacoli
PMONE5Klebsiellapneumoniae
PMONE6Pseudomonasaeruginosa
PMONE7Serratiaspp
