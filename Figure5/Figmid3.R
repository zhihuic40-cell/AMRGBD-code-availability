# Load required packages
library(ggplot2)
library(dplyr)

# Build period rate ratio data for each pathogen
period_rr_aba_att <- apc_web_data_ass_aba_output[["PeriodRR"]] %>% as.data.frame()
period_rr_cit_att <- apc_web_data_ass_cit_output[["PeriodRR"]] %>% as.data.frame()
period_rr_ent_att <- apc_web_data_ass_ent_output[["PeriodRR"]] %>% as.data.frame()
period_rr_eco_att <- apc_web_data_ass_eco_output[["PeriodRR"]] %>% as.data.frame()
period_rr_kpn_att <- apc_web_data_ass_kpn_output[["PeriodRR"]] %>% as.data.frame()
period_rr_pae_att <- apc_web_data_ass_pae_output[["PeriodRR"]] %>% as.data.frame()
period_rr_ser_att <- apc_web_data_ass_ser_output[["PeriodRR"]] %>% as.data.frame()

# Helper to get y-range
get_data_range <- function(data) {
  min_val <- min(data$CILo, na.rm = TRUE)
  max_val <- max(data$CIHi, na.rm = TRUE)
  return(c(min_val, max_val))
}

# Compute overall range across pathogens
all_data_ranges <- list(
  get_data_range(period_rr_aba_att),
  get_data_range(period_rr_cit_att),
  get_data_range(period_rr_ent_att),
  get_data_range(period_rr_eco_att),
  get_data_range(period_rr_kpn_att),
  get_data_range(period_rr_pae_att),
  get_data_range(period_rr_ser_att)
)

# 计算整体的最小值和最大值
y_min <- floor(min(sapply(all_data_ranges, function(x) x[1])) * 10) / 10
y_max <- ceiling(max(sapply(all_data_ranges, function(x) x[2])) * 10) / 10

# Map pathogen names to data
pathogens <- list(
  list(name = "Acinetobacter baumannii", data = period_rr_aba_att),
  list(name = "Citrobacter spp.", data = period_rr_cit_att),
  list(name = "Enterobacter spp.", data = period_rr_ent_att),
  list(name = "Escherichia coli", data = period_rr_eco_att),
  list(name = "Klebsiella pneumoniae", data = period_rr_kpn_att),
  list(name = "Pseudomonas aeruginosa", data = period_rr_pae_att),
  list(name = "Serratia spp.", data = period_rr_ser_att)
)

# Lancet-style color
lancet_colors <- "#ED0000"  # red

# Define common theme
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

# Plotting helper
create_plot <- function(data, title) {
  # Find the reference where Rate Ratio = 1
  reference_point <- data[which.min(abs(data$`Rate Ratio` - 1)), "Period"]
  
  ggplot(data=data, aes(x=Period, y=`Rate Ratio`)) +
    # Reference lines
    geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 0.5) +
    geom_vline(xintercept = reference_point, linetype = "dashed", color = "black", size = 0.5) +
    # Data layers
    geom_ribbon(aes(ymin=CILo, ymax=CIHi), alpha=0.05, fill=lancet_colors) +
    geom_line(size=0.9, color=lancet_colors) + 
    geom_point(size=1.8, color=lancet_colors) +
    # Axes limits and ticks
    scale_x_continuous(limits = c(1990, 2020),
                       breaks = seq(1990, 2020, by = 5)) +
    scale_y_continuous(limits = c(y_min, y_max)) +
    # Labels
    labs(title = title,
         x = "Calendar Year",
         y = "Rate Ratio") +
    common_theme
}

# Create and save plots per pathogen
for (i in 1:length(pathogens)) {
  # Build plot
  p <- create_plot(pathogens[[i]]$data, pathogens[[i]]$name)
  
  # Clean file name (remove special characters)
  clean_name <- gsub(" ", "", pathogens[[i]]$name)
  clean_name <- gsub("\\.", "", clean_name)
  filename <- paste0("PMTHREE", i, clean_name, ".pdf")
  
  # Save plot
  ggsave(filename, p, width = 10.22, height = 6.81)
  
  # Keep plot object in environment
  assign(paste0("PMTHREE", i, clean_name), p)
}

# Show all plots
PMTHREE1Acinetobacterbaumannii
PMTHREE2Citrobacterspp
PMTHREE3Enterobacterspp
PMTHREE4Escherichiacoli
PMTHREE5Klebsiellapneumoniae
PMTHREE6Pseudomonasaeruginosa
PMTHREE7Serratiaspp
