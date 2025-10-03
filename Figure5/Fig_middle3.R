# Load required packages
library(ggplot2)
library(dplyr)

# Load RDS data files for each pathogen
apc_web_data_att_aba_output <- readRDS(file.path("data", "apc_web_data_att_aba_output.rds"))
apc_web_data_att_cit_output <- readRDS(file.path("data", "apc_web_data_att_cit_output.rds"))
apc_web_data_att_ent_output <- readRDS(file.path("data", "apc_web_data_att_ent_output.rds"))
apc_web_data_att_eco_output <- readRDS(file.path("data", "apc_web_data_att_eco_output.rds"))
apc_web_data_att_kpn_output <- readRDS(file.path("data", "apc_web_data_att_kpn_output.rds"))
apc_web_data_att_pae_output <- readRDS(file.path("data", "apc_web_data_att_pae_output.rds"))
apc_web_data_att_ser_output <- readRDS(file.path("data", "apc_web_data_att_ser_output.rds"))
apc_web_data_att_spn_output <- readRDS(file.path("data", "apc_web_data_att_spn_output.rds"))

# Extract PeriodRR data for each pathogen
period_rr_aba_att <- apc_web_data_att_aba_output[["PeriodRR"]] %>% as.data.frame()
period_rr_cit_att <- apc_web_data_att_cit_output[["PeriodRR"]] %>% as.data.frame()
period_rr_ent_att <- apc_web_data_att_ent_output[["PeriodRR"]] %>% as.data.frame()
period_rr_eco_att <- apc_web_data_att_eco_output[["PeriodRR"]] %>% as.data.frame()
period_rr_kpn_att <- apc_web_data_att_kpn_output[["PeriodRR"]] %>% as.data.frame()
period_rr_pae_att <- apc_web_data_att_pae_output[["PeriodRR"]] %>% as.data.frame()
period_rr_ser_att <- apc_web_data_att_ser_output[["PeriodRR"]] %>% as.data.frame()
period_rr_spn_att <- apc_web_data_att_spn_output[["PeriodRR"]] %>% as.data.frame()

# Function to calculate data range
get_data_range <- function(data) {
  min_val <- min(data$CILo, na.rm = TRUE)
  max_val <- max(data$CIHi, na.rm = TRUE)
  return(c(min_val, max_val))
}

# Calculate overall range across all pathogens
all_data_ranges <- list(
  get_data_range(period_rr_aba_att),
  get_data_range(period_rr_cit_att),
  get_data_range(period_rr_ent_att),
  get_data_range(period_rr_eco_att),
  get_data_range(period_rr_kpn_att),
  get_data_range(period_rr_pae_att),
  get_data_range(period_rr_ser_att),
  get_data_range(period_rr_spn_att)
)

# Calculate overall min and max values
y_min <- floor(min(sapply(all_data_ranges, function(x) x[1])) * 10) / 10
y_max <- ceiling(max(sapply(all_data_ranges, function(x) x[2])) * 10) / 10

# Define all pathogens and their data mappings
pathogens <- list(
  list(name = "Acinetobacter baumannii", data = period_rr_aba_att),
  list(name = "Citrobacter spp.", data = period_rr_cit_att),
  list(name = "Enterobacter spp.", data = period_rr_ent_att),
  list(name = "Escherichia coli", data = period_rr_eco_att),
  list(name = "Klebsiella pneumoniae", data = period_rr_kpn_att),
  list(name = "Pseudomonas aeruginosa", data = period_rr_pae_att),
  list(name = "Serratia spp.", data = period_rr_ser_att),
  list(name = "Streptococcus pneumoniae", data = period_rr_spn_att)
)

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

# Create plotting function
create_plot <- function(data, title) {
  # Find the point where Rate Ratio = 1
  reference_point <- data[which.min(abs(data$`Rate Ratio` - 1)), "Period"]
  
  ggplot(data=data, aes(x=Period, y=`Rate Ratio`)) +
    # Add reference lines
    geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 0.5) +
    geom_vline(xintercept = reference_point, linetype = "dashed", color = "black", size = 0.5) +
    # Add data layers
    geom_ribbon(aes(ymin=CILo, ymax=CIHi), alpha=0.05, fill=lancet_colors) +
    geom_line(size=0.9, color=lancet_colors) + 
    geom_point(size=1.8, color=lancet_colors) +
    # Set axis limits and breaks
    scale_x_continuous(limits = c(1990, 2020),
                       breaks = seq(1990, 2020, by = 5)) +
    scale_y_continuous(limits = c(y_min, y_max)) +
    # Add labels
    labs(title = title,
         x = "Calendar Year",
         y = "Rate Ratio") +
    common_theme
}

# Create and save plots for each pathogen
for (i in 1:length(pathogens)) {
  # Create plot
  p <- create_plot(pathogens[[i]]$data, pathogens[[i]]$name)
  
  # Create clean filename (remove special characters)
  clean_name <- gsub(" ", "", pathogens[[i]]$name)
  clean_name <- gsub("\\.", "", clean_name)
  filename <- paste0("PMTHREE", i, clean_name, ".pdf")
  
  # Save plot
  ggsave(filename, p, width = 10.22, height = 6.81)
  
  # Save plot object in environment
  assign(paste0("PMTHREE", i, clean_name), p)
}

# Display all plots
PMTHREE1Acinetobacterbaumannii
PMTHREE2Citrobacterspp
PMTHREE3Enterobacterspp
PMTHREE4Escherichiacoli
PMTHREE5Klebsiellapneumoniae
PMTHREE6Pseudomonasaeruginosa
PMTHREE7Serratiaspp
PMTHREE8Streptococcuspneumoniae

