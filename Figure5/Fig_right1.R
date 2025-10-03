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

# Extract CohortRR data for each pathogen
cohort_rr_aba_att <- apc_web_data_att_aba_output[["CohortRR"]] %>% as.data.frame()
cohort_rr_cit_att <- apc_web_data_att_cit_output[["CohortRR"]] %>% as.data.frame()
cohort_rr_ent_att <- apc_web_data_att_ent_output[["CohortRR"]] %>% as.data.frame()
cohort_rr_eco_att <- apc_web_data_att_eco_output[["CohortRR"]] %>% as.data.frame()
cohort_rr_kpn_att <- apc_web_data_att_kpn_output[["CohortRR"]] %>% as.data.frame()
cohort_rr_pae_att <- apc_web_data_att_pae_output[["CohortRR"]] %>% as.data.frame()
cohort_rr_ser_att <- apc_web_data_att_ser_output[["CohortRR"]] %>% as.data.frame()
cohort_rr_spn_att <- apc_web_data_att_spn_output[["CohortRR"]] %>% as.data.frame()

# Function to calculate data range
get_data_range <- function(data) {
  min_val <- min(data$CILo, na.rm = TRUE)
  max_val <- max(data$CIHi, na.rm = TRUE)
  return(c(min_val, max_val))
}

# Calculate overall range across all pathogens
all_data_ranges <- list(
  get_data_range(cohort_rr_aba_att),
  get_data_range(cohort_rr_cit_att),
  get_data_range(cohort_rr_ent_att),
  get_data_range(cohort_rr_eco_att),
  get_data_range(cohort_rr_kpn_att),
  get_data_range(cohort_rr_pae_att),
  get_data_range(cohort_rr_ser_att),
  get_data_range(cohort_rr_spn_att)
)

# Calculate overall min and max values and round to integers
y_min <- floor(min(sapply(all_data_ranges, function(x) x[1])))
y_max <- ceiling(max(sapply(all_data_ranges, function(x) x[2])))

# Define all pathogens and their data mappings
pathogens <- list(
  list(name = "Acinetobacter baumannii", data = cohort_rr_aba_att),
  list(name = "Citrobacter spp.", data = cohort_rr_cit_att),
  list(name = "Enterobacter spp.", data = cohort_rr_ent_att),
  list(name = "Escherichia coli", data = cohort_rr_eco_att),
  list(name = "Klebsiella pneumoniae", data = cohort_rr_kpn_att),
  list(name = "Pseudomonas aeruginosa", data = cohort_rr_pae_att),
  list(name = "Serratia spp.", data = cohort_rr_ser_att),
  list(name = "Streptococcus pneumoniae", data = cohort_rr_spn_att)
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
  reference_point <- data[which.min(abs(data$`Rate Ratio` - 1)), "Cohort"]
  
  ggplot(data=data, aes(x=Cohort, y=`Rate Ratio`)) +
    # Add reference lines
    geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 0.5) +
    geom_vline(xintercept = reference_point, linetype = "dashed", color = "black", size = 0.5) +
    # Add data layers
    geom_ribbon(aes(ymin=CILo, ymax=CIHi), alpha=0.05, fill=lancet_colors) +
    geom_line(size=0.8, color=lancet_colors) + 
    geom_point(size=2, color=lancet_colors) +
    # Set axis limits and breaks
    scale_x_continuous(limits = c(1900, 2020),
                       breaks = seq(1900, 2020, by = 20)) +
    scale_y_continuous(limits = c(y_min, y_max),
                       breaks = seq(y_min, y_max, by = 1)) +
    # Add labels
    labs(title = title,
         x = "Birth Cohort",
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
  filename <- paste0("PMFOUR", i, clean_name, ".pdf")
  
  # Save plot
  ggsave(filename, p, width = 10.22, height = 6.81)
  
  # Save plot object in environment
  assign(paste0("PMFOUR", i, clean_name), p)
}

# Display all plots
PMFOUR1Acinetobacterbaumannii
PMFOUR2Citrobacterspp
PMFOUR3Enterobacterspp
PMFOUR4Escherichiacoli
PMFOUR5Klebsiellapneumoniae
PMFOUR6Pseudomonasaeruginosa
PMFOUR7Serratiaspp
PMFOUR8Streptococcuspneumoniae

