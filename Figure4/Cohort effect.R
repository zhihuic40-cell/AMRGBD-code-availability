# Load required packages
library(ggplot2)
library(dplyr)
library(patchwork)
library(cowplot)

# Load RDS files
apc_web_data_ass_global_output <- readRDS("data/apc_web_data_ass_global_output.rds")
apc_web_data_ass_CEEECA_output <- readRDS("data/apc_web_data_ass_CEEECA_output.rds")
apc_web_data_ass_HI_output <- readRDS("data/apc_web_data_ass_HI_output.rds")
apc_web_data_ass_LAC_output <- readRDS("data/apc_web_data_ass_LAC_output.rds")
apc_web_data_ass_NAME_output <- readRDS("data/apc_web_data_ass_NAME_output.rds")
apc_web_data_ass_SA_output <- readRDS("data/apc_web_data_ass_SA_output.rds")
apc_web_data_ass_SAEAO_output <- readRDS("data/apc_web_data_ass_SAEAO_output.rds")
apc_web_data_ass_SSA_output <- readRDS("data/apc_web_data_ass_SSA_output.rds")

apc_web_data_att_global_output <- readRDS("data/apc_web_data_att_global_output.rds")
apc_web_data_att_CEEECA_output <- readRDS("data/apc_web_data_att_CEEECA_output.rds")
apc_web_data_att_HI_output <- readRDS("data/apc_web_data_att_HI_output.rds")
apc_web_data_att_LAC_output <- readRDS("data/apc_web_data_att_LAC_output.rds")
apc_web_data_att_NAME_output <- readRDS("data/apc_web_data_att_NAME_output.rds")
apc_web_data_att_SA_output <- readRDS("data/apc_web_data_att_SA_output.rds")
apc_web_data_att_SAEAO_output <- readRDS("data/apc_web_data_att_SAEAO_output.rds")
apc_web_data_att_SSA_output <- readRDS("data/apc_web_data_att_SSA_output.rds")

# Generate data for each subplot
cohort_rr_global <- apc_web_data_ass_global_output[["CohortRR"]] %>% as.data.frame()
cohort_rr_ceeeca <- apc_web_data_ass_CEEECA_output[["CohortRR"]] %>% as.data.frame()
cohort_rr_hi <- apc_web_data_ass_HI_output[["CohortRR"]] %>% as.data.frame()
cohort_rr_lac <- apc_web_data_ass_LAC_output[["CohortRR"]] %>% as.data.frame()
cohort_rr_name <- apc_web_data_ass_NAME_output[["CohortRR"]] %>% as.data.frame()
cohort_rr_sa <- apc_web_data_ass_SA_output[["CohortRR"]] %>% as.data.frame()
cohort_rr_saeao <- apc_web_data_ass_SAEAO_output[["CohortRR"]] %>% as.data.frame()
cohort_rr_ssa <- apc_web_data_ass_SSA_output[["CohortRR"]] %>% as.data.frame()

cohort_rr_global_att <- apc_web_data_att_global_output[["CohortRR"]] %>% as.data.frame()
cohort_rr_ceeeca_att <- apc_web_data_att_CEEECA_output[["CohortRR"]] %>% as.data.frame()
cohort_rr_hi_att <- apc_web_data_att_HI_output[["CohortRR"]] %>% as.data.frame()
cohort_rr_lac_att <- apc_web_data_att_LAC_output[["CohortRR"]] %>% as.data.frame()
cohort_rr_name_att <- apc_web_data_att_NAME_output[["CohortRR"]] %>% as.data.frame()
cohort_rr_sa_att <- apc_web_data_att_SA_output[["CohortRR"]] %>% as.data.frame()
cohort_rr_saeao_att <- apc_web_data_att_SAEAO_output[["CohortRR"]] %>% as.data.frame()
cohort_rr_ssa_att <- apc_web_data_att_SSA_output[["CohortRR"]] %>% as.data.frame()

# Function to calculate data range
get_data_range <- function(ass_data, att_data) {
  min_val <- min(c(ass_data$CILo, att_data$CILo), na.rm = TRUE)
  max_val <- max(c(ass_data$CIHi, att_data$CIHi), na.rm = TRUE)
  return(c(min_val, max_val))
}

# Get all data ranges
ranges <- list(
  global = get_data_range(cohort_rr_global, cohort_rr_global_att),
  ceeeca = get_data_range(cohort_rr_ceeeca, cohort_rr_ceeeca_att),
  hi = get_data_range(cohort_rr_hi, cohort_rr_hi_att),
  lac = get_data_range(cohort_rr_lac, cohort_rr_lac_att),
  name = get_data_range(cohort_rr_name, cohort_rr_name_att),
  sa = get_data_range(cohort_rr_sa, cohort_rr_sa_att),
  saeao = get_data_range(cohort_rr_saeao, cohort_rr_saeao_att),
  ssa = get_data_range(cohort_rr_ssa, cohort_rr_ssa_att)
)

# Calculate overall min and max values
y_min <- floor(min(sapply(ranges, function(x) x[1]), na.rm = TRUE) * 10) / 10
y_max <- ceiling(max(sapply(ranges, function(x) x[2]), na.rm = TRUE) * 10) / 10

# Print range for verification
print(paste("Overall data range:", y_min, "to", y_max))

# Define colors and point shapes
lancet_colors <- c("Associated with CR" = "#00468B", "Attributable to CR" = "#ED0000")
point_shapes <- c("Associated with CR" = 16, "Attributable to CR" = 17)

# Define common theme with inward ticks
common_theme <- theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
    axis.title.x = element_text(size = 8, face = "bold"),
    axis.title.y = element_text(size = 8, face = "bold"),
    axis.text.x = element_text(size = 8, face = "bold", color = "black"),
    axis.text.y = element_text(size = 8, face = "bold", color = "black"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
    axis.ticks = element_line(color = "black", size = 0.5),
    axis.ticks.length = unit(0.15, "cm")
  )

# Define plotting function
plot_curve <- function(data_ass, data_att, title, show_legend = FALSE) {
  # Add group labels
  data_ass$Group <- "Associated with CR"
  data_att$Group <- "Attributable to CR"
  
  # Find the point where Rate Ratio = 1
  reference_point <- data_ass[which.min(abs(data_ass$`Rate Ratio` - 1)), "Cohort"]
  
  # Combine data
  combined_data <- rbind(data_ass, data_att)
  
  # Calculate x-axis range and breaks
  x_min <- 1900
  x_max <- 2020
  
  p <- ggplot(data = combined_data, aes(x = Cohort, y = `Rate Ratio`, color = Group, shape = Group)) +
    geom_line(size = 0.8) +
    geom_point(size = 2) +
    geom_ribbon(aes(ymin = CILo, ymax = CIHi, fill = Group), alpha = 0.1, color = NA) +
    # Add reference lines
    geom_hline(yintercept = 1, 
               linetype = "dashed", 
               color = "black", 
               size = 0.5) +
    geom_vline(xintercept = reference_point,
               linetype = "dashed", 
               color = "black", 
               size = 0.5) +
    scale_color_manual(values = lancet_colors, name = "Counterfactual") +
    scale_shape_manual(values = point_shapes, name = "Counterfactual") +
    scale_fill_manual(values = lancet_colors, name = "Counterfactual") +
    # Control x-axis
    scale_x_continuous(limits = c(x_min, x_max),
                       breaks = seq(x_min, x_max, by = 20)) +
    # Control y-axis                  
    scale_y_continuous(limits = c(y_min, y_max)) +
    labs(title = title,
         x = "Birth Cohort",
         y = "Rate Ratio") +
    common_theme +
    theme(legend.position = if(show_legend) "right" else "none")
  
  return(p)
}

# Create legend data
legend_plot <- plot_curve(
  data_ass = data.frame(
    "Cohort" = 1:2, 
    "Rate Ratio" = c(1,2),
    "CILo" = c(0.5,1.5),
    "CIHi" = c(1.5,2.5),
    check.names = FALSE
  ),
  data_att = data.frame(
    "Cohort" = 1:2,
    "Rate Ratio" = c(3,4),
    "CILo" = c(2.5,3.5),
    "CIHi" = c(3.5,4.5),
    check.names = FALSE
  ),
  title = "",
  show_legend = TRUE
) +
  theme(
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 8, face = "bold"),
    legend.key.size = unit(0.8, "cm"),
    legend.spacing.y = unit(0.2, "cm"),
    legend.box.margin = margin(0, 0, 0, -1),
    legend.key = element_rect(color = NA, fill = "white"),
    legend.background = element_blank()
  )

# Extract legend
shared_legend <- cowplot::get_legend(legend_plot)

# Create plots for each region
plot_global <- plot_curve(cohort_rr_global, cohort_rr_global_att, "Global")
plot_ceeeca <- plot_curve(cohort_rr_ceeeca, cohort_rr_ceeeca_att, 
                          "Central Europe, Eastern\nEurope, and Central Asia")
plot_hi <- plot_curve(cohort_rr_hi, cohort_rr_hi_att, "High-income")
plot_lac <- plot_curve(cohort_rr_lac, cohort_rr_lac_att, "Latin America\nand Caribbean")
plot_name <- plot_curve(cohort_rr_name, cohort_rr_name_att, "North Africa\nand Middle East")
plot_sa <- plot_curve(cohort_rr_sa, cohort_rr_sa_att, "South Asia")
plot_saeao <- plot_curve(cohort_rr_saeao, cohort_rr_saeao_att, 
                         "Southeast Asia, East Asia,\nand Oceania")
plot_ssa <- plot_curve(cohort_rr_ssa, cohort_rr_ssa_att, "Sub-Saharan Africa")

# Define layout
layout <- "
AAAAAAAAB
AAAAAAAAB
AAAAAAAAB
AAAAAAAAB
"

# Combine all elements
final_plot <- wrap_plots(
  wrap_plots(plot_global, plot_ceeeca, plot_hi, plot_lac,
             plot_name, plot_sa, plot_saeao, plot_ssa,
             ncol = 4),
  shared_legend,
  design = layout
)

# Add white background
final_plot <- final_plot & 
  theme(plot.background = element_rect(fill = "white", color = NA))

# Save figure
ggsave("cohort_rate_ratio.pdf", final_plot, width = 15, height = 8)