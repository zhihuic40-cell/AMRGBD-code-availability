# Load required packages
library(ggplot2)
library(dplyr)
library(patchwork)

# Prepare data for each subplot
long_age_global <- apc_web_data_ass_global_output[["LongAge"]] %>% as.data.frame()
long_age_ceeeca <- apc_web_data_ass_CEEECA_output[["LongAge"]] %>% as.data.frame()
long_age_hi <- apc_web_data_ass_HI_output[["LongAge"]] %>% as.data.frame()
long_age_lac <- apc_web_data_ass_LAC_output[["LongAge"]] %>% as.data.frame()
long_age_name <- apc_web_data_ass_NAME_output[["LongAge"]] %>% as.data.frame()
long_age_sa <- apc_web_data_ass_SA_output[["LongAge"]] %>% as.data.frame()
long_age_saeao <- apc_web_data_ass_SAEAO_output[["LongAge"]] %>% as.data.frame()
long_age_ssa <- apc_web_data_ass_SSA_output[["LongAge"]] %>% as.data.frame()

long_age_global_att <- apc_web_data_att_global_output[["LongAge"]] %>% as.data.frame()
long_age_ceeeca_att <- apc_web_data_att_CEEECA_output[["LongAge"]] %>% as.data.frame()
long_age_hi_att <- apc_web_data_att_HI_output[["LongAge"]] %>% as.data.frame()
long_age_lac_att <- apc_web_data_att_LAC_output[["LongAge"]] %>% as.data.frame()
long_age_name_att <- apc_web_data_att_NAME_output[["LongAge"]] %>% as.data.frame()
long_age_sa_att <- apc_web_data_att_SA_output[["LongAge"]] %>% as.data.frame()
long_age_saeao_att <- apc_web_data_att_SAEAO_output[["LongAge"]] %>% as.data.frame()
long_age_ssa_att <- apc_web_data_att_SSA_output[["LongAge"]] %>% as.data.frame()

# Helper to compute combined y-range across ASS and ATT
get_data_range <- function(ass_data, att_data) {
  min_val <- min(c(ass_data$CILo, att_data$CILo), na.rm = TRUE)
  max_val <- max(c(ass_data$CIHi, att_data$CIHi), na.rm = TRUE)
  return(c(min_val, max_val))
}

# Get ranges for all regions
ranges <- list(
  global = get_data_range(long_age_global, long_age_global_att),
  ceeeca = get_data_range(long_age_ceeeca, long_age_ceeeca_att),
  hi = get_data_range(long_age_hi, long_age_hi_att),
  lac = get_data_range(long_age_lac, long_age_lac_att),
  name = get_data_range(long_age_name, long_age_name_att),
  sa = get_data_range(long_age_sa, long_age_sa_att),
  saeao = get_data_range(long_age_saeao, long_age_saeao_att),
  ssa = get_data_range(long_age_ssa, long_age_ssa_att)
)

# Compute overall min/max for y-axis
y_min <- floor(min(sapply(ranges, function(x) x[1]), na.rm = TRUE) / 50) * 50
y_max <- ceiling(max(sapply(ranges, function(x) x[2]), na.rm = TRUE) / 50) * 50

# Print the overall range for reference
print(paste("Overall range:", y_min, "to", y_max))

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
# Plotting helper for a single panel
plot_curve <- function(data_ass, data_att, title, show_legend = FALSE) {
  # Add grouping labels
  data_ass$Group <- "Associated with CR"
  data_att$Group <- "Attributable to CR"
  
  # Combine ASS and ATT data
  combined_data <- rbind(data_ass, data_att)
  
  p <- ggplot(data = combined_data, aes(x = Age, y = Rate, color = Group, shape = Group)) +
    geom_line(size = 0.8) +
    geom_point(size = 2) +
    geom_ribbon(aes(ymin = CILo, ymax = CIHi, fill = Group), alpha = 0.1, color = NA) +
    scale_color_manual(values = lancet_colors, name = "Counterfactual") +
    scale_shape_manual(values = point_shapes, name = "Counterfactual") +
    scale_fill_manual(values = lancet_colors, name = "Counterfactual") +
    scale_x_continuous(breaks = seq(0, 100, by = 20)) +
    scale_y_continuous(limits = c(y_min, y_max),
                       breaks = seq(y_min, y_max, by = 500)) +
    labs(title = title) +
    common_theme +
    theme(legend.position = if(show_legend) "right" else "none")
  
  return(p)
}

# Create a tiny plot to extract a shared legend
legend_plot <- plot_curve(data_ass = data.frame(Age = 1:2, 
                                                Rate = c(1,2),
                                                CILo = c(0.5,1.5),
                                                CIHi = c(1.5,2.5)),
                          data_att = data.frame(Age = 1:2,
                                                Rate = c(3,4),
                                                CILo = c(2.5,3.5),
                                                CIHi = c(3.5,4.5)),
                          title = "",
                          show_legend = TRUE) +
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

# Build panels for each geography
plot_global <- plot_curve(long_age_global, long_age_global_att, "Global")
plot_ceeeca <- plot_curve(long_age_ceeeca, long_age_ceeeca_att, 
                          "Central Europe, Eastern\nEurope, and Central Asia")
plot_hi <- plot_curve(long_age_hi, long_age_hi_att, "High-income")
plot_lac <- plot_curve(long_age_lac, long_age_lac_att, "Latin America\nand Caribbean")
plot_name <- plot_curve(long_age_name, long_age_name_att, "North Africa\nand Middle East")
plot_sa <- plot_curve(long_age_sa, long_age_sa_att, "South Asia")
plot_saeao <- plot_curve(long_age_saeao, long_age_saeao_att, 
                         "Southeast Asia, East Asia,\nand Oceania")
plot_ssa <- plot_curve(long_age_ssa, long_age_ssa_att, "Sub-Saharan Africa")

# Layout with legend on the right
layout <- "
AAAAAAAAB
AAAAAAAAB
AAAAAAAAB
AAAAAAAAB
"

# Assemble plot grid and legend
final_plot <- wrap_plots(
  wrap_plots(plot_global, plot_ceeeca, plot_hi, plot_lac,
             plot_name, plot_sa, plot_saeao, plot_ssa,
             ncol = 4),
  shared_legend,
  design = layout
)

# Ensure white background
final_plot <- final_plot & 
  theme(plot.background = element_rect(fill = "white", color = NA))

# Save figure (mortality rate curves)
ggsave("mortality_rates.pdf", final_plot, width = 15, height = 8)

# Save figure (cohort rate ratio) – reusing layout for consistency
ggsave("cohort_rate_ratio.pdf", final_plot, width = 15, height = 8)