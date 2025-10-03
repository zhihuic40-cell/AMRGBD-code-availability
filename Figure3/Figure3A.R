# Load required packages
library(ggplot2)
library(dplyr)
library(patchwork)
library(ggpubr)
library(cowplot)

# Load RDS data files
# Global
apc_web_data_ass_global_output <- readRDS(file.path("data", "apc_web_data_ass_global_output.rds"))
apc_web_data_att_global_output <- readRDS(file.path("data", "apc_web_data_att_global_output.rds"))

# CEEECA (Central Europe, Eastern Europe, and Central Asia)
apc_web_data_ass_CEEECA_output <- readRDS(file.path("data", "apc_web_data_ass_CEEECA_output.rds"))
apc_web_data_att_CEEECA_output <- readRDS(file.path("data", "apc_web_data_att_CEEECA_output.rds"))

# HI (High-income)
apc_web_data_ass_HI_output <- readRDS(file.path("data", "apc_web_data_ass_HI_output.rds"))
apc_web_data_att_HI_output <- readRDS(file.path("data", "apc_web_data_att_HI_output.rds"))

# LAC (Latin America and Caribbean)
apc_web_data_ass_LAC_output <- readRDS(file.path("data", "apc_web_data_ass_LAC_output.rds"))
apc_web_data_att_LAC_output <- readRDS(file.path("data", "apc_web_data_att_LAC_output.rds"))

# NAME (North Africa and Middle East)
apc_web_data_ass_NAME_output <- readRDS(file.path("data", "apc_web_data_ass_NAME_output.rds"))
apc_web_data_att_NAME_output <- readRDS(file.path("data", "apc_web_data_att_NAME_output.rds"))

# SA (South Asia)
apc_web_data_ass_SA_output <- readRDS(file.path("data", "apc_web_data_ass_SA_output.rds"))
apc_web_data_att_SA_output <- readRDS(file.path("data", "apc_web_data_att_SA_output.rds"))

# SAEAO (Southeast Asia, East Asia, and Oceania)
apc_web_data_ass_SAEAO_output <- readRDS(file.path("data", "apc_web_data_ass_SAEAO_output.rds"))
apc_web_data_att_SAEAO_output <- readRDS(file.path("data", "apc_web_data_att_SAEAO_output.rds"))

# SSA (Sub-Saharan Africa)
apc_web_data_ass_SSA_output <- readRDS(file.path("data", "apc_web_data_ass_SSA_output.rds"))
apc_web_data_att_SSA_output <- readRDS(file.path("data", "apc_web_data_att_SSA_output.rds"))

# Extract data from loaded RDS files
# Global
Local_gASS <- apc_web_data_ass_global_output[["LocalDrifts"]] %>% as.data.frame()
Net_gASS <- apc_web_data_ass_global_output[["NetDrift"]] %>% as.data.frame()
Local_gATT <- apc_web_data_att_global_output[["LocalDrifts"]] %>% as.data.frame()
Net_gATT <- apc_web_data_att_global_output[["NetDrift"]] %>% as.data.frame()

# CEEECA (Central Europe, Eastern Europe, and Central Asia)
Local_ceecaASS <- apc_web_data_ass_CEEECA_output[["LocalDrifts"]] %>% as.data.frame()
Net_ceecaASS <- apc_web_data_ass_CEEECA_output[["NetDrift"]] %>% as.data.frame()
Local_ceecaATT <- apc_web_data_att_CEEECA_output[["LocalDrifts"]] %>% as.data.frame()
Net_ceecaATT <- apc_web_data_att_CEEECA_output[["NetDrift"]] %>% as.data.frame()

# HI (High-income)
Local_hiASS <- apc_web_data_ass_HI_output[["LocalDrifts"]] %>% as.data.frame()
Net_hiASS <- apc_web_data_ass_HI_output[["NetDrift"]] %>% as.data.frame()
Local_hiATT <- apc_web_data_att_HI_output[["LocalDrifts"]] %>% as.data.frame()
Net_hiATT <- apc_web_data_att_HI_output[["NetDrift"]] %>% as.data.frame()

# LAC (Latin America and Caribbean)
Local_lacASS <- apc_web_data_ass_LAC_output[["LocalDrifts"]] %>% as.data.frame()
Net_lacASS <- apc_web_data_ass_LAC_output[["NetDrift"]] %>% as.data.frame()
Local_lacATT <- apc_web_data_att_LAC_output[["LocalDrifts"]] %>% as.data.frame()
Net_lacATT <- apc_web_data_att_LAC_output[["NetDrift"]] %>% as.data.frame()

# NAME (North Africa and Middle East)
Local_nameASS <- apc_web_data_ass_NAME_output[["LocalDrifts"]] %>% as.data.frame()
Net_nameASS <- apc_web_data_ass_NAME_output[["NetDrift"]] %>% as.data.frame()
Local_nameATT <- apc_web_data_att_NAME_output[["LocalDrifts"]] %>% as.data.frame()
Net_nameATT <- apc_web_data_att_NAME_output[["NetDrift"]] %>% as.data.frame()

# SA (South Asia)
Local_saASS <- apc_web_data_ass_SA_output[["LocalDrifts"]] %>% as.data.frame()
Net_saASS <- apc_web_data_ass_SA_output[["NetDrift"]] %>% as.data.frame()
Local_saATT <- apc_web_data_att_SA_output[["LocalDrifts"]] %>% as.data.frame()
Net_saATT <- apc_web_data_att_SA_output[["NetDrift"]] %>% as.data.frame()

# SAEAO (Southeast Asia, East Asia, and Oceania)
Local_seaoASS <- apc_web_data_ass_SAEAO_output[["LocalDrifts"]] %>% as.data.frame()
Net_seaoASS <- apc_web_data_ass_SAEAO_output[["NetDrift"]] %>% as.data.frame()
Local_seaoATT <- apc_web_data_att_SAEAO_output[["LocalDrifts"]] %>% as.data.frame()
Net_seaoATT <- apc_web_data_att_SAEAO_output[["NetDrift"]] %>% as.data.frame()

# SSA (Sub-Saharan Africa)
Local_ssaASS <- apc_web_data_ass_SSA_output[["LocalDrifts"]] %>% as.data.frame()
Net_ssaASS <- apc_web_data_ass_SSA_output[["NetDrift"]] %>% as.data.frame()
Local_ssaATT <- apc_web_data_att_SSA_output[["LocalDrifts"]] %>% as.data.frame()
Net_ssaATT <- apc_web_data_att_SSA_output[["NetDrift"]] %>% as.data.frame()

# Define mapping of all regions with their data and titles
regions <- list(
  list(name = "global", title = "Global", 
       ass = Local_gASS, att = Local_gATT),
  list(name = "ceeca", title = "Central Europe, Eastern Europe, and Central Asia", 
       ass = Local_ceecaASS, att = Local_ceecaATT),
  list(name = "hi", title = "High-income", 
       ass = Local_hiASS, att = Local_hiATT),
  list(name = "lac", title = "Latin America and Caribbean", 
       ass = Local_lacASS, att = Local_lacATT),
  list(name = "name", title = "North Africa and Middle East", 
       ass = Local_nameASS, att = Local_nameATT),
  list(name = "sa", title = "South Asia", 
       ass = Local_saASS, att = Local_saATT),
  list(name = "seao", title = "Southeast Asia, East Asia, and Oceania", 
       ass = Local_seaoASS, att = Local_seaoATT),
  list(name = "ssa", title = "Sub-Saharan Africa", 
       ass = Local_ssaASS, att = Local_ssaATT)
)

# Define Lancet-style colors and point shapes
lancet_colors <- c("Associated with CR" = "#00468B", "Attributable to CR" = "#ED0000")
point_shapes <- c("Associated with CR" = 16, "Attributable to CR" = 17)

# Define common theme
common_theme <- theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold", color = "black"),
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12, face = "bold"),
    legend.key.size = unit(1, "cm"),
    axis.ticks.length = unit(0.15, "cm"),
    axis.ticks = element_line(color = "black", size = 0.5),
    axis.line = element_line(color = "black", size = 0.5)
  )

# Create plot function
create_plot <- function(data, title, position) {
  # Determine whether to show axis titles based on position
  axis_theme <- theme()
  if (!grepl("left", position)) {
    axis_theme <- axis_theme + theme(axis.title.y = element_blank())
  }
  if (!grepl("bottom", position)) {
    axis_theme <- axis_theme + theme(axis.title.x = element_blank())
  }
  
  ggplot(data=data, aes(x=Age, y=`Percent per Year`, color=Group, shape=Group)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = 20, linetype = "dashed", color = "grey50") +
    geom_ribbon(aes(ymin=CILo, ymax=CIHi, fill=Group), alpha=0.05, color=NA) +
    geom_line(size=1) + 
    geom_point(size=2) +
    scale_color_manual(values = lancet_colors) +
    scale_shape_manual(values = point_shapes) +
    scale_fill_manual(values = lancet_colors) +
    scale_y_continuous(breaks = seq(-6, 6, by = 2)) +
    coord_cartesian(ylim = c(-6, 6)) +
    ylab("Annual change of mortality (% per year)") +
    xlab("Age (years)") +
    ggtitle(title) +
    common_theme +
    theme(legend.position = "none") +
    axis_theme
}

# Create all plots
plots <- list()
for (i in 1:length(regions)) {
  position <- c()
  if (i %in% c(1, 5)) position <- c(position, "left")
  if (i %in% 5:8) position <- c(position, "bottom")
  position <- paste(position, collapse = "_")
  if (position == "") position <- "center"
  
  regions[[i]]$ass$Group <- "Associated with CR"
  regions[[i]]$att$Group <- "Attributable to CR"
  combined_data <- rbind(regions[[i]]$ass, regions[[i]]$att)
  plots[[i]] <- create_plot(combined_data, regions[[i]]$title, position)
}

# Create sample data frame with two groups for legend
legend_data <- data.frame(
  Age = rep(1:2, 2),
  Percent_per_Year = c(1, 2, 3, 4),
  Group = rep(c("Associated with CR", "Attributable to CR"), each = 2)
)

# Create a complete example plot for legend extraction
legend_base <- ggplot(legend_data, 
                      aes(x = Age, y = Percent_per_Year,
                          color = Group, shape = Group)) +
  # Add border and fill color with alpha 0.05 to match main plots
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = Group), 
            alpha = 0.05, color = NA) +  
  geom_line() +
  geom_point(size = 3) +
  scale_color_manual(values = lancet_colors, name = "Counterfactual") +
  scale_shape_manual(values = point_shapes, name = "Counterfactual") +
  scale_fill_manual(values = lancet_colors, name = "Counterfactual") +
  theme(
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12, face = "bold"),
    legend.key.size = unit(1, "cm"),
    legend.spacing.y = unit(0.2, "cm"),
    legend.box.margin = margin(0, 0, 0, 12),
    legend.key = element_rect(color = NA, fill = NA),
    legend.key.height = unit(1, "cm"),
    legend.key.width = unit(1, "cm"),
    legend.background = element_blank(),
    panel.background = element_blank()
  )

# Extract legend
shared_legend <- cowplot::get_legend(legend_base)

# Create top and bottom rows of subplots
top_row <- plots[[1]] | plots[[2]] | plots[[3]] | plots[[4]]
bottom_row <- plots[[5]] | plots[[6]] | plots[[7]] | plots[[8]]

# Combine all elements
final_plot <- wrap_plots(
  wrap_plots(top_row, bottom_row, ncol = 1),
  shared_legend,
  ncol = 2,
  widths = c(8, 0.8)
)

# Save figure
ggsave("mortality_trends.pdf", final_plot, width = 22, height = 10, dpi = 300, bg = "white")