# Load required packages
library(ggplot2)
library(dplyr)

# Build age curve data for each pathogen
long_age_aba_att <- apc_web_data_ass_aba_output[["LongAge"]] %>% as.data.frame()
long_age_cit_att <- apc_web_data_ass_cit_output[["LongAge"]] %>% as.data.frame()
long_age_ent_att <- apc_web_data_ass_ent_output[["LongAge"]] %>% as.data.frame()
long_age_eco_att <- apc_web_data_ass_eco_output[["LongAge"]] %>% as.data.frame()
long_age_kpn_att <- apc_web_data_ass_kpn_output[["LongAge"]] %>% as.data.frame()
long_age_pae_att <- apc_web_data_ass_pae_output[["LongAge"]] %>% as.data.frame()
long_age_ser_att <- apc_web_data_ass_ser_output[["LongAge"]] %>% as.data.frame()

# Helper to get y-range
get_data_range <- function(data) {
  min_val <- min(data$CILo, na.rm = TRUE)
  max_val <- max(data$CIHi, na.rm = TRUE)
  return(c(min_val, max_val))
}

# Compute overall range across pathogens
all_data_ranges <- list(
  get_data_range(long_age_aba_att),
  get_data_range(long_age_cit_att),
  get_data_range(long_age_ent_att),
  get_data_range(long_age_eco_att),
  get_data_range(long_age_kpn_att),
  get_data_range(long_age_pae_att),
  get_data_range(long_age_ser_att)
)

# Compute overall min/max (rounded to nearest 25)
overall_min <- floor(min(sapply(all_data_ranges, function(x) x[1])) / 25) * 25  # use multiples of 25
overall_max <- ceiling(max(sapply(all_data_ranges, function(x) x[2])) / 25) * 25  # use multiples of 25

# Choose reasonable tick interval
range_size <- overall_max - overall_min
n_breaks <- 12  # 12 ticks
interval <- ceiling(range_size / n_breaks / 25) * 25  # multiples of 25
y_breaks <- seq(overall_min, overall_max, by = interval)

# If too few ticks remain, relax to multiples of 10
if(length(y_breaks) < 10) {
  interval <- ceiling(range_size / 15 / 10) * 10
  y_breaks <- seq(overall_min, overall_max, by = interval)
}

# Map pathogen names to data
pathogens <- list(
  list(name = "Acinetobacter baumannii", data = long_age_aba_att),
  list(name = "Citrobacter spp.", data = long_age_cit_att),
  list(name = "Enterobacter spp.", data = long_age_ent_att),
  list(name = "Escherichia coli", data = long_age_eco_att),
  list(name = "Klebsiella pneumoniae", data = long_age_kpn_att),
  list(name = "Pseudomonas aeruginosa", data = long_age_pae_att),
  list(name = "Serratia spp.", data = long_age_ser_att)
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
  ggplot(data=data, aes(x=Age, y=Rate)) +
    geom_ribbon(aes(ymin=CILo, ymax=CIHi), alpha=0.05, fill=lancet_colors) +
    geom_line(size=0.8, color=lancet_colors) + 
    geom_point(size=2, color=lancet_colors) +
    scale_x_continuous(breaks = seq(0, 100, by = 20)) +
    scale_y_continuous(limits = c(overall_min, overall_max),
                       breaks = y_breaks) +
    labs(title = title,
         x = "Age (years)",
         y = "Mortality rate (per 100,000 population)") +
    common_theme
}

# 为每个病原体创建和保存图形
for (i in 1:length(pathogens)) {
  # 创建图形
  p <- create_plot(pathogens[[i]]$data, pathogens[[i]]$name)
  
  # 创建干净的文件名（移除特殊字符）
  clean_name <- gsub(" ", "", pathogens[[i]]$name)
  clean_name <- gsub("\\.", "", clean_name)
  filename <- paste0("PMTWO", i, clean_name, ".pdf")
  
  # 保存图形
  ggsave(filename, p, width = 10.22, height = 6.81)
  
  # 在环境中保存图形对象
  assign(paste0("PMTWO", i, clean_name), p)
}

# 显示所有图形
PMTWO1Acinetobacterbaumannii
PMTWO2Citrobacterspp
PMTWO3Enterobacterspp
PMTWO4Escherichiacoli
PMTWO5Klebsiellapneumoniae
PMTWO6Pseudomonasaeruginosa
PMTWO7Serratiaspp

