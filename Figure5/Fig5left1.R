# Load required packages
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)
library(RColorBrewer)
# Set working directory (consider using relative paths in production)
setwd("F:/BaiduSyncdisk/AMRGBD/第一篇/图表改/图5/代码及数据ASS")
# Read Excel file
file_path <- file.path("data", "ASSproportion.xlsx")
data <- read_excel(file_path)
write.csv(data,file="data1.csv")
# Define new age-group recoding function
new_age_group <- function(age) {
  case_when(
    age %in% c('Neonatal') ~ 'Neonatal',
    age %in% c('Post neonatal to 5') ~ 'Post Neonatal to 5 years',
    age %in% c('5 to 49') ~ '5-49 years',
    age %in% c('50 to 69') ~ '50-69 years',
    age %in% c('70+ years') ~ '≥70 years' 
  )
}
unique(data$age_name)
# Define the display order of age groups
age_order <- c('≥70 years',
               '50-69 years',
               '5-49 years',
               'Post Neonatal to 5 years',
               'Neonatal')

# Define target pathogens
path<- c("Acinetobacter baumannii", "Citrobacter spp.", "Enterobacter spp.", 
              "Escherichia coli", "Klebsiella pneumoniae", "Pseudomonas aeruginosa", 
              "Serratia spp.", "Streptococcus pneumoniae")

# Filter data and recode age groups
data_filtered <- data %>%
  dplyr::filter(age_name != 'All ages',
                Pathogen %in% path) %>%
  mutate(new_age_group = new_age_group(age_name)) %>%
  dplyr::filter(new_age_group != 'Other')

# Compute proportions within pathogen-year
data_filtered <- data_filtered %>%
  group_by(Pathogen, year, new_age_group) %>%
  summarise(val = sum(val), .groups = 'drop')

total_data <- data_filtered %>%
  group_by(Pathogen, year) %>%
  summarise(total_val = sum(val), .groups = 'drop')

merged_data <- data_filtered %>%
  left_join(total_data, by = c("Pathogen", "year")) %>%
  mutate(proportion = val / total_val * 100,
         new_age_group = factor(new_age_group, levels = age_order, ordered = TRUE))

# Define Lancet-style palette
lancet_colors <- brewer.pal(6, "Blues")

# Years to show on the x-axis
years_to_show <- c(1990, 2000, 2010, 2020)

# Acinetobacter baumannii
PF1aba <- merged_data %>% 
  dplyr::filter(Pathogen == "Acinetobacter baumannii") %>%
  ggplot(aes(x = factor(year), y = proportion, fill = new_age_group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = lancet_colors) +
  scale_x_discrete(breaks = years_to_show) +
  labs(x = "Year",
       y = "Proportion of deaths (%)",
       fill = "Age groups",
       title = "Acinetobacter baumannii") +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
    axis.text.x = element_text(margin = margin(t = 5), face = "bold", size = 12),
    axis.text.y = element_text(margin = margin(r = 5), face = "bold", size = 12),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(3, "pt"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(face = "bold", size = 12)
  ) +
  coord_cartesian(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

# Citrobacter spp.
PF1cit <- merged_data %>% 
  dplyr::filter(Pathogen == "Citrobacter spp.") %>%
  ggplot(aes(x = factor(year), y = proportion, fill = new_age_group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = lancet_colors) +
  scale_x_discrete(breaks = years_to_show) +
  labs(x = "Year",
       y = "Proportion of deaths (%)",
       fill = "Age groups",
       title = "Citrobacter spp.") +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
    axis.text.x = element_text(margin = margin(t = 5), face = "bold", size = 12),
    axis.text.y = element_text(margin = margin(r = 5), face = "bold", size = 12),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(3, "pt"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(face = "bold", size = 12)
  ) +
  coord_cartesian(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

# Enterobacter spp.
PF1ent <- merged_data %>% 
  dplyr::filter(Pathogen == "Enterobacter spp.") %>%
  ggplot(aes(x = factor(year), y = proportion, fill = new_age_group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = lancet_colors) +
  scale_x_discrete(breaks = years_to_show) +
  labs(x = "Year",
       y = "Proportion of deaths (%)",
       fill = "Age groups",
       title = "Enterobacter spp.") +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
    axis.text.x = element_text(margin = margin(t = 5), face = "bold", size = 12),
    axis.text.y = element_text(margin = margin(r = 5), face = "bold", size = 12),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(3, "pt"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(face = "bold", size = 12)
  ) +
  coord_cartesian(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

# Escherichia coli
PF1eco <- merged_data %>% 
  dplyr::filter(Pathogen == "Escherichia coli") %>%
  ggplot(aes(x = factor(year), y = proportion, fill = new_age_group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = lancet_colors) +
  scale_x_discrete(breaks = years_to_show) +
  labs(x = "Year",
       y = "Proportion of deaths (%)",
       fill = "Age groups",
       title = "Escherichia coli") +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
    axis.text.x = element_text(margin = margin(t = 5), face = "bold", size = 12),
    axis.text.y = element_text(margin = margin(r = 5), face = "bold", size = 12),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(3, "pt"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(face = "bold", size = 12)
  ) +
  coord_cartesian(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

# Klebsiella pneumoniae
PF1kpn <- merged_data %>% 
  dplyr::filter(Pathogen == "Klebsiella pneumoniae") %>%
  ggplot(aes(x = factor(year), y = proportion, fill = new_age_group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = lancet_colors) +
  scale_x_discrete(breaks = years_to_show) +
  labs(x = "Year",
       y = "Proportion of deaths (%)",
       fill = "Age groups",
       title = "Klebsiella pneumoniae") +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
    axis.text.x = element_text(margin = margin(t = 5), face = "bold", size = 12),
    axis.text.y = element_text(margin = margin(r = 5), face = "bold", size = 12),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(3, "pt"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(face = "bold", size = 12)
  ) +
  coord_cartesian(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

# Pseudomonas aeruginosa
PF1pae <- merged_data %>% 
  dplyr::filter(Pathogen == "Pseudomonas aeruginosa") %>%
  ggplot(aes(x = factor(year), y = proportion, fill = new_age_group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = lancet_colors) +
  scale_x_discrete(breaks = years_to_show) +
  labs(x = "Year",
       y = "Proportion of deaths (%)",
       fill = "Age groups",
       title = "Pseudomonas aeruginosa") +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
    axis.text.x = element_text(margin = margin(t = 5), face = "bold", size = 12),
    axis.text.y = element_text(margin = margin(r = 5), face = "bold", size = 12),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(3, "pt"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(face = "bold", size = 12)
  ) +
  coord_cartesian(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

# Serratia spp.
PF1ser <- merged_data %>% 
  dplyr::filter(Pathogen == "Serratia spp.") %>%
  ggplot(aes(x = factor(year), y = proportion, fill = new_age_group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = lancet_colors) +
  scale_x_discrete(breaks = years_to_show) +
  labs(x = "Year",
       y = "Proportion of deaths (%)",
       fill = "Age groups",
       title = "Serratia spp.") +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
    axis.text.x = element_text(margin = margin(t = 5), face = "bold", size = 12),
    axis.text.y = element_text(margin = margin(r = 5), face = "bold", size = 12),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(3, "pt"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(face = "bold", size = 12)
  ) +
  coord_cartesian(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

# Streptococcus pneumoniae
PF1spn <- merged_data %>% 
  dplyr::filter(Pathogen == "Streptococcus pneumoniae") %>%
  ggplot(aes(x = factor(year), y = proportion, fill = new_age_group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = lancet_colors) +
  scale_x_discrete(breaks = years_to_show) +
  labs(x = "Year",
       y = "Proportion of deaths (%)",
       fill = "Age groups",
       title = "Streptococcus pneumoniae") +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
    axis.text.x = element_text(margin = margin(t = 5), face = "bold", size = 12),
    axis.text.y = element_text(margin = margin(r = 5), face = "bold", size = 12),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(3, "pt"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(face = "bold", size = 12)
  ) +
  coord_cartesian(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

# Show all plots
PF1aba
PF1cit
PF1ent
PF1eco
PF1kpn
PF1pae
PF1ser
PF1spn

# Save all plots as PDF with specified dimensions
ggsave("PF1aba.pdf", PF1aba, width = 10.22, height = 6.81)
ggsave("PF1cit.pdf", PF1cit, width = 10.22, height = 6.81)
ggsave("PF1ent.pdf", PF1ent, width = 10.22, height = 6.81)
ggsave("PF1eco.pdf", PF1eco, width = 10.22, height = 6.81)
ggsave("PF1kpn.pdf", PF1kpn, width = 10.22, height = 6.81)
ggsave("PF1pae.pdf", PF1pae, width = 10.22, height = 6.81)
ggsave("PF1ser.pdf", PF1ser, width = 10.22, height = 6.81)
ggsave("PF1spn.pdf", PF1spn, width = 10.22, height = 6.81)


