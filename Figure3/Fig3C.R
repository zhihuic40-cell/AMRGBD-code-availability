# 加载必要的包
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)
library(RColorBrewer)
library(cowplot)
library(export)

# 读取Excel文件
file_path <- file.path("data", "ATTproportion.xlsx")
data <- read_excel(file_path)

# 查看数据结构
head(data)
colnames(data)
unique(data$location_name)

# 定义新的年龄组
new_age_group <- function(age) {
  case_when(
    age %in% c('Neonatal') ~ 'Neonatal',
    age %in% c('Post Neonatal to 5') ~ 'Post Neonatal to 5 years',
    age %in% c('5 to 49') ~ '5-49 years',
    age %in% c('50-69 years') ~ '50-69 years',
    age %in% c('70+ years') ~ '≥70 years' 
  )
}

# 定义年龄组的正确顺序
age_order <- c('≥70 years',
               '50-69 years',
               '5-49 years',
               'Post Neonatal to 5 years',
               'Neonatal')

# 过滤掉"全部年龄"组并重新分类年龄组
data_filtered <- data %>%
  dplyr::filter(age_name != 'All ages') %>%
  mutate(new_age_group = new_age_group(age_name)) %>%
  dplyr::filter(new_age_group != 'Other')

# 按照新年龄组进行分组计算每年的比例
data_filtered <- data_filtered %>%
  group_by(location_name, year, new_age_group) %>%
  summarise(val = sum(val), .groups = 'drop')

total_data <- data_filtered %>%
  group_by(location_name, year) %>%
  summarise(total_val = sum(val), .groups = 'drop')

merged_data <- data_filtered %>%
  left_join(total_data, by = c("location_name", "year")) %>%
  mutate(proportion = val / total_val * 100,
         new_age_group = factor(new_age_group, levels = age_order, ordered = TRUE))

# 定义柳叶刀风格的配色方案
lancet_colors <- brewer.pal(6, "Blues")

# 定义要显示的年份
years_to_show <- c(1990, 2000, 2010, 2020)

# 绘图，分别绘制不同 location_name 的图
unique_locations <- unique(merged_data$location_name)
plots <- list()

for (location in unique_locations) {
  location_data <- merged_data %>% dplyr::filter(location_name == location)
  
  base_plot <- ggplot(location_data, aes(x = factor(year), y = proportion, fill = new_age_group)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = lancet_colors) +
    scale_x_discrete(breaks = years_to_show) +
    labs(x = NULL,
         y = NULL,
         fill = "Age groups") +
    ggtitle(location) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.line = element_line(colour = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(margin = margin(t = 5), face = "bold", size = 12),
      axis.text.y = element_text(margin = margin(r = 5), face = "bold", size = 12),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(3, "pt"),
      # Add these new lines for outward ticks
      axis.ticks.x = element_line(color = "black"),
      axis.ticks.y = element_line(color = "black"),
      axis.ticks.length.x = unit(0.15, "cm"),  # Negative value for outward ticks
      axis.ticks.length.y = unit(0.15, "cm"),  # Negative value for outward ticks
      axis.text.x.top = element_blank(),
      axis.text.y.right = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      panel.spacing = unit(0.1, "lines")
    ) +
    coord_cartesian(clip = "off") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
  
  plots[[location]] <- base_plot
}

# 手动调整位置顺序
location_order <- c("Global", "Central Europe, Eastern Europe, and Central Asia", "High-income", "Latin America and Caribbean", "North Africa and Middle East", "South Asia", "Southeast Asia, East Asia, and Oceania", "Sub-Saharan Africa")
adjusted_plots <- plots[location_order]

# 创建一个3x2的组图，不包含图例
combined_plot <- plot_grid(
  plotlist = lapply(adjusted_plots, function(x) x + theme(legend.position = "none")),
  ncol = 4,
  nrow = 2,
  align = "v",
  rel_widths = c(1, 1, 1, 1),
  rel_heights = c(1.2, 1.2)
)

# 提取图例并设置文本为粗体，增加文字大小
legend <- get_legend(
  ggplot(merged_data, aes(x = factor(year), y = proportion, fill = new_age_group)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = lancet_colors) +
    labs(fill = "Age groups") +
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.title = element_text(face = "bold", size = 12),    # 增加图例标题大小
      legend.text = element_text(face = "bold", size = 12),     # 增加图例文本大小
      legend.margin = margin(l = 0, r = 10, t = 0, b = 0)  # 调整图例边距
    )
)

# 在组合图右侧添加图例并调整距离 - 修改rel_widths比例使图例更靠近图片
combined_with_legend <- plot_grid(
  combined_plot,
  legend,
  ncol = 2,
  rel_widths = c(4.5, 0.8),  # 调整这里的比例让图例更靠近主图
  align = "h"
)

# 在组合图左侧添加粗体纵坐标标注
final_plot <- plot_grid(
  ggdraw() + draw_label("Proportion of deaths (%)", 
                        angle = 90, 
                        x = 0.5, 
                        y = 0.5, 
                        vjust = 0.5,
                        fontface = "bold",
                        size = 14),  # 增加标签文字大小
  combined_with_legend,
  ncol = 2,
  rel_widths = c(0.05, 1)
)

# 添加粗体横坐标标注"Year"
final_plot_with_x <- plot_grid(
  final_plot,
  ggdraw() + draw_label("Year", 
                        x = 0.5, 
                        hjust = 0.5,
                        fontface = "bold",
                        size = 14),  # 增加标签文字大小
  ncol = 1,
  rel_heights = c(1, 0.05)
)

# 使用 export 包将图表保存为 PPT 格式
graph2ppt(x = final_plot_with_x, file = "C:/Users/czhde/Desktop/Fig3B.pptx", width = 18/2.54, height = 10/2.54)