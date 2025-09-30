# 设置工作路径
setwd("F:/BaiduSyncdisk/AMRGBD/第一篇/图表改2019/图2地图/data")
# 加载包
library(sf)
library(patchwork)
library(ggplot2)
library(tidyverse)
library(here)
library(export)
library(here)

# 读取数据
GBD <- read.csv("att-ndrift.csv", header = TRUE)
location <- read.csv("Location.csv")
GBD <- left_join(GBD, location, by = "location_name")
colnames(GBD)
unique(GBD$Measure)
GBDASR <- GBD %>% dplyr::filter(Measure == "Deaths") %>% 
  dplyr::filter(Metric == "Rate (per 100k)") %>% 
  dplyr::filter(Age == "Age-standardized") %>% 
  dplyr::filter(Year == 2021)

# shp数据的读取与配置
map <- st_read("世界国家.shp")
map <- st_set_crs(map, 4326)

## 绘制数据读取
main_map_data <- left_join(map, GBD, by = c("NAME" = "location3")) %>% 
  dplyr::filter(Measure == "Deaths") %>% 
  dplyr::filter(Metric == "Rate (per 100k)") %>% 
  dplyr::filter(Age == "Age-standardized") %>% 
  dplyr::filter(Year == 2021)

head(main_map_data)

# 计算图例范围值（分位数）
breaks <- quantile(main_map_data$val, probs = seq(0, 1, length.out = 6), na.rm = TRUE)
labels <- sprintf("%.2f to %.2f", breaks[-length(breaks)], breaks[-1])

# 将数值变量转换为因子
main_map_data$val_cat <- cut(main_map_data$val, 
                             breaks = breaks, 
                             include.lowest = TRUE, 
                             labels = labels)

# 新的颜色方案
new_colors <- c("#2f4f4f", "#4682b4", "#b0c4de", "#ff6347", "#8b0000")

# 绘制主图
p <- main_map_data %>%
  ggplot() +
  geom_sf(aes(group = NAME, fill = val_cat), color = 'black', size = 0.5) +
  theme_void() +
  scale_fill_manual(values = new_colors, 
                    name = "ASR",
                    labels = labels) +
  labs(x = "", y = "", title = "") +
  theme(legend.position = c(0.1, 0.2),
        legend.title = element_text(color = "black", size = 10),
        plot.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 10),
        panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

p

# 绘制小图前准备
worldData <- map_data('world')
small_map_data <- GBD %>% 
  dplyr::filter(Measure == "Deaths") %>% 
  dplyr::filter(Metric == "Rate (per 100k)") %>% 
  dplyr::filter(Age == "Age-standardized") %>% 
  dplyr::filter(Year == 2021)

# 更新国家名称映射
small_map_data$location[small_map_data$location_name == 'United States of America'] <- 'USA'
small_map_data$location_name[small_map_data$location_name == 'Russian Federation'] <- 'Russia'
small_map_data$location_name[small_map_data$location_name == 'United Kingdom'] <- 'UK'
small_map_data$location_name[small_map_data$location_name == 'Congo'] <- 'Republic of Congo'
small_map_data$location_name[small_map_data$location_name == "Iran (Islamic Republic of)"] <- 'Iran'
small_map_data$location_name[small_map_data$location_name == "Democratic People's Republic of Korea"] <- 'North Korea'
small_map_data$location_name[small_map_data$location_name == "Taiwan (Province of China)"] <- 'Taiwan'
small_map_data$location_name[small_map_data$location_name == "Republic of Korea"] <- 'South Korea'
small_map_data$location_name[small_map_data$location_name == "United Republic of Tanzania"] <- 'Tanzania'
small_map_data$location_name[small_map_data$location_name == "Bolivia (Plurinational State of)"] <- 'Bolivia'
small_map_data$location_name[small_map_data$location_name == "Venezuela (Bolivarian Republic of)"] <- 'Venezuela'
small_map_data$location_name[small_map_data$location_name == "Czechia"] <- 'Czech Republic'
small_map_data$location_name[small_map_data$location_name == "Republic of Moldova"] <- 'Moldova'
small_map_data$location_name[small_map_data$location_name == "Viet Nam"] <- 'Vietnam'
small_map_data$location_name[small_map_data$location_name == "Lao People's Democratic Republic"] <- 'Laos'
small_map_data$location_name[small_map_data$location_name == "Syrian Arab Republic"] <- 'Syria'
small_map_data$location_name[small_map_data$location_name == "North Macedonia"] <- 'Macedonia'
small_map_data$location_name[small_map_data$location_name == "Micronesia (Federated States of)"] <- 'Micronesia'
small_map_data$location_name[small_map_data$location_name == "Macedonia"] <- 'North Macedonia'
small_map_data$location_name[small_map_data$location_name == "Trinidad and Tobago"] <- 'Trinidad'
a <- small_map_data[small_map_data$location_name == "Trinidad", ]
a$location_name <- 'Tobago'
small_map_data <- rbind(small_map_data, a)
small_map_data$location_name[small_map_data$location_name == "Cabo Verde"] <- 'Cape Verde'
small_map_data$location_name[small_map_data$location_name == "United States Virgin Islands"] <- 'Virgin Islands'
small_map_data$location_name[small_map_data$location_name == "Antigua and Barbuda"] <- 'Antigu'
a <- small_map_data[small_map_data$location_name == "Antigu", ]
a$location_name <- 'Barbuda'
small_map_data <- rbind(small_map_data, a)
small_map_data$location_name[small_map_data$location_name == "Saint Kitts and Nevis"] <- 'Saint Kitts'
a <- small_map_data[small_map_data$location_name == "Saint Kitts", ]
a$location_name <- 'Nevis'
small_map_data <- rbind(small_map_data, a)
small_map_data$location_name[small_map_data$location_name == "Côte d'Ivoire"] <- 'Ivory Coast'
small_map_data$location_name[small_map_data$location_name == "Saint Vincent and the Grenadines"] <- 'Saint Vincent'
a <- small_map_data[small_map_data$location_name == "Saint Vincent", ]
a$location_name <- 'Grenadines'
small_map_data <- rbind(small_map_data, a)
small_map_data$location_name[small_map_data$location_name == "Eswatini"] <- 'Swaziland'
small_map_data$location_name[small_map_data$location_name == "Brunei Darussalam"] <- 'Brunei'
small_map_data <- full_join(worldData, small_map_data, by = c('region' = 'location_name')) %>%   
  dplyr::filter(val != "NA")
dim(small_map_data)
head(small_map_data)

# 将小图的数值变量转换为因子
small_map_data$val_cat <- cut(small_map_data$val, 
                              breaks = breaks, 
                              include.lowest = TRUE, 
                              labels = labels)

fig <- small_map_data %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = val_cat),
               colour = "black", size = 0.5) +
  theme_bw() +
  scale_fill_manual(values = new_colors, 
                    name = "ASR") + 
  theme(legend.position = 'none',
        legend.title = element_blank(),
        plot.title = element_text(color = "black", 
                                  size = 10),
        legend.text = element_text(color = "black", 
                                   size = 12),
        panel.grid = element_blank(),
        panel.border = element_rect(color = 'black',
                                    fill = NA,
                                    size = 0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

fig

# 绘制分地区小图
# 绘制Caribbean and Central America地区
p2 <- fig + labs(x = " ", y = "", title = "Caribbean and Central America") +
  coord_cartesian(xlim = c(-92, -60), ylim = c(5, 27))

# 绘制Persian Gulf地区
p3 <- fig + labs(x = " ", y = "", title = "Persian Gulf") +
  coord_cartesian(xlim = c(45, 55), ylim = c(19, 31))

# 绘制Balkan Peninsula地区
p4 <- fig + labs(x = " ", y = "", title = "Balkan Peninsula") +
  coord_cartesian(xlim =  c(12, 32), ylim = c(35, 53))

# 绘制Southeast Asia地区
p5 <- fig + labs(x = " ", y = "", title = "Southeast Asia") +
  coord_cartesian(xlim =  c(98, 123), ylim = c(-10, 8))

# 绘制West Africa地区
p6 <- fig + labs(x = " ", y = "", title = "West Africa") +
  coord_cartesian(xlim =  c(-17, -7), ylim = c(7, 20))

# 绘制Eastern Mediterranean地区
p7 <- fig + labs(x = " ", y = "", title = "Eastern \nMediterranean") +
  coord_cartesian(xlim =  c(32, 37), ylim = c(29, 35))

# 绘制Northern Europe地区
p8 <- fig + labs(x = " ", y = "", title = "Northern Europe") +
  coord_cartesian(xlim =  c(5, 25), ylim = c(48, 60))

# 使用patchwork进行拼图
A = (p6 | p7) / p8 
plot <- p +
  (p2 + p3 + p4 + p5 + A + plot_layout(ncol = 5, widths = c(1.5, 1, 1.1, 1.2, 1))) + 
  plot_layout(ncol = 1, heights = c(9, 3))  
plot