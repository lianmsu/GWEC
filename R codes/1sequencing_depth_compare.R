
setwd("D:/pku/01groundwater/03eu_pre/01sequencing_depth")

# 加载所需包
library(tidyverse)

# 读取文件内容
file_content <- read_lines("base_count_results_china.txt")

# 提取文件路径和Base count
china_data <- tibble(line = file_content) %>%
  # 提取文件路径
  mutate(file_path = str_extract(line, "^Processing file: .*$")) %>%
  # 提取Base count
  mutate(base_count = str_extract(line, "(\\d+\\.\\d+ Gbp)")) %>%
  # 只保留文件路径和Base count
  filter(!is.na(file_path) | !is.na(base_count)) %>%
  fill(file_path, .direction = "down") %>%
  # 选择需要的列
  select(file_path, base_count)

# 将 Base count 列转化为数值类型
china_data <- china_data %>%
  mutate(base_count = str_replace(base_count, " Gbp", "") %>%
           as.numeric()) %>%
  # 从文件路径中提取文件名
  mutate(file_name = str_extract(file_path, "[^/]+(?=\\.fastq\\.gz$)"))

china_data <- china_data[!is.na(china_data$base_count),]

# 删除最后一行
china_data <- head(china_data, -1)  # 或者使用 head(data, -1)

china_data <- china_data[,2:3]

# 加载 dplyr 包
library(dplyr)


# 提取 file_name 列中的前缀（去掉数字部分）
china_data <- china_data %>%
  mutate(file_prefix = str_replace(file_name, "_\\d+$", "")) %>%
  # 按前缀进行分组并对 base_count 求和
  group_by(file_prefix) %>%
  summarize(total_base_count = sum(base_count), .groups = "drop") %>%
  # 重新命名列以匹配原始数据框格式
  select(file_name = file_prefix, base_count = total_base_count)

china_data$type <- "China"


# 读取文件内容
file_content <- read_lines("base_count_results_global.txt")

# 提取文件路径和Base count
global_data <- tibble(line = file_content) %>%
  # 提取文件路径
  mutate(file_path = str_extract(line, "^Processing file: .*$")) %>%
  # 提取Base count
  mutate(base_count = str_extract(line, "(\\d+\\.?\\d* bp)")) %>%
  # 只保留文件路径和Base count
  filter(!is.na(file_path) | !is.na(base_count)) %>%
  fill(file_path, .direction = "down") %>%
  # 选择需要的列
  select(file_path, base_count)

# 将 Base count 列转化为数值类型 (去除" bp"并转换为数值)
global_data <- global_data %>%
  mutate(base_count = str_replace(base_count, " bp", "") %>%
           as.numeric()) %>%
  # 从文件路径中提取文件名
  mutate(file_name = str_extract(file_path, "[^/]+(?=\\.fastq$)"))

global_data <- global_data[!is.na(global_data$base_count),]

# 删除最后一行
global_data <- head(global_data, -1)  # 或者使用 head(data, -1)

global_data <- global_data[,2:3]

# 加载 dplyr 包
library(dplyr)

# 提取 file_name 列中的前缀（去掉数字部分）
global_data <- global_data %>%
  mutate(file_prefix = str_replace(file_name, "_\\d+$", "")) %>%
  # 按前缀进行分组并对 base_count 求和
  group_by(file_prefix) %>%
  summarize(total_base_count = sum(base_count), .groups = "drop") %>%
  # 重新命名列以匹配原始数据框格式
  select(file_name = file_prefix, base_count = total_base_count)

#write.csv(global_data,"global_data_bases.csv")

global_data$base_count <- global_data$base_count * 1e-9

global_data$type <- "Other zones"


sequencing_depth <- rbind(china_data,global_data)


library(ggplot2)
library(ggpubr)

p1 <- 
  ggplot(sequencing_depth, aes(x = type, y = base_count, color=type)) +
  geom_boxplot() +
  labs(title = "Comparison of richness",
       x = "",
       y = "Sequencing depth (Gbp)") +
  scale_color_manual(values = c("China"="#FF85A2","Other zones"="#D6C4CC"))+
  stat_compare_means(
    comparisons = list(c("China", "Other zones")
    ),  # 指定比较组
    method = "wilcox.test",  # 使用 Wilcoxon 检验进行两两比较
    label = "p.signif",      # 显示显著性标记（如 *）
    tip.length = 0.01        # 调整标记线的长度
  )+
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "plain"),  # 标题居中，加粗，字体大小为16
    plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),  # 设置图形边距
    plot.background = element_blank(),  # 去除图形背景
    panel.background = element_blank(),  # 去除面板背景
    panel.border = element_rect(linewidth = 0.6),  # 设置面板边框
    panel.grid = element_blank(),  # 去除网格线
    axis.title.x = element_text(size = 10, face = "plain",color = 'black'),  # 设置 x 轴标题
    axis.title.y = element_text(size = 12, face = "plain",color = 'black'),  # 设置 y 轴标题
    axis.ticks.x = element_line(linewidth = 0.5),  # 设置 x 轴刻度线
    axis.ticks.y = element_line(linewidth = 0.5),  # 设置 y 轴刻度线
    axis.ticks.length.x = unit(0.1, "cm"),  # 设置 x 轴刻度长度
    axis.ticks.length.y = unit(0.1, "cm"),  # 设置 y 轴刻度长度
    axis.text.x = element_text(angle = 0, size = 12, hjust = 0.5, vjust = 0.5,color = 'black'),  # 设置 x 轴文本
    axis.text.y = element_text(size = 10, color = 'black'),  # 设置 y 轴文本
    legend.position = "none"  # 去除图例
  )
p1

# 添加显著性标记
p1 + stat_compare_means(
  method = "wilcox.test",  # 使用 Kruskal-Wallis 检验
  label = "p.format",       # 显示 p 值
  label.x = 1.3,            # 标记的 x 轴位置
  label.y = 35              # 标记的 y 轴位置
)

setwd("D:/pku/01groundwater/03eu_pre/01sequencing_depth")
ggsave(plot = p1,'1_sequencing_depth_compare.pdf',width = 2.6,height = 2.9,units = 'in')




# 读取文件内容
file_content <- read_lines("read_count_results.txt")

# 1. 将 file_content 转换为 tibble，并标记行类型
parsed_data <- tibble(line = file_content) %>%
  mutate(
    line_type = case_when(
      str_detect(line, "^Processing file:") ~ "file_path",
      str_detect(line, "^Read count in") ~ "read_count",
      TRUE ~ NA_character_
    )
  )

# 2. 分别提取 file_path 和 read_count
file_paths <- parsed_data %>%
  filter(line_type == "file_path") %>%
  mutate(file_path = str_remove(line, "^Processing file: ")) %>%
  select(file_path)

read_counts <- parsed_data %>%
  filter(line_type == "read_count") %>%
  mutate(read_count = str_extract(line, "\\d+$")) %>%  # 提取行末的数字
           select(read_count)
         
# 3. 合并（要求 file_path 和 read_count 的行数相同）
if (nrow(file_paths) == nrow(read_counts)) {
  global_data <- bind_cols(file_paths, read_counts) %>%
    mutate(
      read_count = as.integer(read_count),
      sample_id = str_extract(file_path, "(SRR|DRR|ERR)\\d+")  # 提取样品编号
    )
} else {
  stop("Error: The number of file paths and read counts does not match!")
}
         
# 4. 统计每个样品的总 reads 数
read_stats <- global_data %>%
  group_by(sample_id) %>%
  summarise(total_reads = sum(read_count)) %>%
  arrange(desc(total_reads))  # 按 reads 数降序排列

write.csv(read_stats,"global_read_count.csv")


