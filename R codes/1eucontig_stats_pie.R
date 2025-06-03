
setwd("D:/pku/01groundwater/03eu_pre/12eu_contig")

info1 <- read.table("contig_info1.tsv",check.names = F,header = T,sep = '\t')

# 加载必要的库
library(ggplot2)
library(ggpmisc)
library(ggpubr)
library(cowplot)
# 计算百分比
info1$percentage <- info1$y / sum(info1$y) * 100

# 手动设置颜色
colors <- c("Our data" = "#FF85A1", "Public data" = "#D4C3CB")

# 绘制中空饼图（环形图）
p1 <- ggplot(info1, aes(x = 2, y = percentage, fill = x)) +
  geom_bar(stat = "identity", width = 0.4, color = "white") +
  coord_polar(theta = "y") +
  xlim(1, 2.5) +  # 设置中空效果
  theme_void() +  # 去除背景和坐标轴
  scale_fill_manual(values = colors) +  # 手动设置颜色
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 5) +  # 添加百分比标签
  theme(legend.position = "none")  # 移除默认图例
p1
# 创建自定义图例
legend_plot <- ggplot(info1, aes(x = 2, y = percentage, fill = x)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors) +
  theme_void() +
  guides(fill = guide_legend(title = ""))+
  theme(
    legend.text = element_text(size = 12)  # 调整图例文本大小
  )

# 提取图例
legend <- get_legend(legend_plot)

# 创建组合图
combined_plot <- ggdraw() +
  draw_plot(p1, 0, 0, 1, 1) +
  draw_grob(legend, 0.4, 0.4, 0.2, 0.25)  # 调整图例的位置和大小
combined_plot

setwd("D:/pku/01groundwater/03eu_pre/12eu_contig")
ggsave("1eucontig_source_pie.pdf",combined_plot,width = 4,height = 3.65,units = 'in')

########################
info2 <- read.table("contig_info2.tsv",check.names = F,header = T,sep = '\t')

# 计算百分比
info2$percentage <- info2$y / sum(info2$y) * 100

# 手动设置颜色
colors <- c("Our data" = "#FF85A1", "Public data" = "#D4C3CB")

# 绘制中空饼图（环形图）
p2 <- ggplot(info2, aes(x = 2, y = percentage, fill = x)) +
  geom_bar(stat = "identity", width = 0.4, color = "white") +
  coord_polar(theta = "y") +
  xlim(1, 2.5) +  # 设置中空效果
  theme_void() +  # 去除背景和坐标轴
  scale_fill_manual(values = colors) +  # 手动设置颜色
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 5) +  # 添加百分比标签
  theme(legend.position = "none")  # 移除默认图例
p2

write.csv(info2,"fig2a_1.csv")
# 创建自定义图例
legend_plot2 <- ggplot(info2, aes(x = 2, y = percentage, fill = x)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors) +
  theme_void() +
  guides(fill = guide_legend(title = ""))+
  theme(
    legend.text = element_text(size = 12)  # 调整图例文本大小
  )

# 提取图例
legend2 <- get_legend(legend_plot2)

# 创建组合图
combined_plot2 <- ggdraw() +
  draw_plot(p2, 0, 0, 1, 1) +
  draw_grob(legend2, 0.4, 0.4, 0.2, 0.25)  # 调整图例的位置和大小
combined_plot2


setwd("D:/pku/01groundwater/03eu_pre/12eu_contig")
ggsave("1eucontig_source_pie.pdf",combined_plot2,width = 4,height = 3.65,units = 'in')


######################################
library(colorspace)  # 用于调整颜色饱和度
info3 <- read.table("contig_info3.tsv",check.names = F,header = T,sep = '\t')

# 计算百分比
info3$percentage <- info3$y / sum(info3$y) * 100

# 手动设置颜色
colors3 <- c("5kb-10kb" = "#b8e0d2", "10kb-100kb" = "#809bce","100kb" = "#FC8181")

# 绘制中空饼图（环形图）
p3 <- ggplot(info3, aes(x = 2, y = percentage, fill = x)) +
  geom_bar(stat = "identity", width = 0.4, color = "white") +
  coord_polar(theta = "y") +
  xlim(1, 2.5) +  # 设置中空效果
  theme_void() +  # 去除背景和坐标轴
  scale_fill_manual(values = colors3) +  # 手动设置颜色
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 5) +  # 添加百分比标签
  theme(legend.position = "none")  # 移除默认图例
p3

write.csv(info3,"fig2a_2.csv")
# 创建自定义图例
legend_plot3 <- ggplot(info3, aes(x = 2, y = percentage, fill = x)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors3) +
  theme_void() +
  guides(fill = guide_legend(title = ""))+
  theme(
    legend.text = element_text(size = 12)  # 调整图例文本大小
  )

# 提取图例
legend3 <- get_legend(legend_plot3)

# 创建组合图
combined_plot3 <- ggdraw() +
  draw_plot(p3, 0, 0, 1, 1) +
  draw_grob(legend3, 0.4, 0.4, 0.2, 0.25)  # 调整图例的位置和大小
combined_plot3

setwd("D:/pku/01groundwater/03eu_pre/12eu_contig")
ggsave("1eucontig_length_pie.pdf", combined_plot3, width = 4, height = 3.65, units = 'in')



