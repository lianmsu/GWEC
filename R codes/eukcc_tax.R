

#绘制圈图
library(ggplot2)
library(ggpmisc)
library(ggpubr)
library(cowplot)

setwd("D:/pku/01groundwater/02eukaryotes/02eukcc")
eukcc_tax <- readxl::read_xlsx("eukcc.xlsx")



#1 对clade1物种组成绘制饼图####################################################

#1.1 基本统计
#
clade1 <- table(eukcc_tax$clade1) %>% data.frame()
colnames(clade1) <- c("lineage","number")

# 绘制中空饼图
mycolor=c("#c3c0c6", "#739cc9",
          "#a0b4d7","#99c0c7","#a2b7b1", "#f5a7c1", "#b1c7f5","#CAB2D6","#A6CEE3","#d8d6c6",
          "#f8a6b2", "#f3d3b4")
pie <- ggplot(clade1, aes(x = 2, y = number, fill = lineage)) +
  geom_bar(stat = "identity", width = 1, color = "white") +  # 使用 geom_bar 模拟饼图
  coord_polar(theta = "y") +  # 将坐标转换为极坐标
  xlim(0.5,2.5)+
  scale_fill_manual(values = mycolor) +
  geom_text( aes(label = number), 
             position = position_stack(vjust = 0.5),
             color = "white", # 文本颜色
             size = 5) + # 文本大小
  guides(fill = guide_legend(title = "DSs"))+
  theme_void() +  # 移除背景元素
  theme(legend.position = "none",  # 移除默认图例
        axis.text.x = element_blank(),
        axis.ticks = element_blank())  # 移除坐标轴标签和刻度
pie
# 创建自定义图例
legend_plot <- ggplot(clade1, aes(x = 2, y = number, fill = lineage)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = mycolor) +
  theme_void() +
  guides(fill = guide_legend(title = ""))

# 提取图例
legend <- get_legend(legend_plot)

# 创建组合图
combined_plot <- ggdraw() +
  draw_plot(pie, 0, 0, 1, 1) +
  draw_grob(legend, 0.4, 0.4, 0.2, 0.25)  # 调整图例的位置和大小
combined_plot

setwd("D:/pku/01groundwater/02eukaryotes/02eukcc")
ggsave(plot = combined_plot,"eukcc_tax_clade1.pdf",width = 6, height = 4.25, units = "in")

#################################################################################1

######2. 对clade2物种组成绘制饼图####################################################
#2.1 基本统计
#
clade2 <- table(eukcc_tax$clade2) %>% data.frame()
colnames(clade2) <- c("lineage","number")

# 绘制中空饼图
#"#a2b7b1" "#f5a7c1" "#b1c7f5" "#f8a6b2" "#d8d6c6" "#99c0c7" "#c3c0c6" "#f3d3b4" "#a0b4d7" "#739cc9"

mycolor=c("#a2b7b1", "#f5a7c1", "#b1c7f5","#CAB2D6","#A6CEE3","#d8d6c6",
          "#c3c0c6", "#f3d3b4","#f8a6b2", "#739cc9",
          "#a0b4d7","#99c0c7")
pie_clade2 <- ggplot(clade2, aes(x = 2, y = number, fill = lineage)) +
  geom_bar(stat = "identity", width = 1, color = "white") +  # 使用 geom_bar 模拟饼图
  coord_polar(theta = "y") +  # 将坐标转换为极坐标
  xlim(0.5,2.5)+
  scale_fill_manual(values = mycolor) +
  geom_text( aes(label = number), 
             position = position_stack(vjust = 0.5),
             color = "white", # 文本颜色
             size = 5) + # 文本大小
  guides(fill = guide_legend(title = "DSs"))+
  theme_void() +  # 移除背景元素
  theme(legend.position = "none",  # 移除默认图例
        axis.text.x = element_blank(),
        axis.ticks = element_blank())  # 移除坐标轴标签和刻度
pie_clade2
# 创建自定义图例
legend_plot_clade2 <- ggplot(clade2, aes(x = 2, y = number, fill = lineage)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = mycolor) +
  theme_void() +
  guides(fill = guide_legend(title = ""))

# 提取图例
legend_clade2 <- get_legend(legend_plot_clade2)

# 创建组合图
combined_plot_clade2 <- ggdraw() +
  draw_plot(pie_clade2, 0, 0, 1, 1) +
  draw_grob(legend_clade2, 0.4, 0.4, 0.2, 0.25)  # 调整图例的位置和大小
combined_plot_clade2

setwd("D:/pku/01groundwater/02eukaryotes/02eukcc")
ggsave(plot = combined_plot_clade2,"eukcc_tax_clade2.pdf",width = 5.6, height = 5.64, units = "in")

##############################################################################2


