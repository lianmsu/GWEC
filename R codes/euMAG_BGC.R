
library(tidyr)
library(dplyr)
#一、9个fungi MAG中的BGC数据处理###############

setwd("D:/pku/01groundwater/02eukaryotes/antiSMASH_fungi")

euBGC <- read.table("output.tsv",header = T)

euBGC$MAG <- sub("(_[^_]+)_.*", "\\1", euBGC$Contig)

#统计所有的BGC的种类
all_BGC <- table(euBGC$Type) %>% as.data.frame()

# 合并 Freq <= 1 的行为 "Other"
all_BGC <- all_BGC %>%
  mutate(Var1 = ifelse(Freq <= 1, "Other", as.character(Var1))) %>%  # 确保 Var1 是字符类型
  group_by(Var1) %>%                                                # 按 Var1 分组
  summarise(Freq = sum(Freq)) %>%                                   # 对 Freq 求和
  ungroup()                                                         # 取消分组



# 二、绘制中空饼图及图例###############
library(ggplot2)
library(ggpmisc)
library(ggpubr)
library(cowplot)


mycolor=c("#c3c0c6", "#739cc9",
          "#a0b4d7","#99c0c7","#a2b7b1", "#f5a7c1", "#b1c7f5","#CAB2D6","#A6CEE3","#d8d6c6",
          "#f8a6b2", "#f3d3b4")
pie <- ggplot(all_BGC, aes(x = 2, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 1, color = "white") +  # 使用 geom_bar 模拟饼图
  coord_polar(theta = "y") +  # 将坐标转换为极坐标
  xlim(0.5,2.5)+
  scale_fill_manual(values = mycolor) +
  geom_text( aes(label = Freq), 
             position = position_stack(vjust = 0.5),
             color = "grey20", # 文本颜色
             size = 5) + # 文本大小
  guides(fill = guide_legend(title = "DSs"))+
  theme_void() +  # 移除背景元素
  theme(legend.position = "none",  # 移除默认图例
        axis.text.x = element_blank(),
        axis.ticks = element_blank())  # 移除坐标轴标签和刻度
pie
# 创建自定义图例
legend_plot <- ggplot(all_BGC, aes(x = 2, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = mycolor) +
  theme_void() +
  guides(fill = guide_legend(title = ""))+
  theme(
    legend.text = element_text(size = 12)  # 调整图例文本大小
  )

# 提取图例
legend <- get_legend(legend_plot)

# 创建组合图
combined_plot <- ggdraw() +
  draw_plot(pie, 0, 0, 1, 1) +
  draw_grob(legend, 0.4, 0.4, 0.2, 0.25)  # 调整图例的位置和大小
combined_plot

#三、绘制每个MAG中BGC组成，柱状图###################

# 计算每个 MAG 中 Type 的累积数量
data <- euBGC[,c(2,5)] %>%
  group_by(MAG, Type) %>%
  summarise(Count = n()) %>%
  ungroup()

# 绘制柱状图
nine_MAG_BGC <- 
ggplot(data, aes(x = MAG, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = "stack",width = 0.5) +  # 堆叠柱状图
  labs(
    x = "MAG",                                      # 横轴标签
    y = "BGC count",                                    # 纵轴标签
    fill = "Type"                                   # 图例标题
  ) +
  scale_fill_manual(values = mycolor) +
  theme_bw() +
  theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0), "cm"),
        plot.background = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(linewidth = 0.6),
        panel.grid = element_blank(),
        #  axis.line.x = element_line(linewidth = 0.5),
        #  axis.line.y = element_line(linewidth = 0.5),
        axis.title.x = element_text(size = 12, face = "plain"),
        axis.title.y = element_text(size = 12, face = "plain"),
        axis.ticks.x = element_line(linewidth = 0.5),
        axis.ticks.y = element_line(linewidth = 0.5),
        axis.ticks.length.x = unit(0.1, "cm"),
        axis.ticks.length.y = unit(0.1, "cm"),
        axis.text.x = element_text(angle = 30,size = 10,hjust = 1,vjust = 1),
        axis.text.y = element_text(size = 10),
        legend.position = "none")

#四、组合图###################################

# 创建组合图
combined_plot2 <- ggdraw() +
  draw_plot(nine_MAG_BGC) +  # 绘制背景图
  draw_plot(pie, x = 0.1, y = 0.42, width = 1, height = 0.6)+  # 绘制覆盖图
  draw_plot(legend,-0.02,0.5,0.5,0.5)
combined_plot2

setwd("D:/pku/01groundwater/02eukaryotes/antiSMASH_fungi")
ggsave(plot = combined_plot2,"9Fungi_BGC.pdf",width = 5.7, height = 5.7, units = "in")






