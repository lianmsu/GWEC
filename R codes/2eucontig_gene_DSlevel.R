
setwd("D:/pku/01groundwater/03eu_pre/20eucontig_gene_DF")
library(dplyr)


DF_HMM_res <- read.table("only_eu_DF.txt", header = T , check.names = F,sep = '\t')

#提取DF_HMM_res注释到的HMM对应的DS
DF_HMM_res$DS <- gsub("__.*","",DF_HMM_res$`query name`)

#
DF_HMM_res_DS <- table(DF_HMM_res$DS) %>% as.data.frame() %>% arrange(desc(Freq))

# 将 Var1 列按照 Freq 列的值从大到小重新排序
DF_HMM_res_DS$Var1 <- reorder(DF_HMM_res_DS$Var, -DF_HMM_res_DS$Freq)

DF_HMM_res_DS1 <- DF_HMM_res_DS[DF_HMM_res_DS$Freq>=100,]

DF_HMM_res_DS1[17, 1] <- sub("_.*", "", DF_HMM_res_DS1[17, 1])

DF_HMM_res_DS1 <- DF_HMM_res_DS1 %>%
  group_by(Var1) %>%
  summarise(Freq = sum(Freq), .groups = "drop") %>% arrange(desc(Freq))

#绘图
library(ggplot2)
ggplot(DF_HMM_res_DS1, aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_point(size = 4, color = "steelblue") +
  labs(title = "",
       x = "Defense system",
       y = "Gene number") +
  theme_bw() +
  theme(
    plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),  # 设置图形边距
    plot.background = element_blank(),  # 去除图形背景
    panel.background = element_blank(),  # 去除面板背景
    panel.border = element_rect(linewidth = 0.6),  # 设置面板边框
    panel.grid = element_blank(),  # 去除网格线
    axis.title.x = element_text(size = 12, face = "plain"),  # 设置 x 轴标题
    axis.title.y = element_text(size = 12, face = "plain"),  # 设置 y 轴标题
    axis.ticks.x = element_line(linewidth = 0.5),  # 设置 x 轴刻度线
    axis.ticks.y = element_line(linewidth = 0.5),  # 设置 y 轴刻度线
    axis.ticks.length.x = unit(0.1, "cm"),  # 设置 x 轴刻度长度
    axis.ticks.length.y = unit(0.1, "cm"),  # 设置 y 轴刻度长度
    axis.text.x = element_text(angle = 90, size = 8, hjust = 1, vjust = 0.5, color = 'black'),  # 设置 x 轴文本
    axis.text.y = element_text(size = 8, color = 'black'),  # 设置 y 轴文本
    legend.position = "none"  # 去除图例
  )







