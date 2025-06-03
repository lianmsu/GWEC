
#####Wadjet__JetC_III###########
setwd("D:/pku/01groundwater/03eu_pre/24special_gene_secondaryHMM")

Wadjet__JetC_III <- read.delim("eu993_Wadjet__JetC_III_homologs2_eval_increase.txt",
                               header = T, check.names = T, sep = '\t',fill = T
                               )
colnames(Wadjet__JetC_III)
summary(Wadjet__JetC_III)

Wadjet__JetC_III <- Wadjet__JetC_III[Wadjet__JetC_III$query.name == 'extracted_Wadjet__JetC_III_mafft',]

# 确保 score.1 是数值类型（如果不是，先转换）
Wadjet__JetC_III$score.1 <- as.numeric(Wadjet__JetC_III$score.1)
Wadjet__JetC_III$score <-  as.numeric(Wadjet__JetC_III$score)
Wadjet__JetC_III$E.value <-  as.numeric(Wadjet__JetC_III$E.value)
Wadjet__JetC_III$E.value.1 <-  as.numeric(Wadjet__JetC_III$E.value.1)

#以score为横轴，以频数为纵轴绘制柱状图。
library(ggplot2)
mytheme <-  theme(
  panel.background = element_blank(),  # 设置背景为透明
  panel.grid.major = element_blank(),  # 去掉主要网格线
  panel.grid.minor = element_blank(),  # 去掉次要网格线
  panel.border = element_rect(color = "grey20", fill = NA, linewidth = 0.5),  # 添加边界线，颜色为深灰色，填充为透明，大小为0.5
  axis.title = element_text( lineheight = 12, color = "black"),  # 设置坐标轴标题字体为粗体，大小为12，颜色为深灰色
  axis.text = element_text(lineheight = 10, color = "black"),  # 设置坐标轴标签字体大小为10，颜色为深灰色
  axis.text.x = element_text(angle = 0, vjust = 0.8, hjust = 0.5),  # 横轴标签旋转
  axis.ticks = element_line(color = "black", linewidth = 0.5),  # 设置坐标轴刻度线颜色为深灰色，更加细致
  legend.position = 'none',
  #legend.position = c(0.2,0.75),  # 设置图例位置在右侧
  legend.title = element_text( lineheight = 12, color = "black"),  # 设置图例标题字体为粗体，大小为12，颜色为深灰色
  legend.text = element_text(lineheight = 10, color = "black"),  # 设置图例文本字体大小为10，颜色为深灰色
  legend.background = element_blank(),  # 设置图例背景为透明
  legend.box.background = element_rect(color = "grey80", linewidth = 0.5),  # 设置图例框背景为浅灰色，边框更加细致
  legend.key = element_blank()  # 去掉图例键的背景
)

p_Wadjet__JetC_III_score <- 
ggplot(Wadjet__JetC_III, aes(x = score)) +
  geom_bar(fill = "steelblue", color = "steelblue",width = 0.1) +  # 柱状图
  labs(
    title = "Frequency Distribution of Score",
    x = "Score",  # 横轴标签
    y = "Frequency"  # 纵轴标签
  ) +
  geom_vline(xintercept = 200,color = "red", linetype = "dashed", linewidth = 0.5)+
  #geom_text(aes(x = 200, label = "200"), y = max(table(Wadjet__JetC_III$score)), color = "red", vjust = -0.5, size = 5)+
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # 标题居中
  )+
  mytheme
p_Wadjet__JetC_III_score
# ggplot(Wadjet__JetC_III, aes(x = score.1)) +
#   geom_bar(fill = "steelblue", color = "steelblue",width = 0.1) +  # 柱状图
#   labs(
#     title = "Frequency Distribution of Best Domain Score",
#     x = "Score",  # 横轴标签
#     y = "Frequency"  # 纵轴标签
#   ) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5)  # 标题居中
#   )
#Wadjet__JetC_III选择200的hit score阈值

#####Brig1__ADP_ribosyl###########
setwd("D:/pku/01groundwater/03eu_pre/24special_gene_secondaryHMM")

Brig1__ADP_ribosyl <- read.delim("eu993_Brig1__ADP_ribosyl_homologs2_eval_increase.txt",
                               header = T, check.names = T, sep = '\t',fill = T)

colnames(Brig1__ADP_ribosyl)
summary(Brig1__ADP_ribosyl)

Brig1__ADP_ribosyl <- Brig1__ADP_ribosyl[Brig1__ADP_ribosyl$query.name == 'extracted_Brig1__ADP_ribosyl_mafft',]

# 确保 score.1 是数值类型（如果不是，先转换）
Brig1__ADP_ribosyl$score <-  as.numeric(Brig1__ADP_ribosyl$score)
Brig1__ADP_ribosyl$E.value <-  as.numeric(Brig1__ADP_ribosyl$E.value)


#以score为横轴，以频数为纵轴绘制柱状图。
library(ggplot2)
mytheme <-  theme(
  panel.background = element_blank(),  # 设置背景为透明
  panel.grid.major = element_blank(),  # 去掉主要网格线
  panel.grid.minor = element_blank(),  # 去掉次要网格线
  panel.border = element_rect(color = "grey20", fill = NA, linewidth = 0.5),  # 添加边界线，颜色为深灰色，填充为透明，大小为0.5
  axis.title = element_text( lineheight = 12, color = "black"),  # 设置坐标轴标题字体为粗体，大小为12，颜色为深灰色
  axis.text = element_text(lineheight = 10, color = "black"),  # 设置坐标轴标签字体大小为10，颜色为深灰色
  axis.text.x = element_text(angle = 0, vjust = 0.8, hjust = 0.5),  # 横轴标签旋转
  axis.ticks = element_line(color = "black", linewidth = 0.5),  # 设置坐标轴刻度线颜色为深灰色，更加细致
  legend.position = 'none',
  #legend.position = c(0.2,0.75),  # 设置图例位置在右侧
  legend.title = element_text( lineheight = 12, color = "black"),  # 设置图例标题字体为粗体，大小为12，颜色为深灰色
  legend.text = element_text(lineheight = 10, color = "black"),  # 设置图例文本字体大小为10，颜色为深灰色
  legend.background = element_blank(),  # 设置图例背景为透明
  legend.box.background = element_rect(color = "grey80", linewidth = 0.5),  # 设置图例框背景为浅灰色，边框更加细致
  legend.key = element_blank()  # 去掉图例键的背景
)

p_Brig1__ADP_ribosyl_score <- 
  ggplot(Brig1__ADP_ribosyl, aes(x = score)) +
  geom_bar(fill = "steelblue", color = "steelblue",width = 0.1) +  # 柱状图
  labs(
    title = "Frequency Distribution of Score",
    x = "Score",  # 横轴标签
    y = "Frequency"  # 纵轴标签
  ) +
  geom_vline(xintercept = 150,color = "red", linetype = "dashed", linewidth = 0.5)+
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # 标题居中
  )+
  mytheme
p_Brig1__ADP_ribosyl_score

#####CBASS__AG_E1_ThiF###########
setwd("D:/pku/01groundwater/03eu_pre/24special_gene_secondaryHMM")

CBASS__AG_E1_ThiF <- read.delim("eu993_CBASS__AG_E1_ThiF_homologs2_eval_increase.txt",
                                 header = T, check.names = T, sep = '\t',fill = T)

colnames(CBASS__AG_E1_ThiF)
summary(CBASS__AG_E1_ThiF)

CBASS__AG_E1_ThiF <- CBASS__AG_E1_ThiF[CBASS__AG_E1_ThiF$query.name == 'extracted_CBASS__AG_E1_ThiF_mafft',]

# 确保 score.1 是数值类型（如果不是，先转换）
CBASS__AG_E1_ThiF$score <-  as.numeric(CBASS__AG_E1_ThiF$score)
CBASS__AG_E1_ThiF$E.value <-  as.numeric(CBASS__AG_E1_ThiF$E.value)


#以score为横轴，以频数为纵轴绘制柱状图。
library(ggplot2)
mytheme <-  theme(
  panel.background = element_blank(),  # 设置背景为透明
  panel.grid.major = element_blank(),  # 去掉主要网格线
  panel.grid.minor = element_blank(),  # 去掉次要网格线
  panel.border = element_rect(color = "grey20", fill = NA, linewidth = 0.5),  # 添加边界线，颜色为深灰色，填充为透明，大小为0.5
  axis.title = element_text( lineheight = 12, color = "black"),  # 设置坐标轴标题字体为粗体，大小为12，颜色为深灰色
  axis.text = element_text(lineheight = 10, color = "black"),  # 设置坐标轴标签字体大小为10，颜色为深灰色
  axis.text.x = element_text(angle = 0, vjust = 0.8, hjust = 0.5),  # 横轴标签旋转
  axis.ticks = element_line(color = "black", linewidth = 0.5),  # 设置坐标轴刻度线颜色为深灰色，更加细致
  legend.position = 'none',
  #legend.position = c(0.2,0.75),  # 设置图例位置在右侧
  legend.title = element_text( lineheight = 12, color = "black"),  # 设置图例标题字体为粗体，大小为12，颜色为深灰色
  legend.text = element_text(lineheight = 10, color = "black"),  # 设置图例文本字体大小为10，颜色为深灰色
  legend.background = element_blank(),  # 设置图例背景为透明
  legend.box.background = element_rect(color = "grey80", linewidth = 0.5),  # 设置图例框背景为浅灰色，边框更加细致
  legend.key = element_blank()  # 去掉图例键的背景
)

p_CBASS__AG_E1_ThiF_score <- 
  ggplot(CBASS__AG_E1_ThiF, aes(x = score)) +
  geom_bar(fill = "steelblue", color = "steelblue",width = 0.1) +  # 柱状图
  labs(
    title = "Frequency Distribution of Score",
    x = "Score",  # 横轴标签
    y = "Frequency"  # 纵轴标签
  ) +
  geom_vline(xintercept = 400,color = "red", linetype = "dashed", linewidth = 0.5)+
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # 标题居中
  )+
  mytheme
p_CBASS__AG_E1_ThiF_score

#####Hachiman__HamB###########
setwd("D:/pku/01groundwater/03eu_pre/24special_gene_secondaryHMM")

Hachiman__HamB <- read.delim("eu993_Hachiman__HamB_homologs2_eval_increase.txt",
                                header = T, check.names = T, sep = '\t',fill = T)

colnames(Hachiman__HamB)
summary(Hachiman__HamB)

Hachiman__HamB <- Hachiman__HamB[Hachiman__HamB$query.name == 'extracted_Hachiman__HamB_mafft',]

# 确保 score.1 是数值类型（如果不是，先转换）
Hachiman__HamB$score <-  as.numeric(Hachiman__HamB$score)
Hachiman__HamB$E.value <-  as.numeric(Hachiman__HamB$E.value)


#以score为横轴，以频数为纵轴绘制柱状图。
library(ggplot2)
mytheme <-  theme(
  panel.background = element_blank(),  # 设置背景为透明
  panel.grid.major = element_blank(),  # 去掉主要网格线
  panel.grid.minor = element_blank(),  # 去掉次要网格线
  panel.border = element_rect(color = "grey20", fill = NA, linewidth = 0.5),  # 添加边界线，颜色为深灰色，填充为透明，大小为0.5
  axis.title = element_text( lineheight = 12, color = "black"),  # 设置坐标轴标题字体为粗体，大小为12，颜色为深灰色
  axis.text = element_text(lineheight = 10, color = "black"),  # 设置坐标轴标签字体大小为10，颜色为深灰色
  axis.text.x = element_text(angle = 0, vjust = 0.8, hjust = 0.5),  # 横轴标签旋转
  axis.ticks = element_line(color = "black", linewidth = 0.5),  # 设置坐标轴刻度线颜色为深灰色，更加细致
  legend.position = 'none',
  #legend.position = c(0.2,0.75),  # 设置图例位置在右侧
  legend.title = element_text( lineheight = 12, color = "black"),  # 设置图例标题字体为粗体，大小为12，颜色为深灰色
  legend.text = element_text(lineheight = 10, color = "black"),  # 设置图例文本字体大小为10，颜色为深灰色
  legend.background = element_blank(),  # 设置图例背景为透明
  legend.box.background = element_rect(color = "grey80", linewidth = 0.5),  # 设置图例框背景为浅灰色，边框更加细致
  legend.key = element_blank()  # 去掉图例键的背景
)

p_Hachiman__HamB_score <- 
  ggplot(Hachiman__HamB, aes(x = score)) +
  geom_bar(fill = "steelblue", color = "steelblue",width = 0.1) +  # 柱状图
  labs(
    title = "Frequency Distribution of Score",
    x = "Score",  # 横轴标签
    y = "Frequency"  # 纵轴标签
  ) +
  geom_vline(xintercept = 200,color = "red", linetype = "dashed", linewidth = 0.5)+
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # 标题居中
  )+
  mytheme
p_Hachiman__HamB_score

#####Mokosh_type_I__MkoA_B###########
setwd("D:/pku/01groundwater/03eu_pre/24special_gene_secondaryHMM")

Mokosh_type_I__MkoA_B <- read.delim("eu993_Mokosh_type_I__MkoA_B_homologs2_eval_increase.txt",
                             header = T, check.names = T, sep = '\t',fill = T)

colnames(Mokosh_type_I__MkoA_B)
summary(Mokosh_type_I__MkoA_B)

Mokosh_type_I__MkoA_B <- Mokosh_type_I__MkoA_B[Mokosh_type_I__MkoA_B$query.name == 'extracted_Mokosh_type_I__MkoA_B_mafft',]

# 确保 score.1 是数值类型（如果不是，先转换）
Mokosh_type_I__MkoA_B$score <-  as.numeric(Mokosh_type_I__MkoA_B$score)
Mokosh_type_I__MkoA_B$E.value <-  as.numeric(Mokosh_type_I__MkoA_B$E.value)


#以score为横轴，以频数为纵轴绘制柱状图。
library(ggplot2)
mytheme <-  theme(
  panel.background = element_blank(),  # 设置背景为透明
  panel.grid.major = element_blank(),  # 去掉主要网格线
  panel.grid.minor = element_blank(),  # 去掉次要网格线
  panel.border = element_rect(color = "grey20", fill = NA, linewidth = 0.5),  # 添加边界线，颜色为深灰色，填充为透明，大小为0.5
  axis.title = element_text( lineheight = 12, color = "black"),  # 设置坐标轴标题字体为粗体，大小为12，颜色为深灰色
  axis.text = element_text(lineheight = 10, color = "black"),  # 设置坐标轴标签字体大小为10，颜色为深灰色
  axis.text.x = element_text(angle = 0, vjust = 0.8, hjust = 0.5),  # 横轴标签旋转
  axis.ticks = element_line(color = "black", linewidth = 0.5),  # 设置坐标轴刻度线颜色为深灰色，更加细致
  legend.position = 'none',
  #legend.position = c(0.2,0.75),  # 设置图例位置在右侧
  legend.title = element_text( lineheight = 12, color = "black"),  # 设置图例标题字体为粗体，大小为12，颜色为深灰色
  legend.text = element_text(lineheight = 10, color = "black"),  # 设置图例文本字体大小为10，颜色为深灰色
  legend.background = element_blank(),  # 设置图例背景为透明
  legend.box.background = element_rect(color = "grey80", linewidth = 0.5),  # 设置图例框背景为浅灰色，边框更加细致
  legend.key = element_blank()  # 去掉图例键的背景
)

p_Mokosh_type_I__MkoA_B_score <- 
  ggplot(Mokosh_type_I__MkoA_B, aes(x = score)) +
  geom_bar(fill = "steelblue", color = "steelblue",width = 0.1) +  # 柱状图
  labs(
    title = "Frequency Distribution of Score",
    x = "Score",  # 横轴标签
    y = "Frequency"  # 纵轴标签
  ) +
  geom_vline(xintercept = 220,color = "red", linetype = "dashed", linewidth = 0.5)+
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # 标题居中
  )+
  mytheme
p_Mokosh_type_I__MkoA_B_score

####Zorya_TypeI__ZorD###########
setwd("D:/pku/01groundwater/03eu_pre/24special_gene_secondaryHMM")

Zorya_TypeI__ZorD <- read.delim("eu993_Zorya_TypeI__ZorD_homologs2_eval_increase.txt",
                                    header = T, check.names = T, sep = '\t',fill = T)

colnames(Zorya_TypeI__ZorD)
summary(Zorya_TypeI__ZorD)

Zorya_TypeI__ZorD <- Zorya_TypeI__ZorD[Zorya_TypeI__ZorD$query.name == 'extracted_Zorya_TypeI__ZorD_mafft',]

# 确保 score.1 是数值类型（如果不是，先转换）
Zorya_TypeI__ZorD$score <-  as.numeric(Zorya_TypeI__ZorD$score)
Zorya_TypeI__ZorD$E.value <-  as.numeric(Zorya_TypeI__ZorD$E.value)


#以score为横轴，以频数为纵轴绘制柱状图。
library(ggplot2)
mytheme <-  theme(
  panel.background = element_blank(),  # 设置背景为透明
  panel.grid.major = element_blank(),  # 去掉主要网格线
  panel.grid.minor = element_blank(),  # 去掉次要网格线
  panel.border = element_rect(color = "grey20", fill = NA, linewidth = 0.5),  # 添加边界线，颜色为深灰色，填充为透明，大小为0.5
  axis.title = element_text( lineheight = 12, color = "black"),  # 设置坐标轴标题字体为粗体，大小为12，颜色为深灰色
  axis.text = element_text(lineheight = 10, color = "black"),  # 设置坐标轴标签字体大小为10，颜色为深灰色
  axis.text.x = element_text(angle = 0, vjust = 0.8, hjust = 0.5),  # 横轴标签旋转
  axis.ticks = element_line(color = "black", linewidth = 0.5),  # 设置坐标轴刻度线颜色为深灰色，更加细致
  legend.position = 'none',
  #legend.position = c(0.2,0.75),  # 设置图例位置在右侧
  legend.title = element_text( lineheight = 12, color = "black"),  # 设置图例标题字体为粗体，大小为12，颜色为深灰色
  legend.text = element_text(lineheight = 10, color = "black"),  # 设置图例文本字体大小为10，颜色为深灰色
  legend.background = element_blank(),  # 设置图例背景为透明
  legend.box.background = element_rect(color = "grey80", linewidth = 0.5),  # 设置图例框背景为浅灰色，边框更加细致
  legend.key = element_blank()  # 去掉图例键的背景
)

p_Zorya_TypeI__ZorD_score <- 
  ggplot(Zorya_TypeI__ZorD, aes(x = score)) +
  geom_bar(fill = "steelblue", color = "steelblue",width = 0.1) +  # 柱状图
  labs(
    title = "Frequency Distribution of Score",
    x = "Score",  # 横轴标签
    y = "Frequency"  # 纵轴标签
  ) +
  geom_vline(xintercept = 430,color = "red", linetype = "dashed", linewidth = 0.5)+
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # 标题居中
  )+
  mytheme
p_Zorya_TypeI__ZorD_score

library(Cairo)
library(cowplot)
cairo_pdf(width = 7.5,height = 5.6)
g <- plot_grid(p_Mokosh_type_I__MkoA_B_score,p_Zorya_TypeI__ZorD_score,p_Hachiman__HamB_score,
               p_Brig1__ADP_ribosyl_score,p_CBASS__AG_E1_ThiF_score,p_Wadjet__JetC_III_score,
               labels = letters[1:6], nrow = 2,ncol = 3
               #rel_heights = c(1, 1, 1.5),
               # label_x = 0.09,            # 标签x坐标（1为最右侧）
               #  label_y = 0.9             # 标签y坐标（1为最上方）
)
g
setwd("D:/pku/01groundwater/03eu_pre/24special_gene_secondaryHMM")
dev.off()

