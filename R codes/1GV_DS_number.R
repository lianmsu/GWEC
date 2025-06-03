#对defensefinder从MAGs上找到的defense systems进行分析。
#绘制DS DG分布柱状图；DS DG/MAG size的折线图
library(dplyr)

library(ggplot2)
#DSs在所有MAGs中的存在百分比柱状图####
#1.读取数据表###

setwd("D:/pku/01groundwater/03eu_pre/30NCLDV")

dss <- read.delim2('gw_NCDLV_vOTUs_defense_finder_systems.tsv',sep = '\t',
                   quote = "",header = T,check.names = F) 

DS_sumary <- table(dss$type) %>% as.data.frame()
colnames(DS_sumary) <- c("DS","num")
DS_sumary <- DS_sumary[order(-DS_sumary$num), ]


mytheme <-  theme(
  panel.background = element_blank(),  # 设置背景为透明
  panel.grid.major = element_blank(),  # 去掉主要网格线
  panel.grid.minor = element_blank(),  # 去掉次要网格线
  panel.border = element_rect(color = "grey20", fill = NA, linewidth = 0.5),  # 添加边界线，颜色为深灰色，填充为透明，大小为0.5
  axis.title = element_text( lineheight = 12, color = "black"),  # 设置坐标轴标题字体为粗体，大小为12，颜色为深灰色
  axis.text = element_text(lineheight = 10, color = "black"),  # 设置坐标轴标签字体大小为10，颜色为深灰色
  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # 横轴标签旋转
  axis.ticks = element_line(color = "black", linewidth = 0.5),  # 设置坐标轴刻度线颜色为深灰色，更加细致
  legend.position = 'none',
  #legend.position = c(0.2,0.75),  # 设置图例位置在右侧
  legend.title = element_text( lineheight = 12, color = "black"),  # 设置图例标题字体为粗体，大小为12，颜色为深灰色
  legend.text = element_text(lineheight = 10, color = "black"),  # 设置图例文本字体大小为10，颜色为深灰色
  legend.background = element_blank(),  # 设置图例背景为透明
  legend.box.background = element_rect(color = "grey80", linewidth = 0.5),  # 设置图例框背景为浅灰色，边框更加细致
  legend.key = element_blank()  # 去掉图例键的背景
  )
DS_num_distribution <- ggplot(DS_sumary) +
  geom_bar(aes(x = reorder(DS,-num), y = num),
           stat = "identity", position = "dodge",
           show.legend = T,alpha = .9,linewidth = 0.5,
           fill='#81D0D6'#E9AF89,sa#9CB9D7,ss  #DFA4BA,ws#E9AF89,sa
  )+
  scale_y_continuous(limits = c(0,100),expand = c(0,0))+
  labs( x=NULL,y="Number of DSs") +
  mytheme
DS_num_distribution

#ggsave("DSs_percentage.pdf", plot = p, width = 10, height = 8, units = "in")
#write.csv(new_df,"Fig1A.csv",quote = F)

DS_subtype_sumary <- table(dss$subtype) %>% as.data.frame()
colnames(DS_subtype_sumary) <- c("DSsubtype","num")
DS_subtype_sumary <- DS_subtype_sumary[order(-DS_subtype_sumary$num), ]
DS_subtype_sumary_RM <- DS_subtype_sumary[c(1:4),]

DS_RMsubtype_num_distribution <- ggplot(DS_subtype_sumary_RM) +
  geom_bar(aes(x = reorder(DSsubtype,-num), y = num),
           stat = "identity", position = "dodge",
           show.legend = T,alpha = .9,linewidth = 0.5,
           fill='#81D0D6'#E9AF89,sa#9CB9D7,ss  #DFA4BA,ws#E9AF89,sa
  )+
  scale_y_continuous(limits = c(0,75),expand = c(0,0))+
  labs( x=NULL,y="Number of DSs") +
  mytheme
DS_RMsubtype_num_distribution

# 创建组合图
library(ggpmisc)
library(ggpubr)
library(cowplot)

combined_plot <- ggdraw() +
  draw_plot(DS_num_distribution) +  # 绘制背景图
  draw_plot(DS_RMsubtype_num_distribution, x = 0.3, y = 0.3, 
            width = 0.7, height = 0.7)  # 绘制覆盖图

combined_plot

setwd("D:/pku/01groundwater/03eu_pre/30NCLDV")
ggsave("GV_DS_number.pdf",plot = combined_plot, width = 3.45, height = 3.7, units = 'in')



#防御基因层面统计,可以看限制酶和内切酶的存在比例####
dgs <- read.delim2('gw_NCDLV_vOTUs_defense_finder_genes.tsv',sep = '\t',
                   quote = "",header = T,check.names = F) 
RM_DG <- dgs[dgs$type=="RM",]
RM_DG_sumary <- table(RM_DG$gene_name) %>% as.data.frame()
colnames(RM_DG_sumary) <- c("DG","num")
RM_DG_sumary <- RM_DG_sumary[order(-RM_DG_sumary$num), ]


