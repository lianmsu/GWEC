#探究基因的功能组成
library(dplyr)
library(ggplot2)


#1. 读取数据
setwd("D:/pku/01groundwater/02eukaryotes/euMAG_eggnog")
eggnog <- readxl::read_excel('out.emapper.annotations.xlsx',skip = 2,)
eggnog <- eggnog[1:131432,]

#1.2 删除eggnog中属于冗余MAG的条目
derep_MAG <- read.table("dereplicated_euMAG.txt")
derep_MAG$V1 <- gsub('_renamed.fa','',derep_MAG$V1)
library(dplyr)    # 用于数据操作
library(stringr)  # 用于字符串匹配

# 过滤 eggnog 数据框
filtered_eggnog <- eggnog %>%
  filter(str_detect(query, paste(derep_MAG$V1, collapse = "|")))


#2. 探究COG组成####
# 使用 dplyr 包的 count 函数来统计 COG_category 列的各个字符串的数量
category_counts <- filtered_eggnog %>%
  count(COG_category)

# # 首先，过滤出 n 列小于20的行
# filtered_counts2 <- category_counts %>%
#   filter(n < 10)
# other <- data.frame(COG_category='Others',
#                     n=sum(filtered_counts2$n)
#                     )
# # 首先，过滤出 n 列大于20的行
# filtered_counts1 <- category_counts %>%
#   filter(n > 10)
# 
# category_counts <- rbind(filtered_counts1,other)
# category_counts$COG_category[1] <- 'Unknown'

library(ggpubr)
p1 <- ggdotchart(category_counts[category_counts$n>=100,],x = 'COG_category', y = 'n',
           color = "#00AFBB",                                # Color by groups
          # palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Custom color palette
           sorting = "descending",                       # Sort value in descending order
           add = "segments",                             # Add segments from y = 0 to dots
           add.params = list(color = "lightgray", size = 1), # Change segment color and size
         #  group = "cyl",                                # Order by groups
           dot.size = 2,                                 # Large dot size
         #  label = round(dfm$mpg_z,1),                        # Add mpg values as dot labels
           font.label = list(color = "white", size = 9,
                             vjust = 0.5),               # Adjust label parameters
           ggtheme = theme_pubr()                        # ggplot2 theme
           ) +
      labs(x=NULL,y='Gene number')+
  annotate("text", x = Inf, y = Inf, label = "COG annotation", hjust = 1.1,
           vjust = 2, size = 5, color = "black", parse = FALSE)+
      theme(
        panel.background = element_blank(),  # 设置背景为透明
        panel.grid.major = element_blank(),  # 去掉主要网格线
        panel.grid.minor = element_blank(),  # 去掉次要网格线
        panel.border = element_rect(color = "grey20", fill = NA, size = 0.5),  # 添加边界线，颜色为深灰色，填充为透明，大小为0.5
        axis.title = element_text(face = "bold", size = 12, color = "grey20"),  # 设置坐标轴标题字体为粗体，大小为12，颜色为深灰色
        axis.text = element_text(size = 10, color = "grey20"),  # 设置坐标轴标签字体大小为10，颜色为深灰色
        axis.text.x = element_text(angle = 0, vjust = 0.8, hjust = 0.5),  # 横轴标签旋转
        axis.ticks = element_line(color = "grey20", size = 0.5),  # 设置坐标轴刻度线颜色为深灰色，更加细致
        legend.position = 'none',
        #legend.position = c(0.2,0.75),  # 设置图例位置在右侧
        legend.title = element_text(face = "bold", size = 12, color = "grey20"),  # 设置图例标题字体为粗体，大小为12，颜色为深灰色
        legend.text = element_text(size = 10, color = "grey20"),  # 设置图例文本字体大小为10，颜色为深灰色
        legend.background = element_blank(),  # 设置图例背景为透明
        legend.box.background = element_rect(color = "grey80", size = 0.5),  # 设置图例框背景为浅灰色，边框更加细致
        legend.key = element_blank()  # 去掉图例键的背景
      )
p1      
setwd("D:/pku/01groundwater/02eukaryotes/euMAG_eggnog")
ggsave(plot=p1,'1_25euMAG_COG.pdf',width = 8.27,height = 2.7,units = 'in')

#3. KO, PFAM等的组成
library(dplyr)
library(tidyr)
# 假设 filtered_eggnog 是你的数据框
ko_counts <- filtered_eggnog %>%
  # 将 KEGG_ko 列中的多个条目拆分为单独的行
  separate_rows(KEGG_ko, sep = ",") %>%
  # 统计每个 ko 的出现次数
  count(KEGG_ko)

#ko_counts <- filtered_eggnog %>%
#  count(KEGG_ko)


pfam <-  filtered_eggnog %>%
  count(PFAMs)

p2 <- ggdotchart(ko_counts[ko_counts$n>=70 & ko_counts$n<1000,],x = 'KEGG_ko', y = 'n',
                 color = "#FC4E07",                                # Color by groups
                 # palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Custom color palette
                 sorting = "descending",                       # Sort value in descending order
                 add = "segments",                             # Add segments from y = 0 to dots
                 add.params = list(color = "lightgray", size = 1), # Change segment color and size
                 #  group = "cyl",                                # Order by groups
                 dot.size = 2,                                 # Large dot size
                 #  label = round(dfm$mpg_z,1),                        # Add mpg values as dot labels
                 font.label = list(color = "white", size = 9,
                                   vjust = 0.5)               # Adjust label parameters
                
) +
  annotate("text", x = Inf, y = Inf, label = "KEGG annotation", hjust = 1.1,
           vjust = 2, size = 5, color = "black", parse = FALSE)+
  labs(x=NULL,y='Gene number')+
  theme(
    panel.background = element_blank(),  # 设置背景为透明
    panel.grid.major = element_blank(),  # 去掉主要网格线
    panel.grid.minor = element_blank(),  # 去掉次要网格线
    panel.border = element_rect(color = "grey20", fill = NA, size = 0.5),  # 添加边界线，颜色为深灰色，填充为透明，大小为0.5
    axis.title = element_text(face = "bold", size = 12, color = "grey20"),  # 设置坐标轴标题字体为粗体，大小为12，颜色为深灰色
    axis.text = element_text(size = 10, color = "grey20"),  # 设置坐标轴标签字体大小为10，颜色为深灰色
    axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1),  # 横轴标签旋转
    axis.ticks = element_line(color = "grey20", size = 0.5),  # 设置坐标轴刻度线颜色为深灰色，更加细致
    legend.position = 'none',
    #legend.position = c(0.2,0.75),  # 设置图例位置在右侧
    legend.title = element_text(face = "bold", size = 12, color = "grey20"),  # 设置图例标题字体为粗体，大小为12，颜色为深灰色
    legend.text = element_text(size = 10, color = "grey20"),  # 设置图例文本字体大小为10，颜色为深灰色
    legend.background = element_blank(),  # 设置图例背景为透明
    legend.box.background = element_rect(color = "grey80", size = 0.5),  # 设置图例框背景为浅灰色，边框更加细致
    legend.key = element_blank()  # 去掉图例键的背景
  )
p2
setwd("D:/pku/01groundwater/02eukaryotes/euMAG_eggnog")
ggsave(plot=p2,'2_25euMAG_ko.pdf',width = 8.27,height = 2.7,units = 'in')

p3 <- ggdotchart(pfam[pfam$n>=100 & pfam$n<2000,],x = 'PFAMs', y = 'n',
                 color = "#E7B800",                                # Color by groups
                 # palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Custom color palette
                 sorting = "descending",                       # Sort value in descending order
                 add = "segments",                             # Add segments from y = 0 to dots
                 add.params = list(color = "lightgray", size = 1), # Change segment color and size
                 #  group = "cyl",                                # Order by groups
                 dot.size = 2,                                 # Large dot size
                 #  label = round(dfm$mpg_z,1),                        # Add mpg values as dot labels
                 font.label = list(color = "white", size = 9,
                                   vjust = 0.5),               # Adjust label parameters
                 ggtheme = theme_pubr()                        # ggplot2 theme
) +
  labs(x=NULL,y='Gene number')+
  annotate("text", x = Inf, y = Inf, label = "PFAM annotation", hjust = 1.1,
           vjust = 2, size = 5, color = "black", parse = FALSE)+
  theme(
    panel.background = element_blank(),  # 设置背景为透明
    panel.grid.major = element_blank(),  # 去掉主要网格线
    panel.grid.minor = element_blank(),  # 去掉次要网格线
    panel.border = element_rect(color = "grey20", fill = NA, size = 0.5),  # 添加边界线，颜色为深灰色，填充为透明，大小为0.5
    axis.title = element_text(face = "bold", size = 12, color = "grey20"),  # 设置坐标轴标题字体为粗体，大小为12，颜色为深灰色
    axis.text = element_text(size = 10, color = "grey20"),  # 设置坐标轴标签字体大小为10，颜色为深灰色
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # 横轴标签旋转
    axis.ticks = element_line(color = "grey20", size = 0.5),  # 设置坐标轴刻度线颜色为深灰色，更加细致
    legend.position = 'none',
    #legend.position = c(0.2,0.75),  # 设置图例位置在右侧
    legend.title = element_text(face = "bold", size = 12, color = "grey20"),  # 设置图例标题字体为粗体，大小为12，颜色为深灰色
    legend.text = element_text(size = 10, color = "grey20"),  # 设置图例文本字体大小为10，颜色为深灰色
    legend.background = element_blank(),  # 设置图例背景为透明
    legend.box.background = element_rect(color = "grey80", size = 0.5),  # 设置图例框背景为浅灰色，边框更加细致
    legend.key = element_blank()  # 去掉图例键的背景
  )
p3
setwd("D:/pku/01groundwater/02eukaryotes/euMAG_eggnog")
ggsave(plot=p3,'3_25euMAG_PFAM.pdf',width = 8.27,height = 6.5,units = 'in')





library(cowplot)
g1 <- plot_grid(p2,p3,labels = LETTERS[2:3], nrow = 1,rel_heights = c(1, 2))
g1

g2 <- plot_grid(p1,g1,labels = LETTERS[1], nrow = 2,rel_heights = c(1, 2))
g2

g3 <- plot_grid(p1,p3,labels = LETTERS[1:2], nrow = 2,
                rel_heights = c(1, 2)
                )
g3



setwd("D:/pku/01groundwater/02eukaryotes/euMAG_eggnog")
ggsave(plot = g2,'1_DI_nonDGgene_all.pdf',width = 6.7,height = 6.7,units = 'in')




#GO分析############################
library(ggplot2)
library(dplyr)

GO_table <- read.table('out.emapper_GO_anno_stats.txt',header = T,quote = '"', check.names = F,sep = "\t")

colnames(GO_table) <-c("V1","V2","V3","V4")

# order tab
#GO_table <- arrange(GO_table, desc(V2), desc(V4))
GO_table <- arrange(GO_table, desc(V2), desc(V3))
level <- GO_table$V3
GO_table$V3<- factor(GO_table$V3, levels=level)


# color_list ==  pal_uchicago("default")(9)
color_list <- c("#ffa69e", "#faf3dd", "#b8f2e6", "#8A9045FF", 
                "#155F83FF", "#C16622FF", "#8F3931FF", "#58593FFF", 
                "#350E20FF")


p4 <- 
ggplot(GO_table, aes(x = V3 , y = V4 , fill = V2)) +
  # 条形图函数：stat表明取用样本点对应纵轴值
  # position_dodge(0.5) 表示同组内不同系列间错位比例 ：0.1表示90%重叠 
  geom_bar(stat = "identity" ,position = position_dodge(0.9),
           width =0.8,colour="black",size=0.2 )+
  # scale_color_manual(values = rep("black",26)) +
  scale_fill_manual(values= color_list)+
  # scale_fill_manual(values=brewer.pal(8, "Set2"))+
  geom_text(aes(label = V4), size = 3, position = position_dodge(width = 1), vjust = 0.5, hjust = 0)+
 # geom_text(aes(label = V4   ), size=3,  position = position_dodge(1))+
  #去除绘图区和X轴之间的gap
  scale_y_continuous(expand = c(0, 0),limits = c(0,max(GO_table$V4)+2000)) +
  labs(x = "GO Functional Classification", y = "No. of Genes")+
  coord_flip()+
  #theme_classic()
  theme_bw()+
  guides(color = guide_legend(ncol = 1),fill = guide_legend(reverse = F))+  #显示单列
  theme(legend.title=element_blank(), # 图例标题
        legend.background = element_blank(),# 图例背景
        #legend.position = c("right"), #top,right,left, bottom
        legend.position = "none", #top,right,left, bottom
        legend.text = element_text( size = 9), #图例字体
        panel.grid = element_blank())

setwd("D:/pku/01groundwater/02eukaryotes/euMAG_eggnog")
ggsave(plot = p4,'p4_36MAG_GO.pdf',width = 7,height = 5,units = 'in')






