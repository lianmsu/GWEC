
#1. treatment of raw data######################
#caution! the HMM results are redundant!

library(dplyr)

setwd("D:/pku/01groundwater/03eu_pre/20eucontig_gene_DF")
DF_HMM_res <- read.table("only_eu_DF.txt", header = T , check.names = F,sep = '\t')

# 对hit_id列去重，保留i_eval更小的行
# DF_HMM_res <- DF_HMM_res %>%
#   group_by(hit_id) %>%
#   slice_min(i_eval,with_ties = FALSE) %>%
#   ungroup()
# 
# DF_HMM_res <- DF_HMM_res[DF_HMM_res$i_eval<=1e-3,]

#1.2. 对HMM结果的Uniref90注释结果分析#####################
setwd("D:/pku/01groundwater/03eu_pre/20eucontig_gene_DF")

HMM_uniref <- read.csv("only_eu_DF_UniRef90_cleaned.tsv", header = T ,sep = '\t',
                       check.names = F)

#tax_id <- read.csv("tax_report.txt",header = T, sep = '|')

library(stringr)
# 拆分lineage列并提取最后两个数字
#tax_id$lineage_split <- strsplit(as.character(tax_id$lineage), " ")
# 提取最后两个数字，如果不足则用0填充
# tax_id$last_two <- lapply(tax_id$lineage_split, function(x) {
#   len <- length(x)
#   if (len >= 2) {
#     return(as.numeric(x[(len-1):len]))
#   } else if (len == 1) {
#     return(c(0, as.numeric(x)))
#   } else {
#     return(c(0, 0))
#   }
# })
# # 将结果转换为数据框的两列
# tax_id$last_one <- sapply(tax_id$last_two, `[`, 2)
# tax_id$last_two <- sapply(tax_id$last_two, `[`, 1)

#把物种结果合并回HMM_uniref
# 合并操作
# HMM_uniref_merged <- HMM_uniref %>%
#   left_join(tax_id, by = c("TaxID" = "taxid"))

# 确保HMM_uniref_merged中的UniRef90_ID是唯一的
HMM_uniref_merged <- HMM_uniref[!duplicated(HMM_uniref$UniRef90_ID), ]

#只保留HMM_uniref_merged的last_one的值为2759
#HMM_uniref_merged <- HMM_uniref_merged[HMM_uniref_merged$last_two == 2759,]

#这里是统计所有对应到的Uniref90的类别。非冗余！！！
HMM_uniref_protein <- table(HMM_uniref_merged$Description) %>% as.data.frame()

viral_rows <- HMM_uniref_protein[grepl("viral", HMM_uniref_protein$Var1, ignore.case = TRUE), ]
print(viral_rows)

#可以绘制一个图展示非冗余Uniref90的组成


#合并HMM_uniref_merged与DF_HMM_res

#提取DF_HMM_res的hit_id列的|前的字符串部分，作为新列uniref90id
# 使用 str_extract 提取 | 前的部分
# DF_HMM_res <- DF_HMM_res %>%
#   mutate(uniref90id = str_extract(`target name`, "^[^|]+"))

DF_HMM_res$uniref90id <- str_extract(DF_HMM_res$`target name`, "^[^|]+")

# 合并数据框
DF_HMM_res <- merge(DF_HMM_res, HMM_uniref_merged[, c("UniRef90_ID", "Description")], 
                    by.x = "uniref90id", by.y = "UniRef90_ID", all.x = TRUE)

#write.table(DF_HMM_res, file = "DF_HMM_res.tsv",quote = F, sep = '\t',row.names = F)

# 删除 Description 列中为 NA 的行（也就是删除原核、病毒)
#DF_HMM_res <- DF_HMM_res[!is.na(DF_HMM_res$Description), ]


#HMM结果不适合以DS角度去画图描述，因为都是单个基因结果。
DF_HMM_res_DG <- table(DF_HMM_res$`query name`) %>% as.data.frame() %>% arrange(desc(Freq))

# 将 Var1 列按照 Freq 列的值从大到小重新排序
DF_HMM_res_DG$Var1 <- reorder(DF_HMM_res_DG$Var, -DF_HMM_res_DG$Freq)

DF_HMM_res_DG1 <- DF_HMM_res_DG[DF_HMM_res_DG$Freq>=3,]

#去除Tiamat__TmtA_2752664533 RosmerTA__RmrA_2734955840 	
# RM_Type_II__Type_II_REase 	
# RM_Type_II__Type_II_MTases	
# RM__Type_I_REases


#2. DF_HMM结果，>=50的基因#############

library(ggplot2)
library(ggbreak)

p <- 
ggplot(DF_HMM_res_DG1, aes(x = Var1, y = Freq)) +
  geom_bar(
    stat = "identity", position = "dodge",
    show.legend = TRUE, alpha = 0.9, linewidth = 0.5, fill = "#a4c3b2"  # 设置柱子颜色
  ) +
  geom_text(
    aes(label = Freq), angle = 90, hjust = 0, vjust = 0.5,
    position = position_dodge(width = 0.9),  # 调整文本位置
    color = "black", size = 2  # 设置文本颜色和大小
  ) +
  scale_y_continuous(limits = c(0, 10000), expand = c(0, 0)) +  # 设置 y 轴范围和扩展
  labs(x = "", y = "Homologous gene numbers") +  # 设置轴标签
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
p

setwd("D:/pku/01groundwater/03eu_pre/20eucontig_gene_DF")
#未去冗余的结果
ggsave("1eucontig_gene_DF.pdf",plot = p, width = 6.8, height = 5, units = 'in')



#3. DF_HMM结果，<50的基因#############

DF_HMM_res_DG2 <- DF_HMM_res_DG[DF_HMM_res_DG$Freq<50,]
p2 <- 
  ggplot(DF_HMM_res_DG2, aes(x = Var1, y = Freq)) +
  geom_bar(
    stat = "identity", position = "dodge",
    show.legend = TRUE, alpha = 0.9, linewidth = 0.5, fill = "#a4c3b2"  # 设置柱子颜色
  ) +
  geom_text(
    aes(label = Freq), angle = 90, hjust = 0, vjust = 0.5,
    position = position_dodge(width = 0.9),  # 调整文本位置
    color = "black", size = 3.5  # 设置文本颜色和大小
  ) +
  scale_y_continuous(limits = c(0, 50), expand = c(0, 0)) +  # 设置 y 轴范围和扩展
  labs(x = "", y = "Gene numbers") +  # 设置轴标签
  theme_bw() +
  theme(
    plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),  # 设置图形边距
    plot.background = element_blank(),  # 去除图形背景
    panel.background = element_blank(),  # 去除面板背景
    panel.border = element_rect(linewidth = 0.6),  # 设置面板边框
    panel.grid = element_blank(),  # 去除网格线
    axis.title.x = element_text(size = 6, face = "plain",color = 'black'),  # 设置 x 轴标题
    axis.title.y = element_text(size = 6, face = "plain",color = 'black'),  # 设置 y 轴标题
    axis.ticks.x = element_line(linewidth = 0.5),  # 设置 x 轴刻度线
    axis.ticks.y = element_line(linewidth = 0.5),  # 设置 y 轴刻度线
    axis.ticks.length.x = unit(0.1, "cm"),  # 设置 x 轴刻度长度
    axis.ticks.length.y = unit(0.1, "cm"),  # 设置 y 轴刻度长度
    axis.text.x = element_text(angle = 90, size = 6, hjust = 1, vjust = 0.5,color = 'black'),  # 设置 x 轴文本
    axis.text.y = element_text(size = 6, color = 'black'),  # 设置 y 轴文本
    legend.position = "none"  # 去除图例
  )
p2

setwd("D:/pku/01groundwater/02eukaryotes/41euMAG_defense")
ggsave("euMAG_DS_less_10.pdf",plot = p2, width = 5.9, height = 6.2, units = 'in')


#4. 组合图###################################

# 创建组合图
library(ggpmisc)
library(ggpubr)
library(cowplot)

combined_plot <- ggdraw() +
  draw_plot(p) +  # 绘制背景图
  draw_plot(p2, x = 0.1, y = 0.45, width = 0.9, height = 0.51)  # 绘制覆盖图
  
combined_plot

setwd("D:/pku/01groundwater/02eukaryotes/41euMAG_defense")
ggsave("euMAG_HMM_DG.pdf",plot = combined_plot, width = 5.9, height = 7.3, units = 'in')


#5. 上面是DG，下面绘制对应的uniref90注释结果###################

#HMM结果不适合以DS角度去画图描述，因为都是单个基因结果。
DF_HMM_res_uniprot90_Descript <- table(DF_HMM_res$Description) %>% as.data.frame() %>% arrange(desc(Freq))

# 将 Var1 列按照 Freq 列的值从大到小重新排序
DF_HMM_res_uniprot90_Descript$Var1 <- reorder(DF_HMM_res_uniprot90_Descript$Var1, -DF_HMM_res_uniprot90_Descript$Freq)

DF_HMM_res_uniprot90_Descript_G1 <- DF_HMM_res_uniprot90_Descript[DF_HMM_res_uniprot90_Descript$Freq>=100,]

#6. DF_HMM结果，>=10的基因#############

library(ggplot2)
library(ggbreak)

p3 <- 
  ggplot(DF_HMM_res_uniprot90_Descript_G1, aes(x = Var1, y = Freq)) +
  geom_bar(
    stat = "identity", position = "dodge",
    show.legend = TRUE, alpha = 0.9, linewidth = 0.5, fill = "#cddafd"  # 设置柱子颜色
  ) +
  geom_text(
    aes(label = Freq), angle = 90, hjust = 0, vjust = 0.5,
    position = position_dodge(width = 0.9),  # 调整文本位置
    color = "black", size = 3.5  # 设置文本颜色和大小
  ) +
  scale_y_continuous(limits = c(0, 3050), expand = c(0, 0)) +  # 设置 y 轴范围和扩展
  labs(x = "", y = "Gene numbers") +  # 设置轴标签
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
p3
setwd("D:/pku/01groundwater/03eu_pre/20eucontig_gene_DF")
#未去冗余的结果
ggsave("1eucontig_gene_DF_Uniref90.pdf",plot = p3, width = 6.8, height = 5, units = 'in')



library(Cairo)
library(cowplot)
cairo_pdf(filename = "euMAG_HMM_DG_all.pdf" , width = 9,height = 6.68)
g <- plot_grid(combined_plot,p3,
               labels = LETTERS[1:2], nrow = 1
               #rel_heights = c(1, 1, 1.5),
               # label_x = 0.09,            # 标签x坐标（1为最右侧）
               #  label_y = 0.9             # 标签y坐标（1为最上方）
)
g
setwd("D:/pku/01groundwater/02eukaryotes/41euMAG_defense")
dev.off()

#统计基因对应的宿主##########################
setwd("D:/pku/01groundwater/02eukaryotes/02eukcc")
euMAG_tax <- readxl::read_excel("eukcc.xlsx")
euMAG_tax$bin <- gsub("_renamed.fa","",euMAG_tax$bin)

DF_HMM_res_bin <- table(DF_HMM_res$genome) %>% as.data.frame() 

DF_HMM_res_bin <- merge(DF_HMM_res_bin,euMAG_tax,by.x = 'Var1',by.y = 'bin')

ggplot(DF_HMM_res_bin,aes(x=clade2,y=Freq))+
  geom_boxplot()

# 绘制柱状图
ggplot(DF_HMM_res_bin, aes(x = clade2, y = Freq)) +
  geom_col() +  # 使用 geom_col() 绘制柱状图
  labs(
    x = "Clade",  # x 轴标签
    y = "Frequency",  # y 轴标签
    title = "Frequency by Clade"  # 图表标题
  ) +
  theme_minimal() +  # 使用简洁的主题
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # 调整 x 轴标签的角度


#Cas结果############
setwd("D:/pku/01groundwater/03eu_pre/20eucontig_gene_DF")

eucontig_cas <- read.table("only_eu_Cas.txt", header = T , check.names = F,sep = '\t')
eucontig_cas$uniref90id <- str_extract(eucontig_cas$`target name`, "^[^|]+")

Cas_HMM_uniref <- read.csv("only_eu_Cas_UniRef90_cleaned.tsv", header = T ,sep = '\t',
                           check.names = F)


Cas_HMM_res <- merge(eucontig_cas, Cas_HMM_uniref[, c("UniRef90_ID", "Description")], 
                     by.x = "uniref90id", by.y = "UniRef90_ID", all.x = TRUE)

#HMM结果不适合以DS角度去画图描述，因为都是单个基因结果。
Cas_HMM_res_DG <- table(Cas_HMM_res$`query name`) %>% as.data.frame() %>% arrange(desc(Freq))

# 将 Var1 列按照 Freq 列的值从大到小重新排序
Cas_HMM_res_DG$Var1 <- reorder(Cas_HMM_res_DG$Var, -Cas_HMM_res_DG$Freq)

#DF_HMM_res_DG1 <- DF_HMM_res_DG[DF_HMM_res_DG$Freq>=50,]


#2. DF_HMM结果，>=50的基因#############

library(ggplot2)
library(ggbreak)

p_cas <- 
  ggplot(Cas_HMM_res_DG, aes(x = Var1, y = Freq)) +
  geom_bar(
    stat = "identity", position = "dodge",
    show.legend = TRUE, alpha = 0.9, linewidth = 0.5, fill = "#a4c3b2"  # 设置柱子颜色
  ) +
  geom_text(
    aes(label = Freq), angle = 90, hjust = 0, vjust = 0.5,
    position = position_dodge(width = 0.9),  # 调整文本位置
    color = "black", size = 3.5  # 设置文本颜色和大小
  ) +
  scale_y_continuous(limits = c(0, 200), expand = c(0, 0)) +  # 设置 y 轴范围和扩展
  labs(x = "", y = "Homologous gene numbers") +  # 设置轴标签
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
p_cas

setwd("D:/pku/01groundwater/03eu_pre/20eucontig_gene_DF")
#未去冗余的结果
ggsave("2eucontig_gene_Cas.pdf",plot = p_cas, width = 2.5, height = 4.1, units = 'in')

#Cas对应的Uniprot90注释！
#HMM结果不适合以DS角度去画图描述，因为都是单个基因结果。
Cas_HMM_res_uniprot90_Descript <- table(Cas_HMM_res$Description) %>% as.data.frame() %>% arrange(desc(Freq))

# 将 Var1 列按照 Freq 列的值从大到小重新排序
Cas_HMM_res_uniprot90_Descript$Var1 <- reorder(Cas_HMM_res_uniprot90_Descript$Var1, -Cas_HMM_res_uniprot90_Descript$Freq)

Cas_HMM_res_uniprot90_Descript_G1 <- Cas_HMM_res_uniprot90_Descript[Cas_HMM_res_uniprot90_Descript$Freq>=5,]

#6. DF_HMM结果，>=10的基因#############

library(ggplot2)
library(ggbreak)

p3_cas <- 
  ggplot(Cas_HMM_res_uniprot90_Descript_G1, aes(x = Var1, y = Freq)) +
  geom_bar(
    stat = "identity", position = "dodge",
    show.legend = TRUE, alpha = 0.9, linewidth = 0.5, fill = "#cddafd"  # 设置柱子颜色
  ) +
  geom_text(
    aes(label = Freq), angle = 90, hjust = 0, vjust = 0.5,
    position = position_dodge(width = 0.9),  # 调整文本位置
    color = "black", size = 3.5  # 设置文本颜色和大小
  ) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +  # 设置 y 轴范围和扩展
  labs(x = "", y = "Gene numbers") +  # 设置轴标签
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
p3_cas
setwd("D:/pku/01groundwater/03eu_pre/20eucontig_gene_DF")
#未去冗余的结果
ggsave("2eucontig_gene_Cas_Uniref90.pdf",plot = p3_cas, width = 4.5, height = 4.8, units = 'in')



#7. 对DS 的基因背景分析#############################
library(stringr)
library(ggplot2)
library(gggenes)

#绘制含有DS的真核contig######

#a. 准备gff文件
setwd("D:/pku/01groundwater/02eukaryotes/41euMAG_defense")

eu_DS_contig <- read.delim("DS_contig.gff",check.names = F,header = F)
eu_DS_contig$orientation <- ifelse(eu_DS_contig$V7 == "-", 0, 1)

#删除eu_DS_contig中V3为mRNA和exon的行。因为mRNA与DNA一致，exon与CDS一致。V9不一样！
eu_DS_contig <- eu_DS_contig[!eu_DS_contig$V3 %in% c('exon', 'mRNA'), ]


# 提取V9列中的ID部分
# 处理后
eu_DS_contig <- eu_DS_contig %>%
  mutate(
    TCS_ID_part = str_extract(V9, "TCS_ID=[^;]+"),
    Target_part = str_extract(TCS_ID_part, "UniRef90_[^|]+\\|[^|]+\\|[+-]"),
    uniref90id = str_extract(TCS_ID_part, "UniRef90_[^|]+")
  ) %>%
  select(-TCS_ID_part)

# b. 准备gff文件的注释文件
setwd("D:/pku/01groundwater/02eukaryotes/41euMAG_defense")

DF_HMM_res <- read.table("DF_HMM_res.tsv", sep = '\t',header = T)

DF_DS <- read.table("DF_systems_extract.txt", header = T , check.names = F)

#提取DF_HMM_res的hit_id列的部分字符串
DF_HMM_res$Target_part <- str_extract(DF_HMM_res$hit_id, "UniRef90_[^|]+\\|[^|]+\\|[+-]")
#提取DF_DS的sys_beg列的部分字符串
DF_DS$Target_part <- str_extract(DF_DS$sys_beg, "UniRef90_[^|]+\\|[^|]+\\|[+-]")

# 搜索包含特定字符串的行
DF_HMM_res %>%
  filter(str_detect(Target_part, fixed("UniRef90_A0A2V0QQL5|xin26_bin.2_3063|+")))

eu_DS_contig %>%
  filter(str_detect(V9, fixed("UniRef90_A0A2V0QQL5|xin26_bin.2_3063|+")))

#c. 合并注释信息到gff文件
eu_DS_contig_anno <- merge(eu_DS_contig, DF_HMM_res[, c("Target_part", "Description","gene_name")], 
                    by.x = "Target_part", by.y = "Target_part", all.x = TRUE)


#d. 整理并合并contig上不属于DG的其他基因的uniref90注释信息
setwd("D:/pku/01groundwater/02eukaryotes/41euMAG_defense")

contig_gene_uniref_anno <- read.csv("DS_contig_UniRef90_cleaned.tsv",
                                      sep = '\t', header = T
                                      )
# 删除 Description 列
eu_DS_contig_anno <- eu_DS_contig_anno %>%
  select(-Description)
eu_DS_contig_anno <- merge(eu_DS_contig_anno, contig_gene_uniref_anno[, c("UniRef90_ID", "Description")], 
                           by.x = "uniref90id", by.y = "UniRef90_ID", all.x = TRUE)

#e. 给eu_DS_contig_anno新增一列Type，如果gene_name不为NA，则对应的Type为anti-phage；
#如果gene_name为NA，则对应的Type为others

# 新增 Type 列
eu_DS_contig_anno <- eu_DS_contig_anno %>%
  mutate(Type = ifelse(!is.na(gene_name), "anti-phage", "others"))

#write.table(eu_DS_contig_anno, file = "eu_DS_contig_anno.tsv",quote = F, sep = '\t',row.names = F)

#f. 准备eu_DS_contig_anno_subgene
eu_DS_contig_anno_subgene <- eu_DS_contig_anno
eu_DS_contig_anno_subgene$from <- eu_DS_contig_anno_subgene$V4
eu_DS_contig_anno_subgene$to <- eu_DS_contig_anno_subgene$V5

# 找到每个 Target_part 对应的 gene 行的 V4 值
gene_v4_values <- eu_DS_contig_anno_subgene %>%
  filter(V3 == "gene") %>%
  select(Target_part, V4) %>%
  rename(gene_V4 = V4)

# 将 gene 行的 V4 值合并到 CDS 行
eu_DS_contig_anno_subgene <- eu_DS_contig_anno_subgene %>%
  left_join(gene_v4_values, by = "Target_part") %>%
  mutate(V4 = ifelse(V3 == "CDS", gene_V4, V4)) %>%
  select(-gene_V4)

# 找到每个 Target_part 对应的 gene 行的 V5 值
gene_v5_values <- eu_DS_contig_anno_subgene %>%
  filter(V3 == "gene") %>%
  select(Target_part, V5) %>%
  rename(gene_V5 = V5)

# 将 gene 行的 V5 值合并到 CDS 行
eu_DS_contig_anno_subgene <- eu_DS_contig_anno_subgene %>%
  left_join(gene_v5_values, by = "Target_part") %>%
  mutate(V5 = ifelse(V3 == "CDS", gene_V5, V5)) %>%
  select(-gene_V5)

eu_DS_contig_anno_subgene <- eu_DS_contig_anno_subgene[eu_DS_contig_anno_subgene$V3 != 'gene',]

eu_DS_contig_anno_gene <- eu_DS_contig_anno[eu_DS_contig_anno$V3 == 'gene',]

#f. 绘图
library(ggplot2)
library(gggenes)

"#7779AE""#91C8ED"
custom_colors <- c("anti-phage" = "#ADC9BA", 
                   "TxSS" = "#E8C07C", 
                   "type3" = "green", 
                   "others" = "#D3DEFC")  # 包含 "others" 的自定义颜色

p_dscontig <- ggplot(eu_DS_contig_anno_gene,
                     aes(xmin = V4, xmax = V5, 
                         y = V1, fill = Type,
                         forward = orientation)) +
  facet_wrap(~ V1, scales = "free", ncol = 1) +
  # scale_fill_manual(values = c("grey"),  # 为特定的分类指定颜色
  #                   limits = c("others")  # 设置图例的顺序和分类
  #                   )+
  geom_feature(
    data = eu_DS_contig_anno[!is.na(eu_DS_contig_anno$gene_name),],
    aes(x = V4, y = V1, forward = T,fill = Type)
  ) +
  geom_feature_label(
    data = eu_DS_contig_anno[!is.na(eu_DS_contig_anno$gene_name),],
    aes(x = V4, y = V1, label = gene_name, forward = T,color = Type)
  ) +
geom_gene_arrow(arrowhead_height = unit(3, "mm"),
                size = 0.1,
                #color = NA,  #控制基因箭头的边框
                arrowhead_width = unit(1, "mm")
                # fill = 'white'
) +
  scale_fill_manual(values = custom_colors) +  # 使用自定义颜色
  scale_color_manual(values = custom_colors) +  # 添加文字颜色的手动映射
  
  # scale_fill_brewer(palette = "Set3")+  #Others的颜色是"#FDB462"
  # scale_fill_manual(values = c('grey','grey','grey','grey',
  #                              'grey','grey','grey'))+
  theme_genes()+
  theme(axis.title.y = element_blank(),# 隐藏y轴标题
        axis.text.y = element_blank()   # 隐藏y轴刻度标签
  )+
  labs(y = "",x=NULL)+
  guides(fill=guide_legend(title = "Genes",nrow =1,position = 'bottom'))
p_dscontig

#展示subgene
p_dscontig <- ggplot(eu_DS_contig_anno_gene,
                         aes(xmin = V4, xmax = V5, 
                             y = V1, fill = Type,
                             forward = orientation)) +
  facet_wrap(~ V1, scales = "free", ncol = 1) +
  # scale_fill_manual(values = c("grey"),  # 为特定的分类指定颜色
  #                   limits = c("others")  # 设置图例的顺序和分类
  #                   )+
  geom_feature(
    data = eu_DS_contig_anno[!is.na(eu_DS_contig_anno_gene$gene_name),],
    aes(x = V4, y = V1, forward = T,fill = Type)
  ) +
  geom_feature_label(
    data = eu_DS_contig_anno_gene[!is.na(eu_DS_contig_anno_gene$gene_name),],
    aes(x = V4, y = V1, label = gene_name, forward = T,color = Type)
  ) +
  geom_feature_label(
    data = eu_DS_contig_anno_gene[is.na(eu_DS_contig_anno_gene$gene_name),],
    aes(x = V4, y = V1, label = Description, forward = T,color = Type)
  ) +
  geom_gene_arrow(arrowhead_height = unit(3, "mm"),
                  size = 0.1,
                  #color = NA,  #控制基因箭头的边框
                  arrowhead_width = unit(1, "mm"),
                  fill = 'white'
                  ) +  #data = 
  geom_subgene_arrow(data = eu_DS_contig_anno_subgene,
                     aes( xmin = V4, xmax = V5,  y = V1, fill = Type, 
                         xsubmin = from, xsubmax = to),
                     arrowhead_height = unit(3, "mm"),
                     size = 0.1,
                     #color = NA,  #控制基因箭头的边框
                     arrowhead_width = unit(1, "mm"),
                     color="black", alpha=.7) +
  scale_fill_manual(values = custom_colors) +  # 使用自定义颜色
  scale_color_manual(values = custom_colors) +  # 添加文字颜色的手动映射
  
  # scale_fill_brewer(palette = "Set3")+  #Others的颜色是"#FDB462"
  # scale_fill_manual(values = c('grey','grey','grey','grey',
  #                              'grey','grey','grey'))+
  theme_genes()+
  theme(axis.title.y = element_blank(),# 隐藏y轴标题
       # axis.text.y = element_blank()   # 隐藏y轴刻度标签
  )+
  labs(y = "",x=NULL)+
  guides(fill=guide_legend(title = "Genes",nrow =1,position = 'bottom'))
p_dscontig

setwd("D:/pku/01groundwater/02eukaryotes/41euMAG_defense")
ggsave("p_dscontig_gene_background2.pdf",p_dscontig,width = 8,height = 15,units = 'in',
       limitsize = FALSE)




