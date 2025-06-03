

setwd("D:/pku/01groundwater/03eu_pre/11eukdetect")

eukdetect_res <- read.table("merged_table.txt",sep = '\t',header = T)

# 加载必要的包
library(dplyr)
library(tidyr)

# 定义分类级别
taxonomic_levels <- c("phylum", "class", "order", "family", "genus", "species")

# 初始化新列
eukdetect_res[taxonomic_levels] <- NA

# 处理 Lineage 列
for (i in 1:nrow(eukdetect_res)) {
  lineage_parts <- strsplit(eukdetect_res$Lineage[i], "\\|")[[1]]
  for (part in lineage_parts) {
    level_name <- strsplit(part, "-")[[1]]
    level <- level_name[1]
    name <- level_name[2]
    if (level %in% taxonomic_levels) {
      eukdetect_res[i, level] <- name
    }
  }
}

# 删除原始的 Lineage 列（可选）
eukdetect_res <- eukdetect_res %>% select(-Lineage)
eukdetect_res <- eukdetect_res[,-10:-11]

# 将 NA 替换为 no_assigned_name
eukdetect_res <- eukdetect_res %>%
  mutate(across(c(phylum, class, order, family, genus, species), ~ replace_na(., "no_assigned_name")))

#2. 绘制桑基图展示物种组成###################

library(tidyverse)
library(ggsankeyfier) 
library(MetBrewer)


setwd("D:/pku/01groundwater/03eu_pre/11eukdetect")


df <- eukdetect_res[,c(10:15)]

# 统计 phylum 列中每个门的计数
phylum_counts <- df %>%
  count(phylum)
# 找出计数小于 10 的门
low_count_phyla <- phylum_counts %>%
  filter(n < 10) %>%
  pull(phylum)
# 将计数小于 10 的门及其后续分类等级设置为 "others"
df <- df %>%
  mutate(across(phylum:species, ~ ifelse(phylum %in% low_count_phyla, "Others", .)))

# 统计 class 列中每个门的计数
class_counts <- df %>%
  count(class)
# 找出计数小于 15 的class
low_count_class <- class_counts %>%
  filter(n < 15) %>%
  pull(class)
# 将计数小于 15 的class及其后续分类等级设置为 "others"
df <- df %>%
  mutate(across(class:species, ~ ifelse(class %in% low_count_class, "Others", .)))

# 统计 order 列中每个门的计数
order_counts <- df %>%
  count(order)
# 找出计数小于 15 的order
low_count_order <- order_counts %>%
  filter(n < 15) %>%
  pull(order)
# 将计数小于 15 的class及其后续分类等级设置为 "others"
df <- df %>%
  mutate(across(order:species, ~ ifelse(order %in% low_count_order, "Others", .)))

# 统计 family 列中每个门的计数
family_counts <- df %>%
  count(family)
# 找出计数小于 15 的order
low_count_family <- family_counts %>%
  filter(n < 20) %>%
  pull(family)
# 将计数小于 15 的class及其后续分类等级设置为 "others"
df <- df %>%
  mutate(across(family:species, ~ ifelse(family %in% low_count_family, "Others", .)))

# 统计 genus 列中每个门的计数
genus_counts <- df %>%
  count(genus)
# 找出计数小于 20 的genus
low_count_genus <- genus_counts %>%
  filter(n < 20) %>%
  pull(genus)
# 将计数小于 20 的genus及其后续分类等级设置为 "others"
df <- df %>%
  mutate(across(genus:species, ~ ifelse(genus %in% low_count_genus, "Others", .)))


df1 <- df %>% select(1,2) %>% group_by(phylum,class) %>% count() %>%
  pivot_stages_longer(.,stages_from = c("phylum", "class"),
                      values_from = "n")
df2 <- df %>% select(2,3) %>% group_by(class,order) %>% count() %>%
  pivot_stages_longer(.,stages_from = c("class", "order"),
                      values_from = "n")
df3 <- df %>% select(3,4) %>% group_by(order,family) %>% count() %>%
  pivot_stages_longer(.,stages_from = c("order", "family"),
                      values_from = "n")
df4 <- df %>% select(4,5) %>% group_by(family,genus) %>% count() %>%
  pivot_stages_longer(.,stages_from = c("family", "genus"),
                      values_from = "n")



eukaryotes_sankey_3level <- 
ggplot(data=df1,aes(x = stage,y =n,group = node,
                    edge_id = edge_id,connector = connector))+
  # 绘制第 1，2 层级
  geom_sankeyedge(aes(fill = node),
                  position = position_sankey(order ="ascending",v_space="auto",
                                             width = 0.01))+
  geom_sankeynode(aes(fill=node,color=node),
                  position = position_sankey(order = "ascending",v_space ="auto",
                                             width = 0.05))+
  geom_text(data=df1 %>% filter(connector=="from"),
            aes(label = node),stat = "sankeynode",
            position = position_sankey(v_space ="auto",order="ascending",nudge_x=0.05),
            hjust=0,size=3,fontface="plain",color="grey20")+
  # 绘制第 2，3 层级
  geom_sankeyedge(data=df2,aes(fill = node),
                  position = position_sankey(order = "ascending",v_space ="auto",
                                             width = 0.01))+
  geom_sankeynode(data=df2,aes(fill=node,color=node),
                  position = position_sankey(order = "ascending",v_space ="auto",
                                             width = 0.05))+
  geom_text(data=df1 %>% filter(connector=="to"),
            aes(label = node),stat = "sankeynode",angle=0,
            position = position_sankey(v_space ="auto",order="ascending", nudge_x=0.05),
            hjust=0,size=2.5,vjust=0.5,color="grey20",fontface="plain")+
  geom_text(data=df2 %>% filter(connector=="to"),
            aes(label = node),stat = "sankeynode",angle=0,
            position = position_sankey(v_space ="auto",order="ascending",nudge_x=0.05),
            hjust=0,size=2.5,color="grey20",fontface="plain")+
  coord_cartesian(clip="off")+
  scale_x_discrete(position = "top")+
  # scale_fill_manual(values = met.brewer("Nizami")) +
  # scale_color_manual(values = met.brewer("Nizami")) +
  theme_void()+
  theme(plot.margin = margin(0,0,0,0,unit = "cm"),
        axis.text.x=element_text(color="black",face="plain",size=12,
                                 margin = margin(b=0.05,unit = "cm")),
        legend.position="none")

eukaryotes_sankey_3level




# 5个分类等级绘图。
eukaryotes_sankey_5level <- 
ggplot(data=df1,aes(x = stage,y =n,group = node,
                    edge_id = edge_id,connector = connector))+
  # 绘制第 1，2 层级
  geom_sankeyedge(aes(fill = node),
                  position = position_sankey(order ="ascending",v_space="auto",
                                             width = 0.01))+
  geom_sankeynode(aes(fill=node,color=node),
                  position = position_sankey(order = "ascending",v_space ="auto",
                                             width = 0.05))+
  geom_text(data=df1 %>% filter(connector=="from"),
            aes(label = node),stat = "sankeynode",
            position = position_sankey(v_space ="auto",order="ascending",nudge_x=0.05),
            hjust=0,size=3,fontface="plain",color="black")+
  # 绘制第 2，3 层级
  geom_sankeyedge(data=df2,aes(fill = node),
                  position = position_sankey(order = "ascending",v_space ="auto",
                                             width = 0.01))+
  geom_sankeynode(data=df2,aes(fill=node,color=node),
                  position = position_sankey(order = "ascending",v_space ="auto",
                                             width = 0.05))+
  geom_text(data=df1 %>% filter(connector=="to"),
            aes(label = node),stat = "sankeynode",angle=0,
            position = position_sankey(v_space ="auto",order="ascending", nudge_x=0.05),
            hjust=0,size=3,vjust=0.5,color="black",fontface="plain")+
  geom_text(data=df2 %>% filter(connector=="to"),
            aes(label = node),stat = "sankeynode",angle=0,
            position = position_sankey(v_space ="auto",order="ascending",nudge_x=0.05),
            hjust=0,size=2.5,color="black",fontface="plain")+
  # 绘制第 3，4 层级
  geom_sankeyedge(data=df3,aes(fill = node),
                  position = position_sankey(order = "ascending",v_space ="auto",
                                             width = 0.01))+
  geom_sankeynode(data=df3,aes(fill=node,color=node),
                  position = position_sankey(order = "ascending",v_space ="auto",
                                             width = 0.05))+
  geom_text(data=df3 %>% filter(connector=="to"),
            aes(label = node),stat = "sankeynode",angle=0,
            position = position_sankey(v_space ="auto",order="ascending", nudge_x=0.05),
            hjust=0,size=2,vjust=0.5,color="black",fontface="plain")+
  # 绘制第 4，5 层级
  geom_sankeyedge(data=df4,aes(fill = node),
                  position = position_sankey(order = "ascending",v_space ="auto",
                                             width = 0.01))+
  geom_sankeynode(data=df4,aes(fill=node,color=node),
                  position = position_sankey(order = "ascending",v_space ="auto",
                                             width = 0.05))+
  geom_text(data=df4 %>% filter(connector=="to"),
            aes(label = node),stat = "sankeynode",angle=0,
            position = position_sankey(v_space ="auto",order="ascending", nudge_x=0.05),
            hjust=0,size=2,vjust=0.5,color="black",fontface="plain")+
  coord_cartesian(clip="off")+
  scale_x_discrete(position = "top")+
  # scale_fill_manual(values = met.brewer("Nizami")) +
  # scale_color_manual(values = met.brewer("Nizami")) +
  theme_void()+
  theme(plot.margin = margin(0,0,0,0,unit = "in"),
        axis.text.x=element_text(color="black",face="plain",size=12,
                                 margin = margin(b=0.1,unit = "cm")),
        legend.position="none")

eukaryotes_sankey_5level

setwd("D:/pku/01groundwater/03eu_pre/11eukdetect")
ggsave("eukaryotes_sankey_5level.pdf",eukaryotes_sankey_5level,width = 6,height = 5.3,units = 'in')


#所有样本整体的phylum class 等水平的数量柱状图#############
# 统计每列的非冗余字符串数量
df <- eukdetect_res[,c(10:15)]
unique_counts <- df %>%
  summarise(across(everything(), ~ length(unique(.)))) %>%
  pivot_longer(everything(), names_to = "Column", values_to = "Unique_Count")

unique_counts$Unique_Count <- unique_counts$Unique_Count -1

# 设置列的顺序
unique_counts$Column <- factor(unique_counts$Column, levels = c("phylum", "class", "order", "family", "genus", "species"))

# # 绘制柱状图
# ggplot(unique_counts, aes(x = Column, y = Unique_Count, fill = Column)) +
#   geom_bar(stat = "identity") +
#   geom_text(aes(label = Unique_Count), vjust = -0.5, size = 5, color = "black") +  # 在柱子上方显示数量
#   labs(title = "Number of Unique Strings per Column",
#        x = "Column",
#        y = "Number of Unique Strings") +
#   theme_minimal() +
#   theme(legend.position = "none")  # 不显示图例

# 手动设置颜色
custom_colors <- c(
  "phylum" = "#9ecae1",  # 淡蓝色
  "class" = "#fdae6b",   # 淡橙色
  "order" = "#a1d99b",   # 淡绿色
  "family" = "#fc9272",  # 淡红色
  "genus" = "#bcbddc",   # 淡紫色
  "species" = "#ffeda0"  # 淡黄色
)
# 绘制水平柱状图
tax_all_level <- 
ggplot(unique_counts, aes(x = Column, y = Unique_Count, fill = Column)) +
  geom_bar(stat = "identity",width = 0.7) +
  geom_text(aes(label = Unique_Count), hjust = -0.2, size = 3, color = "black") +  # 在柱子右侧显示数量
  coord_flip() +  # 将柱状图水平显示
  scale_fill_manual(values = custom_colors) +  # 手动设置颜色
  labs(title = "Taxonomic annotation of groundwater eukaryotes",
       x = "",
       y = "Number of taxonomy") +
  theme_minimal() +
  theme(
    plot.margin = margin(0.1,0.1,0.1,0.1,unit = "cm"),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),  # 标题居中，加粗，字体大小为16
    panel.grid.major = element_blank(),  # 去除主要网格线
    panel.grid.minor = element_blank(),  # 去除次要网格线
    legend.position = "none")  # 不显示图例

tax_all_level

setwd("D:/pku/01groundwater/03eu_pre/11eukdetect")
ggsave("tax_all_level.pdf",tax_all_level,width = 5,height = 2.2,units = 'in')

#计算每个样点/china与other global的比较/七大地质环境分区的richness############

# 统计每个 Source 对应的非冗余数量
sample_richness <- eukdetect_res %>%
  group_by(Source) %>%
  summarise(
    unique_phylum = n_distinct(phylum),
    unique_class = n_distinct(class),
    unique_family = n_distinct(family),
    unique_genus = n_distinct(genus),
    unique_species = n_distinct(species)
  )

sample_richness$Source <- gsub(".clean","",sample_richness$Source)

#读取地质分区数据
setwd("D:/pku/01groundwater/03eu_pre/10sample_map")
library(readxl)
genome_info <- read_xlsx('Geoinfo.xlsx')
zone_info <- read_xlsx('Zone.xlsx')
sample_info <- merge(genome_info,zone_info,by = 'Sample ID')
sample_info$Location <- 'China'
#sample_info <- sample_info[,c(1,4)]

sample_richness <- merge(sample_richness,sample_info,
                         by.x = "Source",by.y = "Sample ID",all = T)

sample_richness <- sample_richness[!is.na(sample_richness$unique_phylum),]

sample_richness[is.na(sample_richness$Location),]$Location <- 'Other zones'


library(ggplot2)
library(ggpubr)

p2 <- 
ggplot(sample_richness, aes(x = Location, y = unique_genus, color=Location)) +
  geom_boxplot() +
  labs(title = "Comparison of richness",
       x = "",
       y = "Number of genus") +
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
p2

# 添加显著性标记
p2 + stat_compare_means(
  method = "wilcox.test",  # 使用 Kruskal-Wallis 检验
  label = "p.format",       # 显示 p 值
  label.x = 1.3,            # 标记的 x 轴位置
  label.y = 35              # 标记的 y 轴位置
)


# 绘制七大地质区箱线图

library(dplyr)

# 替换 Geo-environmental zone 列的值
sample_richness <- sample_richness %>%
  mutate(`Geo-environmental zone` = case_when(
    `Geo-environmental zone` == "Huanghuaihai and river delta Yangtze plain" ~ "II",
    `Geo-environmental zone` == "South China bedrock low mountain foothill" ~ "III",
    `Geo-environmental zone` == "Northwest loess plateau" ~ "IV",
    `Geo-environmental zone` == "Northeast plain-mountain" ~ "I",
    `Geo-environmental zone` == "Southwest China Karst rock mountain" ~ "V",
    `Geo-environmental zone` == "Northwest arid desert" ~ "VI",
    `Geo-environmental zone` == "Qinghai-Tibet plateau Alpine frozen soil" ~ "VII",
    TRUE ~ `Geo-environmental zone`  # 保留其他未提及的值
  ))


p3 <- 
ggplot(sample_richness[!is.na(sample_richness$`Geo-environmental zone`),],
       aes(x = `Geo-environmental zone`, y = unique_genus, color = `Geo-environmental zone`)) +
  geom_boxplot() +
  labs(title = "",
       x = "7 Geo-environmental Zone of China",
       y = "Number of genus") +
  scale_color_manual(values = c("I"="#FCB2AF","II"="#9BDFDF",
                               "III"="#FFE2CE","IV"= "#C4D8E9",
                               "V"= "#BEBCDF","VI"="#FB8C62","VII"="#FFF3CA"
  )) +
#  coord_flip()+
  stat_compare_means(
    method = "kruskal.test",  # 使用 Wilcoxon 检验进行两两比较
    label = "p.signif",      # 显示显著性标记（如 *）
    tip.length = 0.01,       # 调整标记线的长度
    hide.ns = TRUE           # 隐藏不显著的结果
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
p3

# Kruskal-Wallis 检验
kruskal_result <- kruskal.test(unique_genus ~ `Geo-environmental zone`,
                              data = sample_richness[!is.na(sample_richness$`Geo-environmental zone`),])
print(kruskal_result)

# 事后检验（Dunn 检验）
library(dunn.test)
if (kruskal_result$p.value < 0.05) {
  library(dunn.test)
  dunn_result <- dunn.test(sample_richness[!is.na(sample_richness$`Geo-environmental zone`),]$unique_genus,
                           sample_richness[!is.na(sample_richness$`Geo-environmental zone`),]$`Geo-environmental zone`, method = "bonferroni")
  print(dunn_result)
}

# 提取 Dunn 检验的显著结果
dunn_pairs <- data.frame(
  group1 = sub(" - .*", "", dunn_result$comparisons),  # 提取比较组1
  group2 = sub(".* - ", "", dunn_result$comparisons),  # 提取比较组2
  p.adj = dunn_result$P.adjusted  # 提取校正后的 p 值
)

# 仅保留显著的比较
dunn_pairs <- dunn_pairs[dunn_pairs$p.adj < 0.05, ]

# 添加显著性标记
p4 <- 
p3 + stat_pvalue_manual(
  data = dunn_pairs,  # 使用 Dunn 检验的结果
  y.position = max(sample_richness$unique_genus, na.rm = TRUE) * 1,  # 调整显著性标记的位置
  label = "p.adj",  # 显示校正后的 p 值
#  label = "p.signif",
  tip.length = 0.01  # 调整标记线的长度
) 
p4

library(Cairo)
library(cowplot)
cairo_pdf(filename = "geozone_comparison.pdf" , width = 2.7,height = 5.5)
g <- plot_grid(p2,p4,
               labels = letters[2:3], ncol = 1
               #rel_heights = c(1, 1, 1.5),
               # label_x = 0.09,            # 标签x坐标（1为最右侧）
               #  label_y = 0.9             # 标签y坐标（1为最上方）
)
g
setwd("D:/pku/01groundwater/03eu_pre/11eukdetect")
dev.off()


#3. eukfrac结果#################################################################

setwd("D:/pku/01groundwater/03eu_pre/11eukdetect")

eukfrac_res <- read.table("merged_eukfrac.txt",sep = '\t',
                          check.names = F,header = T)

eukfrac_res <- eukfrac_res[,-10:-11]

# 去冗余
# eukfrac_res_cleaned <- eukfrac_res %>%
#   group_by(RPKS, Euk_fraction, Reads, Amt_marker_sequence, Source) %>%  # 按指定列分组
#   arrange(desc(nchar(Lineage))) %>%  # 按 Lineage 列的长度降序排列
#   slice(1) %>%  # 保留每组的第一行（即 Lineage 最长的行）
#   ungroup()  # 取消分组
# 
# euktable_res <- read.table("merged_table.txt",sep = '\t',header = T)
# 
# eukfrac_res_clean <- eukfrac_res[eukfrac_res$Lineage %in% euktable_res$Lineage,]
# 
# eukfrac_res_cleaned <- eukfrac_res_cleaned[eukfrac_res_cleaned$Lineage %in% euktable_res$Lineage,]

#


#eukdetect数据库



phylum_absent <- eukdetect_res %>%
  group_by(phylum) %>%
  summarise(
    absent_sample_num = n_distinct(Source)
  )




