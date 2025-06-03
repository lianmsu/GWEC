
setwd("D:/pku/01groundwater/03eu_pre/12eu_contig")

#处理metaeuk的物种注释结果文件。
library(dplyr)
library(tidyr)

contig_tax <- read.csv("eu_5kb_ddcontig_tax_tax_per_contig.tsv",sep = "\t",header = F)
colnames(contig_tax) <- c("Contig","NCBItxid","Rank","Lineage","un1","un2","un3","un4","Detail_lineage")


#######################contig Rank############################
contig_rank <- table(contig_tax$Rank) %>% as.data.frame()

library(dplyr)
# 按Freq列降序排列
contig_rank_sorted <- contig_rank %>% arrange(desc(Freq))

# 合并从第三行开始的行
others_row <- data.frame(Var1 = "Deeper taxonomic assignment", Freq = sum(contig_rank_sorted$Freq[3:nrow(contig_rank_sorted)]))
contig_rank_final <- rbind(contig_rank_sorted[1:2, ], others_row)


info4 <- contig_rank_final
colnames(info4) <- c("x","y")
info4$x <- c("no rank","superkingdom","others")
# 计算百分比
info4$percentage <- info4$y / sum(info4$y) * 100

# 手动设置颜色
colors4 <- c("no rank" = "#d2d2cf", "superkingdom" = "#ffd8be","others" = "#b8b8ff")

library(ggplot2)
library(ggpmisc)
library(ggpubr)
library(cowplot)
# 绘制中空饼图（环形图）
p4 <- ggplot(info4, aes(x = 2, y = percentage, fill = x)) +
  geom_bar(stat = "identity", width = 0.4, color = "white") +
  coord_polar(theta = "y",start = -2*pi*0.2) +
  xlim(1, 2.5) +  # 设置中空效果
  theme_void() +  # 去除背景和坐标轴
  scale_fill_manual(values = colors4) +  # 手动设置颜色
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 5) +  # 添加百分比标签
  theme(legend.position = "none")  # 移除默认图例
p4
write.csv(info4,"fig2b_1.csv")
# 创建自定义图例
legend_plot4 <- ggplot(info4, aes(x = 2, y = percentage, fill = x)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors4) +
  theme_void() +
  guides(fill = guide_legend(title = ""))+
  theme(
    legend.text = element_text(size = 12)  # 调整图例文本大小
  )

# 提取图例
legend4 <- get_legend(legend_plot4)

# 创建组合图
combined_plot4 <- ggdraw() +
  draw_plot(p4, 0, 0, 1, 1) +
  draw_grob(legend4, 0.4, 0.4, 0.2, 0.25)  # 调整图例的位置和大小
combined_plot4

setwd("D:/pku/01groundwater/03eu_pre/12eu_contig")
ggsave("2eucontig_tax_pie.pdf",combined_plot4,width = 4,height = 3.65,units = 'in')

######################contig Detailed taxonomy#################################
contig_tax_filter <- contig_tax[contig_tax$Rank!="no rank",]  

# 假设 contig_tax_filter 是你的数据框
contig_tax_filter <- contig_tax_filter %>%
  mutate(Sample = sapply(strsplit(as.character(Contig), "_"), function(x) paste(x[1:1], collapse = "_")))

# 提取信息并生成新列
library(tidyverse)
contig_tax_filter <- contig_tax_filter %>%
  mutate(
    d = str_extract(Detail_lineage, "(?<=d_)[^;]+"),  # 提取 d_ 后的内容
    k = str_extract(Detail_lineage, "(?<=k_)[^;]+"),  # 提取 k_ 后的内容
    p = str_extract(Detail_lineage, "(?<=p_)[^;]+"),  # 提取 p_ 后的内容
    c = str_extract(Detail_lineage, "(?<=c_)[^;]+"),  # 提取 c_ 后的内容
    o = str_extract(Detail_lineage, "(?<=o_)[^;]+"),  # 提取 o_ 后的内容
    f = str_extract(Detail_lineage, "(?<=f_)[^;]+"),  # 提取 f_ 后的内容
    g = str_extract(Detail_lineage, "(?<=g_)[^;]+"),  # 提取 g_ 后的内容
    s = str_extract(Detail_lineage, "(?<=s_)[^;]+")   # 提取 s_ 后的内容
  )

# 从 s 列开始倒序处理
for (i in nrow(contig_tax_filter):1) {
  if (!is.na(contig_tax_filter$s[i])) {
    contig_tax_filter[i, c("k", "p", "c", "o", "f", "g")] <- lapply(contig_tax_filter[i, c("k", "p", "c", "o", "f", "g")], function(x) ifelse(is.na(x), "no_assigned_name", x))
  } else if (!is.na(contig_tax_filter$g[i])) {
    contig_tax_filter[i, c("k", "p", "c", "o", "f")] <- lapply(contig_tax_filter[i, c("k", "p", "c", "o", "f")], function(x) ifelse(is.na(x), "no_assigned_name", x))
  } else if (!is.na(contig_tax_filter$f[i])) {
    contig_tax_filter[i, c("k", "p", "c", "o")] <- lapply(contig_tax_filter[i, c("k", "p", "c", "o")], function(x) ifelse(is.na(x), "no_assigned_name", x))
  } else if (!is.na(contig_tax_filter$o[i])) {
    contig_tax_filter[i, c("k", "p", "c")] <- lapply(contig_tax_filter[i, c("k", "p", "c")], function(x) ifelse(is.na(x), "no_assigned_name", x))
  } else if (!is.na(contig_tax_filter$c[i])) {
    contig_tax_filter[i, c("k", "p")] <- lapply(contig_tax_filter[i, c("k", "p")], function(x) ifelse(is.na(x), "no_assigned_name", x))
  } else if (!is.na(contig_tax_filter$p[i])) {
    contig_tax_filter[i, "k"] <- ifelse(is.na(contig_tax_filter[i, "k"]), "no_assigned_name", contig_tax_filter[i, "k"])
  }
}

#把contig_tax_filter的k p c o f g s各列剩余的NA改为unknown
# 使用 mutate 和 across 替换 NA 为 "unknown"
contig_tax_filter <- contig_tax_filter %>%
  mutate(across(c(k, p, c, o, f, g, s), ~ replace(., is.na(.), "unknown")))

setwd("D:/pku/01groundwater/03eu_pre/12eu_contig")
#write_tsv(contig_tax_filter,"contig_tax_filter.tsv")

# 绘制桑基图展示物种组成###################

library(tidyverse)
library(ggsankeyfier) 
library(MetBrewer)

setwd("D:/pku/01groundwater/03eu_pre/12eu_contig")

df <- contig_tax_filter[,c(11:18)]

# 统计 k 列中每个k的计数
k_counts <- df %>%
  count(k)

# 统计 phylum 列中每个门的计数
p_counts <- df %>%
  count(p)
# 找出计数小于 1000 的门
low_count_p <- p_counts %>%
  filter(n < 1000) %>%
  pull(p)
# 将计数小于 1000 的门及其后续分类等级设置为 "others"
df <- df %>%
  mutate(across(p:s, ~ ifelse(. == "unknown", .,  # 保留 "unknown"
                              ifelse(p %in% low_count_p, "Others", .) # 将低计数门及后续分类设为 "Others"
  )))

# 统计 class 列中每个门的计数
c_counts <- df %>%
  count(c)
# 找出计数小于 1000 的class
low_count_c <- c_counts %>%
  filter(n < 1000) %>%
  pull(c)
# 将计数小于 1000 的c及其后续分类等级设置为 "others"
df <- df %>%
  mutate(across(c:s, ~ ifelse(. == "unknown", .,  # 保留 "unknown"
                              ifelse(c %in% low_count_c, "Others", .) # 将低计数门及后续分类设为 "Others"
  )))

library(tidyverse)
library(ggsankeyfier) 
library(MetBrewer)
df1 <- df %>% select(1,2) %>% group_by(d,k) %>% count() %>%
  pivot_stages_longer(.,stages_from = c("d", "k"),
                      values_from = "n")
df2 <- df %>% select(2,3) %>% group_by(k,p) %>% count() %>%
  pivot_stages_longer(.,stages_from = c("k", "p"),
                      values_from = "n")
df3 <- df %>% select(3,4) %>% group_by(p,c) %>% count() %>%
  pivot_stages_longer(.,stages_from = c("p", "c"),
                      values_from = "n")

eukaryotes_sankey_4level <- 
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
            hjust=0,size=3,color="black",fontface="plain")+
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
            hjust=0,size=3,vjust=0.5,color="black",fontface="plain")+
  
  coord_cartesian(clip="off")+
  scale_x_discrete(position = "bottom")+
  # scale_fill_manual(values = met.brewer("Nizami")) +
  # scale_color_manual(values = met.brewer("Nizami")) +
  theme_void()+
  theme(plot.margin = margin(0,0,0,0,unit = "in"),
        axis.text.x=element_text(color="black",face="plain",size=12,
                                 margin = margin(b=0.1,unit = "cm")),
        legend.position="none")

eukaryotes_sankey_4level

setwd("D:/pku/01groundwater/03eu_pre/12eu_contig")
ggsave("2eucontig_sankey_4level.pdf",eukaryotes_sankey_4level,width = 6,height = 3.8,units = 'in')

write.csv(df,"fig2b_2.csv")

#########################统计各分类级别上unknown的比例绘制柱状图#############

# 计算每列中 "unknown" 字符串的比例
unknown_tax_bili <- apply(contig_tax_filter[, 11:18], 2, function(x) {
  (sum(x == "unknown", na.rm = TRUE) + 58420) / (length(x) + 58420) 
}) %>% as.data.frame() %>% setNames("Unannotated")

unknown_tax_bili$Annotated <- 1 - unknown_tax_bili$Unannotated

rownames(unknown_tax_bili) <- c("domin","kingdom","phylum","class","order","family","genus","species")

head(unknown_tax_bili)


# 将数据框转换为长格式
unknown_tax_bili_long <- unknown_tax_bili %>%
  rownames_to_column(var = "Taxonomic_Level") %>%
  pivot_longer(cols = -Taxonomic_Level, names_to = "Category", values_to = "Percentage")

# 将 Taxonomic_Level 列转为因子，并设定顺序
unknown_tax_bili_long$Taxonomic_Level <- factor(
  unknown_tax_bili_long$Taxonomic_Level,
  levels = c("domin","kingdom", "phylum", "class", "order", "family", "genus","species")
)

# 绘制百分比柱状图
GWEC_tax_bili <- 
ggplot(unknown_tax_bili_long, aes(x = Taxonomic_Level, y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", position = "stack",width = 0.7) +
  #coord_polar()+
  coord_flip() +  # 将条形图横放
  scale_y_continuous(labels = scales::percent_format()) +  # 将 y 轴标签转换为百分比格式
  labs(
    x = "",
    y = "Relative proportion",
    title = "Taxonomic annotation of the GWECs",
    fill = "Category"
  ) +
  scale_fill_manual(values = c("Unannotated"="#FF8F8F","Annotated"="#EFEFEF"))+
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
        axis.text.x = element_text(angle = 0,size = 10,hjust = 0.5,vjust = 1),
        axis.text.y = element_text(size = 10),
        legend.position = "none")

GWEC_tax_bili

setwd("D:/pku/01groundwater/03eu_pre/12eu_contig")
ggsave("2GWEC_tax_bili.pdf",GWEC_tax_bili,width = 3.78,height = 2,units = 'in')

write.csv(unknown_tax_bili_long,"fig2a_3.csv")
