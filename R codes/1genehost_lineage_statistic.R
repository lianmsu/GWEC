
setwd("D:/pku/01groundwater/03eu_pre/21_2genehost_lineage")

gene_host_lineage <- read.delim2("merged_output_genehost_lineage.txt",
                                 sep = ' ',header = F)

taxid2lineage <- read.delim2("taxid2lineage.txt",header = F)


gene_host_lineage$lineage1 <- taxid2lineage$V2
gene_host_lineage$lineage2 <- taxid2lineage$V3

# 加载必要的包
library(tidyr)
library(dplyr)

# 分割lineage2列
# gene_host_lineage_expanded <- gene_host_lineage %>%
#   separate(
#     col = lineage2,
#     into = c("superkingdom", "phylum", "class", "order", "family", "genus", "species"),
#     sep = ";",
#     remove = FALSE,  # 保留原始列
#     fill = "right"   # 允许缺失值
#   ) %>%
#   # 清理每个分类层级名称（移除前面的"unclassified"等描述）
#   mutate(across(superkingdom:species, ~ gsub("^unclassified (.*?) (superkingdom|phylum|class|order|family|genus|species)$", "\\1", .)))

gene_host_lineage_expanded <- gene_host_lineage %>%
  separate(
    col = lineage2,
    into = c("superkingdom", "phylum", "class", "order", "family", "genus", "species"),
    sep = ";",
    remove = FALSE,
    fill = "right"
  ) %>%
  # 修改清理逻辑：包含"unclassified"则输出"unclassified"，否则保持原值
  mutate(across(superkingdom:species, ~ {
    ifelse(grepl("unclassified", .), "unclassified", .)
  }))


# 查看结果
head(gene_host_lineage_expanded[, c("V1", "superkingdom",
                                    "phylum", "class", "order",
                                    "family", "genus", "species")])

#把数据框gene_host_lineage_expanded的最后7列中的“cellular organisms”改为“Eukaryota”
# 方法2：直接操作列（如果知道列名）
# 假设最后7列是：superkingdom, phylum, class, order, family, genus, species
cols_to_modify <- c("superkingdom", "phylum", "class", "order", "family", "genus", "species")

gene_host_lineage_expanded[cols_to_modify] <- lapply(
  gene_host_lineage_expanded[cols_to_modify],
  function(x) ifelse(x == "unclassified", "Unclassified", x)
)


#2. 绘制桑基图展示物种组成###################

library(tidyverse)
library(ggsankeyfier) 
library(MetBrewer)


setwd("D:/pku/01groundwater/03eu_pre/21_2genehost_lineage")


df <- gene_host_lineage_expanded[,c(33:38)]

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
# 找出计数小于 10 的genus
low_count_genus <- genus_counts %>%
  filter(n < 10) %>%
  pull(genus)
# 将计数小于 10 的genus及其后续分类等级设置为 "others"
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
            hjust=0,size=3,vjust=0.5,color="black",fontface="plain")+
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

setwd("D:/pku/01groundwater/03eu_pre/21_2genehost_lineage")
ggsave("allgenehost_sankey_5level.pdf",eukaryotes_sankey_5level,width = 6,height = 5.3,units = 'in')



#分别绘制六种基因对应所在contig的物种组成,Mokosh_type_I__MkoA_B#################

gene_count <- gene_host_lineage_expanded %>% count(V4)

df <- gene_host_lineage_expanded[gene_host_lineage_expanded$V4 == "Mokosh_type_I__MkoA_B",c(33:38)]

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
# 找出计数小于 10 的genus
low_count_genus <- genus_counts %>%
  filter(n < 10) %>%
  pull(genus)
# 将计数小于 10 的genus及其后续分类等级设置为 "others"
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

# 5个分类等级绘图。
MkoA_sankey_5level <- 
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
            hjust=0,size=3,vjust=0.5,color="black",fontface="plain")+
  coord_cartesian(clip="off")+
  scale_x_discrete(position = "top")+
  # scale_fill_manual(values = met.brewer("Nizami")) +
  # scale_color_manual(values = met.brewer("Nizami")) +
  theme_void()+
  theme(plot.margin = margin(0,0,0,0,unit = "in"),
        axis.text.x=element_text(color="black",face="plain",size=12,
                                 margin = margin(b=0.1,unit = "cm")),
        legend.position="none")

MkoA_sankey_5level
setwd("D:/pku/01groundwater/03eu_pre/21_2genehost_lineage")
ggsave("MkoA_sankey_5level.pdf",MkoA_sankey_5level,width = 6,height = 5.3,units = 'in')


#分别绘制六种基因对应所在contig的物种组成,Zorya_TypeI__ZorD#################

gene_count <- gene_host_lineage_expanded %>% count(V4)

df <- gene_host_lineage_expanded[gene_host_lineage_expanded$V4 == "Zorya_TypeI__ZorD",c(33:38)]

# 统计 phylum 列中每个门的计数
phylum_counts <- df %>%
  count(phylum)
# 找出计数小于 5 的门
low_count_phyla <- phylum_counts %>%
  filter(n < 5) %>%
  pull(phylum)
# 将计数小于 5 的门及其后续分类等级设置为 "others"
df <- df %>%
  mutate(across(phylum:species, ~ ifelse(phylum %in% low_count_phyla, "Others", .)))

# 统计 class 列中每个门的计数
class_counts <- df %>%
  count(class)
# 找出计数小于 5 的class
low_count_class <- class_counts %>%
  filter(n < 5) %>%
  pull(class)
# 将计数小于 5 的class及其后续分类等级设置为 "others"
df <- df %>%
  mutate(across(class:species, ~ ifelse(class %in% low_count_class, "Others", .)))

# 统计 order 列中每个门的计数
order_counts <- df %>%
  count(order)
# 找出计数小于 5 的order
low_count_order <- order_counts %>%
  filter(n < 5) %>%
  pull(order)
# 将计数小于 5 的class及其后续分类等级设置为 "others"
df <- df %>%
  mutate(across(order:species, ~ ifelse(order %in% low_count_order, "Others", .)))

# 统计 family 列中每个门的计数
family_counts <- df %>%
  count(family)
# 找出计数小于 5 的order
low_count_family <- family_counts %>%
  filter(n < 5) %>%
  pull(family)
# 将计数小于 5 的class及其后续分类等级设置为 "others"
df <- df %>%
  mutate(across(family:species, ~ ifelse(family %in% low_count_family, "Others", .)))

# 统计 genus 列中每个门的计数
genus_counts <- df %>%
  count(genus)
# 找出计数小于 5 的genus
low_count_genus <- genus_counts %>%
  filter(n < 5) %>%
  pull(genus)
# 将计数小于 5 的genus及其后续分类等级设置为 "others"
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

# 5个分类等级绘图。
ZorD_sankey_5level <- 
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
            hjust=0,size=3,vjust=0.5,color="black",fontface="plain")+
  coord_cartesian(clip="off")+
  scale_x_discrete(position = "top")+
  # scale_fill_manual(values = met.brewer("Nizami")) +
  # scale_color_manual(values = met.brewer("Nizami")) +
  theme_void()+
  theme(plot.margin = margin(0,0,0,0,unit = "in"),
        axis.text.x=element_text(color="black",face="plain",size=12,
                                 margin = margin(b=0.1,unit = "cm")),
        legend.position="none")

ZorD_sankey_5level

setwd("D:/pku/01groundwater/03eu_pre/21_2genehost_lineage")
ggsave("ZorD_sankey_5level.pdf",ZorD_sankey_5level,width = 5.7,height = 3.9,units = 'in')


#分别绘制六种基因对应所在contig的物种组成,Hachiman__HamB#################

gene_count <- gene_host_lineage_expanded %>% count(V4)

df <- gene_host_lineage_expanded[gene_host_lineage_expanded$V4 == "Hachiman__HamB",c(33:38)]

# 统计 phylum 列中每个门的计数
phylum_counts <- df %>%
  count(phylum)
# 找出计数小于 5 的门
low_count_phyla <- phylum_counts %>%
  filter(n < 5) %>%
  pull(phylum)
# 将计数小于 5 的门及其后续分类等级设置为 "others"
df <- df %>%
  mutate(across(phylum:species, ~ ifelse(phylum %in% low_count_phyla, "Others", .)))

# 统计 class 列中每个门的计数
class_counts <- df %>%
  count(class)
# 找出计数小于 5 的class
low_count_class <- class_counts %>%
  filter(n < 5) %>%
  pull(class)
# 将计数小于 5 的class及其后续分类等级设置为 "others"
df <- df %>%
  mutate(across(class:species, ~ ifelse(class %in% low_count_class, "Others", .)))

# 统计 order 列中每个门的计数
order_counts <- df %>%
  count(order)
# 找出计数小于 5 的order
low_count_order <- order_counts %>%
  filter(n < 5) %>%
  pull(order)
# 将计数小于 5 的class及其后续分类等级设置为 "others"
df <- df %>%
  mutate(across(order:species, ~ ifelse(order %in% low_count_order, "Others", .)))

# 统计 family 列中每个门的计数
family_counts <- df %>%
  count(family)
# 找出计数小于 5 的order
low_count_family <- family_counts %>%
  filter(n < 5) %>%
  pull(family)
# 将计数小于 5 的class及其后续分类等级设置为 "others"
df <- df %>%
  mutate(across(family:species, ~ ifelse(family %in% low_count_family, "Others", .)))

# 统计 genus 列中每个门的计数
genus_counts <- df %>%
  count(genus)
# 找出计数小于 5 的genus
low_count_genus <- genus_counts %>%
  filter(n < 5) %>%
  pull(genus)
# 将计数小于 5 的genus及其后续分类等级设置为 "others"
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

# 5个分类等级绘图。
HamB_sankey_5level <- 
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
            hjust=0,size=3,vjust=0.5,color="black",fontface="plain")+
  coord_cartesian(clip="off")+
  scale_x_discrete(position = "top")+
  # scale_fill_manual(values = met.brewer("Nizami")) +
  # scale_color_manual(values = met.brewer("Nizami")) +
  theme_void()+
  theme(plot.margin = margin(0,0,0,0,unit = "in"),
        axis.text.x=element_text(color="black",face="plain",size=12,
                                 margin = margin(b=0.1,unit = "cm")),
        legend.position="none")

HamB_sankey_5level


setwd("D:/pku/01groundwater/03eu_pre/21_2genehost_lineage")
ggsave("HamB_sankey_5level.pdf",HamB_sankey_5level,width = 5.7,height = 3.9,units = 'in')

#分别绘制六种基因对应所在contig的物种组成,Brig1__ADP_ribosyl#################

gene_count <- gene_host_lineage_expanded %>% count(V4)

df <- gene_host_lineage_expanded[gene_host_lineage_expanded$V4 == "Brig1__ADP_ribosyl",c(33:38)]

# 统计 phylum 列中每个门的计数
phylum_counts <- df %>%
  count(phylum)
# 找出计数小于 5 的门
low_count_phyla <- phylum_counts %>%
  filter(n < 5) %>%
  pull(phylum)
# 将计数小于 5 的门及其后续分类等级设置为 "others"
df <- df %>%
  mutate(across(phylum:species, ~ ifelse(phylum %in% low_count_phyla, "Others", .)))

# 统计 class 列中每个门的计数
class_counts <- df %>%
  count(class)
# 找出计数小于 5 的class
low_count_class <- class_counts %>%
  filter(n < 5) %>%
  pull(class)
# 将计数小于 5 的class及其后续分类等级设置为 "others"
df <- df %>%
  mutate(across(class:species, ~ ifelse(class %in% low_count_class, "Others", .)))

# 统计 order 列中每个门的计数
order_counts <- df %>%
  count(order)
# 找出计数小于 5 的order
low_count_order <- order_counts %>%
  filter(n < 5) %>%
  pull(order)
# 将计数小于 5 的class及其后续分类等级设置为 "others"
df <- df %>%
  mutate(across(order:species, ~ ifelse(order %in% low_count_order, "Others", .)))

# 统计 family 列中每个门的计数
family_counts <- df %>%
  count(family)
# 找出计数小于 5 的order
low_count_family <- family_counts %>%
  filter(n < 5) %>%
  pull(family)
# 将计数小于 5 的class及其后续分类等级设置为 "others"
df <- df %>%
  mutate(across(family:species, ~ ifelse(family %in% low_count_family, "Others", .)))

# 统计 genus 列中每个门的计数
genus_counts <- df %>%
  count(genus)
# 找出计数小于 5 的genus
low_count_genus <- genus_counts %>%
  filter(n < 5) %>%
  pull(genus)
# 将计数小于 5 的genus及其后续分类等级设置为 "others"
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

# 5个分类等级绘图。
Brig1_sankey_5level <- 
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
            hjust=0,size=3,vjust=0.5,color="black",fontface="plain")+
  coord_cartesian(clip="off")+
  scale_x_discrete(position = "top")+
  # scale_fill_manual(values = met.brewer("Nizami")) +
  # scale_color_manual(values = met.brewer("Nizami")) +
  theme_void()+
  theme(plot.margin = margin(0,0,0,0,unit = "in"),
        axis.text.x=element_text(color="black",face="plain",size=12,
                                 margin = margin(b=0.1,unit = "cm")),
        legend.position="none")

Brig1_sankey_5level


setwd("D:/pku/01groundwater/03eu_pre/21_2genehost_lineage")
ggsave("Brig1_sankey_5level.pdf",Brig1_sankey_5level,width = 5.4,height = 3.4,units = 'in')


#分别绘制六种基因对应所在contig的物种组成,CBASS__AG_E1_ThiF#################

gene_count <- gene_host_lineage_expanded %>% count(V4)

df <- gene_host_lineage_expanded[gene_host_lineage_expanded$V4 == "CBASS__AG_E1_ThiF",c(33:38)]

# 统计 phylum 列中每个门的计数
phylum_counts <- df %>%
  count(phylum)
# 找出计数小于 5 的门
low_count_phyla <- phylum_counts %>%
  filter(n < 5) %>%
  pull(phylum)
# 将计数小于 5 的门及其后续分类等级设置为 "others"
df <- df %>%
  mutate(across(phylum:species, ~ ifelse(phylum %in% low_count_phyla, "Others", .)))

# 统计 class 列中每个门的计数
class_counts <- df %>%
  count(class)
# 找出计数小于 5 的class
low_count_class <- class_counts %>%
  filter(n < 5) %>%
  pull(class)
# 将计数小于 5 的class及其后续分类等级设置为 "others"
df <- df %>%
  mutate(across(class:species, ~ ifelse(class %in% low_count_class, "Others", .)))

# 统计 order 列中每个门的计数
order_counts <- df %>%
  count(order)
# 找出计数小于 5 的order
low_count_order <- order_counts %>%
  filter(n < 5) %>%
  pull(order)
# 将计数小于 5 的class及其后续分类等级设置为 "others"
df <- df %>%
  mutate(across(order:species, ~ ifelse(order %in% low_count_order, "Others", .)))

# 统计 family 列中每个门的计数
family_counts <- df %>%
  count(family)
# 找出计数小于 5 的order
low_count_family <- family_counts %>%
  filter(n < 5) %>%
  pull(family)
# 将计数小于 5 的class及其后续分类等级设置为 "others"
df <- df %>%
  mutate(across(family:species, ~ ifelse(family %in% low_count_family, "Others", .)))

# 统计 genus 列中每个门的计数
genus_counts <- df %>%
  count(genus)
# 找出计数小于 5 的genus
low_count_genus <- genus_counts %>%
  filter(n < 5) %>%
  pull(genus)
# 将计数小于 5 的genus及其后续分类等级设置为 "others"
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

# 5个分类等级绘图。
ThiF_sankey_5level <- 
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
            hjust=0,size=3,vjust=0.5,color="black",fontface="plain")+
  coord_cartesian(clip="off")+
  scale_x_discrete(position = "top")+
  # scale_fill_manual(values = met.brewer("Nizami")) +
  # scale_color_manual(values = met.brewer("Nizami")) +
  theme_void()+
  theme(plot.margin = margin(0,0,0,0,unit = "in"),
        axis.text.x=element_text(color="black",face="plain",size=12,
                                 margin = margin(b=0.1,unit = "cm")),
        legend.position="none")

ThiF_sankey_5level


setwd("D:/pku/01groundwater/03eu_pre/21_2genehost_lineage")
ggsave("ThiF_sankey_5level.pdf",ThiF_sankey_5level,width = 5.4,height = 3.4,units = 'in')


#分别绘制六种基因对应所在contig的物种组成,Wadjet__JetC_III#################

gene_count <- gene_host_lineage_expanded %>% count(V4)

df <- gene_host_lineage_expanded[gene_host_lineage_expanded$V4 == "Wadjet__JetC_III",c(33:38)]

# 统计 phylum 列中每个门的计数
phylum_counts <- df %>%
  count(phylum)
# 找出计数小于 5 的门
low_count_phyla <- phylum_counts %>%
  filter(n < 5) %>%
  pull(phylum)
# 将计数小于 5 的门及其后续分类等级设置为 "others"
df <- df %>%
  mutate(across(phylum:species, ~ ifelse(phylum %in% low_count_phyla, "Others", .)))

# 统计 class 列中每个门的计数
class_counts <- df %>%
  count(class)
# 找出计数小于 5 的class
low_count_class <- class_counts %>%
  filter(n < 5) %>%
  pull(class)
# 将计数小于 5 的class及其后续分类等级设置为 "others"
df <- df %>%
  mutate(across(class:species, ~ ifelse(class %in% low_count_class, "Others", .)))

# 统计 order 列中每个门的计数
order_counts <- df %>%
  count(order)
# 找出计数小于 5 的order
low_count_order <- order_counts %>%
  filter(n < 5) %>%
  pull(order)
# 将计数小于 5 的class及其后续分类等级设置为 "others"
df <- df %>%
  mutate(across(order:species, ~ ifelse(order %in% low_count_order, "Others", .)))

# 统计 family 列中每个门的计数
family_counts <- df %>%
  count(family)
# 找出计数小于 5 的order
low_count_family <- family_counts %>%
  filter(n < 5) %>%
  pull(family)
# 将计数小于 5 的class及其后续分类等级设置为 "others"
df <- df %>%
  mutate(across(family:species, ~ ifelse(family %in% low_count_family, "Others", .)))

# 统计 genus 列中每个门的计数
genus_counts <- df %>%
  count(genus)
# 找出计数小于 5 的genus
low_count_genus <- genus_counts %>%
  filter(n < 5) %>%
  pull(genus)
# 将计数小于 5 的genus及其后续分类等级设置为 "others"
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

# 5个分类等级绘图。
JetC_sankey_5level <- 
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
            hjust=0,size=3,vjust=0.5,color="black",fontface="plain")+
  coord_cartesian(clip="off")+
  scale_x_discrete(position = "top")+
  # scale_fill_manual(values = met.brewer("Nizami")) +
  # scale_color_manual(values = met.brewer("Nizami")) +
  theme_void()+
  theme(plot.margin = margin(0,0,0,0,unit = "in"),
        axis.text.x=element_text(color="black",face="plain",size=12,
                                 margin = margin(b=0.1,unit = "cm")),
        legend.position="none")

JetC_sankey_5level


setwd("D:/pku/01groundwater/03eu_pre/21_2genehost_lineage")
ggsave("JetC_sankey_5level.pdf",JetC_sankey_5level,width = 4.9,height = 2.6,units = 'in')



#统计antiphage/antivirus真核同源物的宿主###########














