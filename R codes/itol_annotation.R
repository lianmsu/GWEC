
#真核MAGs itol系统发育树注释

# Tong Zhou, Kuidong Xu, Feng Zhao et. al. itol.toolkit accelerates working with 
# iTOL (Interactive Tree Of Life) by an automated generation of annotation files, 
# Bioinformatics, 2023;, btad339, https://doi.org/10.1093/bioinformatics/btad339

#参考学习https://tongzhou2017.github.io/itol.toolkit/articles/Get_Start.html
rm(list = ls())
library(itol.toolkit)
library(data.table) # 用于元数据文件读取
library(ape) # 用于树文件读写
library(stringr) # 用于字符串处理
library(dplyr)

setwd("D:/pku/01groundwater/02eukaryotes/04euk_phylo")
# 假设您的树存储在一个名为"tree"的变量中
tree <- "fasttree4.nwk"

phylo <- ape::read.tree(tree)

hub <- create_hub(tree)

#1.计算树的特征####
node_depths <- node.depth(phylo)
print(node_depths)

#节点支持度
boot_result <- boot.trees(tree, stat="max", B=100) # B是自举次数
#树长
tree_length <- sum(phylo$edge.length)

node_depths <- node.depth(phylo)#

#计算每个节点的系统发育深度
phylo_depth <- vcv.phylo(phylo)#消耗内存巨大，算一次就行
phylo_depth_diag <- diag(phylo_depth) %>% as.data.frame() %>% setNames('phylo_depth')
write.csv(phylo_depth_diag,"1453archaeal_phylo_depth_diag.csv")

# 进化树有多少末端分类单元？
Ntip(phylo)
# 查看末端分类单元名称， 取前5个
phylo$tip.label[1:5]

#2.准备itol树的注释文件

#2.1 物种注释信息####
setwd("D:/pku/01groundwater/02eukaryotes/02eukcc")
mags_tax <- readxl::read_xlsx('eukcc.xlsx')

#按门设置颜色
library(dplyr)
df_phycolor <- mags_tax %>% select(bin, clade2)
set.seed(666)
setwd("D:/pku/01groundwater/02eukaryotes/04euk_phylo")

#手动为门添加颜色
colorss <- c(
  # "#1f77b4",  # 蓝色
  # "#ff7f0e",  # 橙色
  # "#2ca02c",  # 绿色
  # "#d62728",  # 红色
  # "#9467bd",  # 紫色
  # "#8c564b",  # 棕色
  # "#e377c2",  # 粉色
  #"#7f7f7f",  # 灰色
  # "#bcbd22",  # 橄榄色
  # "#17becf",  # 青色
  # "#aec7e8",  # 浅蓝色
  # "#ffbb78",  # 浅橙色
  # "#98df8a",  # 浅绿色
  # "#ff9896",  # 浅红色
  # "#c5b0d5",  # 浅紫色
  # "#c49c94",  # 浅棕色
  "#eae4e9",
  "#fff1e6",
  "#fde2e4",
  "#fad2e1",
  "#e2ece9",
  "#bee1e6",
  "#f0efeb",
  "#dfe7fd"
  # "#f7b6d2",  # 浅粉色
  # "#dbdb8d",  # 浅橄榄色
  # "#9edae5",  # 浅青色
  # "#393b79",  # 深蓝色
  # "#637939"   # 深绿色
)
element_counts <- df_phycolor %>%
  count(clade2)

element_count_sorted <- element_counts[order(-element_counts$n), ]
element_count_sorted$color <- c(colorss)

#
df_phycolor <- merge(df_phycolor,element_count_sorted,by.x = "clade2",by.y = "clade2")
df_phycolor <- df_phycolor[, c("bin", "clade2","color")]

#subtype = "range"才能大面积改变颜色
unit_2 <- create_unit(data = df_phycolor, 
                      key = "1euMAG25_tax_color_rangetree4", 
                      type = "TREE_COLORS", 
                      subtype = "range", 
                      #color = "wesanderson",  #需要的颜色超过了集合中有的颜色
                      # line_type = c(rep("normal",4),"dashed"),
                      # size_factor = 5, 
                      tree = phylo)
write_unit(unit_2)

unit_3 <- create_unit(data = df_phycolor, 
                      key = "1euMAG25_phylo_color_branch_tree4", 
                      type = "TREE_COLORS", 
                      subtype = "branch", 
                      #color = "wesanderson",  #需要的颜色超过了集合中有的颜色
                      # line_type = c(rep("normal",4),"dashed"),
                      # size_factor = 5, 
                      tree = phylo)
write_unit(unit_3)
#MAG size####

setwd("D:/pku/01groundwater/02eukaryotes/00eMAGs")

size_info <- read.csv('genome_stats.csv',sep = ",",header = T)
size_info$MAG_size <- size_info$total_length/1000000
size_info$N50 <- size_info$N50/1000
size_info$N90 <- size_info$N90/1000

#size_info <- size_info[size_info$Name %in% mags_tax$binname,]
#MAGs的大小
setwd("D:/pku/01groundwater/02eukaryotes/04euk_phylo")
unit_4 <- create_unit(data = size_info[,c(1,3,4,5)],
                      key = "2mag_length_tree4", 
                      type = "DATASET_MULTIBAR", 
                      tree = tree)
write_unit(unit_4)


mag_size <- size_info[,c(1,3)]
mag_size$mb2 <- mag_size$N50
unit_5 <- create_unit(data = mag_size,
                      key = "mag_N50", 
                      type = "DATASET_SIMPLEBAR", 
                      method = 'mean',
                      tree = tree)
write_unit(unit_5)

mag_size <- size_info[,c(1,5)]
mag_size$mb2 <- mag_size$MAG_size
unit_6 <- create_unit(data = mag_size,
                      key = "1euMAG25_total_size_tree4", 
                      type = "DATASET_SIMPLEBAR", 
                      method = 'mean',
                      tree = tree)
write_unit(unit_6)

#基因组完整度信息
setwd("D:/pku/01groundwater/02eukaryotes/02busco")
busco <- read.csv("batch_summary.txt",header = T,sep = '\t')
busco$Completeness <- (busco$Complete+busco$Fragmented)

setwd("D:/pku/01groundwater/02eukaryotes/04euk_phylo")
quality <- busco[,c(1,13)]
quality$mb2 <- quality$Completeness
unit_7 <- create_unit(data = quality,
                      key = "3mag_completeness_tree4", 
                      type = "DATASET_SIMPLEBAR", 
                      method = 'mean',
                      color = "#9c89b8",
                      tree = tree)
write_unit(unit_7)



#添加主要DSs的信息####
#读取DS
#DSs读取
setwd("D:/wenjian/research/defensome/groundwater/archaea/02defense")
ar_defense <- read.csv("archaea_ds_counts_matrix.csv",check.names = F)
ar_defense$genome <- gsub('*.fna','',ar_defense$genome)

#50_10_MAG,checkm1
setwd("D:/wenjian/research/defensome/groundwater/archaea/01MAG_info")
MAG_5010 <- readxl::read_xlsx("2269_metainfo.xlsx")

#70_10_MAG,checkm1
setwd("D:/wenjian/research/defensome/groundwater/archaea/01MAG_info")
MAG_7010 <- readxl::read_xlsx("zhong_70_10.xlsx")

#MAG_size_info,checkm2
setwd("D:/wenjian/research/defensome/groundwater/archaea/01MAG_info")
MAG_checkm2 <- read.csv("quality_report_checkm2.tsv",sep = '\t',check.names = F)

#合并不含DS的MAG
not_in <- MAG_5010$binname[!MAG_5010$binname %in% ar_defense$genome]
# 使用不在 ar_defense 行名中的 genome 值创建新的数据框
new_rows <- data.frame(
  genome = not_in,  # 重复 genome 值
  stringsAsFactors = FALSE)
# 为 new_rows 添加与 ar_defense 相同的列，并将这些列的值填充为 0
col_names <- setdiff(names(ar_defense), "genome")  # 获取 dss 中除了 genome 之外的列名
new_rows[col_names] <- lapply(col_names, function(col_name) rep(0, nrow(ar_defense)))
rownames(new_rows) <- new_rows$genome
#合并new_rows与dss
ar_defense <- rbind(ar_defense,new_rows)

#提取70_10的MAGs
ar_defense_7010 <- ar_defense[ar_defense$genome %in% MAG_7010$binname,]
rownames(ar_defense_7010) <- ar_defense_7010$genome

## 计算每一列非0数字所占的百分比并输出到新数据框
percent_nonzero <- colSums(ar_defense_7010[,-1] != 0) / nrow(ar_defense_7010) * 100
new_df <- data.frame(Column = names(percent_nonzero), Percentage = percent_nonzero)
# 按照值的大小对数据集进行降序排列
ds_persentage <- new_df[order(-new_df$Percentage), ]
ds_persentage <- ds_persentage[ds_persentage$Percentage>=0.5,]

dss_22 <- ar_defense_7010[,colnames(ar_defense_7010) %in% ds_persentage$Column]
#将dss_33中大于0的值全部设置为1
dss_22 <- dss_22 %>%
  mutate(across(everything(), ~if_else(. > 0, 1, .)))

dss_22$user_genome <- rownames(dss_22)
dss_22 <- dss_22 %>%
  select(user_genome, everything())

setwd("D:/wenjian/research/defensome/groundwater/archaea/1gtdb_tree")
unit_5 <- create_unit(data = dss_22,
                      key = 'dss22_p-a_heatmap',
                      type = 'DATASET_HEATMAP',
                      tree = tree
)

unit_5@specific_themes$heatmap$tree$tree_display <- 0 #不聚类
unit_5@specific_themes$heatmap$color$min <- "#ffffff"
unit_5@specific_themes$heatmap$color$max <- "#EBB795"
write_unit(unit_5)

#######DS和DG的密度

#DS density
md_archaea <- merge(MAG_7010,MAG_checkm2,by.x = 'binname',by.y = 'Name')
ar_de_sum <- data.frame(ds_sum = rowSums(ar_defense_7010[,-1]),
                        genome = ar_defense_7010$genome,
                        richness = apply(ar_defense_7010[,-1], 1, function(row) sum(row > 0))
)
md_archaea <- merge(md_archaea,ar_de_sum,by.x = 'binname',by.y = 'genome')
md_archaea$ds_density <- md_archaea$ds_sum/md_archaea$size*1000000

#DG density
#DGs读取
setwd("D:/wenjian/research/defensome/groundwater/archaea/02defense")
ar_dg <- read.csv("archaea_dg_counts_matrix.csv",check.names = F)
ar_dg$genome <- gsub("*.fna","",ar_dg$genome)

#50_10_MAG,checkm1
setwd("D:/wenjian/research/defensome/groundwater/archaea/01MAG_info")
MAG_5010 <- readxl::read_xlsx("2269_metainfo.xlsx")

#70_10_MAG,checkm1
setwd("D:/wenjian/research/defensome/groundwater/archaea/01MAG_info")
MAG_7010 <- readxl::read_xlsx("zhong_70_10.xlsx")
#write.table(MAG_7010,file="mag_7010_zhong.txt",quote = F,sep = '\t',row.names = F)
#MAG_size_info,checkm2
#setwd("D:/wenjian/research/defensome/groundwater/archaea/01MAG_info")
#MAG_checkm2 <- read.csv("quality_report_checkm2.tsv",sep = '\t',check.names = F)

#合并不含DS的MAG
not_in <- MAG_5010$binname[!MAG_5010$binname %in% ar_dg$genome]
# 使用不在 ar_defense 行名中的 genome 值创建新的数据框
new_rows <- data.frame(
  genome = not_in,  # 重复 genome 值
  stringsAsFactors = FALSE)
# 为 new_rows 添加与 ar_defense 相同的列，并将这些列的值填充为 0
col_names <- setdiff(names(ar_dg), "genome")  # 获取 dss 中除了 genome 之外的列名
new_rows[col_names] <- lapply(col_names, function(col_name) rep(0, nrow(ar_dg)))
rownames(new_rows) <- new_rows$genome
#合并new_rows与dss
ar_dg <- rbind(ar_dg,new_rows)

#提取70_10的MAGs
ar_dg_7010 <- ar_dg[ar_dg$genome %in% MAG_7010$binname,]
sum(rowSums(ar_dg_7010[,-1])!=0) #956个MAGs中找到353种DGs，共5427个。

ar_dg_sum <- data.frame(dg_sum = rowSums(ar_dg_7010[,-1]),
                        genome = ar_dg_7010$genome,
                        dgrichness = apply(ar_dg_7010[,-1], 1, function(row) sum(row > 0))
)

md_archaea <- merge(md_archaea,ar_dg_sum, by.x='binname',by.y = 'genome')
md_archaea$dgdensity <- md_archaea$dg_sum/md_archaea$size*1000000


#准备itol的热图注释文件
setwd("D:/wenjian/research/defensome/groundwater/archaea/1gtdb_tree")

ar_ds_density <- md_archaea[,c('binname','ds_density')]
ar_ds_density$den2 <- ar_ds_density$ds_density
unit_6 <- create_unit(data = ar_ds_density,
                      key = 'ds_density_heatmap',
                      type = 'DATASET_GRADIENT',
                      method = 'mean',
                      tree = tree
)
unit_6@specific_themes$heatmap$color$min <- "white"
unit_6@specific_themes$heatmap$color$mid <- "#74c69d"
unit_6@specific_themes$heatmap$color$max <- "#1b4332"
write_unit(unit_6)

ar_dg_density <- md_archaea[,c('binname','dgdensity')]
ar_dg_density$den2 <- ar_dg_density$dgdensity
unit_7 <- create_unit(data = ar_dg_density,
                      key = 'dg_density_heatmap',
                      type = 'DATASET_GRADIENT',
                      method = 'mean',
                      tree = tree
)
unit_7@specific_themes$heatmap$color$min <- "white"
unit_7@specific_themes$heatmap$color$mid <- "#8DB3D8"
unit_7@specific_themes$heatmap$color$max <- "#1969AF"
write_unit(unit_7)



