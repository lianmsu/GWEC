#处理metaeuk的物种注释结果文件。
library(dplyr)
library(tidyr)

setwd("D:/pku/01groundwater/02eukaryotes/01metaeuk")
contig_tax <- read.csv("eMAG_tax_tax_per_contig.tsv",sep = "\t")

contig_tax_filter <- contig_tax[contig_tax$Rank!="no rank",]  

# 假设 contig_tax_filter 是你的数据框
contig_tax_filter <- contig_tax_filter %>%
  mutate(MAGs = sapply(strsplit(as.character(Contig), "_"), function(x) paste(x[1:2], collapse = "_")))

# 找到 Detailed_linage 列中最多有多少个分隔符
max_split_count <- max(sapply(strsplit(contig_tax_filter$Detail_lineage, ";"), length))
# 使用这个最大值来确定列数
contig_tax_filter <- contig_tax_filter %>%
  separate(Detail_lineage, into = paste0("col", 1:max_split_count), sep = ";", fill = "right")

mag_tax <- contig_tax_filter %>% 
  group_by(MAGs) %>%
  count(MAGs) %>%
  arrange(MAGs, desc(n))

#col1水平
sum(contig_tax_filter$col1 == "-_cellular organisms")
#删除不是"-_cellular organisms"的行
contig_tax_filter <- contig_tax_filter[contig_tax_filter$col1=="-_cellular organisms",]

#col2水平
col2 <- contig_tax_filter %>% count(col2)

sum(contig_tax_filter$col2 == "d_Eukaryota")
#删除不是"d_Eukaryota"的行
contig_tax_filter <- contig_tax_filter[contig_tax_filter$col2=="d_Eukaryota",]

#col3水平
# 对 contig_tax_filter 数据框按 MAGs 列进行分组
# 然后对每组的 col3 列进行字符串种类及数量统计
col3_result <- contig_tax_filter %>%
  group_by(MAGs) %>%  # 按照 MAGs 列进行分组
  count(col3) %>%     # 统计每个 MAG 对应的 col3 字符串种类和数量
  arrange(MAGs, desc(n))  # 可以根据需要排列结果，例如按照 MAGs 和出现次数降序排列

#col4水平
col4_result <- contig_tax_filter %>%
  group_by(MAGs) %>%  # 按照 MAGs 列进行分组
  count(col4) %>%     # 统计每个 MAG 对应的 col3 字符串种类和数量
  arrange(MAGs, desc(n)) 

#多级一起统计
# 对数据进行转换，将 col2 到 col34 转换为长格式
long_data <- contig_tax_filter %>%
  pivot_longer(cols = col3:col34,    # 指定要转换的列范围
               names_to = "col_name", # 新列，表示原列名
               values_to = "value")   # 新列，表示原列的值

# 然后按 MAGs 列进行分组，统计每个 MAG 对应的 value 列的种类及其数量
all_result <- long_data %>%
  filter(!is.na(value)) %>%
  group_by(MAGs, value) %>%  # 按 MAGs 和 value 进行分组
  summarise(count = n(), .groups = 'drop') %>%  # 统计每个值的出现次数
  arrange(MAGs, value)  # 按 MAGs 和 value 排序，按需调整排序顺序

#把表格中n=1的行删除
all_result <- all_result[all_result$count != 1,]

#把n<10的也删除
all_result <- all_result[all_result$count > 10,]

# 假设你的数据框叫 df，并且它包含一个 name 列
all_result$value <- gsub("^-_", "", all_result$value)

setwd("D:/pku/01groundwater/02eukaryotes/01metaeuk")
write.csv(all_result,file = "MAGtax_contignum.csv",quote = F)



