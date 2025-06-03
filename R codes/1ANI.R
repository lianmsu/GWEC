
setwd("D:/pku/01groundwater/03eu_pre/15euMAG_ANI")
ani <- read.table("fastani.result.txt", sep = '\t', header = TRUE, row.names = 1)
# 删除行名中的 "_renamed.fa"
rownames(ani) <- gsub("_renamed.fa", "", rownames(ani))
# 删除列名中的 "_renamed.fa"
colnames(ani) <- gsub("_renamed.fa", "", colnames(ani))

library(ggplot2)
library(pheatmap)
p0 <-
  pheatmap(ani,
           fontsize = 8,#字体大小
           # fontface="italic",#斜体,其中'font'和'fontface'两个量只能设定一个
           border=FALSE,#边框
           cluster_rows = F,
           cluster_cols = F,
           treeheight_row = 0,
           treeheight_col = 0,
          # color = c("#DDDDDD", colorRampPalette(c("#EFA5CA", "#AAE4E4", "#7272FF"))(50)),  # 颜色向量
          # breaks = c(-Inf, 0, seq(75, 100, length.out = 51))  # 断点设置
           color = colorRampPalette(c( "#EFA5CA","#AAE4E4", "#7272FF"))(50)#字体颜色
  )#导出为图片
p0

setwd("D:/pku/01groundwater/03eu_pre/15euMAG_ANI")
ggsave(plot=p0,'1_30MAG_ANI_legend.pdf',width = 4.9,height = 4.4,units = 'in')


# 使用lapply逐列检查并替换NaN为0
ani[] <- lapply(ani, function(x) {
  if (is.numeric(x)) {
    x[is.nan(x)] <- 0  # 将NaN替换为0
  }
  return(x)
})


library(ggplot2)
library(pheatmap)

# 定义颜色
#color_palette <- c("grey", # 0 值的颜色
#                   colorRampPalette(c("#EFA5CA", "#AAE4E4", "#7272FF"))(50)[25:50])  # 75-100的渐变色




p1 <- 
pheatmap(ani,
         fontsize = 8,#字体大小
         # fontface="italic",#斜体,其中'font'和'fontface'两个量只能设定一个
         border=FALSE,#边框
         #cluster_rows = F,
         #cluster_cols = F,
         treeheight_row = 0,
         treeheight_col = 0,
         color = c("#DDDDDD", colorRampPalette(c("#EFA5CA", "#AAE4E4", "#7272FF"))(50)),  # 颜色向量
         breaks = c(-Inf, 0, seq(75, 100, length.out = 51))  # 断点设置
         #color = colorRampPalette(c( "#EFA5CA","#AAE4E4", "#7272FF"))(50)#字体颜色
          )#导出为图片
p1
# 获取聚类后的行顺序
clustered_row_names <- rownames(ani)[p1$tree_row$order]
print(clustered_row_names)

setwd("D:/pku/01groundwater/03eu_pre/15euMAG_ANI")
ggsave(plot=p1,'1_30MAG_ANI.pdf',width = 4.9,height = 4.4,units = 'in')


write.csv(ani,"fig2c_1.csv")
