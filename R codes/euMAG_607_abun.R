
library(dplyr)

#1. 真核MAG 丰度预处理###########
setwd("D:/pku/01groundwater/02eukaryotes/03mag_abundance")

euMAG_abun <- read.table("merged_rpkm.tsv", sep = '\t',header = T,row.names = 1)


sample_RPKM_sum <- colSums(euMAG_abun) %>% as.data.frame() %>% setNames('RPKM_sum')
sample_RPKM_sum$sample <- rownames(sample_RPKM_sum)
sample_RPKM_sum$sample <- gsub('.sorted.RPKM','',sample_RPKM_sum$sample)

sample_RPKM_sum$RPKM_ap <- sample_RPKM_sum$RPKM_sum
sample_RPKM_sum$RPKM_ap <- ifelse(sample_RPKM_sum$RPKM_ap > 0, 'present', 'absent') 


#2. 地图上的点以真核MAG存在/不存在着色##########################
library(readxl)
library(ggspatial)
library(ggplot2)
library(cowplot)
library(colorspace)
library(sf)
library(dplyr)



### 设置工作路径
setwd("D:/pku/01groundwater/02eukaryotes/03mag_abundance")

## 读取采样点经纬度数据
### 读取数据
scatter_df <- read_xlsx("location.xlsx", sheet = 1) ### 提取plotlocation数据文件中第1个sheet中的数据
### 样点数据包括：经度lon和纬度lat信息
scatter_df_tro <- st_as_sf(scatter_df, coords = c("lng", "lat"), crs = 4326) ## 设定参考坐标系


##
sample_df <- read_xlsx('GW_DATA_lpw.xlsx')
sample_df2 <- read_xlsx('env.xlsx',sheet = 3) %>% select(ID,zone,新建改建,经度,纬度)

lat_lng1 <- sample_df2[sample_df2$ID %in% sample_df$sample,]
# 删除所有存在NA的行
lat_lng1 <- na.omit(lat_lng1)
colnames(lat_lng1) <- c('ID','zone','type','lon','lat')


scatter_df_tro <- st_as_sf(lat_lng1, coords = c("lon", "lat"), crs = 4326) ## 设定参考坐标系


#师兄数据
genome_info <- read_xlsx('Geoinfo.xlsx')
zone_info <- read_xlsx('Zone.xlsx')
sample_info <- merge(genome_info,zone_info,by = 'Sample ID')

sample_info <- merge(sample_info, sample_RPKM_sum, by.x = 'Sample ID',
                     by.y = 'sample')

scatter_df_tro <- st_as_sf(sample_info,
                           coords = c("Longitude", "Latitude"),
                           crs = 4326,na.fail = F) ## 设定参考坐标系


### 加载标准的中国地图底图
# china_shp <- "中国省级地图GS（2019）1719号.geojson"
# nine <- "九段线GS（2019）1719号.geojson"
china <- sf::read_sf('https://geo.datav.aliyun.com/areas_v3/bound/100000_full.json')
#nine_line <- sf::read_sf(nine)
# head(china) ## 查看中国地图行政区划的信息

#加载地理分区
dilifenqu <- sf::read_sf("./bar/bar.shp")
#dilifenqu <- dilifenqu[dilifenqu$QDABB1!='',]

my_dili <- dilifenqu[c(1:37,40,41),]


## 绘制中国大陆地图并添加采样点信息
china_map <- ggplot() +
  geom_sf(data = my_dili, aes(fill = QDABB1), size = 0.2, color = 'black', lwd = 0.5) + # 中国地图轮廓
  scale_fill_manual(values = c("I"="#FCB2AF","Ⅱ"="#9BDFDF",
                               "Ⅲ"="#FFE2CE","Ⅳ"= "#C4D8E9",
                               "Ⅴ"= "#BEBCDF","Ⅵ"="#FB8C62","Ⅶ"="#FFF3CA",
                               "present" = "#00b4d8", "absent" = "#ff6b6b"
  )) +
  # 调整样点信息的外观
  geom_sf(data = scatter_df_tro, 
          aes(fill = RPKM_ap,color=RPKM_ap),
          shape = 21,           # 设置形状为带边框的圆形
          size = 1.5,           # 增大点的大小
          stroke = 0.5,         # 设置点边界的宽度
         # color = '#00b4d8',      # 边界颜色设置为黑色
          #fill = '#00b4d8',        # 填充颜色为蓝色
          alpha = 0.7) +        # 设置透明度
  # geom_sf(data = scatter_df_tro, 
  #         #aes(color=`Geo-environmental zone`),
  #         color = 'blue', 
  #         shape = 20,
  #         size = 0.8) + # 添加样点信息
  # 为 RPKM_sum 设置边框颜色梯度
  #scale_color_gradient(low = "#ff6b6b", high = "#00b4d8", name = "RPKM Sum") +
  geom_sf(data = china[c(21,32,35),], fill = 'NA', size = 0.5,lwd = 0.5, color = 'black') + 
  coord_sf(ylim = c(-2387082, 1654989), crs = '+proj=laea +lat_0=40 +lon_0=104') + #截取中国地图，不在大图上显示九段线区域
  # 添加比例尺
  annotation_scale(location = 'bl') +
  # 添加指北针
  annotation_north_arrow(location = 'tl', which_north = 'false', style = north_arrow_nautical()) +
  # 添加审图号并设定位置
  #labs(caption = '审图号: GS (2019) 1719号')+
  theme(plot.caption = element_text(hjust = 0, vjust = 0.0, size = 6.0)) + ## 设置主题的位置和大小，face = 'bold'为字体加粗
  # 设置坐标轴字体
  theme_linedraw()+
  theme(text = element_text(family = 'sans',size = 10, face = "bold"),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "grey80", linewidth = 0.06), ## 设置轴标签大小
        legend.key = element_rect(fill = "NA"),
        #        legend.position = 'bottom',
        legend.position = "none")
china_map

## 绘制中国大陆地图并添加采样点信息
china_map <- ggplot() +
  geom_sf(data = my_dili, aes(fill = QDABB1), size = 0.2, color = 'black', lwd = 0.5) + # 中国地图轮廓
  scale_fill_manual(values = c("I"="#FCB2AF","Ⅱ"="#9BDFDF",
                               "Ⅲ"="#FFE2CE","Ⅳ"= "#C4D8E9",
                               "Ⅴ"= "#BEBCDF","Ⅵ"="#FB8C62","Ⅶ"="#FFF3CA",
                               "present" = "#00b4d8", "absent" = "#ff6b6b"
  )) +
  # 调整样点信息的外观
  geom_sf(data = scatter_df_tro, 
          aes(fill = RPKM_ap,       # 将 fill 映射到 RPKM_ap 列
              color = RPKM_ap,     # 将 color 映射到 RPKM_sum 列
              size = RPKM_sum),
          #shape = 21,           # 设置形状为带边框的圆形
         # size = 1.5,           # 增大点的大小
          stroke = 0.5,         # 设置点边界的宽度
          #color = 'blue',      # 边界颜色设置为黑色
          #fill = '#00b4d8',        # 填充颜色为蓝色
          alpha = 0.7) +        # 设置透明度
  # geom_sf(data = scatter_df_tro, 
  #         #aes(color=`Geo-environmental zone`),
  #         color = 'blue', 
  #         shape = 20,
  #         size = 0.8) + # 添加样点信息
  # 为 RPKM_sum 设置边框颜色梯度
  #scale_color_gradient(low = "#ff6b6b", high = "#00b4d8", name = "RPKM Sum") +
  geom_sf(data = china[c(21,32,35),], fill = 'NA', size = 0.5,lwd = 0.5, color = 'black') + 
  coord_sf(ylim = c(-2387082, 1654989), crs = '+proj=laea +lat_0=40 +lon_0=104') + #截取中国地图，不在大图上显示九段线区域
  # 添加比例尺
  annotation_scale(location = 'bl') +
  # 添加指北针
  annotation_north_arrow(location = 'tl', which_north = 'false', style = north_arrow_nautical()) +
  # 添加审图号并设定位置
  #labs(caption = '审图号: GS (2019) 1719号')+
  theme(plot.caption = element_text(hjust = 0, vjust = 0.0, size = 6.0)) + ## 设置主题的位置和大小，face = 'bold'为字体加粗
  # 设置坐标轴字体
  theme_linedraw()+
  theme(text = element_text(family = 'sans',size = 10, face = "bold"),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "grey80", linewidth = 0.06), ## 设置轴标签大小
        legend.key = element_rect(fill = "NA"),
        #        legend.position = 'bottom',
        legend.position = "none")
china_map



## 绘制南海九段线小图
nine_map <- ggplot() +
  geom_sf(data = my_dili, aes(fill = QDABB1), size = 0.2, color = 'black', lwd = 0.5) + # 中国地图轮廓
  scale_fill_manual(values = c("I"="#FCB2AF","Ⅱ"="#9BDFDF",
                               "Ⅲ"="#FFE2CE","Ⅳ"= "#C4D8E9",
                               "Ⅴ"= "#BEBCDF","Ⅵ"="#FB8C62","Ⅶ"="#FFF3CA"
  )) +
  # 调整样点信息的外观
  
  geom_sf(data = china, fill = 'NA', size = 0.2, color = 'black') + 
  #geom_sf(data = nine_line, color = 'black', size = 0.5)+
  coord_sf(ylim = c(-4028017, -1577844), xlim = c(117131.4, 2115095), crs = "+proj=laea +lat_0=40 +lon_0=104")+
  theme_linedraw()+
  theme(
    # aspect.ratio = 1.25, #调节长宽比
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    #panel.background = element_blank(),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    #panel.border = element_rect(fill = NA, color = "grey10", linetype = 1, linewidth = 1.0),
    plot.margin = unit(c(0, 0, 0, 0), "mm"),
    legend.position = "none"
  )
nine_map

## 使用cowplot包将中国大陆地图和南海九段线地图拼在一起
gg_inset_map <- ggdraw() +
  draw_plot(china_map,x = 0, y = 0, width = 1, height = 1) +
  draw_plot(nine_map, x = 0.885, y = 0.038, width = 0.1, height = 0.20)
gg_inset_map #Times字体格式无法保存

setwd("D:/pku/01groundwater/02eukaryotes/03mag_abundance")
ggsave("gg_inset_map.pdf",gg_inset_map,width = 5.6,height = 5,units = 'in')

## 保存图片为png格式
png('China_sampling.png', bg = "transparent", width = 9600, height = 6400, res = 2000) ## 设置图片大小和分辨率
## 显示图片
gg_inset_map

## 关闭画框——图片保存到本地工作目录
dev.off()


#3. 36真核MAGs之间的关系，网络图####################################
rm(list=ls())#clear Global Environment
setwd("D:/pku/01groundwater/02eukaryotes/03mag_abundance")#设置工作路径

euMAG_abun <- read.table("merged_rpkm.tsv", sep = '\t',header = T,row.names = 1)

colnames(euMAG_abun) <- gsub(".sorted.RPKM", "", colnames(euMAG_abun))
rownames(euMAG_abun) <- gsub("_renamed", "", rownames(euMAG_abun))



# 查看已加载的包
print(search())

#载入所需R包；
library(igraph)
library(Hmisc)
library(ggplot2)
library(ggpubr)
library(ggsignif)
##################A组#############
#读入OTU绝对丰度表；

euMAG_abun <- euMAG_abun[,colSums(euMAG_abun) != 0]

dereplicated_MAG <- read.table("dereplicated_euMAG.txt")
dereplicated_MAG$V1 <- gsub("_renamed.fa","",dereplicated_MAG$V1)

euMAG_abun <- euMAG_abun[rownames(euMAG_abun) %in% dereplicated_MAG$V1,]

otu_A <- euMAG_abun
otu_A[1:10,1:10]
#转成矩阵，之后的相关性计算需要矩阵对象；
otu_A<-as.matrix(otu_A)
dim(otu_A)
head(otu_A)
#将丰度值大于1的值替换为1，便于计算不同otu的检测率；
#dt<-otu_A
#dt[dt>1]<-1
# #将样本发现率低于20%的otu过滤掉；
#no<-which(rowSums(dt)/ncol(dt)>0.2)
#length(no)
#otu_A<-otu_A[no,]

#otu_A <- subset(otu_A,select = c('A403','A725','A814','B403','B725','B814','C403','C725','C814'))
#sum <- rowSums(otu_A)
#otu_A$sum <- sum

#otu_A <- subset(otu_A, otu_A$sum>=10)

#otu_A <- otu_A[,-10]

#write.table(otu_A,'out_dayu10A.txt',quote = F,sep = '\t')
#otudayu10 <- read.table('out_dayu10A.txt',row.names = 1)
# rownames(otudayu10) <- otudayu10[,1]
# colnames(otudayu10) <- otudayu10[1,]
# otudayu10 <- otudayu10[-1,]
# 
# 
# 
# coou <- rbind(otudayu10,argsubsub)

#计算相关性系数；
sp.cor<- rcorr(t(otu_A),type="spearman")

#提取r、p值矩阵；
r.cor<-sp.cor$r
p.cor<-sp.cor$P

#使用Benjamini-Hochberg("FDR-BH")法进行多重检验校正；
p.adj <- p.adjust(p.cor, method="BH")

#确定物种间存在相互作用关系的阈值，将相关性R矩阵内不符合的数据转换为0
r.cor[p.cor>=0.05|abs(r.cor)<0.6] = 0

#对角线处的1不计
diag(r.cor) <- 0
write.csv(r.cor,file='euMAG_25_相关系数.csv')

#查看过滤后的矩阵；
dim(r.cor)
r.cor[1:10,1:10]

#使用邻接矩阵（即相关系数矩阵）创建网络；
A<-graph.adjacency(r.cor,weight=T,mode="undirected")
#去掉冗余的边（multiple edges、loop edges）；
A<-simplify(A)

#提取权重
df_weight = E(A)$weight
# 设定边的宽度，这里我们将相关系数与边宽进行关联
E(A)$width = abs(df_weight)*5


#生成网络图的结点标签（OTU id）和degree属性；degree表示一个点的中心性
V(A)$label <- V(A)$name
V(A)$degree <- degree(A)

#查看网络图的对象结构;
print(A)

#将网络图导出为"graphml"、"gml"格式，方便导入Gephi中使用；
write_graph(A, "A.graphml", format="graphml")
write_graph(A, "A.gml", format="gml")
#支持的格式有"edgelist", "pajek", "ncol", "lgl","graphml", "dimacs", "gml", "dot", "leda"), 
df_degree<-data.frame(V(A)$G,V(A)$degree)
colnames(df_degree)<-c('group','Degree')
write.table (df_degree, file ="df_degree_A.txt",sep ="\t", quote =F)


#25个真核与细菌的网络#########################################################

#3. 18真核MAGs之间的关系，网络图####################################
rm(list=ls())#clear Global Environment
setwd("D:/pku/01groundwater/02eukaryotes/03mag_abundance")#设置工作路径

euMAG_abun <- read.table("merged_rpkm.tsv", sep = '\t',header = T,row.names = 1)

colnames(euMAG_abun) <- gsub(".sorted.RPKM", "", colnames(euMAG_abun))
rownames(euMAG_abun) <- gsub("_renamed", "", rownames(euMAG_abun))

euMAG18 <- readxl::read_excel("eukcc.xlsx")
euMAG18$bin <- gsub("_renamed.fa", "", euMAG18$bin)

euMAG_abun <- euMAG_abun[rownames(euMAG_abun) %in% euMAG18$bin,]


# 查看已加载的包
print(search())

#载入所需R包；
library(igraph)
library(Hmisc)
library(ggplot2)
library(ggpubr)
library(ggsignif)
##################A组#############
#读入OTU绝对丰度表；

euMAG_abun <- euMAG_abun[,colSums(euMAG_abun) != 0]

#dereplicated_MAG <- read.table("dereplicated_euMAG.txt")
#dereplicated_MAG$V1 <- gsub("_renamed.fa","",dereplicated_MAG$V1)

#euMAG_abun <- euMAG_abun[rownames(euMAG_abun) %in% dereplicated_MAG$V1,]

otu_A <- euMAG_abun
otu_A[1:10,1:10]
#转成矩阵，之后的相关性计算需要矩阵对象；
otu_A<-as.matrix(otu_A)
dim(otu_A)
head(otu_A)
#将丰度值大于1的值替换为1，便于计算不同otu的检测率；
#dt<-otu_A
#dt[dt>1]<-1
# #将样本发现率低于20%的otu过滤掉；
#no<-which(rowSums(dt)/ncol(dt)>0.2)
#length(no)
#otu_A<-otu_A[no,]

#otu_A <- subset(otu_A,select = c('A403','A725','A814','B403','B725','B814','C403','C725','C814'))
#sum <- rowSums(otu_A)
#otu_A$sum <- sum

#otu_A <- subset(otu_A, otu_A$sum>=10)

#otu_A <- otu_A[,-10]

#write.table(otu_A,'out_dayu10A.txt',quote = F,sep = '\t')
#otudayu10 <- read.table('out_dayu10A.txt',row.names = 1)
# rownames(otudayu10) <- otudayu10[,1]
# colnames(otudayu10) <- otudayu10[1,]
# otudayu10 <- otudayu10[-1,]
# 
# 
# 
# coou <- rbind(otudayu10,argsubsub)

#计算相关性系数；
sp.cor<- rcorr(t(otu_A),type="spearman")

#提取r、p值矩阵；
r.cor<-sp.cor$r
p.cor<-sp.cor$P

#使用Benjamini-Hochberg("FDR-BH")法进行多重检验校正；
p.adj <- p.adjust(p.cor, method="BH")

#确定物种间存在相互作用关系的阈值，将相关性R矩阵内不符合的数据转换为0
r.cor[p.cor>=0.05|abs(r.cor)<0.6] = 0

#对角线处的1不计
diag(r.cor) <- 0
write.csv(r.cor,file='euMAG_25_相关系数.csv')

#查看过滤后的矩阵；
dim(r.cor)
r.cor[1:10,1:10]

#使用邻接矩阵（即相关系数矩阵）创建网络；
A<-graph.adjacency(r.cor,weight=T,mode="undirected")
#去掉冗余的边（multiple edges、loop edges）；
A<-simplify(A)

#提取权重
df_weight = E(A)$weight
# 设定边的宽度，这里我们将相关系数与边宽进行关联
E(A)$width = abs(df_weight)*5


#生成网络图的结点标签（OTU id）和degree属性；degree表示一个点的中心性
V(A)$label <- V(A)$name
V(A)$degree <- degree(A)

#查看网络图的对象结构;
print(A)

#将网络图导出为"graphml"、"gml"格式，方便导入Gephi中使用；
write_graph(A, "euMAG18.graphml", format="graphml")
write_graph(A, "A.gml", format="gml")
#支持的格式有"edgelist", "pajek", "ncol", "lgl","graphml", "dimacs", "gml", "dot", "leda"), 
df_degree<-data.frame(V(A)$G,V(A)$degree)
colnames(df_degree)<-c('group','Degree')
write.table (df_degree, file ="df_degree_A.txt",sep ="\t", quote =F)

#18euMAG与细菌MAG的共现网络分析###################################################

setwd("D:/pku/01groundwater/02eukaryotes/03mag_abundance")#设置工作路径

#真核MAG RPKM丰度
euMAG_abun <- read.table("merged_rpkm.tsv", sep = '\t',header = T,row.names = 1)

colnames(euMAG_abun) <- gsub(".sorted.RPKM", "", colnames(euMAG_abun))
rownames(euMAG_abun) <- gsub("_renamed", "", rownames(euMAG_abun))

euMAG18 <- readxl::read_excel("eukcc.xlsx")
euMAG18$bin <- gsub("_renamed.fa", "", euMAG18$bin)

euMAG_abun <- euMAG_abun[rownames(euMAG_abun) %in% euMAG18$bin,]

#细菌MAG RPKM丰度
bacteria_abundance <- read.table("bac.rpkm.tsv",sep = '\t',header = T,row.names = 1)

bac_ani95 <- read.csv("Wdb_ANI95.csv")
bac_ani95$genome <- gsub(".fasta","",bac_ani95$genome)

bacteria_abundance <- bacteria_abundance[rownames(bacteria_abundance) %in% bac_ani95$genome,]

#合并真核与细菌
eu_bac_abun <- rbind(euMAG_abun,bacteria_abundance)

#载入所需R包；
library(igraph)
library(Hmisc)
library(ggplot2)
library(ggpubr)
library(ggsignif)
##################A组#############
#读入OTU绝对丰度表；

eu_bac_abun <- eu_bac_abun[,colSums(eu_bac_abun) != 0]

#dereplicated_MAG <- read.table("dereplicated_euMAG.txt")
#dereplicated_MAG$V1 <- gsub("_renamed.fa","",dereplicated_MAG$V1)

#euMAG_abun <- euMAG_abun[rownames(euMAG_abun) %in% dereplicated_MAG$V1,]

otu_A <- eu_bac_abun
otu_A[1:10,1:10]
#转成矩阵，之后的相关性计算需要矩阵对象；
otu_A<-as.matrix(otu_A)
dim(otu_A)
head(otu_A)
#将丰度值大于1的值替换为1，便于计算不同otu的检测率；
#dt<-otu_A
#dt[dt>1]<-1
# #将样本发现率低于20%的otu过滤掉；
#no<-which(rowSums(dt)/ncol(dt)>0.2)
#length(no)
#otu_A<-otu_A[no,]

#otu_A <- subset(otu_A,select = c('A403','A725','A814','B403','B725','B814','C403','C725','C814'))
#sum <- rowSums(otu_A)
#otu_A$sum <- sum

#otu_A <- subset(otu_A, otu_A$sum>=10)

#otu_A <- otu_A[,-10]

#write.table(otu_A,'out_dayu10A.txt',quote = F,sep = '\t')
#otudayu10 <- read.table('out_dayu10A.txt',row.names = 1)
# rownames(otudayu10) <- otudayu10[,1]
# colnames(otudayu10) <- otudayu10[1,]
# otudayu10 <- otudayu10[-1,]
# 
# 
# 
# coou <- rbind(otudayu10,argsubsub)

#计算相关性系数；接近20000个MAGs，太多了。本地计算困难
sp.cor<- rcorr(t(otu_A),type="spearman")

#提取r、p值矩阵；
r.cor<-sp.cor$r
p.cor<-sp.cor$P

#使用Benjamini-Hochberg("FDR-BH")法进行多重检验校正；
p.adj <- p.adjust(p.cor, method="BH")

#确定物种间存在相互作用关系的阈值，将相关性R矩阵内不符合的数据转换为0
r.cor[p.cor>=0.05|abs(r.cor)<0.6] = 0

#对角线处的1不计
diag(r.cor) <- 0
write.csv(r.cor,file='euMAG_25_相关系数.csv')

#查看过滤后的矩阵；
dim(r.cor)
r.cor[1:10,1:10]

#使用邻接矩阵（即相关系数矩阵）创建网络；
A<-graph.adjacency(r.cor,weight=T,mode="undirected")
#去掉冗余的边（multiple edges、loop edges）；
A<-simplify(A)

#提取权重
df_weight = E(A)$weight
# 设定边的宽度，这里我们将相关系数与边宽进行关联
E(A)$width = abs(df_weight)*5


#生成网络图的结点标签（OTU id）和degree属性；degree表示一个点的中心性
V(A)$label <- V(A)$name
V(A)$degree <- degree(A)

#查看网络图的对象结构;
print(A)

#将网络图导出为"graphml"、"gml"格式，方便导入Gephi中使用；
write_graph(A, "euMAG18.graphml", format="graphml")
write_graph(A, "A.gml", format="gml")
#支持的格式有"edgelist", "pajek", "ncol", "lgl","graphml", "dimacs", "gml", "dot", "leda"), 
df_degree<-data.frame(V(A)$G,V(A)$degree)
colnames(df_degree)<-c('group','Degree')
write.table (df_degree, file ="df_degree_A.txt",sep ="\t", quote =F)




############4. 统计特定MAGs之间的丰度相关性#####################################
setwd("D:/pku/01groundwater/02eukaryotes/03mag_abundance")

euMAG_abun <- read.table("merged_rpkm.tsv", sep = '\t',header = T,row.names = 1)


sample_RPKM_sum <- colSums(euMAG_abun) %>% as.data.frame() %>% setNames('RPKM_sum')
sample_RPKM_sum$sample <- rownames(sample_RPKM_sum)
sample_RPKM_sum$sample <- gsub('.sorted.RPKM','',sample_RPKM_sum$sample)

sample_RPKM_sum$RPKM_ap <- sample_RPKM_sum$RPKM_sum
sample_RPKM_sum$RPKM_ap <- ifelse(sample_RPKM_sum$RPKM_ap > 0, 'present', 'absent') 

###NM23_bin.5与NM21_bin.3##

special_MAG <- euMAG_abun[rownames(euMAG_abun) %in% c("NM23_bin.5_renamed","NM21_bin.3_renamed"),]

special_MAG <- special_MAG[ , colSums(special_MAG)!=0]

plot(special_MAG)

special_MAG <- t(special_MAG) %>% as.data.frame()
# 提取丰度数据
abundance_data <- special_MAG[c("NM23_bin.5_renamed", "NM21_bin.3_renamed")]

# 计算皮尔逊相关系数
correlation <- cor(abundance_data$NM23_bin.5_renamed, abundance_data$NM21_bin.3_renamed, method = "pearson")

# 输出相关系数
correlation

library(ggplot2)

# 绘制散点图
ggplot(abundance_data, aes(x = NM23_bin.5_renamed, y = NM21_bin.3_renamed)) +
  geom_point() +
  labs(title = "Correlation between NM23_bin.5_renamed and NM21_bin.3_renamed",
       x = "NM23_bin.5_renamed abundance",
       y = "NM21_bin.3_renamed abundance") +
  theme_minimal()




