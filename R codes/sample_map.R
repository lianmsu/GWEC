# library(tidyverse)
# library(rnaturalearth)
# library(raster)
# library(ggrepel)
# library(showtext)

library(readxl)
library(ggspatial)
library(ggplot2)
library(cowplot)
library(colorspace)
library(sf)
library(dplyr)



### 设置工作路径
setwd("D:/pku/01groundwater/03eu_pre/10sample_map")

## 读取采样点经纬度数据
### 读取数据
# scatter_df <- read_xlsx("location.xlsx", sheet = 1) ### 提取plotlocation数据文件中第1个sheet中的数据
# ### 样点数据包括：经度lon和纬度lat信息
# scatter_df_tro <- st_as_sf(scatter_df, coords = c("lng", "lat"), crs = 4326) ## 设定参考坐标系


##
# sample_df <- read_xlsx('GW_DATA_lpw.xlsx')
# sample_df2 <- read_xlsx('env.xlsx',sheet = 3) %>% select(ID,zone,新建改建,经度,纬度)
# 
# lat_lng1 <- sample_df2[sample_df2$ID %in% sample_df$sample,]
# # 删除所有存在NA的行
# lat_lng1 <- na.omit(lat_lng1)
# colnames(lat_lng1) <- c('ID','zone','type','lon','lat')
# 
# 
# scatter_df_tro <- st_as_sf(lat_lng1, coords = c("lon", "lat"), crs = 4326) ## 设定参考坐标系

#global samples
global_sample_df <- read_xlsx('样品信息.xlsx')
global_sample_df <- global_sample_df[,c(2,5,4,3)]
colnames(global_sample_df) <- c("Sample ID","Longitude","Latitude","Location")

#师兄数据
genome_info <- read_xlsx('Geoinfo.xlsx')
zone_info <- read_xlsx('Zone.xlsx')
sample_info <- merge(genome_info,zone_info,by = 'Sample ID')
sample_info$Location <- 'China'
sample_info <- sample_info[,c(1,2,3,5)]

sample_info <- rbind(sample_info,global_sample_df)

#所有样品来自的国家的统计
sample_info_country <- sample_info %>% count(Location, name = "Count")

custom_colors <- c(
  "#9ecae1",  # 淡蓝色
  "#fdae6b",  # 淡橙色
  "#a1d99b",  # 淡绿色
  "#fc9272",  # 淡红色
  "#bcbddc",  # 淡紫色
  "#ffeda0",  # 淡黄色
  "#c6dbef",  # 浅蓝色
  "#fdd0a2",  # 浅橙色
  "#c7e9c0",  # 浅绿色
  "#fcbba1",  # 浅红色
  "#dadaeb",  # 浅紫色
  "#ffa69e"   # 浅黄色
)
# 绘制水平柱状图
sample_country <- 
ggplot(sample_info_country, aes(x = Location, y = Count, fill = Location)) +
  geom_bar(stat = "identity",width = 0.8) +
  geom_text(aes(label = Count), hjust = -0.2, size = 3.5, color = "black") +  # 在柱子右侧显示数量
  coord_flip() +  # 将柱状图水平显示
  scale_y_continuous(limits = c(0, 650), expand = c(0, 0)) +  # 设置 y 轴范围和扩展
  scale_fill_manual(values = custom_colors) +  # 手动设置颜色
  labs(title = "",
       x = "",
       y = "Number of samples") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # 标题居中，加粗，字体大小为16
    panel.grid.major = element_blank(),  # 去除主要网格线
    panel.grid.minor = element_blank(),  # 去除次要网格线
    axis.text.x = element_blank(),  # 去除纵轴刻度标签
    axis.ticks.x = element_blank(),  # 去除纵轴刻度线
    #axis.ticks.y = element_line(color = "black"),  # 保留 x 轴刻度线
    legend.position = "none"  # 不显示图例
  )
sample_country
setwd("D:/pku/01groundwater/03eu_pre/10sample_map")
ggsave("sample_country.pdf",sample_country,width = 4.22,height = 2.5,units = 'in')

write.csv(sample_info_country,"fig1a.csv")

#
scatter_df_tro <- st_as_sf(sample_info,
                           coords = c("Longitude", "Latitude"),
                           crs = 4326,na.fail = F) ## 设定参考坐标系


### 加载标准的中国地图底图
# china_shp <- "中国省级地图GS（2019）1719号.geojson"
# nine <- "九段线GS（2019）1719号.geojson"
china <- sf::read_sf('https://geo.datav.aliyun.com/areas_v3/bound/100000_full.json')

global <- sf::read_sf('https://echarts.apache.org/examples/data/asset/geo/world.json')

global2 <- sf::read_sf('https://raw.githubusercontent.com/iuvc/magicJs/refs/heads/main/public/worldMap/world-min.json')

#nine_line <- sf::read_sf(nine)
# head(china) ## 查看中国地图行政区划的信息

#加载地理分区
setwd("D:/pku/01groundwater/03eu_pre/10sample_map")
dilifenqu <- sf::read_sf("./bar/bar.shp")
#dilifenqu <- dilifenqu[dilifenqu$QDABB1!='',]

my_dili <- dilifenqu[c(1:37,40,41),]

# 统计每个 geometry 出现的次数
size_counts <- scatter_df_tro %>%
  group_by(geometry) %>%
  summarise(size = n(), .groups = 'drop')

library(scales)
size_counts$size <- rescale(size_counts$size,to = c(1,10))
# 将统计结果合并到原始数据框


global_map <- 
ggplot() +
  geom_sf(data = global2, fill = 'NA', size = 0.5,lwd = 0.2, color = 'black') + 
  geom_sf(data = china, fill = 'NA', size = 0.2,lwd = 0.2, color = 'black') + 
  geom_sf(data = my_dili, aes(fill = QDABB1), size = 0.2, color = 'black', lwd = 0.2) + # 中国地图轮廓
  scale_fill_manual(values = c("I"="#FCB2AF","Ⅱ"="#9BDFDF",
                               "Ⅲ"="#FFE2CE","Ⅳ"= "#C4D8E9",
                               "Ⅴ"= "#BEBCDF","Ⅵ"="#FB8C62","Ⅶ"="#FFF3CA"
  )) +
  # 调整样点信息的外观
  geom_sf(data = size_counts, 
          aes(color = 'lightblue'),
          shape = 21,           # 设置形状为带边框的圆形
          size = size_counts$size,           # 增大点的大小
          stroke = 0.2,         # 设置点边界的宽度
          color = 'blue',      # 边界颜色设置为黑色
          fill = '#00b4d8',        # 填充颜色为蓝色
          alpha = 0.7) +        # 设置透明度
  # geom_sf(data = scatter_df_tro, 
  #         #aes(color=`Geo-environmental zone`),
  #         color = 'blue', 
  #         shape = 20,
  #         size = 0.8) + # 添加样点信息

  #  coord_sf(ylim = c(-2387082, 1654989), crs = '+proj=laea +lat_0=40 +lon_0=104') + #截取中国地图，不在大图上显示九段线区域
  # 添加比例尺
  annotation_scale(location = 'bl') +
  # 添加指北针
  annotation_north_arrow(location = 'tl',
                         width=unit(1, "cm"), 
                         height=unit(1, "cm"), 
                         which_north = 'false', style = north_arrow_nautical()) +
  # 添加审图号并设定位置
  #labs(caption = '审图号: GS (2019) 1719号')+
  theme(plot.caption = element_text(hjust = 0, vjust = 0.0, size = 6.0)) + ## 设置主题的位置和大小，face = 'bold'为字体加粗
  # 设置坐标轴字体
  theme_linedraw()+
  theme(text = element_text(size = 10, face = "bold"),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "grey80", linewidth = 0.06), ## 设置轴标签大小
        legend.key = element_rect(fill = "NA"),
        #legend.position = 'bottom',
        legend.position = "none")
global_map

setwd("D:/pku/01groundwater/03eu_pre/10sample_map")
ggsave("global_sample_map.pdf",global_map,width = 8,height = 7,units = 'in')


global_map2 <- 
  ggplot() +
  geom_sf(data = global2, fill = 'NA', size = 0.5,lwd = 0.2, color = 'black') + 
  geom_sf(data = china, fill = 'NA', size = 0.2,lwd = 0.2, color = 'black') + 
  #geom_sf(data = my_dili, aes(fill = QDABB1), size = 0.2, color = 'black', lwd = 0.2) + # 中国地图轮廓
  # scale_fill_manual(values = c("I"="#FCB2AF","Ⅱ"="#9BDFDF",
  #                              "Ⅲ"="#FFE2CE","Ⅳ"= "#C4D8E9",
  #                              "Ⅴ"= "#BEBCDF","Ⅵ"="#FB8C62","Ⅶ"="#FFF3CA"
  # )) +
  # 调整样点信息的外观
  geom_sf(data = scatter_df_tro, 
          aes(color = 'lightblue'),
          shape = 21,           # 设置形状为带边框的圆形
    #      size = size_counts$size,           # 增大点的大小
          stroke = 0.2,         # 设置点边界的宽度
          color = 'blue',      # 边界颜色设置为黑色
          fill = '#00b4d8',        # 填充颜色为蓝色
          alpha = 0.7) +        # 设置透明度
  # geom_sf(data = scatter_df_tro, 
  #         #aes(color=`Geo-environmental zone`),
  #         color = 'blue', 
  #         shape = 20,
  #         size = 0.8) + # 添加样点信息
  
  #  coord_sf(ylim = c(-2387082, 1654989), crs = '+proj=laea +lat_0=40 +lon_0=104') + #截取中国地图，不在大图上显示九段线区域
  # 添加比例尺
  annotation_scale(location = 'bl') +
  # 添加指北针
  annotation_north_arrow(location = 'tl',
                         width=unit(1, "cm"), 
                         height=unit(1, "cm"), 
                         which_north = 'false', style = north_arrow_nautical()) +
  # 添加审图号并设定位置
  #labs(caption = '审图号: GS (2019) 1719号')+
  theme(plot.caption = element_text(hjust = 0, vjust = 0.0, size = 6.0)) + ## 设置主题的位置和大小，face = 'bold'为字体加粗
  # 设置坐标轴字体
  theme_linedraw()+
  theme(text = element_text(size = 10, face = "bold"),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "grey80", linewidth = 0.06), ## 设置轴标签大小
        legend.key = element_rect(fill = "NA"),
        #legend.position = 'bottom',
        legend.position = "none")
global_map2

#三个点属于瑞典，被错误注释在芬兰。美国一个点没有坐标

setwd("D:/pku/01groundwater/03eu_pre/10sample_map")
ggsave("global_sample_map2.pdf",global_map2,width = 8,height = 7,units = 'in')





# 图例绘制

library(ggplot2)
library(sf)

legend <-
ggplot() +
  # 绘制全球地图
  geom_sf(data = global2, fill = 'NA', size = 0.5, lwd = 0.2, color = 'black') + 
  # 绘制中国地图
  geom_sf(data = china, fill = 'NA', size = 0.2, lwd = 0.2, color = 'black') + 
  # 绘制自定义区域（my_dili），填充颜色根据 QDABB1 列
  geom_sf(data = my_dili, aes(fill = QDABB1), size = 0.2, color = 'black', lwd = 0.2) +
  # 设置填充颜色的图例
  scale_fill_manual(values = c("I" = "#FCB2AF", "Ⅱ" = "#9BDFDF",
                               "Ⅲ" = "#FFE2CE", "Ⅳ" = "#C4D8E9",
                               "Ⅴ" = "#BEBCDF", "Ⅵ" = "#FB8C62", "Ⅶ" = "#FFF3CA")) +
  # 绘制点数据（size_counts），点大小根据 size 列
  geom_sf(data = size_counts, 
          aes(size = size),  # 将 size 映射到 aes() 中
          shape = 21,        # 设置形状为带边框的圆形
          stroke = 0.2,      # 设置点边界的宽度
          color = 'blue',    # 边界颜色设置为蓝色
          fill = '#00b4d8',  # 填充颜色为浅蓝色
          alpha = 0.7) +     # 设置透明度
  # 设置点大小的连续图例
  scale_size_continuous(
    name = "Dot Size",       # 图例标题
    range = c(1, 10),        # 点大小的范围
    breaks = seq(min(size_counts$size), max(size_counts$size), length.out = 5),  # 图例的断点
    guide = guide_legend(override.aes = list(color = "blue", fill = "#00b4d8"))  # 自定义图例外观
  ) +
  # 添加比例尺
  annotation_scale(location = 'bl') +
  # 添加指北针
  annotation_north_arrow(location = 'tl',
                         width = unit(1, "cm"), 
                         height = unit(1, "cm"), 
                         which_north = 'false', style = north_arrow_nautical()) +
  # 设置主题
  theme_linedraw() +
  theme(
    text = element_text(size = 10, face = "bold"),
    panel.background = element_rect(fill = NA),
    panel.grid.major = element_line(colour = "grey80", linewidth = 0.06),
    legend.key = element_rect(fill = "NA"),
    legend.position = "right"  # 将图例放在右侧
  )
legend
setwd("D:/pku/01groundwater/03eu_pre/10sample_map")
ggsave("global_sample_map_legend.pdf",legend,width = 8,height = 7,units = 'in')







## 绘制中国大陆地图并添加采样点信息
china_map <- ggplot() +
  geom_sf(data = my_dili, aes(fill = QDABB1), size = 0.2, color = 'black', lwd = 0.5) + # 中国地图轮廓
  scale_fill_manual(values = c("I"="#FCB2AF","Ⅱ"="#9BDFDF",
                               "Ⅲ"="#FFE2CE","Ⅳ"= "#C4D8E9",
                               "Ⅴ"= "#BEBCDF","Ⅵ"="#FB8C62","Ⅶ"="#FFF3CA"
  )) +
  # 调整样点信息的外观
  geom_sf(data = scatter_df_tro, 
          aes(color = 'blue'),
          shape = 21,           # 设置形状为带边框的圆形
          size = 1.5,           # 增大点的大小
          stroke = 0.5,         # 设置点边界的宽度
          color = 'blue',      # 边界颜色设置为黑色
          fill = '#00b4d8',        # 填充颜色为蓝色
          alpha = 0.7) +        # 设置透明度
  # geom_sf(data = scatter_df_tro, 
  #         #aes(color=`Geo-environmental zone`),
  #         color = 'blue', 
  #         shape = 20,
  #         size = 0.8) + # 添加样点信息
  geom_sf(data = china[c(21,32,35),], fill = 'NA', size = 0.5,lwd = 0.5, color = 'black') + 
#  coord_sf(ylim = c(-2387082, 1654989), crs = '+proj=laea +lat_0=40 +lon_0=104') + #截取中国地图，不在大图上显示九段线区域
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

ggsave("s20_map2_2.pdf",gg_inset_map,width = 5.6,height = 5,units = 'in')

## 保存图片为png格式
png('China_sampling.png', bg = "transparent", width = 9600, height = 6400, res = 2000) ## 设置图片大小和分辨率
## 显示图片
gg_inset_map

## 关闭画框——图片保存到本地工作目录
dev.off()
